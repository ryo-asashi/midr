#' Create Variable Encoders and Encode Data
#'
#' @description
#' \code{make_encoder()} creates a variable encoder, which can then be used to convert a vector or data frame into a design matrix.
#'
#' \code{encode()} applies an encoder to data, returning the encoded design matrix.
#'
#' @param x a vector, data frame, or frame specification to be encoded.
#' @param ... further arguments passed to or from other methods.
#'
#' @details
#' For numeric variables, the encoding method is controlled by \code{type}:
#' When \code{type} = "linear" (or \code{1}), the variable's effect is modeled as a piecewise linear function with \code{k} knots including both ends.
#' For each value, the encoder finds the two nearest knots and assigns a weight to each based on its relative position.
#' This results in a design matrix where each row has at most two non-zero values that sum to \code{1}, creating a smooth, continuous representation.
#' When \code{type} = "constant" (or \code{0}), the variable's effect is modeled as a step function by dividing its range into \code{k} intervals (bins).
#' The encoder assigns \code{1} to the corresponding bin column and \code{0} elsewhere.
#'
#' For factor, character, or logical variables, one-hot encoding is applied:
#' Each unique level becomes a column in the output matrix. When a variable has high cardinality, you can use \code{lump} and \code{k} to reduce dimensionality.
#'
#' @examples
#' data(iris, package = "datasets")
#'
#' # Create an encoder for a quantitative variable
#' enc_num <- make_encoder(iris$Sepal.Length, k = 5L, tag = "Sepal.Length")
#' enc_num
#' encode(enc_num, c(4:8, NA, Inf))
#'
#' # Create an encoder for a qualitative variable with lumping
#' enc_fac <- make_encoder(iris$Species, k = 2L, lump = "rank", tag = "Species")
#' enc_fac
#' encode(enc_fac, iris$Species[c(1, 51, 101)])
#'
#' # Directly encode a raw vector
#' head(encode(iris$Petal.Width, k = 4L, type = "constant"))
#' @returns
#' \code{make_encoder()} returns an object of class "encoder", which is a list containing:
#' \item{frame}{a single-column data.frame containing the encoding knots, interval labels, or factor levels.}
#' \item{n}{the number of encoding levels (columns in the design matrix).}
#' \item{type}{a character string describing the encoding type ("linear", "constant", "factor", or "null").}
#' \item{tag}{a character string specifying the variable name.}
#' \item{envir}{an environment enclosing the \code{transform()} and \code{encode()} functions.}
#' \item{transform}{a function \code{transform(x, ...)} that preprocesses a vector before encoding.}
#' \item{encode}{a function \code{encode(x, ...)} that converts a vector into the encoded matrix.}
#'
#' \code{encode()} returns a numeric matrix (the encoded design matrix).
#'
#' @export make_encoder
#'
make_encoder <- function(x, ...)
  UseMethod("make_encoder")


#' @exportS3Method midr::make_encoder
#'
make_encoder.default <- function(x, ...) {
  if (!is.null(x) && is.atomic(x)) {
    return(make_encoder(as.factor(x), ...))
  }
  stop(
    sprintf("make_encoder() cannot handle objects of class '%s'.", class(x)[1L]),
    call. = FALSE
  )
}


#' @exportS3Method midr::make_encoder
#'
make_encoder.data.frame <- function(x, ...) {
  mapply(FUN = function(nm, vec) make_encoder(vec, tag = nm, ...),
         clean_names(x), x, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

#' @exportS3Method midr::make_encoder
#'
make_encoder.AsIs <- function(x, ...) {
  make_encoder(unclass(x), ...)
}

#' @exportS3Method midr::make_encoder
#'
make_encoder.matrix <- function(x, ...) {
  make_encoder(as.data.frame(x), ...)
}


#' @exportS3Method midr::make_encoder
#'
make_encoder.encoder <- function(x, tag = NULL, ...) {
  if (!is.null(tag)) {
    x$tag <- tag
    colnames(x$frame) <- tag
  }
  x
}


# closure factory methods: encoding frame -->> encoder

#' @exportS3Method midr::make_encoder
#'
make_encoder.numeric_frame <- function(x, tag = NULL, ...) {
  type <- attr(x, "type")
  tag <- tag %||% attr(x, "tag") %||% "x"
  colnames(x) <- tag
  envir <- new.env(parent = getNamespace("midr"))
  envir$knots <- knots <- attr(x, "knots")
  envir$breaks <- breaks <- attr(x, "breaks")
  envir$range <- attr(x, "range") %||% range(breaks)
  envir$digits <- digits <- attr(x, "digits")
  envir$labels <- attr(x, "labels")
  n_knots <- length(knots)
  transform <- function(x, ...) x
  environment(transform) <- envir
  envir$transform <- transform
  if (type == "linear") {
    encode <- function(x, ...) {
      n <- length(x)
      n_knots <- length(knots)
      x <- transform(x)
      mat <- matrix(0, nrow = n, ncol = n_knots)
      itv <- findInterval(x, knots)
      mat[itv == 0L, 1L] <- 1
      mat[itv == n_knots, n_knots] <- 1
      ok <- (!is.na(x) & itv > 0L & itv < n_knots)
      l <- knots[itv[ok]]
      r <- knots[itv[ok] + 1L]
      prop <- (r - x[ok]) / (r - l)
      if (!is.null(digits)) {
        rounder <- 10 ^ digits
        prop <- floor(prop * rounder + 0.5) / rounder
      }
      mat[cbind(which(ok), itv[ok])] <- prop
      mat[cbind(which(ok), itv[ok] + 1L)] <- 1 - prop
      colnames(mat) <- labels %||% format(knots, digits = 6L)
      mat
    }
  } else if (type == "constant") {
    encode <- function(x, ...) {
      n <- length(x)
      n_knots <- length(knots)
      x <- transform(x)
      mat <- matrix(0, nrow = n, ncol = n_knots)
      itv <- findInterval(x, breaks, all.inside = TRUE)
      ok <- !is.na(x)
      mat[cbind(which(ok), itv[ok])] <- 1
      colnames(mat) <- labels
      mat
    }
  } else {
    encode <- function(x, ...) {
      mat <- matrix(0, nrow = length(x), ncol = 1L)
      colnames(mat) <- "Void"
      mat
    }
  }
  environment(encode) <- envir
  attributes(x) <- list(
    names = names(x), row.names = attr(x, "row.names"), class = "data.frame"
  )
  enc <- list(
    frame = x, n = n_knots, type = type, tag = tag,
    envir = envir, transform = transform, encode = encode
  )
  structure(enc, class = "encoder")
}


#' @exportS3Method midr::make_encoder
#'
make_encoder.factor_frame <- function(x, tag = NULL, ...) {
  tag <- tag %||% attr(x, "tag") %||% "x"
  colnames(x) <- tag
  envir <- new.env(parent = getNamespace("midr"))
  envir$levels <- levels <- attr(x, "levels")
  envir$orig_levels <- orig_levels <- attr(x, "orig_levels")
  envir$others <- others <- attr(x, "others")
  envir$map <- map <- attr(x, "map")
  n_levels <- length(levels)
  type <- if (n_levels == 0L) "null" else "factor"
  if (type == "factor") {
    transform <- function(x, lumped = TRUE, ...) {
      x <- as.character(x)
      if (!lumped) {
        return(factor(x, orig_levels))
      }
      if (!is.null(map)) {
        x_mapped <- map[x]
        ok <- !is.na(x_mapped)
        x[ok] <- x_mapped[ok]
      }
      x <- factor(x, levels)
      if (!is.null(others)) {
        x[is.na(x)] <- others
      }
      x
    }
    encode <- function(x, ...) {
      n <- length(x)
      n_levels <- length(levels)
      x <- transform(x)
      ok <- !is.na(x)
      mat <- matrix(0, nrow = n, ncol = n_levels)
      mat[cbind(which(ok), as.integer(x[ok]))] <- 1
      colnames(mat) <- levels
      mat
    }
  } else {
    transform <- function(x, lumped = TRUE, ...) x
    encode <- function(x, ...) {
      mat <- matrix(0, nrow = length(x), ncol = 1L)
      colnames(mat) <- "Void"
      mat
    }
  }
  environment(transform) <- envir
  envir$transform <- transform
  environment(encode) <- envir
  attributes(x) <- list(
    names = names(x), row.names = attr(x, "row.names"), class = "data.frame"
  )
  enc <- list(
    frame = x, n = n_levels, type = type, tag = tag,
    envir = envir, transform = transform, encode = encode
  )
  structure(enc, class = "encoder")
}


# data-driven calculation methods: atomic vector -->> encoding frame

#' @rdname make_encoder
#'
#' @param k an integer specifying the coarseness of the encoding (max knots, bins, or levels). If not positive or \code{NULL}, all unique values of \code{x} are used.
#' @param type a character string or an integer specifying the encoding method for numeric variables: "linear" (\code{1}; default) or "constant" (\code{0}).
#' @param split a character string specifying the splitting strategy for numeric variables: "quantile" (default) or "uniform".
#' @param weights an optional numeric vector of sample weights for \code{x}.
#' @param digits an integer specifying the rounding digits for piecewise linear encoding (\code{type = "linear"}).
#' @param tag the name of the variable.
#' @param spec an atomic vector specifying the encoding frame, or an "encoder" object.
#'
#' @exportS3Method midr::make_encoder
#'
make_encoder.numeric <- function(
    x, k = 25L, type = c("linear", "constant", "null"),
    split = c("quantile", "uniform"), weights = NULL, digits = NULL, tag = "x",
    spec = NULL, ...
) {
  if (!is.null(spec)) {
    if (is.atomic(spec))
      class(spec) <- unique(c("numeric_spec", class(spec)))
    return(make_encoder(spec, type = type, digits = digits, tag = tag, ...))
  }
  if (is.numeric(type)) type <- if (type == 0L) "constant" else "linear"
  type <- match.arg(type)
  split <- match.arg(split)
  if (any(is.infinite(x))) {
    xmax <- .Machine$double.xmax
    x[xmax < x] <- xmax
    x[x < -xmax] <- -xmax
  }
  uni <- sort(unique(round(x, 12L)))
  nuni <- length(uni)
  rng <- if (nuni > 0L) c(uni[1L], uni[nuni]) else c(NA_real_, NA_real_)
  if (nuni == 0L || type == "null") {
    knots <- 0
  } else if (is.null(k) || k <= 0L) {
    knots <- uni
  } else {
    if (type == "linear") {
      prob <- if (k == 1L) 0.5 else seq.int(0, 1, length.out = k)
    } else {
      prob <- seq.int(0, 1, length.out = 2L * k + 1L)
      prob <- prob[seq.int(2L, 2L * k, by = 2L)]
    }
    if (split == "quantile") {
      knots <- weighted_quantile(
        x = x, w = weights, probs = prob, na.rm = TRUE, names = FALSE, type = 1L
      )
    } else {
      knots <- prob * (uni[nuni] - uni[1L]) + uni[1L]
    }
    knots <- unique(round(knots, 12L))
  }
  n_knots <- length(knots)
  if (any(is.na(knots)) || n_knots == 0L) {
    knots <- 0
    n_knots <- 1L
    breaks <- c(-1, 1)
    type <- "null"
  } else if (n_knots == 1L) {
    if (nuni == 1L) {
      breaks <- c(uni[1L] - 1L, uni[1L] + 1L)
    } else {
      breaks <- c(uni[1L], uni[nuni])
    }
    type <- "null"
  } else {
    breaks <- (knots[1L:(n_knots - 1L)] + knots[2L:n_knots]) / 2
    breaks <- c(uni[1L], breaks, uni[nuni])
  }
  frame <- new_numeric_frame(knots, breaks, type, digits, tag, range = rng)
  make_encoder(frame, ...)
}


#' @rdname make_encoder
#'
#' @param lump a character string specifying the lumping strategy for factor variables: \code{"none"}, \code{"auto"}, \code{"rank"}, or \code{"order"}.
#' @param others a character string for the catch-all level (used when \code{lump = "rank"}).
#' @param sep a character string used to separate start and end levels when merging ordered factors.
#'
#' @exportS3Method midr::make_encoder
#'
make_encoder.factor <- function(
    x, k = 25L, lump = c("none", "auto", "rank", "order"), others = "others",
    sep = ">", weights = NULL, tag = "x", spec = NULL, ...
) {
  if (!is.null(spec)) {
    if (is.atomic(spec))
      class(spec) <- unique(c("factor_spec", class(spec)))
    return(make_encoder(spec, others = others, tag = tag, ...))
  }
  lump <- match.arg(lump)
  if (lump == "auto") {
    lump <- if (is.ordered(x)) "order" else "rank"
  }
  orig_levels <- levels(as.factor(x))
  n_orig <- length(orig_levels)
  if (n_orig == 0L) {
    levels <- "Void"
    others <- NULL
    map <- NULL
  } else if (lump == "none" || is.null(k) || k < 1L || n_orig <= k) {
    levels <- orig_levels
    others <- NULL
    map <- NULL
  } else if (lump == "order") {
    if (!is.ordered(x)) x <- ordered(x)
    tbl <- weighted_tabulate(bin = match(x, orig_levels), w = weights, nbins = n_orig)
    wcum <- cumsum(tbl)
    wsum <- wcum[length(wcum)]
    cuts <- wsum * seq(0, 1, length.out = k + 1L)
    grps <- findInterval(wcum, cuts, left.open = TRUE, all.inside = TRUE)
    grps <- factor(grps, levels = sort.int(unique(grps)))
    levels <- vapply(
      X = split(orig_levels, grps),
      FUN = function(z) {
        if (length(z) == 1L) z else paste0(z[1L], sep, z[length(z)])
      },
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE
    )
    others <- NULL
    map <- stats::setNames(rep(levels, times = tabulate(grps)), orig_levels)
  } else {
    tbl <- weighted_tabulate(bin = match(x, orig_levels), w = weights, nbins = n_orig)
    ord <- order(tbl, decreasing = TRUE)
    levels <- orig_levels[ord][seq_len(k - 1L)]
    levels <- orig_levels[orig_levels %in% levels]
    if (!is.null(others)) levels <- unique(c(levels, others))
    map <- NULL
  }
  frame <- new_factor_frame(levels, orig_levels, others, map, tag)
  make_encoder(frame, ...)
}


# frame specification methods: atomic vector -->> encoding frame

#' @exportS3Method midr::make_encoder
#'
make_encoder.numeric_spec <- function(
    x, type = c("linear", "constant", "null"), digits = NULL, tag = "x", ...
) {
  if (is.numeric(type)) type <- if (type == 0L) "constant" else "linear"
  type <- match.arg(type)
  x_num <- as.numeric(x)
  if (type == "constant") {
    breaks <- sort(unique(x_num))
    n_br <- length(breaks)
    if (n_br < 2L) stop("'breaks' must contain at least 2 unique values", call. = FALSE)
    knots <- (breaks[1L:(n_br - 1L)] + breaks[2L:n_br]) / 2
  } else {
    knots <- sort(unique(x_num))
    n_knots <- length(knots)
    if (n_knots < 1L) stop("'knots' must contain at least 1 value", call. = FALSE)
    if (n_knots == 1L) {
      breaks <- c(knots[1L] - 1, knots[1L] + 1)
      type <- "null"
    } else {
      breaks <- (knots[1L:(n_knots - 1L)] + knots[2L:n_knots]) / 2
      breaks <- c(knots[1L], breaks, knots[n_knots])
    }
  }
  n_knots <- length(knots)
  if (length(breaks) != n_knots + 1L) {
    stop("the length of 'breaks' must be the length of 'knots' plus 1", call. = FALSE)
  }
  ng <- c(knots <= breaks[1L:n_knots], knots > breaks[2L:(n_knots + 1L)])
  ng[1L] <- (knots[1L] < breaks[1L])
  if (any(ng)) {
    stop("representative values must be included in (min, max] of each bin", call. = FALSE)
  }
  frame <- new_numeric_frame(knots, breaks, type, digits, tag, range = range(breaks))
  make_encoder(frame, ...)
}


#' @exportS3Method midr::make_encoder
#'
make_encoder.factor_spec <- function(x, others = NULL, tag = "x", ...) {
  levels <- as.character(unique(x))
  if (!is.null(others)) {
    levels <- unique(c(levels, others))
  }
  orig_levels <- names(x)
  map <- if (length(orig_levels) > 0L) {
    stats::setNames(as.character(x), orig_levels)
  } else NULL
  if (is.null(orig_levels)) orig_levels <- levels
  frame <- new_factor_frame(levels, orig_levels, others, map, tag)
  make_encoder(frame, ...)
}


# misc methods for encoders

#' @exportS3Method base::print
#'
print.encoder <- function(x, digits = NULL, ...) {
  ty <- switch(
    x$type,
    null = c("Null", add_s(x$n, "class", "es")),
    linear = c("Linear", add_s(x$n, "knot")),
    constant = c("Constant", add_s(x$n, "interval")),
    factor = c("Factor", add_s(x$n, "level"))
  )
  cat("\n", ty[1L], " encoder with ", ty[2L], "\n", sep = "")
  if (x$type != "null") {
    cat("\nFrame:\n")
    print.data.frame(x$frame, digits = digits)
    cat("\n")
  }
  invisible(x)
}


#' @exportS3Method base::as.data.frame
#'
as.data.frame.encoder <- function(x, ...) {
  x$frame
}


#' @rdname make_encoder
#'
#' @param object an atomic vector, "encoder" object, or other object containing encoding information. If an atomic vector is provided, an encoder is created and applied.
#' @param variable a character string specifying the variable name to encode.
#' @param order an integer specifying which encoder should be used.
#' @param data a data frame containing the variables to be encoded.
#'
#' @export encode
#'
encode <- function(object, ...)
  UseMethod("encode")


#' @exportS3Method midr::encode
#'
encode.default <- function(object, ...) {
  if (!is.null(object) && is.atomic(object)) {
    return(make_encoder(object, ...)$encode(object, ...))
  }
  cls <- class(object)[1L]
  stop(sprintf("encode() cannot handle objects of class '%s'.", cls),
       call. = FALSE)
}


#' @rdname make_encoder
#'
#' @exportS3Method midr::encode
#'
encode.encoder <- function(object, x, ...) {
  object$encode(x, ...)
}



get_encoder <- function(object, variable, order = 1L) {
  if (is.null(order)) {
    out <- object$encoders$main.effects[[variable]] %||%
      object$encoders$interactions[[variable]]
    return(out)
  }
  ostr <- if (identical(order, 1L) || identical(order, 1)) {
    "main.effects"
  } else if (identical(order, 2L) || identical(order, 2)) {
    "interactions"
  } else {
    stop("'order' must be NULL, 1 (main effect) or 2 (interaction)", call. = FALSE)
  }
  object$encoders[[ostr]][[variable]]
}


#' @rdname make_encoder
#'
#' @exportS3Method midr::encode
#'
encode.mid <- function(
    object, variable, order = 1L, data = NULL, ...
) {
  if (is.null(data)) {
    stop("'data' must be supplied", call. = FALSE)
  }
  enc <- get_encoder(object = object, variable = variable, order = order)
  if (is.null(enc)) {
    stop(sprintf("encoder for '%s' is not found in 'object'", variable),
         call. = FALSE)
  }
  if (!(variable %in% colnames(data))) {
    stop(sprintf("column '%s' is not found in 'data'", variable), call. = FALSE)
  }
  vec <- if (is.matrix(data)) data[, variable] else data[[variable]]
  encode(enc, vec, ...)
}


#' @exportS3Method midr::encode
#'
encode.midrib <- function(object, ...) {
  encode.mid(object, ...)
}


# helpers

format_interval_labels <- function(breaks) {
  n <- length(breaks) - 1L
  for (d in c(6L, 8L, 12L)) {
    bs <- format(breaks, digits = d, trim = TRUE)
    bs[c(1L, length(bs))] <- c("-Inf", "Inf")
    labs <- paste0("[", bs[1L:n], ", ", bs[2L:(n + 1L)], ")")
    if (!anyDuplicated(labs)) return(labs)
  }
  make.unique(labs)
}


new_numeric_frame <- function(knots, breaks, type, digits, tag, range) {
  if (type == "constant") {
    labels <- format_interval_labels(breaks)
    frame <- data.frame(factor(labels, levels = labels), stringsAsFactors = FALSE)
  } else {
    labels <- NULL
    frame <- data.frame(knots)
  }
  colnames(frame) <- tag
  structure(
    frame,
    class = c("numeric_frame", "data.frame"),
    type = type,
    knots = knots,
    breaks = breaks,
    range = range,
    digits = digits,
    labels = labels,
    tag = tag
  )
}


new_factor_frame <- function(levels, orig_levels, others, map, tag) {
  frame <- data.frame(factor(levels, levels = levels), stringsAsFactors = FALSE)
  colnames(frame) <- tag
  structure(
    frame,
    class = c("factor_frame", "data.frame"),
    levels = levels,
    orig_levels = orig_levels,
    others = others,
    map = map,
    tag = tag
  )
}


encoder_levels <- function(x, original = FALSE) {
  if (x$type == "factor") {
    if (original) x$envir$orig_levels else x$envir$levels
  } else if (x$type == "constant") {
    x$envir$labels %||% levels(x$frame[[1L]])
  } else {
    NULL
  }
}


encoder_knots <- function(x) {
  x$envir$knots
}


encoder_breaks <- function(x) {
  x$envir$breaks
}


encoder_range <- function(x) {
  if (x$type == "factor") {
    lvs <- x$envir$levels
    if (length(lvs) > 0L) c(lvs[1L], lvs[length(lvs)]) else NULL
  } else {
    x$envir$range %||% range(x$envir$breaks)
  }
}


#' Weighted Sample Quantile
#'
#' @description
#' \code{weighted_quantile()} produces weighted sample quantiles corresponding to the given probabilities.
#'
#' @details
#' \code{weighted_quantile()} is a wrapper function of \code{stats::quantile()} for weighted quantiles.
#' For the weighted quantile, only the "type 1" quantile, the inverse of the empirical distribution function, is available.
#' This function is used in \code{make_encoder()} to enable weights-based encoding.
#'
#' @param x a numeric vector whose weighted sample quantiles are wanted.
#' @param w a numeric vector of the sample weights for each value in \code{x}.
#' @param probs a numeric vector of probabilities with values in \code{[0, 1]}.
#' @param na.rm logical. If \code{TRUE}, any \code{NA} and \code{NaN}s are removed from \code{x} before the quantiles are computed.
#' @param names logical. If \code{TRUE}, the result has a "names" attribute.
#' @param digits used only when \code{names} is \code{TRUE}. The precision to use when formatting the percentages.
#' @param type an integer between \code{1} and \code{9} selecting the quantile algorithms. Only \code{1} is available for the weighted quantile.
#' @param ... further arguments passed to \code{stats::quantile()} when weights are not passed.
#'
#' @returns
#' \code{weighted_quantile()} returns weighted sample quantiles corresponding to the given probabilities.
#'
#' @seealso \code{\link{weighted_tabulate}}
#'
#' @keywords internal
#'
weighted_quantile <- function(
    x, w = NULL, probs = seq(0, 1, 0.25), na.rm = FALSE,
    names = TRUE, digits = 7L, type = 1L, ...)
{
  if (is.null(w) || diff(range(w, na.rm = TRUE)) == 0)
    return(stats::quantile(x = x, probs = probs, na.rm = na.rm,
                           names = names, digits = digits, type = type, ...))
  if (type != 1L)
    stop("'type' must be 1 for the weighted quantile")
  if (length(x) != length(w) || any(w < 0))
    stop("'w' must be a non-negative numeric vector of the same length with 'x'")
  lx <- NULL
  if (is.factor(x)) {
    if (is.ordered(x)) {
      if (!any(type == c(1L, 3L)))
        stop("'type' must be 1 or 3 for ordered factors")
    }
    else stop("(unordered) factors are not allowed")
    lx <- levels(x)
    x <- as.integer(x)
  }
  else if (is.null(x)) {
    x <- numeric()
  }
  if (na.rm) {
    x.ok <- !is.na(x)
    x <- x[x.ok]
    w <- w[x.ok]
  } else if (anyNA(x)) {
    stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
  }
  eps <- 100 * .Machine$double.eps
  ok <- !is.na(probs)
  if (any(ok & (probs < -eps | 1 + eps < probs)))
    stop("'probs' outside [0,1]")
  probs <- pmax(0, pmin(1, probs))
  nx <- length(x)
  np <- length(probs)
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  wcum <- c(0, cumsum(w))
  wsum <- wcum[length(wcum)]
  wppm <- wsum * probs
  j <- findInterval(wppm, wcum, all.inside = TRUE, left.open = TRUE)
  out <- x[j]
  out[!ok] <- NA
  if (is.character(lx))
    out <- factor(out, levels = seq_along(lx), labels = lx, ordered = TRUE)
  if (names && np > 0L) {
    names(out) <- paste0(format(100 * probs, trim = TRUE), "%")
    names(out)[!ok] <- character(1L)
  }
  out
}


#' Weighted Tabulation for Vectors
#'
#' @description
#' \code{weighted_tabulate()} returns the sum of weights for each integer in the vector \code{bin}.
#'
#' @details
#' \code{weighted_tabulate()} is a wrapper function of \code{tabulate()} to reflect sample weights.
#' This function is used in \code{make_encoder()} to enable weights-based encoding.
#'
#' @param bin a numeric vector of positive integers, or a factor.
#' @param w a numeric vector of the sample weights for each value in \code{bin}.
#' @param nbins the number of bins to be used.
#'
#' @returns
#' \code{weighted_tabulate()} returns a numeric vector.
#'
#' @seealso \code{\link{weighted_quantile}}
#'
#' @keywords internal
#'
weighted_tabulate <- function(
    bin, w = NULL, nbins = max(1L, bin, na.rm = TRUE)) {
  if (is.null(w) || diff(range(w, na.rm = TRUE)) == 0)
    return(tabulate(bin = bin, nbins = nbins))
  if (length(bin) != length(w) || any(w < 0))
    stop("'w' must be a non-negative numeric vector of the same length with 'bin'")
  if (!is.numeric(bin) && !is.factor(bin))
    stop("'bin' must be numeric or a factor")
  if (typeof(bin) != "integer")
    bin <- as.integer(bin)
  if (nbins > .Machine$integer.max)
    stop("attempt to make a table with >= 2^31 elements")
  nbins <- as.integer(nbins)
  if (is.na(nbins))
    stop(gettextf("invalid value of %s", "'nbins'"), domain = NA)
  out <- numeric(nbins)
  ok <- !is.na(bin) & bin >= 1L & bin <= nbins
  if (any(ok)) {
    bin_ok <- bin[ok]
    w_ok <- w[ok]
    for (i in seq_along(bin_ok)) {
      b <- bin_ok[i]
      out[b] <- out[b] + w_ok[i]
    }
  }
  out
}
