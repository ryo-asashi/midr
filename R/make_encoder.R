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
#' \item{frame}{a data.frame containing the encoding metadata.}
#' \item{n}{the number of encoding levels (columns in the design matrix).}
#' \item{type}{a character string describing the encoding type ("linear", "constant", "factor", or "null").}
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
make_encoder.matrix <- function(x, ...) {
  make_encoder(as.data.frame(x), ...)
}


#' @exportS3Method midr::make_encoder
#'
make_encoder.encoder <- function(x, tag = NULL, ...) {
  if (!is.null(tag)) {
    if (x$type == "linear" || x$type == "constant") {
      colnames(x$frame) <- paste0(tag, c("", "_min", "_max"))
    } else if (x$type == "factor") {
      colnames(x$frame) <- paste0(tag, c("", "_level"))
    }
  }
  x
}

# closure factory methods: encoding frame -->> encoder

#' @exportS3Method midr::make_encoder
#'
make_encoder.numeric_frame <- function(x, tag = NULL, ...) {
  if (!is.null(tag)) {
    colnames(x) <- paste0(tag, c("", "_min", "_max"))
  }
  envir <- new.env(parent = getNamespace("midr"))
  envir$nrep <- nrep <- attr(x, "nrep")
  envir$reps <- reps <- attr(x, "reps")
  envir$breaks <- breaks <- attr(x, "breaks")
  envir$digits <- digits <- attr(x, "digits")
  type <- attr(x, "type")
  transform <- function(x, ...) x
  environment(transform) <- envir
  envir$transform <- transform
  if (type == "linear") {
    encode <- function(x, ...) {
      n <- length(x)
      x <- transform(x)
      mat <- matrix(0, nrow = n, ncol = nrep)
      itv <- findInterval(x, reps)
      mat[itv == 0L, 1L] <- 1
      mat[itv == nrep, nrep] <- 1
      ok <- (!is.na(x) & itv > 0L & itv < nrep)
      l <- reps[itv[ok]]
      r <- reps[itv[ok] + 1L]
      prop <- (r - x[ok]) / (r - l)
      if (!is.null(digits)) {
        rounder <- 10 ^ digits
        prop <- floor(prop * rounder + 0.5) / rounder
      }
      mat[cbind(which(ok), itv[ok])] <- prop
      mat[cbind(which(ok), itv[ok] + 1L)] <- 1 - prop
      colnames(mat) <- format(reps, digits = 6L)
      mat
    }
  } else if (type == "constant") {
    encode <- function(x, ...) {
      n <- length(x)
      x <- transform(x)
      mat <- matrix(0, nrow = n, ncol = nrep)
      itv <- findInterval(x, breaks, all.inside = TRUE)
      ok <- !is.na(x)
      mat[cbind(which(ok), itv[ok])] <- 1
      bs <- format(breaks, digits = 6L)
      bs[c(1L, length(bs))] <- c("-Inf", "Inf")
      colnames(mat) <- paste0("[", bs[1L:nrep], ", ", bs[2L:(nrep + 1L)], ")")
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
    frame = x, n = nrep, type = type,
    envir = envir, transform = transform, encode = encode
  )
  structure(enc, class = "encoder")
}


#' @exportS3Method midr::make_encoder
#'
make_encoder.factor_frame <- function(x, tag = NULL, ...) {
  if (!is.null(tag)) {
    colnames(x) <- paste0(tag, c("", "_level"))
  }
  envir <- new.env(parent = getNamespace("midr"))
  envir$nlvs <- nlvs <- attr(x, "nlvs")
  envir$flvs <- flvs <- attr(x, "flvs")
  envir$others <- others <- attr(x, "others")
  envir$olvs <- olvs <- attr(x, "olvs")
  envir$map <- map <- attr(x, "map")
  type <- if (length(flvs) == 0L) "null" else "factor"
  if (type == "factor") {
    transform <- function(x, lumped = TRUE, ...) {
      x <- as.character(x)
      if (!lumped) {
        return(factor(x, olvs))
      }
      if (!is.null(map)) {
        x.mapped <- map[x]
        ok <- !is.na(x.mapped)
        x[ok] <- x.mapped[ok]
      }
      x <- factor(x, flvs)
      if (!is.null(others)) {
        x[is.na(x)] <- others
      }
      x
    }
    encode <- function(x, ...) {
      n <- length(x)
      x <- transform(x)
      ok <- !is.na(x)
      mat <- matrix(0, nrow = n, ncol = nlvs)
      mat[cbind(which(ok), as.integer(x[ok]))] <- 1
      colnames(mat) <- flvs
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
    frame = x, n = nlvs, type = type,
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
  if (nuni == 0L || type == "null") {
    reps <- 0
  } else if (is.null(k) || k <= 0L) {
    reps <- uni
  } else {
    if (type == "linear") {
      prob <- if (k == 1L) 0.5 else seq.int(0, 1, length.out = k)
    } else {
      prob <- seq.int(0, 1, length.out = 2L * k + 1L)
      prob <- prob[seq.int(2L, 2L * k, by = 2L)]
    }
    if (split == "quantile") {
      reps <- weighted.quantile(
        x = x, w = weights, probs = prob, na.rm = TRUE, names = FALSE, type = 1L
      )
    } else {
      reps <- prob * (uni[nuni] - uni[1L]) + uni[1L]
    }
    reps <- unique(round(reps, 12L))
  }
  nrep <- length(reps)
  if (any(is.na(reps)) || nrep == 0L) {
    reps <- 0
    nrep <- 1L
    breaks <- c(-1, 1)
    type <- "null"
  } else if (nrep == 1L) {
    if (nuni == 1L) {
      breaks <- c(nuni - 1L, uni[1L] + 1L)
    } else {
      breaks <- c(uni[1L], uni[nuni])
    }
    type <- "null"
  } else {
    breaks <- (reps[1L:(nrep - 1L)] + reps[2L:nrep]) / 2L
    breaks <- c(uni[1L], breaks, uni[nuni])
  }
  frame <- data.frame(reps, breaks[1L:nrep], breaks[2L:(nrep + 1L)])
  colnames(frame) <- paste0(tag, c("", "_min", "_max"))
  class(frame) <- c("numeric_frame", "data.frame")
  frame <- structure(
    frame, type = type, nrep = nrep, reps = reps, breaks = breaks, digits = digits
  )
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
  olvs <- levels(as.factor(x))
  if (length(olvs) == 0L) {
    flvs <- "Void"
    others <- NULL
    map <- NULL
  } else if (lump == "none" || is.null(k) || k < 1L || length(olvs) <= k) {
    flvs <- olvs
    others <- NULL
    map <- NULL
  } else if (lump == "order") {
    if (!is.ordered(x)) x <- ordered(x)
    tbl <- weighted.tabulate(bin = match(x, olvs), w = weights)
    wcum <- cumsum(tbl)
    wsum <- wcum[length(wcum)]
    cuts <- wsum * seq(0, 1, length.out = k + 1L)
    grps <- findInterval(wcum, cuts, left.open = TRUE, all.inside = TRUE)
    flvs <- tapply(
      X = olvs, INDEX = grps,
      FUN = function(z) {
        if (length(z) == 1L) z else paste0(z[1L], sep, z[length(z)])
      }
    )
    flvs <- as.character(flvs)
    others <- NULL
    map <- stats::setNames(rep(flvs, times = table(grps)), olvs)
  } else {
    tbl <- weighted.tabulate(bin = match(x, olvs), w = weights)
    ord <- order(tbl, decreasing = TRUE)
    flvs <- olvs[ord][seq_len(k - 1L)]
    flvs <- olvs[olvs %in% flvs]
    if (!is.null(others)) flvs <- unique(c(flvs, others))
    map <- NULL
  }
  nlvs <- length(flvs)
  frame <- data.frame(factor(flvs, levels = flvs), stringsAsFactors = FALSE)
  frame[[2L]] <- as.integer(frame[[1L]])
  colnames(frame) <- paste0(tag, c("", "_level"))
  class(frame) <- c("factor_frame", "data.frame")
  frame <- structure(
    frame, nlvs = nlvs, flvs = flvs, others = others, olvs = olvs, map = map
  )
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
    n.br <- length(breaks)
    if (n.br < 2L) stop("'breaks' must contain at least 2 unique values", call. = FALSE)
    reps <- (breaks[1L:(n.br - 1L)] + breaks[2L:n.br]) / 2L
  } else {
    reps <- sort(unique(x_num))
    nrep <- length(reps)
    if (nrep < 1L) stop("'reps' must contain at least 1 value", call. = FALSE)
    if (nrep == 1L) {
      breaks <- c(reps[1L] - 1, reps[1L] + 1)
      type <- "null"
    } else {
      breaks <- (reps[1L:(nrep - 1L)] + reps[2L:nrep]) / 2L
      breaks <- c(reps[1L], breaks, reps[nrep])
    }
  }
  nrep <- length(reps)
  if (length(breaks) != nrep + 1L) {
    stop("the length of 'breaks' must be the length of 'reps' plus 1", call. = FALSE)
  }
  frame <- data.frame(reps, breaks[1L:nrep], breaks[2L:(nrep + 1L)])
  ng <- c(frame[[1L]] <= frame[[2L]], frame[[1L]] > frame[[3L]])
  ng[1L] <- (frame[1L, 1L] < frame[1L, 2L])
  if (any(ng)) {
    stop("representative values must be included in (min, max] of each bin", call. = FALSE)
  }
  colnames(frame) <- paste0(tag, c("", "_min", "_max"))
  class(frame) <- c("numeric_frame", "data.frame")
  frame <- structure(frame, type = type, nrep = nrep,
                     reps = reps, breaks = breaks, digits = digits)
  make_encoder(frame, ...)
}


#' @exportS3Method midr::make_encoder
#'
make_encoder.factor_spec <- function(x, others = NULL, tag = "x", ...) {
  flvs <- as.character(unique(x))
  if (!is.null(others)) {
    flvs <- unique(c(flvs, others))
  }
  olvs <- names(x)
  map <- if (length(olvs) > 0L) stats::setNames(as.character(x), olvs) else NULL
  if (is.null(olvs)) olvs <- flvs

  nlvs <- length(flvs)
  frame <- data.frame(factor(flvs, levels = flvs), stringsAsFactors = FALSE)
  frame[[2L]] <- as.integer(frame[[1L]])
  colnames(frame) <- paste0(tag, c("", "_level"))
  class(frame) <- c("factor_frame", "data.frame")
  frame <- structure(frame, nlvs = nlvs, flvs = flvs,
                     others = others, olvs = olvs, map = map)
  make_encoder(frame, ...)
}


# misc methods for encoders

#' @exportS3Method base::print
#'
print.encoder <- function(x, digits = NULL, ...) {
  ty <- switch(
    x$type, c("Null", add_s(x$n, "class", "es")),
    linear = c("Linear", add_s(x$n, "knot")),
    constant = c("Constant", add_s(x$n, "interval")),
    factor = c("Factor", add_s(x$n, "level"))
  )
  cat("\n", ty[1L], " encoder with ", ty[2L], "\n", sep = "")
  if (ty[1L] != "Null") {
    cat("\nFrame:\n")
    cl <- switch(x$type, linear = 1L, constant = 2L:3L, factor = 1L)
    print.data.frame(x$frame[, cl, drop = FALSE], digits = digits)
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


#' @rdname make_encoder
#'
#' @exportS3Method midr::encode
#'
encode.mid <- function(
    object, variable, order = 1L, data = NULL, ...
) {
  data <- data %||% model_data(object, env = parent.frame())
  ostr <- switch(
    order, me = "main.effect", ie = "interaction",
    stop("'order' must be 1L (main effect) or 2L (interaction)")
  )
  enc <- object$encoders[[paste0(ostr, "s")]][[variable]]
  if (is.null(enc)) stop(
    sprintf("%s encoder for '%s' is not found in 'object'", order, variable),
    call. = FALSE
  )
  if (!(variable %in% colnames(data))) stop(
    sprintf("column '%s' is not found in 'data'", variable), call. = FALSE
  )
  encode(enc, data[, variable], ...)
}


#' @exportS3Method midr::encode
#'
encode.midrib <- function(object, ...) {
  encode.mid(object, ...)
}


interaction_frame <- function(x, y) {
  nx <- nrow(x)
  ny <- nrow(y)
  z <- data.frame(
    x[rep(seq_len(nx), times = ny), , drop = FALSE],
    y[rep(seq_len(ny), each = nx), , drop = FALSE],
    row.names = NULL, check.names = FALSE
  )
  attributes(z) <- list(
    names = names(z), row.names = attr(z, "row.names"), class = "data.frame"
  )
  z
}
