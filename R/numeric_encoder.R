#' Encoder for Quantitative Variables
#'
#' @description
#' \code{numeric.encoder()} creates an encoder function for a quantitative variable.
#' This encoder can then be used to convert a numeric vector into a design matrix using either piecewise linear or one-hot interval encoding, which are core components for modeling effects in a MID model.
#'
#' @details
#' The primary purpose of the encoder is to transform a single numeric variable into a design matrix for the MID model's linear system formulation.
#' The output of the encoder depends on the \code{type} argument.
#'
#' When \code{type = 1}, the variable's effect is modeled as a piecewise linear function with \code{k} knots including both ends.
#' For each value, the encoder finds the two nearest knots and assigns a weight to each, based on its relative position.
#' This results in a design matrix where each row has at most two non-zero values that sum to \code{1}.
#' This approach creates a smooth, continuous representation of the effect.
#'
#' When \code{type = 0}, the variable's effect is modeled as a step function by dividing its range into \code{k} intervals (bins).
#' The encoder determines which interval each value falls into and assigns a \code{1} to the corresponding column in the design matrix, with all other columns being \code{0}.
#' This results in a standard one-hot encoded matrix and creates a discrete, bin-based representation of the effect.
#'
#' @param x a numeric vector to be encoded.
#' @param k an integer specifying the coarseness of the encoding. If not positive, all unique values of \code{x} are used as knots or bins.
#' @param type a character string or an integer specifying the encoding method: \code{"linear"} / \code{1} (default) or \code{"constant"} / \code{1}.
#' @param split a character string specifying the splitting strategy: \code{"quantile"} (default) creates bins/knots based on data density; \code{"uniform"} creates equally spaced bins/knots over the data range.
#' @param digits an integer specifying the rounding digits for the piecewise linear encoding (\code{type = "linear"}).
#' @param weights an optional numeric vector of sample weights for \code{x}.
#' @param frame a "numeric.frame" object or a numeric vector that explicitly defines the knots or breaks for the encoding.
#' @param tag the name of the variable.
#'
#' @examples
#' # Create an encoder for a quantitative variable
#' data(iris, package = "datasets")
#' enc <- numeric.encoder(x = iris$Sepal.Length, k = 5L, tag = "Sepal.Length")
#' enc
#'
#' # Encode a numeric vector with NA and Inf
#' enc$encode(x = c(4:8, NA, Inf))
#'
#' # Create an encoder with a pre-defined encoding frame
#' frm <- numeric.frame(breaks = c(3, 5, 7, 9), type = 0L)
#' enc <- numeric.encoder(x = iris$Sepal.Length, frame = frm)
#' enc$encode(x = c(4:8, NA, Inf))
#'
#' # Create an encoder with a numeric vector specifying the knots
#' enc <- numeric.encoder(x = iris$Sepal.Length, frame = c(3, 5, 7, 9))
#' enc$encode(x = c(4:8, NA, Inf))
#' @returns
#' \code{numeric.encoder()} returns an object of class "encoder". This is a list containing the following components:
#' \item{frame}{a "numeric.frame" object containing the encoding information.}
#' \item{encode}{a function to convert a numeric vector \code{x} into a dummy matrix.}
#' \item{n}{the number of encoding levels (i.e., columns in the design matrix).}
#' \item{type}{a character string describing the encoding type: "linear", "constant", or "null".}
#'
#' @seealso \code{\link{factor.encoder}}
#'
#' @export numeric.encoder
#'
numeric.encoder <- function(
    x, k, type = c("linear", "constant"), split = c("quantile", "uniform"),
    digits = NULL, weights = NULL, frame = NULL, tag = "x") {
  if (is.numeric(type))
    type <- if (type <= 0) "constant" else "linear"
  type <- match.arg(type, c("linear", "constant"))
  if (!is.null(frame)) {
    if (is.vector(frame)) {
      frame <- if (type == "constant") {
        numeric.frame(breaks = frame, tag = tag, type = type)
      } else {
        numeric.frame(reps = frame, tag = tag, type = type)
      }
    }
    if (!inherits(frame, "numeric.frame"))
      stop("'frame' must be a numeric vector or a valid 'numeric.frame' object")
    reps <- attr(frame, "reps")
    nrep <- length(reps)
    br <- attr(frame, "breaks")
    type <- attr(frame, "type") %||% type
    digits <- attr(frame, "digits") %||% digits
  } else {
    # set breaks and representative values based on x --------
    if (any(is.infinite(x))) {
      xmax <- .Machine$double.xmax
      x[xmax < x] <- xmax
      x[x < - xmax] <- - xmax
    }
    split <- match.arg(split)
    uni <- sort(unique(round(x, 12L)))
    nuni <- length(uni)
    if (nuni == 0L) {
      reps <- 0
    } else if (k <= 0L) {
      reps <- uni
    } else {
      if (type == "linear") {
        prob <- if (k == 1) 0.5 else seq.int(0, 1, length.out = k)
      } else {
        prob <- seq.int(0, 1, length.out = 2L * k + 1L)
        prob <- prob[seq.int(2L, 2L * k, by = 2L)]
      }
      if (split == "quantile") {
        reps <- weighted.quantile(x = x, w = weights, probs = prob,
                                  na.rm = TRUE, names = FALSE, type = 1L)
      } else {
        reps <- prob * (uni[nuni] - uni[1L]) + uni[1L]
      }
      reps <- unique(round(reps, 12L))
    }
    nrep <- length(reps)
    if (any(is.na(reps)) || nrep == 0L) {
      reps <- 0
      nrep <- 1L
      br <- c(-1, 1)
      type <- "null"
    } else if (nrep == 1L) {
      if (nuni == 1L) {
        br <- c(nuni - 1L, uni[1L] + 1L)
      } else {
        br <- c(uni[1L], uni[nuni])
      }
      type = "null"
    } else {
      br <- (reps[1L:nrep - 1L] + reps[2L:nrep]) / 2
      br <- c(uni[1L], br, uni[nuni])
    }
  }
  frame <- numeric.frame(reps = reps, breaks = br, type = type,
                         digits = digits, tag = tag)
  # define encoder function --------
  if (type == "linear") {
    encode <- function(x, ...) {
      n <- length(x)
      mat <- matrix(0, nrow = n, ncol = nrep)
      itv <- findInterval(x, reps)
      mat[itv == 0, 1L] <- 1
      mat[itv == nrep, nrep] <- 1
      ok <- (!is.na(x) & itv > 0 & itv < nrep)
      l <- reps[itv[ok]]
      r <- reps[itv[ok] + 1L]
      prop <- (r - x[ok]) / (r - l)
      if (!is.null(digits)) {
        rounder <- 10 ^ digits
        prop <- floor(prop * rounder + .5) / rounder
      }
      mat[cbind(which(ok), itv[ok])] <- prop
      mat[cbind(which(ok), itv[ok] + 1L)] <- 1 - prop
      colnames(mat) <- format(reps, digits = 6L)
      mat
    }
  } else if (type == "constant") {
    encode <- function(x, ...) {
      n <- length(x)
      mat <- matrix(0, nrow = n, ncol = nrep)
      itv <- findInterval(x, br, all.inside = TRUE)
      ok <- !is.na(x)
      mat[cbind(which(ok), itv[ok])] <- 1
      bs <- format(br, digits = 6L)
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
  environment(encode) <- rlang::env(
    rlang::ns_env("midr"),
    nrep = nrep, reps = reps, br = br, digits = digits
  )
  enc <- list(frame = frame, encode = encode, n = nrep, type = type)
  structure(enc, class = "encoder")
}


#' @rdname numeric.encoder
#'
#' @description
#' \code{numeric.frame()} is a helper function to create a "numeric.frame" object that defines the encoding scheme.
#'
#' @param reps a numeric vector to be used as the representative values (knots).
#' @param breaks a numeric vector to be used as the binning breaks.
#'
#' @returns
#' \code{numeric.frame()} returns a "numeric.frame" object containing the encoding information.
#'
#' @export numeric.frame
#'
numeric.frame <- function(reps = NULL, breaks = NULL, type = NULL,
                          digits = NULL, tag = "x") {
  if (is.null(reps) && is.null(breaks))
    stop("at least one of 'reps' or 'breaks' must be supplied")
  if (is.null(reps)) {
    breaks <- sort(unique(breaks))
    n.br <- length(breaks)
    reps <- (breaks[1L:(n.br - 1L)] + breaks[2L:n.br]) / 2L
    nrep <- length(reps)
  } else {
    reps <- sort(unique(reps))
    nrep <- length(reps)
    if (is.null(breaks)) {
      breaks <- (reps[1L:(nrep - 1L)] + reps[2L:nrep]) / 2L
      breaks <- c(reps[1L], breaks, reps[nrep])
    } else {
      breaks <- sort(unique(breaks))
    }
  }
  if (length(breaks) != nrep + 1L)
    stop("the length of 'breaks' must be the length of 'reps' plus 1")
  frame <- data.frame(reps, breaks[1L:nrep], breaks[2L:(nrep + 1)])
  ng <- c(frame[[1L]] <= frame[[2L]], frame[[1L]] > frame[[3L]])
  ng[1L] <- frame[1L, 1L] < frame[1L, 2L]
  if (any(ng))
    stop("representative values must be included in (min, max] of each bin")
  colnames(frame) <- paste0(tag, c("", "_min", "_max"))
  class(frame) <- c("numeric.frame", "data.frame")
  structure(frame, reps = reps, breaks = breaks,
            type = type, digits = digits)
}


#' @exportS3Method base::print
#'
print.encoder <- function(x, digits = NULL, ...) {
  ty <- switch(x$type, c("Null", "class", "es"),
               linear = c("Linear", "knot", "s"),
               constant = c("Constant", "interval", "s"),
               factor = c("Factor", "level", "s"))
  cat(paste0("\n", ty[1L], " encoder with ", x$n, " ",
             ty[2L], if(x$n > 1) ty[3L], "\n"))
  if (ty[1L] != "Null") {
    cat("\nFrame:\n")
    cl <- switch(x$type, linear = 1L, constant = 2L:3L, factor = 1L)
    print.data.frame(x$frame[, cl, drop = FALSE], digits = digits)
    cat("\n")
  }
}
