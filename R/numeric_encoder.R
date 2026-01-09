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
#' @param type an integer (\code{1} or \code{0}) specifying the encoding method (see the "details" section).
#' @param encoding.digits an integer specifying the rounding digits for the piecewise linear encoding (\code{type = 1}).
#' @param tag the name of the variable.
#' @param frame a "numeric.frame" object or a numeric vector that explicitly defines the knots or breakes for the encoding.
#' @param weights an optional numeric vector of sample weights for \code{x}.
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
    x, k, type = 1L, encoding.digits = NULL, tag = "x", frame = NULL,
    weights = NULL) {
  # set breaks and representative values --------
  if (any(is.infinite(x))) {
    xmax <- .Machine$double.xmax
    x[xmax < x] <- xmax
    x[x < - xmax] <- - xmax
  }
  uni <- sort(unique(round(x, 12L)))
  if (length(uni) == 0L)
    uni <- 0
  n.uni <- length(uni)
  if (!is.null(frame)) {
    if (is.vector(frame)) {
      frame <- if (type == 0L) {
        numeric.frame(breaks = frame, tag = tag, type = type)
      } else {
        numeric.frame(reps = frame, tag = tag, type = type)
      }
    }
    if (!inherits(frame, "numeric.frame"))
      stop("'frame' must be a numeric vector or a valid 'numeric.frame' object")
    reps <- attr(frame, "reps")
    n.rep <- length(reps)
    br <- attr(frame, "breaks")
    if (!is.null(attr(frame, "type")))
      type <- attr(frame, "type")
    if (!is.null(attr(frame, "encoding.digits")))
      encoding.digits <- attr(frame, "encoding.digits")
  } else if (k <= 0L) {
    reps <- uni
    n.rep <- length(reps)
    if (n.rep == 1L) {
      br <- c(uni[1L] - 1L, uni[1L] + 1L)
      type <- -1L
    } else {
      br <- (reps[1L:n.rep - 1L] + reps[2L:n.rep]) / 2
      br <- c(uni[1L], br, uni[n.uni])
    }
  } else {
    if (type == 1L) {
      pr <- seq.int(0, 1, length.out = k)
    } else {
      mgn <- 1 / (2 * k)
      pr <- seq.int(mgn, 1 - mgn, length.out = k)
    }
    qs <- weighted.quantile(x = x, w = weights, probs = pr,
                            na.rm = TRUE, names = FALSE, type = 1L)
    reps <- unique(round(qs, 12L))
    if (any(is.na(reps)))
      reps <- 0
    n.rep <- length(reps)
    if (n.rep == 1L) {
      if (n.uni == 1L) {
        br <- c(uni[1L] - 1L, uni[1L] + 1L)
      } else {
        br <- c(uni[1L], uni[n.uni])
      }
      type = -1L
    } else {
      br <- (reps[1L:n.rep - 1L] + reps[2L:n.rep]) / 2
      br <- c(uni[1L], br, uni[n.uni])
    }
  }
  frame <- numeric.frame(reps = reps, breaks = br, type = type,
                         encoding.digits = encoding.digits, tag = tag)
  # define encoder function --------
  if (type == 1L) {
    encode <- function(x, ...) {
      n <- length(x)
      mat <- matrix(0, nrow = n, ncol = n.rep)
      itv <- findInterval(x, reps)
      mat[itv == 0, 1L] <- 1
      mat[itv == n.rep, n.rep] <- 1
      ok <- (!is.na(x) & itv > 0 & itv < n.rep)
      l <- reps[itv[ok]]
      r <- reps[itv[ok] + 1L]
      prop <- (r - x[ok]) / (r - l)
      if (!is.null(encoding.digits)) {
        rounder <- 10 ^ encoding.digits
        prop <- floor(prop * rounder + .5) / rounder
      }
      mat[cbind(which(ok), itv[ok])] <- prop
      mat[cbind(which(ok), itv[ok] + 1L)] <- 1 - prop
      colnames(mat) <- format(reps, digits = 6L)
      mat
    }
  } else if (type == 0L) {
    encode <- function(x, ...) {
      n <- length(x)
      mat <- matrix(0, nrow = n, ncol = n.rep)
      itv <- findInterval(x, br, all.inside = TRUE)
      ok <- !is.na(x)
      mat[cbind(which(ok), itv[ok])] <- 1
      bs <- format(br, digits = 6L)
      bs[c(1L, length(bs))] <- c("-Inf", "Inf")
      colnames(mat) <- paste0("[", bs[1L:n.rep], ", ", bs[2L:(n.rep + 1L)], ")")
      mat
    }
  } else {
    encode <- function(x, ...) {
      mat <- matrix(0, nrow = length(x), ncol = 1L)
      colnames(mat) <- "Void"
      mat
    }
  }
  type <- switch(type + 2L, "null", "constant", "linear")
  environment(encode) <- rlang::env(
    rlang::ns_env("midr"),
    n.rep = n.rep, reps = reps, br = br, encoding.digits = encoding.digits
  )
  enc <- list(frame = frame, encode = encode, n = n.rep, type = type)
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
                          encoding.digits = NULL, tag = "x") {
  if (is.null(reps) && is.null(breaks))
    stop("at least one of 'reps' or 'breaks' must be supplied")
  if (is.null(reps)) {
    breaks <- sort(unique(breaks))
    n.br <- length(breaks)
    reps <- (breaks[1L:(n.br - 1L)] + breaks[2L:n.br]) / 2L
    n.rep <- length(reps)
  } else {
    reps <- sort(unique(reps))
    n.rep <- length(reps)
    if (is.null(breaks)) {
      breaks <- (reps[1L:(n.rep - 1L)] + reps[2L:n.rep]) / 2L
      breaks <- c(reps[1L], breaks, reps[n.rep])
    } else {
      breaks <- sort(unique(breaks))
    }
  }
  if (length(breaks) != n.rep + 1L)
    stop("the length of 'breaks' must be the length of 'reps' plus 1")
  frame <- data.frame(reps, breaks[1L:n.rep], breaks[2L:(n.rep + 1)])
  ng <- c(frame[[1L]] <= frame[[2L]], frame[[1L]] > frame[[3L]])
  ng[1L] <- frame[1L, 1L] < frame[1L, 2L]
  if (any(ng))
    stop("representative values must be included in (min, max] of each bin")
  colnames(frame) <- paste0(tag, c("", "_min", "_max"))
  class(frame) <- c("numeric.frame", "data.frame")
  structure(frame, reps = reps, breaks = breaks,
            type = type, encoding.digits = encoding.digits)
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
