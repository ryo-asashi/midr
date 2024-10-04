#' Encoder for Quantitative Variables
#'
#' Returns a list consisting of the encoding information of a predictor variable as a quantitative variable.
#'
#' @param x a numeric vector which is to be encoded.
#' @param k an integer specifying the coarseness of the encoding. If not positive, all unique values of x are chosen as sample points.
#' @param type an integer specifying the shape of the function to be fit. \code{1} is for a piecewise linear function and \code{0} is for a piecewise constant (step) function.
#' @param encoding.digits an integer specifying the rounding digits for encoding numeric variables when \code{type} is 1 (piecewise linear functions).
#' @param tag name of the corresponding predictor variable.
#' @param frame a \code{numeric.frame} object containing the information about the binning of the variable.
#' @param weights optional. a numeric vector indicating the weight of each value of 'x'.
#'
#' @export numeric.encoder
#'
numeric.encoder <- function(
    x, k, type = 1L, encoding.digits = NULL, tag = "x", frame = NULL,
    weights = NULL) {
  # set breaks and representative values --------
  uni <- sort(unique(round(x, 12L)))
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
      stop(paste0("invalid numeric.frame supplied"))
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
    qs <- weighted.quantile(x, weights, pr, TRUE, FALSE, type = 1L)
    reps <- unique(round(qs, 12L))
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
    encode <- function(new_x, ...) {
      n <- length(new_x)
      mat <- matrix(0, nrow = n, ncol = n.rep)
      itv <- findInterval(new_x, reps)
      for (i in seq_len(n)) {
        if (is.na(itv[i]))
          next
        if (itv[i] == 0L) {
          mat[i, 1L] <- 1
        } else if (itv[i] == n.rep) {
          mat[i, n.rep] <- 1
        } else {
          l <- reps[itv[i]]
          r <- reps[itv[i] + 1L]
          prop <- (r - new_x[i]) / (r - l)
          if (!is.null(encoding.digits))
            prop <- round(prop, digits = encoding.digits)
          mat[i, itv[i]] <- prop
          mat[i, itv[i] + 1L] <- 1 - prop
        }
      }
      mat
    }
  } else if (type == 0L) {
    encode <- function(new_x, ...) {
      n <- length(new_x)
      mat <- matrix(0, nrow = n, ncol = n.rep)
      itv <- findInterval(new_x, br, all.inside = TRUE)
      for (i in seq_len(n)) {
        if (is.na(itv[i]))
          next
        mat[i, itv[i]] <- 1
      }
      mat
    }
  } else {
    encode <- function(new_x, ...) {
      matrix(0, nrow = length(new_x), ncol = 1L)
    }
  }
  type <- switch(type + 2L, "null", "constant", "linear")
  list(frame = frame, encode = encode, n = n.rep, type = type)
}


#' @rdname numeric.encoder
#' @param reps a numeric vector specifying the representative values of each bin.
#' @param breaks a numeric vector to be used as the breaks of the binning.
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
  ng <- c(frame[, 1L] <= frame[, 2L], frame[, 1L] > frame[, 3L])
  ng[1L] <- frame[1L, 1L] < frame[1L, 2L]
  if (any(ng))
    stop("representative values must be included in (min, max] of each bin")
  colnames(frame) <- paste0(tag, c("", "_min", "_max"))
  class(frame) <- c("data.frame", "numeric.frame")
  structure(frame, reps = reps, breaks = breaks,
            type = type, encoding.digits = encoding.digits)
}
