#' Encoder for Quantitative Variables
#'
#' \code{numeric.encoder()} returns an encoder for a quantitative variable.
#'
#' \code{numeric.encoder()} selects sample points from the variable \code{x} and returns a list containing the \code{encode()} function to convert a vector into a dummy matrix.
#' If \code{type} is \code{1}, \code{k} is considered the maximum number of knots, and the values between two knots are encoded as two decimals, reflecting the relative position to the knots.
#' If \code{type} is \code{0}, \code{k} is considered the maximum number of intervals, and the values are converted using one-hot encoding on the intervals.
#'
#' @param x a numeric vector to be encoded.
#' @param k an integer specifying the coarseness of the encoding. If not positive, all unique values of x are used as sample points.
#' @param type an integer specifying the encoding method. If \code{1}, values are encoded to a \code{[0, 1]} scale based on linear interpolation of the knots. If \code{0}, values are encoded to \code{0} or \code{1} using ont-hot encoding on the intervals.
#' @param encoding.digits an integer specifying the rounding digits for the encoding in case \code{type} is \code{1}.
#' @param tag character string. The name of the variable.
#' @param frame a "numeric.frame" object or a numeric vector that defines the sample points of the binning.
#' @param weights optional. A numeric vector of sample weights for each value of \code{x}.
#' @examples
#' data(iris, package = "datasets")
#' enc <- numeric.encoder(x = iris$Sepal.Length, k = 5L, tag = "Sepal.Length")
#' enc$frame
#' enc$encode(new_x = c(4:8, NA))
#'
#' frm <- numeric.frame(breaks = seq(3, 9, 2), type = 0L)
#' enc <- numeric.encoder(x = iris$Sepal.Length, frame = frm)
#' enc$encode(new_x = c(4:8, NA))
#'
#' enc <- numeric.encoder(x = iris$Sepal.Length, frame = seq(3, 9, 2))
#' enc$encode(new_x = c(4:8, NA))
#' @returns
#' \code{numeric.encoder()} returns a list containing the following components:
#' \item{frame}{an object of class "numeric.frame".}
#' \item{encode}{a function to encode \code{new_x} into a dummy matrix.}
#' \item{n}{the number of encoding levels.}
#' \item{type}{the type of encoding, "linear" or "constant".}
#' \code{numeric.frame()} returns a "numeric.frame" object containing the encoding information.
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
      colnames(mat) <- format(reps, digits = 6L)
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
      bs <- format(br, digits = 6L)
      bs[c(1L, length(bs))] <- c("-Inf", "Inf")
      colnames(mat) <- paste0("[", bs[1L:n.rep], ", ", bs[2L:(n.rep + 1L)], ")")
      mat
    }
  } else {
    encode <- function(new_x, ...) {
      mat <- matrix(0, nrow = length(new_x), ncol = 1L)
      colnames(mat) <- "Void"
      mat
    }
  }
  type <- switch(type + 2L, "null", "constant", "linear")
  list(frame = frame, encode = encode, n = n.rep, type = type)
}


#' @rdname numeric.encoder
#' @param reps a numeric vector to be used as the representative values (knots).
#' @param breaks a numeric vector to be used as the binning breaks.
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
  class(frame) <- c("numeric.frame", "data.frame")
  structure(frame, reps = reps, breaks = breaks,
            type = type, encoding.digits = encoding.digits)
}
