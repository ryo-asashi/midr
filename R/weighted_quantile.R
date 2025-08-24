#' Weighted Sample Quantile
#'
#' @description
#' \code{weighted.quantile()} produces weighted sample quantiles corresponding to the given probabilities.
#'
#' @details
#' \code{weighted.quantile()} is a wrapper function of \code{stats::quantile()} for weighted quantiles.
#' For the weighted quantile, only the "type 1" quantile, the inverse of the empirical distribution function, is available.
#' This function is used in \code{numeric.encoder()} to enable weights-based encoding.
#'
#' @param x a numeric vector whose weighted sample quantiles are wanted.
#' @param w a numeric vector of the sample weights for each value in \code{x}.
#' @param probs a numeric vector of probabilities with values in \code{[0, 1]}.
#' @param na.rm logical. If \code{TRUE}, any \code{NA} and \code{NaN}s are removed from \code{x} before the quantiles are computed.
#' @param names logical. If \code{TRUE}, the result has a "names" attribute.
#' @param digits used only when \code{names} is \code{TRUE}. The precision to use when formatting the percentages.
#' @param type an integer between \code{1} and \code{9} selecting the quantile algorithms. Only \code{1} is available for the weighted quantile.
#' @param ... further arguments passed to \code{stats::quantile()} when the weights is not passed.
#'
#' @returns
#' \code{weighted.quantile()} returns weighted sample quantiles corresponding to the given probabilities.
#'
#' @seealso \code{\link{weighted.tabulate}}
#'
#' @keywords internal
#'
weighted.quantile <- function(
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
#' \code{weighted.tabulate()} returns the sum of weights for each integer in the vector \code{bin}.
#'
#' @details
#' \code{weighted.tabulate()} is a wrapper function of \code{tabulate()} to reflect sample weights.
#' This function is used in \code{factor.encoder()} to enable weights-based encoding.
#'
#' @param bin a numeric vector of positive integers, or a factor.
#' @param w a numeric vector of the sample weights for each value in \code{bin}.
#' @param nbins the number of bins to be used.
#'
#' @returns
#' \code{weighted.tabulate()} returns an numeric vector.
#'
#' @seealso \code{\link{weighted.quantile}}
#'
#' @keywords internal
#'
weighted.tabulate <- function(
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
  n <- length(bin)
  for (i in seq_len(n))
    out[bin[i]] <- out[bin[i]] + w[i]
  out
}
