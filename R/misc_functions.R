ifnot.null <- function(x, y) {
  if (!is.null(x)) x else y
}

attract <- function(x, margin) {
  x[abs(x) <= margin] <- 0
  x
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

characterize <- function(expr) {
  if (is.character(expr)) expr else deparse(expr)
}

rescale <- function(x) {
  if (!is.numeric(x))
    x <- as.numeric(as.factor(x))
  rng <- range(x, na.rm = TRUE)
  if (rng[1L] == rng[2L])
    return(x - rng[1L])
  (x - rng[1L]) / (rng[2L]- rng[1L])
}

term.split <- function(x) {
  unlist(strsplit(x, split = ":"), use.names = FALSE)
}

term.check <- function(x, terms, stop = TRUE) {
  if (is.na(x)) {
    if (stop)
      stop("term can't be NA")
    return(NA)
  }
  if (!any(x == terms)) {
    rx <- paste0(rev(term.split(x)), collapse = ":")
    if (!any(rx == terms)) {
      if (stop)
        stop(paste0("term '", x, "' does not exist"))
      message(paste0("term '", x, "' does not exist"))
      return(NA)
    }
    return(rx)
  }
  return(x)
}


#' Weighted Sample Quantile
#'
#' Returns weighted sample quantiles corresponding to the given probabilities. For the weighted quantiles, only "type 1" quantiles of \code{stats::quantile()} (the inverse of empirical distribution function) is available.
#'
#' @param x an object containing the values whose weighted quantiles is to be computed.
#' @param w a numeric vector of the same length as 'x' giving the weights to use for elements of it.
#' @param probs a numeric vector of probabilities with values in [0, 1].
#' @param na.rm logical. If TRUE, any \code{NA} and \code{NaN}s are removed from 'x' before the quantiles are computed.
#' @param names logical. If TRUE, the result has a names attribute.
#' @param digits used only when \code{names} is TRUE. The precision to use when formatting the percentages.
#' @param type an integer selecting the quantile algorithms. Only 1 is available for the weighted quantile.
#' @param ... further arguments passed to \code{stats::quantile()} when the weights is not passed.
#' @examples
#' weighted.quantile(x = 1:10, w = 1:10, probs = c(0, .25, .50, .75, 1))
#' @returns
#' \code{weighted.quantile()} produces sample weighted quantiles corresponding to the given probabilities.
#' @export weighted.quantile
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
#' Returns the sum of weights for each interger occurs in the integer valued argument \code{bin}.
#'
#' @param bin a numeric vector of positive integers or a factor.
#' @param w a numeric vector of the same length as 'bin' giving the weights to use for elements of it.
#' @param nbins the number of bins to be used.
#' @examples
#' weighted.tabulate(bin = c(2, 2, 3, 5), w = 1:4)
#' @returns
#' \code{weighted.tabulate()} returns an integer valued vector.
#' @export weighted.tabulate
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


#' Weighted Loss Functions
#'
#' Returns a weighted loss calculated for the given vector(s). \code{weighted.rmse} is for the root mean square error, \code{weighted.mae} is for the mean absolute error, and \code{weighted.medae} is for the median absolute error
#'
#' @param x a numeric vector of deviations based on which the loss is calculated.
#' @param y an optional numeric vector. If 'y' is passed, the loss is calculated based on the difference of x minus y.
#' @param w a numeric vector of the same length as 'x' giving the weights to use for elements of it.
#' @param ... optional augments passed to other functions and methods.
#' @param na.rm logical. If TRUE, any \code{NA} and \code{NaN}s are removed from 'x' before the quantiles are computed.
#' @examples
#' weighted.rmse(x = c(0, 10), y = c(0, 0), w = c(99, 1))
#' weighted.mae(x = c(0, 10), y = c(0, 0), w = c(99, 1))
#' weighted.medae(x = c(0, 10), y = c(0, 0), w = c(99, 1))
#' @returns
#' \code{weighted.rmse()} (root mean squared error), \code{weighted.mae()} (mean absolute error) and \code{weighted.medae} (median absolute error) returns a weighted loss between two numeric vectors.
#' @export weighted.rmse
#'
weighted.rmse <- function(x, y = NULL, w = NULL, ..., na.rm = FALSE) {
  if (!is.null(y))
    x <- x - y
  if (is.null(w)) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(sqrt(mean(x ^ 2)))
  }
  if (length(w) != length(x))
    stop("'x' and 'w' must have the same length")
  if (na.rm) {
    ok <- !is.na(x) & w != 0
    x <- x[ok]
    w <- w[ok]
  }
  sqrt(sum(x ^ 2 * w) / sum(w))
}


#' @rdname weighted.rmse
#' @export weighted.mae
#'
weighted.mae <- function(x, y = NULL, w = NULL, ..., na.rm = FALSE) {
  if (!is.null(y))
    x <- x - y
  if (is.null(w)) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(mean(abs(x)))
  }
  if (length(w) != length(x))
    stop("'x' and 'w' must have the same length")
  if (na.rm) {
    ok <- !is.na(x) & w != 0
    x <- x[ok]
    w <- w[ok]
  }
  sum(abs(x) * w) / sum(w)
}


#' @rdname weighted.rmse
#' @export weighted.medae
#'
weighted.medae <- function(
    x, y = NULL, w = NULL, ..., na.rm = FALSE) {
  if (!is.null(y))
    x <- x - y
  if (is.null(w)) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(stats::quantile(abs(x), probs = 0.5,
                           na.rm = na.rm, names = FALSE, type = 1L, ...))
  }
  if (length(w) != length(x))
    stop("'x' and 'w' must have the same length")
  if (na.rm) {
    ok <- !is.na(x) & w != 0
    x <- x[ok]
    w <- w[ok]
  }
  weighted.quantile(abs(x), w, probs = 0.5,
                    na.rm = na.rm, names = FALSE, type = 1L, ...)
}


#' Themes for ggplot Objects
#'
#' Returns a complete theme for \code{ggplot} objects.
#' @param grid_type one of "none", "x", "y" or "both".
#' @param base_size a positive value for the base font size, given in pts.
#' @param base_family a character specifying the base font family.
#' @param base_line_size a positive value for the base size for line elements.
#' @param base_rect_size a positive value for the base size for rect elements.
#' @examples
#' ggplot2::theme_set(theme_midr())
#' @returns
#' \code{theme_midr()} provides a ggplot2 theme customized for the \code{midr} package.
#' @export
#'
theme_midr <- function(
    grid_type = c("none", "x", "y", "xy"),
    base_size = 11,
    base_family = "serif",
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22) {
  grid_type = match.arg(grid_type)
  grid_x <- any(grid_type == c("x", "xy"))
  grid_y <- any(grid_type == c("y", "xy"))
  e1 <- ggplot2::theme_light(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  )
  e2 <- ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(fill = NA,
                                         colour = "gray5",
                                         linewidth = ggplot2::rel(0.5)),
    panel.grid.major.x = if (!grid_x) ggplot2::element_blank(),
    panel.grid.minor.x = if (!grid_x) ggplot2::element_blank(),
    panel.grid.major.y = if (!grid_y) ggplot2::element_blank(),
    panel.grid.minor.y = if (!grid_y) ggplot2::element_blank(),
    complete = TRUE
  )
  e1[names(e2)] <- e2
  e1
}
