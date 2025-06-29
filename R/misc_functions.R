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

model.reframe <- function(object, data) {
  if (!is.null(formula <- eval(object$call$formula))) {
    if (!is.data.frame(data) && !is.environment(data))
      data <- as.data.frame(data)
    res <- try(stats::model.frame.default(formula, data, na.action = "na.pass"),
               silent = TRUE)
    if (inherits(res, "try-error")) {
      formula[[2L]] <- NULL
      res <- stats::model.frame(formula, data, na.action = "na.pass")
    }
    return(res)
  }
  data
}

model.data <- function(object, env = parent.frame()) {
  fcl <- object$call
  if (!is.null(fcl$data))
    return(eval(fcl$data, envir = env))
  if (!is.null(fml <- fcl$formula)) {
    env <- ifnot.null(environment(fml), parent.frame())
    return(env)
  }
  if (!is.null(fcl$x)) {
    x <- eval(fcl$x, envir = env)
    if (!is.null(dim(x)[2]) && is.null(colnames(x)))
      colnames(x) <- paste0("x", seq_len(dim(x)[2]))
    if (!is.data.frame(x))
      x <- as.data.frame(x)
    if (!is.null(fcl$y)) {
      y <- eval(fcl$y, envir = env)
      x <- cbind.data.frame(x, y)
      colnames(x)[[ncol(x)]] <- "y"
    }
    return(x)
  }
  NULL
}


apply.catchall <- function(x, encoder) {
  catchall <- attr(encoder$frame, "catchall")
  if (!is.null(catchall)) {
    x <- factor(x, attr(encoder$frame, "levels"))
    x[is.na(x)] <- catchall
  }
  x
}

#' Weighted Sample Quantile
#'
#' \code{weighted.quantile()} produces weighted sample quantiles corresponding to the given probabilities.
#'
#' \code{weighted.quantile()} is a wrapper function of \code{stats::quantile()} for weighted quantiles.
#' For the weighted quantile, only the "type 1" quantile, the inverse of the empirical distribution function, is available.
#'
#' @param x a numeric vector whose weighted sample quantiles are wanted.
#' @param w a numeric vector of the sample weights for each value in \code{x}.
#' @param probs a numeric vector of probabilities with values in \code{[0, 1]}.
#' @param na.rm logical. If \code{TRUE}, any \code{NA} and \code{NaN}s are removed from \code{x} before the quantiles are computed.
#' @param names logical. If \code{TRUE}, the result has a "names" attribute.
#' @param digits used only when \code{names} is \code{TRUE}. The precision to use when formatting the percentages.
#' @param type an integer between \code{1} and \code{9} selecting the quantile algorithms. Only \code{1} is available for the weighted quantile.
#' @param ... further arguments passed to \code{stats::quantile()} when the weights is not passed.
#' @examples
#' stats::quantile(x = 1:10, type = 1L, probs = c(0, .25, .50, .75, 1))
#' weighted.quantile(x = 1:10, w = 1:10, probs = c(0, .25, .50, .75, 1))
#' @returns
#' \code{weighted.quantile()} returns weighted sample quantiles corresponding to the given probabilities.
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
#' \code{weighted.tabulate()} returns the sum of weights for each integer in the vector \code{bin}.
#'
#' \code{weighted.tabulate()} is a wrapper function of \code{tabulate()} to reflect sample weights.
#'
#' @param bin a numeric vector of positive integers, or a factor.
#' @param w a numeric vector of the sample weights for each value in \code{bin}.
#' @param nbins the number of bins to be used.
#' @examples
#' tabulate(bin = c(2, 2, 3, 5))
#' weighted.tabulate(bin = c(2, 2, 3, 5), w = 1:4)
#' @returns
#' \code{weighted.tabulate()} returns an numeric vector.
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
#' \code{weighted.mse()}, \code{weighted.rmse()}, \code{weighted.mae()} and \code{weighted.medae()} compute the loss based on the differences of two numeric vectors or deviations from the mean of a numeric vector.
#'
#' \code{weighted.mse()} returns the mean square error, \code{weighted.rmse()} returns the root mean square error, \code{weighted.mae()} returns the mean absolute error, and \code{weighted.medae()} returns the median absolute error between two weighted vectors \code{x} and \code{y}. If \code{y} is not passed, these functions return the corresponding statistic based on the deviations from the mean of \code{x}.
#'
#' @param x a numeric vector.
#' @param y an optional numeric vector. If passed, the loss is calculated for the differences between \code{x} and \code{y}. If not, the loss is calculated for the deviations of \code{x} from the weighted mean of itself.
#' @param w a numeric vector of sample weights for each value in \code{x}.
#' @param ... optional parameters.
#' @param na.rm logical. If \code{TRUE}, any \code{NA} and \code{NaN}s are removed from \code{x} before the calculation.
#' @examples
#' weighted.rmse(x = c(0, 10), y = c(0, 0), w = c(99, 1))
#' weighted.mae(x = c(0, 10), y = c(0, 0), w = c(99, 1))
#' weighted.medae(x = c(0, 10), y = c(0, 0), w = c(99, 1))
#' # compute uninterpreted rate
#' mid <- interpret(dist ~ speed, cars)
#' weighted.mse(cars$dist, predict(mid, cars)) / weighted.mse(cars$dist)
#' mid$ratio
#' @returns
#' \code{weighted.mse()} (mean square error), \code{weighted.rmse()} (root mean square error), \code{weighted.mae()} (mean absolute error) and \code{weighted.medae} (median absolute error) returns a single numeric value.
#' @export weighted.mse
#'
weighted.mse <- function(x, y = NULL, w = NULL, ..., na.rm = FALSE) {
  if (is.null(w)) {
    if (na.rm) {
      ok <- !is.na(x)
      x <- x[ok]
      if (length(y) > 1L) y <- y[ok]
    }
    if (is.null(y)) y <- mean(x)
    x <- x - y
    return(mean(x ^ 2))
  }
  if (length(w) != length(x))
    stop("'x' and 'w' must have the same length")
  if (!is.numeric(w) || anyNA(w))
    stop("'w' must be a numeric vector without missing values")
  if (na.rm) {
    ok <- !is.na(x) & w != 0
    x <- x[ok]
    w <- w[ok]
    if (length(y) > 1L) y <- y[ok]
  }
  if (is.null(y)) y <- sum(x * w) / sum(w)
  x <- x - y
  sum(x ^ 2 * w) / sum(w)
}


#' @rdname weighted.mse
#' @export weighted.rmse
#'
weighted.rmse <- function(x, y = NULL, w = NULL, ..., na.rm = FALSE) {
  sqrt(weighted.mse(x = x, y = y, w = w, ..., na.rm = na.rm))
}


#' @rdname weighted.mse
#' @export weighted.mae
#'
weighted.mae <- function(x, y = NULL, w = NULL, ..., na.rm = FALSE) {
  if (is.null(w)) {
    if (na.rm) {
      ok <- !is.na(x)
      x <- x[ok]
      if (length(y) > 1L) y <- y[ok]
    }
    if (is.null(y)) y <- mean(x)
    x <- x - y
    return(mean(abs(x)))
  }
  if (length(w) != length(x))
    stop("'x' and 'w' must have the same length")
  if (!is.numeric(w) || anyNA(w))
    stop("'w' must be a numeric vector without missing values")
  if (na.rm) {
    ok <- !is.na(x) & w != 0
    x <- x[ok]
    w <- w[ok]
    if (length(y) > 1L) y <- y[ok]
  }
  if (is.null(y)) y <- sum(x * w) / sum(w)
  x <- x - y
  sum(abs(x) * w) / sum(w)
}


#' @rdname weighted.mse
#' @export weighted.medae
#'
weighted.medae <- function(x, y = NULL, w = NULL, ..., na.rm = FALSE) {
  if (is.null(w)) {
    if (na.rm) {
      ok <- !is.na(x)
      x <- x[ok]
      if (length(y) > 1L) y <- y[ok]
    }
    if (is.null(y)) y <- mean(x)
    x <- x - y
    return(stats::quantile(abs(x), probs = 0.5,
                           na.rm = na.rm, names = FALSE, type = 1L, ...))
  }
  if (length(w) != length(x))
    stop("'x' and 'w' must have the same length")
  if (!is.numeric(w) || anyNA(w))
    stop("'w' must be a numeric vector without missing values")
  if (na.rm) {
    ok <- !is.na(x) & w != 0
    x <- x[ok]
    w <- w[ok]
    if (length(y) > 1L) y <- y[ok]
  }
  if (is.null(y)) y <- sum(x * w) / sum(w)
  x <- x - y
  weighted.quantile(abs(x), w, probs = 0.5,
                    na.rm = na.rm, names = FALSE, type = 1L, ...)
}


#' Theme for ggplot Objects
#'
#' \code{theme_midr()} returns a complete theme for "ggplot" objects. \code{par.midr()} can be used to set graphical parameters at the package default.
#'
#' @param grid_type one of "none", "x", "y" or "xy".
#' @param base_size base font size, given in pts.
#' @param base_family base font family.
#' @param base_line_size base size for line elements.
#' @param base_rect_size base size for rect elements.
#' @examples
#' X <- data.frame(x = 1:10, y = 1:10)
#' ggplot2::ggplot(X) +
#'   ggplot2::geom_point(ggplot2::aes(x, y)) +
#'   theme_midr()
#' ggplot2::ggplot(X) +
#'   ggplot2::geom_col(ggplot2::aes(x, y)) +
#'   theme_midr(grid_type = "y")
#' ggplot2::ggplot(X) +
#'   ggplot2::geom_line(ggplot2::aes(x, y)) +
#'   theme_midr(grid_type = "xy")
#' old.par <- par.midr()
#' plot(y ~ x, data = X)
#' plot(y ~ x, data = X, type = "l")
#' plot(y ~ x, data = X, type = "h")
#' par(old.par)
#' @returns
#' \code{theme_midr()} provides a ggplot2 theme customized for the midr package. \code{par.midr()} returns the previous values of the changed parameters in an invisible named list.
#' @export theme_midr
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


#' @rdname theme_midr
#' @param ... optional arguments in \code{tag = value} form to be passed to \code{graphics::par()}.
#' @export par.midr
#'
par.midr <- function(...) {
  dots <- list(...)
  args <- list(
    bg = "white", bty = "o", mar = c(4.1, 4.1, 2.1, 1.1), family = "serif",
    font = 1L, font.axis = 1L, font.lab = 1L, font.main = 1L, font.sub = 1L,
    col = "black", col.axis = "black", col.lab = "black", col.main ="black",
    col.sub = "black", cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1.2,
    cex.sub = .9, las = 0L, lty = "solid", lwd = 1, pch = 16L
  )
  args <- override(args = args, dots = dots,
                   params = names(graphics::par(no.readonly = TRUE)))
  do.call(what = graphics::par, args = args)
}


adjusted.mai <- function(labels, margin = 1/16) {
  mai <- graphics::par("mai")
  cex <- graphics::par("cex.lab")
  req <- max(graphics::strwidth(labels, "inch", cex = cex), na.rm = TRUE)
  req <- mai[4L] + req + margin
  if (mai[2L] < req)
    mai[2L] <- req
  mai
}


barplot2 <- function(
    to, from = 0, labels = NULL, horizontal = FALSE, limits = NULL, width = .9,
    type = c("b", "d", "n"), col = NA, fill = "gray65", lty = NULL, lwd = 1L,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, cex = 1, pch = 16, ...
) {
  type <- match.arg(type)
  if (horizontal) {
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    graphics::par(mai = adjusted.mai(labels = labels), las = 1L)
  }
  n <- max(length(to), length(from))
  to <- rep(to, length.out = n)
  from <- rep(from, length.out = n)
  col <- rep(col, length.out = n)
  fill <- rep(fill, length.out = n)
  rng <- range(c(to, from), na.rm = TRUE)
  mgn <- if (diff(rng) == 0) 0.5 else abs(diff(rng)) / 100
  limits <- ifnot.null(limits, c(rng[1L] - mgn, rng[2L] + mgn))
  at <- (if (horizontal) (n:1L) else (1L:n))
  graphics::plot.new()
  graphics::plot.window(xlim = if (horizontal) limits else c(0, n) + 0.5,
                        ylim = if (horizontal) c(0, n) + 0.5 else limits)
  graphics::box()
  graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  graphics::axis(side = if (horizontal) 1L else 2L)
  graphics::axis(side = if (horizontal) 2L else 1L, at = at, labels = labels)
  if (type == "b") {
    hw <- width / 2
    args <- as.list(numeric(8L))
    names(args) <- if (horizontal) {
      c("xleft", "xright", "ybottom", "ytop", "col", "border", "lty", "lwd")
    } else {
      c("ybottom", "ytop", "xleft", "xright", "col", "border", "lty", "lwd")
    }
    args[[7L]] <- ifnot.null(lty, 1L)
    args[[8L]] <- ifnot.null(lwd, 1L)
    for (i in seq_len(n)) {
      args[[1L]] <- from[i]
      args[[2L]] <- to[i]
      args[[3L]] <- at[i] - hw
      args[[4L]] <- at[i] + hw
      args[[5L]] <- fill[i]
      args[[6L]] <- col[i]
      do.call(graphics::rect, args)
    }
  } else if (type == "d") {
    for (i in seq_len(n)) {
      graphics::lines.default(
        x = if (horizontal) c(from[i], to[i]) else c(at[i], at[i]),
        y = if (horizontal) c(at[i], at[i]) else c(from[i], to[i]),
        col = "black", lty = ifnot.null(lty, 3L), lwd = ifnot.null(lwd, 1L)
      )
      graphics::points.default(
        x = if (horizontal) to[i] else at[i],
        y = if (horizontal) at[i] else to[i],
        col = col[i], pch = pch, cex = cex
      )
    }
  }
  invisible(NULL)
}

override <- function(args, dots,
    params = c("fill", "color", "colour", "col", "size", "cex", "shape", "pch",
               "linetype", "lty", "linewidth", "lwd",
               "title", "main", "subtitle", "sub", "xlab", "ylab"),
    read.as = list(color = "col", colour = "col", size = "cex",
                   shape = "pch", linetype = "lty", linewidth = "lwd",
                   title = "main", subtitle = "sub")
  ) {
  for (param in params) {
    arg <- ifnot.null(read.as[[param]], param)
    args[[arg]] <- ifnot.null(dots[[param]], args[[arg]])
  }
  args
}


get.link <- function(link) {
  res <- switch(
    link,
    "transprobit" = list(
      linkfun = function(mu) stats::qnorm(mu, mean = .5, sd = sqrt(.5 / pi)),
      linkinv = function(eta) stats::pnorm(eta, mean = .5, sd = sqrt(.5 / pi))
    ),
    "identity-gaussian" = list(
      linkfun = function(mu) mu,
      linkinv = function(eta) stats::pnorm(eta, mean = .5, sd = sqrt(.5 / pi))
    ),
    "translogit" = list(
      linkfun = function(mu) .5 - .25 * log(1 / mu - 1),
      linkinv = function(eta) 1 / (1 + exp(2 - 4 * eta))
    ),
    "identity-logistic" = list(
      linkfun = function(mu) mu,
      linkinv = function(eta) 1 / (1 + exp(2 - 4 * eta))
    ),
    stats::make.link(link)
  )
  res$name <- as.character(link)
  if (!inherits(res, "link-glm"))
    class(res) <- "link-midr"
  res
}


verbose <- function(text, verbosity = 1L, level = 1L, timestamp = FALSE) {
  if (verbosity >= level) {
    ltag <- if (level >= 3L) "- [debug] " else if (level >= 2L) "[info] " else ""
    stamp <- if (timestamp)
      format(Sys.time(), " (%Y-%m-%d %H:%M:%S)") else NULL
    text <- paste0(ltag, text, stamp)
    message(text)
  }
}


examples <- function(x, n = 3L, ...) {
  dts <- if (length(x) > n) ", ..." else ""
  n <- min(length(x), n)
  paste0(paste(trimws(format(x[seq_len(n)]), ...), collapse = ", "), dts)
}
