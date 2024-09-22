#' Calculate and Visualize MID-based Individual Conditional Expectation
#'
#' Creates a data frame to be used to visualize the individual conditional expectation.
#'
#' @param object a mid object
#' @param data a data frame representing the observation results.
#' @param variable a name of the predictor variable to calculate the individual conditional expectations for.
#' @param partition an integer specifying the coarseness of the grid for a "raster" type interaction plot.
#' @param type the type of the prediction to use when the model has a link function. The default is "response".
#'
#' @export mid.conditional
#'
mid.conditional <- function(
    object, data, variable, partition = 100L, type = c("response", "link")) {
  type <- match.arg(type)
  rf <- length(tf <- mid.terms(object, remove = variable))
  rv <- length(tv <- mid.terms(object, require = variable))
  if (length(variable) != 1L || rv == 0L)
    stop("'variable' must be a character denoting a valid predictor variable of the model")
  if (!is.data.frame(data))
    data <- as.data.frame(data)
  ids <- rownames(data)
  if ("mid" %in% colnames(data))
    colnames(data)[colnames(data) == "mid"] <- ".mid"
  if (!is.null(formula <- eval(object$call$formula))) {
    yvar <- deparse(formula[[2L]])
    data[, yvar] <- 0L
    data <- stats::model.frame(formula = formula,
                               data = data, na.action = "na.pass")
    data <- data[, -which(colnames(data) == yvar)]
  }
  n <- nrow(data)
  pmat <- matrix(0, nrow = n, ncol = rf)
  for (i in seq_len(rf))
    pmat[, i] <- mid.f(object, tf[i], x = data)
  pf <- apply(pmat, MARGIN = 1L, FUN = sum)
  pmat <- matrix(0, nrow = n, ncol = rv, dimnames = list(NULL, tv))
  for (i in seq_len(rv))
    pmat[, i] <- mid.f(object, tv[i], x = data)
  pv <- apply(pmat, MARGIN = 1L, FUN = sum)
  yhat <- pf + pv + object$intercept
  if (type == "response" && !is.null(object$link))
    yhat <- object$link$linkinv(yhat)
  res <- list()
  res$terms <- tv
  res$observed <- cbind(id = ids, yhat = yhat, data)
  res$observed.effects <- pmat
  x <- data[, variable]
  if (is.numeric(x)) {
    rng <- range(x)
    if (diff(rng) == 0)
      rng <- rng + c(-.5, +.5)
    vals <- seq.int(from = rng[1L], to = rng[2L], length.out = partition)
  } else {
    vals <- sort(unique(x))
  }
  m <- length(vals)
  ldata <- list()
  for (col in colnames(data)) {
    if (col == variable) {
      ldata[[col]] <- lvals <- rep(vals, each = n)
      next
    }
    ldata[[col]] <- rep.int(data[[col]], times = m)
  }
  ldata <- as.data.frame(ldata)
  pmat <- matrix(0, nrow = n * m, ncol = rv, dimnames = list(NULL, tv))
  for (i in seq_len(rv))
    pmat[, i] <- mid.f(object, tv[i], x = ldata)
  pv <- apply(pmat, MARGIN = 1L, FUN = sum)
  pf <- rep.int(pf, m)
  lyhat <- pf + pv + object$intercept
  if (type == "response" && !is.null(object$link))
    lyhat <- object$link$linkinv(lyhat)
  res$conditional <- cbind(id = rep.int(ids, m), yhat = lyhat, ldata)
  res$conditional.effects <- pmat
  res$grid.points <- vals
  class(res) <- c("mid.conditional")
  attr(res, "variable") <- variable
  attr(res, "n") <- n
  res
}


#'
#' @rdname mid.conditional
#' @param limits NULL or a numeric vector of length two providing limits of the scale. NA is replaced by the minimum or maximum mid value.
#' @param plot.main logical. If TRUE, lines representing the individual conditional expectations are drawn.
#' @param centered logical.
#' @param show.dots logical. If TRUE, points representing the predictions at the observed values are
#' @param sample a vector specifying the set of names of the observations to be plotted.
#' @param term an optional character specifying one of the relevant terms. If passed, the individual conditional expectations for the specified term are plotted.
#' @param alpha the parameter to be passed to \code{ggplot2::geom_line()}, determining the opacity of lines.
#' @param ... additional parameters to be passed to \code{ggplot2::geom_line()}.
#' @exportS3Method midr::ggmid
#'
ggmid.mid.conditional <- function(
    object, limits = c(NA, NA), plot.main = TRUE, centered = FALSE,
    show.dots = TRUE, sample = NULL, term = NULL, alpha = .2, ...) {
  v <- attr(object, "variable")
  obs <- object$observed
  con <- object$conditional
  yvar <- "yhat"
  if (!is.null(term)) {
    if (!term %in% object$terms) {
      term <- paste0(rev(unlist(strsplit(term, ":"))), collapse = ":")
      if (!term %in% object$terms)
        stop(paste0("'", term, "' is not a relevant term"))
    }
    yvar <- paste0("mid(", term, ")")
    obs[, yvar] <- object$observed.effects[, term]
    con[, yvar] <- object$conditional.effects[, term]
  }
  if (centered) {
    n <- attr(object, "n")
    stp <- con[, yvar][1:n]
    ynew <- paste0("centered ", yvar)
    obs[, ynew] <- obs[, yvar] - stp
    con[, ynew] <- con[, yvar] - stp
    yvar <- ynew
  }
  if (!is.null(sample)) {
    obs <- obs[obs$id %in% sample, ]
    con <- con[con$id %in% sample, ]
  }
  pl <- ggplot2::ggplot(data = con,
    ggplot2::aes(x = .data[[v]], y = .data[[yvar]], group = .data[["id"]]))
  if (plot.main)
    pl <- pl + ggplot2::geom_line(alpha = alpha, ...)
  if (show.dots)
    pl <- pl + ggplot2::geom_point(ggplot2::aes(group = NULL), data = obs)
  if (!is.null(limits))
    pl <- pl + ggplot2::scale_y_continuous(limits = limits)
  pl
}


#'
#' @rdname mid.conditional
#' @param x a mid.conditional object to print.
#' @exportS3Method base::print
#'
print.mid.conditional <- function(x, ...) {
  n <- attr(x, "n")
  cat(paste0("\nIndividual Conditional Expectation for ",
             n, " Observation", if (n > 1L) "s", "\n"))
  cat(paste0("\nVariable: ", attr(x, "variable"), "\n"))
  cat("\nObserved:\n")
  print.data.frame(utils::head(x$observed), ...)
  cat("\nConditional:\n")
  print.data.frame(utils::head(x$conditional), ...)
}


#'
#' @rdname mid.conditional
#' @exportS3Method base::summary
#'
summary.mid.conditional <- function(object, ...) {
  n <- attr(object, "n")
  cat(paste0("\nIndividual Conditional Expectation for ",
             n, " Observation", if (n > 1L) "s", "\n"))
  cat(paste0("\nVariable: ", attr(object, "variable"), "\n"))
  cat("\nObserved:\n")
  print(summary.data.frame(object$observed, ...))
  cat("\nConditional:\n")
  print(summary.data.frame(object$conditional, ...))
}

