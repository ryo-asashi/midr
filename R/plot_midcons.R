#' Compare MID Conditional Expectations
#'
#' @description
#' For "midcons" collection objects, \code{plot()} visualizes and compares Individual Conditional Expectation (ICE) curves derived from multiple fitted MID models.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that produces comparative ICE curves from a "midcons" object.
#' It plots one line for each observation in the data per model.
#'
#' For \code{type = "iceplot"} and \code{"centered"}, lines are colored by the model label.
#' For \code{type = "series"}, lines are colored by the feature value and plotted across models.
#'
#' The \code{var.alpha}, \code{var.linetype}, and \code{var.linewidth} arguments allow you to map aesthetics to other variables in your data using (possibly) unquoted expressions.
#'
#' @param x a "midcons" collection object to be visualized.
#' @param type the plotting style. One of "iceplot", "centered", or "series".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param var.alpha a variable name or expression to map to the alpha aesthetic.
#' @param var.linetype a variable name or expression to map to the linetype aesthetic.
#' @param var.linewidth a variable name or expression to map to the linewidth aesthetic.
#' @param reference an integer specifying the index of the evaluation point to use as the reference for centering the c-ICE plot.
#' @param sample an optional vector specifying the names of observations to be plotted.
#' @param labels an optional numeric or character vector to specify the model labels. Defaults to the labels found in the object.
#' @param ... optional parameters passed on to the graphing functions (e.g., \code{col}, \code{lty}, \code{lwd}).
#'
#' @examples
#' data(mtcars, package = "datasets")
#'
#' # Fit two different models for comparison
#' mid1 <- interpret(mpg ~ wt + hp + cyl, data = mtcars)
#' mid2 <- interpret(mpg ~ (wt + hp + cyl)^2, data = mtcars)
#'
#' # Calculate conditional expectations for both models
#' cons <- midlist(
#'   "Main Effects" = mid.conditional(mid1, "wt", data = mtcars[3:5, ]),
#'   "Interactions" = mid.conditional(mid2, "wt", data = mtcars[3:5, ])
#' )
#'
#' # Create an ICE plot (default)
#' plot(cons)
#'
#' # Create a centered-ICE plot
#' plot(cons, type = "centered")
#'
#' # Create a series plot to observe trends across models
#' plot(cons, type = "series", var.linetype = ".id")
#' @returns
#' \code{plot.midcons()} produces a plot as a side effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{plot.midcon}}, \code{\link{ggmid.midcons}}
#'
#' @exportS3Method base::plot
#'
plot.midcons <- function(
    x, type = c("iceplot", "centered", "series"), theme = NULL,
    var.alpha = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, sample = NULL, labels = NULL, ...
) {
  dots <- override(list(), list(...))
  type <- match.arg(type)
  variable <- x[[1L]]$variable
  obs <- x[[1L]]$observed
  con <- summary(x, shape = "long")
  values <- x[[1L]]$values
  olabs <- unique(con$label)
  labels <- labels %||% olabs
  if (length(labels) != length(olabs)) {
    stop("length of 'labels' must match the number of models in the collection")
  }
  con$label <- labels[match(con$label, olabs)]
  parsed <- suppressWarnings(as.numeric(labels))
  discrete <- anyNA(parsed)
  if (!discrete) {
    labels <- parsed
    con$label <- as.numeric(con$label)
  } else if (!is.factor(labels)) {
    labels <- factor(labels, levels = unique(labels))
    con$label <- factor(con$label, levels = levels(labels))
  }
  nlabs <- length(labels)
  nvals <- length(values)
  fv <- is.discrete(values)
  yvar <- "yhat"
  if (type == "centered") {
    if (reference < 0) reference <- nvals
    refval <- values[min(nvals, max(1L, reference))]
    ref <- con[con[[variable]] == refval, , drop = FALSE]
    key1 <- paste(con$.id, con$label, sep = "_")
    key2 <- paste(ref$.id, ref$label, sep = "_")
    ynew <- paste0("centered ", yvar)
    con[, ynew] <- con[, yvar] - ref[[yvar]][match(key1, key2)]
    yvar <- ynew
  }
  if (!is.null(sample)) {
    obs <- obs[obs$.id %in% sample, ]
    con <- con[con$.id %in% sample, ]
  }
  n <- nrow(obs)
  if (n == 0L) {
    message("no observations found")
    return(invisible(NULL))
  }
  aes <- list(alpha = rep.int(1, n), lty = rep.int(1L, n), lwd = rep.int(1L, n))
  if (!is.null(alphaexpr <- substitute(var.alpha))) {
    if (is.character(alphaexpr)) alphaexpr <- str2lang(alphaexpr)
    ref <- rescale(eval(alphaexpr, envir = obs))
    aes$alpha <- ref * .75 + .25
  }
  if (!is.null(ltyexpr <- substitute(var.linetype))) {
    if (is.character(ltyexpr)) ltyexpr <- str2lang(ltyexpr)
    ref <- rescale(eval(ltyexpr, envir = obs))
    aes$lty <- pmin(ref * 6 + 1, 6L)
  }
  if (!is.null(lwdexpr <- substitute(var.linewidth))) {
    if (is.character(lwdexpr)) lwdexpr <- str2lang(lwdexpr)
    ref <- rescale(eval(lwdexpr, envir = obs))
    aes$lwd <- ref * 3
  }
  # iceplot and centered
  if (type %in% c("iceplot", "centered")) {
    theme <- theme %||% (
      if (discrete) getOption("midr.qualitative", "HCL")
      else getOption("midr.sequential", "bluescale")
    )
    theme <- color.theme(theme)
    basecol <- dots$col %||% (
      if (discrete) theme$palette(nlabs) else to.colors(labels, theme)
    )
    dots$col <- NULL
    nlines <- n * nlabs
    mat <- matrix(NA_real_, nrow = nvals, ncol = nlines)
    rows <- match(con[[variable]], values)
    cols <- (match(con$label, labels) - 1L) * n + match(con$.id, obs$.id)
    ok <- !is.na(rows) & !is.na(cols)
    mat[cbind(rows[ok], cols[ok])] <- con[[yvar]][ok]
    aes$col <- rep(basecol, each = n)
    aes$lty <- rep(aes$lty, times = nlabs)
    aes$lwd <- rep(aes$lwd, times = nlabs)
    aes$alpha <- rep(aes$alpha, times = nlabs)
    xvals <- if (fv) seq_along(values) else values
    args <- list(
      x = xvals,
      xlim = range(xvals, na.rm = TRUE), ylim = range(mat, na.rm = TRUE),
      xlab = variable, ylab = yvar, type = "n", xaxt = if (fv) "n"
    )
    args <- set.alpha(override(args, dots), on = "col")
    do.call(graphics::plot.default, args)
    if (fv)
      graphics::axis(
        side = 1L, at = seq_along(values), labels = as.character(values)
      )
    args <- list(
      x = xvals, y = mat, type = "l", add = TRUE,
      col = aes$col, lty = aes$lty, lwd = aes$lwd, alpha = aes$alpha
    )
    args <- set.alpha(override(args, dots), on = "col")
    do.call(graphics::matplot, args)
    return(invisible(NULL))
  # series
  } else if (type == "series") {
    theme <- theme %||% (
      if (fv) getOption("midr.qualitative", "HCL")
      else getOption("midr.sequential", "bluescale")
    )
    theme <- color.theme(theme)
    basecol <- dots$col %||% (
      if (fv) theme$palette(nvals) else to.colors(values, theme)
    )
    dots$col <- NULL
    nlines <- n * nvals
    mat <- matrix(NA_real_, nrow = nlabs, ncol = nlines)
    rows <- match(con$label, labels)
    cols <- (match(con[[variable]], values) - 1L) * n + match(con$.id, obs$.id)
    ok <- !is.na(rows) & !is.na(cols)
    mat[cbind(rows[ok], cols[ok])] <- con[[yvar]][ok]
    aes$col <- rep(basecol, each = n)
    aes$lty <- rep(aes$lty, times = nvals)
    aes$lwd <- rep(aes$lwd, times = nvals)
    aes$alpha <- rep(aes$alpha, times = nvals)
    ord <- if (discrete) seq_along(labels) else order(labels)
    xvals <- if (discrete) seq_along(labels) else labels
    args <- list(
      x = xvals[ord],
      xlim = range(xvals, na.rm = TRUE), ylim = range(mat, na.rm = TRUE),
      xlab = "", ylab = yvar, type = "n", xaxt = if (discrete) "n"
    )
    args <- set.alpha(override(args, dots), on = "col")
    do.call(graphics::plot.default, args)
    if (discrete)
      graphics::axis(
        side = 1L, at = seq_along(labels), labels = as.character(labels)
      )
    args <- list(
      x = xvals[ord], y = mat[ord, , drop = FALSE], add = TRUE,
      type = if (discrete) "b" else "l", pch = 16L,
      col = aes$col, lty = aes$lty, lwd = aes$lwd, alpha = aes$alpha
    )
    args <- set.alpha(override(args, dots), on = "col")
    do.call(graphics::matplot, args)
    return(invisible(NULL))
  }
}
