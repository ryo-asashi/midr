#' Compare MID Conditional Expectations with ggplot2
#'
#' @description
#' For "midcons" collection objects, \code{ggmid()} visualizes and compares Individual Conditional Expectation (ICE) curves derived from multiple fitted MID models.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that produces comparative ICE curves from a "midcons" object.
#' It plots one line for each observation in the data per model.
#'
#' For \code{type = "iceplot"} and \code{"centered"}, lines are colored by the model label.
#' For \code{type = "series"}, lines are colored by the feature value and plotted across models.
#'
#' The \code{var.alpha}, \code{var.linetype}, and \code{var.linewidth} arguments allow you to map aesthetics to other variables in your data using (possibly) unquoted expressions.
#'
#' @param object a "midcons" collection object to be visualized.
#' @param type the plotting style. One of "iceplot", "centered", or "series".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param var.alpha a variable name or expression to map to the alpha aesthetic.
#' @param var.linetype a variable name or expression to map to the linetype aesthetic.
#' @param var.linewidth a variable name or expression to map to the linewidth aesthetic.
#' @param reference an integer specifying the index of the evaluation point to use as the reference for centering the c-ICE plot.
#' @param sample an optional vector specifying the names of observations to be plotted.
#' @param labels an optional numeric or character vector to specify the model labels. Defaults to the labels found in the object.
#' @param ... optional parameters passed on to the main layer.
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
#' ggmid(cons)
#'
#' # Create a centered-ICE plot
#' ggmid(cons, type = "centered")
#'
#' # Create a series plot to observe trends across models
#' ggmid(cons, type = "series", var.linetype = ".id")
#' @returns
#' \code{ggmid.midcons()} returns a "ggplot" object.
#'
#' @seealso \code{\link{ggmid.midcon}}, \code{\link{plot.midcons}}
#'
#' @exportS3Method midr::ggmid
#'
ggmid.midcons <- function(
    object, type = c("iceplot", "centered", "series"), theme = NULL,
    var.alpha = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, sample = NULL, labels = NULL, ...) {
  type <- match.arg(type)
  variable <- object[[1L]]$variable
  obs <- object[[1L]]$observed
  con <- summary(object, shape = "long")
  values <- object[[1L]]$values
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
  yvar <- "yhat"
  if (type == "centered") {
    if (reference < 0) reference <- length(values)
    refval <- values[min(length(values), max(1L, reference))]
    ref <- con[con[[variable]] == refval, , drop = FALSE]
    key1 <- sprintf("%s(%s)", con$.id, con$label)
    key2 <- sprintf("%s(%s)", ref$.id, ref$label)
    ynew <- paste0("centered ", yvar)
    con[, ynew] <- con[, yvar] - ref[match(key1, key2), yvar]
    yvar <- ynew
  }
  if (!is.null(sample)) {
    obs <- obs[obs$.id %in% sample, ]
    con <- con[con$.id %in% sample, ]
  }
  if (nrow(obs) == 0L) {
    message("no observations found")
    return(invisible(NULL))
  }
  xvar <- if (type == "series") "label" else variable
  pl <- ggplot2::ggplot(
    mapping = ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]])
  )
  if (!is.null(alp <- substitute(var.alpha))) {
    if (is.character(alp)) alp <- str2lang(alp)
    obs$.alp <- eval(alp, envir = obs)
    con <- merge(con, obs[, c(".id", ".alp")], by = ".id")
    pl <- pl + ggplot2::aes(alpha = .data[[".alp"]]) +
      ggplot2::labs(alpha = alp)
  }
  if (!is.null(lty <- substitute(var.linetype))) {
    if (is.character(lty)) lty <- str2lang(lty)
    obs$.lty <- eval(lty, envir = obs)
    con <- merge(con, obs[, c(".id", ".lty")], by = ".id")
    pl <- pl + ggplot2::aes(linetype = .data[[".lty"]]) +
      ggplot2::labs(linetype = lty)
    if (!is.discrete(obs$.lty))
      pl <- pl + ggplot2::scale_linetype_binned()
  }
  if (!is.null(lwd <- substitute(var.linewidth))) {
    if (is.character(lwd)) lwd <- str2lang(lwd)
    obs$.lwd <- eval(lwd, envir = obs)
    con <- merge(con, obs[, c(".id", ".lwd")], by = ".id")
    pl <- pl + ggplot2::aes(linewidth = .data[[".lwd"]]) +
      ggplot2::labs(linewidth = lwd)
    if (is.discrete(obs$.lwd)) {
      pl <- pl + ggplot2::scale_linewidth_discrete(range = c(0, 1))
    } else {
      pl <- pl + ggplot2::scale_linewidth_continuous(range = c(0, 1))
    }
  }
  if (type == "series") {
    theme <- theme %||% (
      if (is.discrete(con[[variable]])) getOption("midr.qualitative", "HCL")
      else getOption("midr.sequential", "bluescale")
    )
    theme <- color.theme(theme)
    pl <- pl +
      ggplot2::aes(
        color = .data[[variable]],
        group = interaction(.data[[".id"]], factor(.data[[variable]]))
      )
    pl <- pl + if (discrete) .geom_linepoint(data = con, ...) else
      .geom_line(data = con, ...)
    pl <- pl + ggplot2::labs(x = NULL) +
      scale_color_theme(theme, discrete = is.discrete(con[[variable]]))
  } else if (type == "iceplot" || type == "centered") {
    theme <- theme %||% (
      if (discrete) getOption("midr.qualitative", "HCL")
      else getOption("midr.sequential", "bluescale")
    )
    pl <- pl + .geom_line(
      mapping = ggplot2::aes(
        color = .data[["label"]],
        group = interaction(.data[[".id"]], factor(.data[["label"]]))
      ), data = con, ...
    ) + scale_color_theme(theme = theme, discrete = discrete)
  }
  pl
}


#' @rdname ggmid.midcons
#' @exportS3Method ggplot2::autoplot
#'
autoplot.midcons <- function(object, ...) {
  mcall <- match.call(expand.dots = TRUE)
  mcall[[1L]] <- quote(ggmid.midcons)
  mcall[["object"]] <- object
  eval(mcall, parent.frame())
}
