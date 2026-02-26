#' Compare MID Component Functions with ggplot2
#'
#' @description
#' For "mids" collection objects, \code{ggmid()} visualizes and compares a single main effect across multiple models.
#'
#' @details
#' This method evaluates the specified \code{term} over a grid of values and compares the results across all models in the collection.
#' For continuous variables, it uses line plots (or step plots for constant encodings). For factors, it uses grouped bar plots.
#'
#' Note: Comparative plotting for interaction terms (2D surfaces) is not supported for collection objects.
#'
#' @param object a "mids" or "midlist" collection object to be visualized.
#' @param term a character string specifying the main effect to evaluate.
#' @param type the plotting style. Currently only "effect" is supported for collections.
#' @param theme a character string or object defining the color theme.
#' @param intercept logical. If \code{TRUE}, the model intercept is added to the component effect.
#' @param resolution an integer specifying the number of evaluation points for continuous variables.
#' @param ... optional parameters passed to the main layer (e.g., \code{linewidth}, \code{alpha}).
#'
#' @exportS3Method midr::ggmid
#'
ggmid.mids <- function(
    object, term, type = c("effect"), theme = NULL,
    intercept = FALSE, resolution = 500L, ...
) {
  tags <- term.split(term)
  term <- term.check(term, mid.terms(object), stop = TRUE)
  type <- match.arg(type)
  if (length(tags) > 1L) {
    stop("comparative plotting for interaction terms is not supported for collections")
  }
  if (missing(theme))
    theme <- getOption("midr.qualitative", "HCL")
  theme <- color.theme(theme)
  base <- as.list(object)[[1L]]
  enc <- base$encoders$main.effects[[term]]
  if (enc$type == "factor") {
    xgrid_df <- factor.frame(enc$envir$olvs, tag = term)
    xgrid_eval <- xgrid_df
  } else {
    rng <- range(base$main.effects[[term]][, term], na.rm = TRUE)
    xgrid_eval <- seq(rng[1L], rng[2L], length.out = resolution)
  }
  eff_mat <- mid.effect(object, term = term, x = xgrid_eval)
  if (intercept) {
    ints <- vapply(as.list(object), `[[`, 0.0, "intercept")
    eff_mat <- sweep(eff_mat, 2L, ints, "+")
  }
  nms <- colnames(eff_mat)
  n_points <- nrow(eff_mat)
  m_models <- ncol(eff_mat)
  x_coords <- if(is.data.frame(xgrid_eval)) xgrid_eval[[term]] else xgrid_eval
  plot_df <- data.frame(
    x = rep(x_coords, times = m_models),
    mid = as.vector(eff_mat),
    label = factor(rep(nms, each = n_points), levels = nms)
  )
  colnames(plot_df)[1L] <- term
  pl <- ggplot2::ggplot(
    plot_df, ggplot2::aes(x = .data[[term]], y = .data[["mid"]])
  )
  if (enc$type == "factor") {
    pl <- pl + ggplot2::geom_col(
      ggplot2::aes(fill = .data[["label"]]),
      position = ggplot2::position_dodge(), ...
    ) + scale_fill_theme(theme, discrete = TRUE)
  } else {
    pl <- pl + ggplot2::geom_line(
      ggplot2::aes(color = .data[["label"]]), ...
    ) + scale_color_theme(theme, discrete = TRUE)
  }
  pl
}

#' @rdname ggmid.mids
#' @exportS3Method ggplot2::autoplot
autoplot.mids <- function(object, ...) {
  ggmid.mids(object = object, ...)
}
