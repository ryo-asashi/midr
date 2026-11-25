# R/colortheme_scale.R

#' Color Theme Scales for ggplot2 Graphics
#'
#' @description
#' \code{scale_color_theme()} and its family of functions provide a unified interface to apply color themes to the \code{colour} and \code{fill} aesthetics of \code{ggplot2} objects.
#'
#' @details
#' This function automatically selects the appropriate \strong{ggplot2} scale based on the theme's type:
#' \itemize{
#'   \item If the theme is \code{"qualitative"}, a discrete scale is used by default (\code{discrete = TRUE}).
#'   \item If the theme is \code{"sequential"} or \code{"diverging"}, a continuous scale is used by default. Diverging themes use \code{scales::rescale_mid()} to center the gradient around \code{middle}.
#' }
#'
#' @param theme a color theme name (e.g., \code{"Viridis"}), a \code{"colortheme"} object, a character vector of color names, or a palette/ramp function. See \code{\link{colortheme}} for details.
#' @param ... optional arguments passed to \code{ggplot2::continuous_scale()} or \code{ggplot2::discrete_scale()}.
#' @param discrete logical. If \code{TRUE}, a discrete scale is used regardless of the theme type.
#' @param middle a numeric value specifying the midpoint for diverging color themes. Default is \code{0}.
#' @param aesthetics the aesthetic(s) to be scaled (\code{"colour"}, \code{"color"}, or \code{"fill"}).
#'
#' @returns
#' A \code{ggplot2} scale object (\code{"ScaleContinuous"} or \code{"ScaleDiscrete"}).
#'
#' @seealso \code{\link{colortheme}}
#'
#' @keywords internal
#' @export
scale_color_theme <- function(
    theme = NULL, ..., discrete = NULL, middle = 0, aesthetics = "colour"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("scales", quietly = TRUE)) {
    stop("packages 'ggplot2' and 'scales' are required for 'scale_color_theme()'.",
         call. = FALSE)
  }
  args <- list(...)
  args$aesthetics <- aesthetics
  theme <- theme %||%
    colortheme_default(if (isTRUE(discrete)) "qualitative" else "sequential")
  theme <- colortheme(theme)
  discrete <- discrete %||% (theme$type == "qualitative")
  if (discrete) {
    args$palette <- theme$palette
    args$guide <- args$guide %||% "legend"
    args$na.value <- args$na.value %||% (theme$options$na.color %||% "grey50")
    scale <- do.call(ggplot2::discrete_scale, args)
  } else {
    rescale_fun <- if (theme$type == "diverging") {
      function(x, to = c(0, 1), from = range(x, na.rm = TRUE))
        scales::rescale_mid(x, to, from, mid = middle)
    } else {
      scales::rescale
    }
    args$palette <- theme$ramp
    args$rescaler <- rescale_fun
    args$guide <- args$guide %||% "colorbar"
    args$na.value <- args$na.value %||% (theme$options$na.color %||% "grey50")
    scale <- do.call(ggplot2::continuous_scale, args)
  }
  scale
}


#' @rdname scale_color_theme
#' @export
scale_colour_theme <- function(
    theme = NULL, ..., discrete = NULL, middle = 0, aesthetics = "colour"
) {
  scale_color_theme(
    theme = theme, ..., discrete = discrete,
    middle = middle, aesthetics = aesthetics
  )
}


#' @rdname scale_color_theme
#' @export
scale_fill_theme <- function(
    theme = NULL, ..., discrete = NULL, middle = 0, aesthetics = "fill"
) {
  scale_color_theme(
    theme = theme, ..., discrete = discrete,
    middle = middle, aesthetics = aesthetics
  )
}
