#' Color Theme Scales for ggplot2 Graphics
#'
#' @description
#' \code{scale_color_theme()} and its family of functions provide a unified interface to apply custom color themes to the "colour" and "fill" aesthetics of ggplot objects.
#'
#' @details
#' This function automatically determines the appropriate ggplot2 scale based on the theme's type.
#' If the theme is "qualitative", a discrete scale is used by default to assign distinct colors to categorical data.
#' The \code{discrete} argument is automatically set to \code{TRUE} if not specified.
#' If the theme is "sequential" or "diverging", a continuous scale is used by default.
#' The "diverging" themes are handled specially by \code{scales::rescale_mid()} to correctly center the gradient around the \code{middle} value.
#'
#' @param theme a color theme name (e.g., "Viridis"), a character vector of color names, or a palette/ramp function. See \code{?color.theme} for more details.
#' @param ... optional arguments to be passed to \code{ggplot2::continuous_scale()} or \code{ggplot2::discrete_scale()}.
#' @param discrete logical. If \code{TRUE}, a discrete scale is used regardless of the theme type.
#' @param middle a numeric value specifying the middle point for the diverging color themes.
#' @param aesthetics the aesthetic to be scaled. Can be "colour", "color", or "fill".
#'
#' @examples
#' data(txhousing, package = "ggplot2")
#' cities <- c("Houston", "Fort Worth", "San Antonio", "Dallas", "Austin")
#' df <- subset(txhousing, city %in% cities)
#' d <- ggplot2::ggplot(data = df, ggplot2::aes(x = sales, y = median)) +
#'   ggplot2::geom_point(ggplot2::aes(colour = city))
#'
#' # Plot with a qualitative theme
#' d + scale_color_theme("Set 1")
#'
#' # Use a sequential theme as a discrete scale
#' d + scale_color_theme("SunsetDark", discrete = TRUE)
#'
#' data(faithfuld, package = "ggplot2")
#' v <- ggplot2::ggplot(faithfuld) +
#'   ggplot2::geom_tile(ggplot2::aes(waiting, eruptions, fill = density))
#'
#' # Plot with continuous themes
#' v + scale_fill_theme("Plasma")
#'
#' # Use a diverging theme with a specified midpoint
#' v + scale_fill_theme("midr", middle = 0.017)
#' @returns
#' \code{scale_color_theme()} returns a ggplot2 scale object (either a "ScaleContinuous" or "ScaleDiscrete" object) that can be added to a "ggplot" object.
#'
#' @seealso \code{\link{color.theme}}
#'
#' @export scale_color_theme
#'
scale_color_theme <- function(
    theme, ..., discrete = NULL, middle = 0, aesthetics = "colour") {
  args <- list(...)
  args$aesthetics <- aesthetics
  theme <- color.theme(theme)
  if (is.null(discrete))
    discrete <- theme$type == "qualitative"
  if (discrete) {
    args$palette <- theme$palette
    args$guide <- ifnot.null(args$guide, "legend")
    args$na.value <- ifnot.null(args$na.value, theme$options$na.color)
    scale <- do.call(ggplot2::discrete_scale, args)
  } else {
    if (theme$type == "qualitative")
      stop("qualitative color theme can't be used for continuous scales")
    rescale_fun <- if (theme$type == "sequential") {
      scales::rescale
    } else if (theme$type == "diverging") {
      function(x, to = c(0, 1), from = range(x, na.rm = TRUE))
        scales::rescale_mid(x, to, from, mid = middle)
    }
    args$palette <- theme$ramp
    args$rescaler <- rescale_fun
    args$guide <- ifnot.null(args$guide, "colorbar")
    args$na.value <- ifnot.null(args$na.value, theme$options$na.color)
    scale <- do.call(ggplot2::continuous_scale, args)
  }
  scale
}

#' @rdname scale_color_theme
#'
#' @export scale_colour_theme
#'
scale_colour_theme <- function(
    theme, ..., discrete = NULL, middle = 0, aesthetics = "colour") {
  scale_color_theme(theme = theme, ..., discrete = discrete,
                    middle = middle, aesthetics = aesthetics)
}

#' @rdname scale_color_theme
#'
#' @export scale_fill_theme
#'
scale_fill_theme <- function(
    theme, ..., discrete = NULL, middle = 0, aesthetics = "fill") {
  scale_color_theme(theme = theme, ..., discrete = discrete,
                    middle = middle, aesthetics = aesthetics)
}

