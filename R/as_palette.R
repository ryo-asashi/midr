#' Extract Palette or Ramp Function
#'
#' @param x a "color_theme" object, a theme name (character string), a color vector, or a color-generating function.
#' @param ... additional arguments passed to \code{\link{color_theme}}.
#'
#' @returns
#' \code{as_palette()} returns a palette function taking \code{n}.
#' \code{as_ramp()} returns a ramp function taking \code{x} in [0, 1].
#'
#' @examples
#' pal <- as_palette("Okabe-Ito")
#' pal(8L)
#'
#' ramp <- as_ramp("Viridis")
#' ramp(c(0, 0.5, 1.0))
#' @export as_palette
#'
as_palette <- function(x, ...)
UseMethod("as_palette")


#' @exportS3Method midr::as_palette
#'
as_palette.color_theme <- function(x, ...) {
  if (length(list(...)) > 0L) {
    x <- modify_theme(x, ...)
  }
  x$palette
}

#' @exportS3Method midr::as_palette
#'
as_palette.default <- function(x, ...) {
  color_theme(x, ...)$palette
}

#' @rdname as_palette
#' @export
#'
as_ramp <- function(x, ...)
UseMethod("as_ramp")


#' @exportS3Method midr::as_ramp
#'
as_ramp.color_theme <- function(x, ...) {
  if (length(list(...)) > 0L) {
    x <- modify_theme(x, ...)
  }
  x$ramp
}

#' @exportS3Method midr::as_ramp
#'
as_ramp.default <- function(x, ...) {
  if (is.character(x) && length(list(...)) == 0L && (length(x) > 1L || is_color(x))) {
    if (length(x) > 1L || !is_registered_theme(x)) {
      return(colors_to_ramp(x))
    }
  }
  color_theme(x, ...)$ramp
}
