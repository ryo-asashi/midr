# R/colortheme_utility.R

#' Map Values to Colors using Color Themes
#'
#' @description
#' \code{map_to_colors()} maps a vector of data values to a vector of hexadecimal color codes based on a specified color theme.
#' It automatically handles both discrete and continuous variables, appropriately scaling values and handling missing data.
#'
#' @param x a vector of data values (discrete or continuous) to be mapped to colors.
#' @param theme a color theme name (e.g., \code{"Viridis"}), a \code{"colortheme"} object, a character vector of color names, or a palette/ramp function. See \code{\link{colortheme}} for details.
#' @param discrete logical. If \code{TRUE}, a discrete scale is used regardless of the data type and the theme type.
#' @param middle a numeric value specifying the midpoint for diverging color themes. Default is \code{0}.
#' @param na.value a character string specifying the color for \code{NA} values. If \code{NULL} (default), falls back to the theme's \code{na.color} option.
#'
#' @returns
#' A character vector of hexadecimal color codes.
#'
#' @seealso \code{\link{colortheme}}
#'
#' @keywords internal
map_to_colors <- function(x, theme = NULL, discrete = NULL, middle = 0, na.value = NULL) {
  discrete <- discrete %||% is_discrete(x)
  theme <- theme %||% colortheme_default(if (discrete) "qualitative" else "sequential")
  theme <- colortheme(theme)
  na.value <- na.value %||% (theme$options$na.color %||% NA_character_)
  if (discrete) {
    f <- as.factor(x)
    n <- nlevels(f)
    cols <- if (n > 0L) theme$palette(n)[as.integer(f)] else character(length(x))
  } else {
    m <- if (theme$type == "diverging") middle else NULL
    x <- rescale(x, middle = m)
    cols <- theme$ramp(x)
  }
  cols[is.na(cols)] <- na.value
  cols
}


# local helpers

rescale <- function(x, middle = NULL) {
  if (is.character(x))
    x <- as.factor(x)
  if (is.factor(x) || is.logical(x))
    x <- as.numeric(x)
  from <- range(x, na.rm = TRUE, finite = TRUE)
  if (is.null(middle)) {
    d <- from[2L] - from[1L]
    if (d == 0)
      return(ifelse(is.na(x), NA, 0.5))
    res <- (x - from[1L]) / d
  } else {
    d <- 2 * max(abs(from - middle))
    if (d == 0)
      return(ifelse(is.na(x), NA, 0.5))
    res <- (x - middle) / d + 0.5
  }
  pmax(0, pmin(1, res))
}


#' Extract Palette or Ramp Function
#'
#' @description
#' \code{as_palette()} and \code{as_ramp()} extract the underlying discrete palette function or continuous color ramp function from a color theme or color kernel.
#'
#' @param x a \code{"colortheme"} object, a theme name (character string), a color vector, or a color-generating function.
#' @param ... additional arguments passed to \code{\link{colortheme}} to modify the theme before extracting the function.
#'
#' @returns
#' \itemize{
#'   \item \code{as_palette()} returns a palette function taking an integer \code{n} and returning \code{n} colors.
#'   \item \code{as_ramp()} returns a ramp function taking a numeric vector \code{x} in \code{[0, 1]} and returning corresponding colors.
#' }
#'
#' @seealso \code{\link{colortheme}}
#'
#' @keywords internal
as_palette <- function(x, ...)
UseMethod("as_palette")


#' @rdname as_palette
#' @exportS3Method midr::as_palette
as_palette.colortheme <- function(x, ...) {
  if (length(list(...)) > 0L) {
    x <- modify_theme(x, ...)
  }
  x$palette
}


#' @rdname as_palette
#' @exportS3Method midr::as_palette
as_palette.default <- function(x, ...) {
  colortheme(x, ...)$palette
}


#' @rdname as_palette
#' @keywords internal
as_ramp <- function(x, ...)
UseMethod("as_ramp")


#' @rdname as_palette
#' @exportS3Method midr::as_ramp
as_ramp.colortheme <- function(x, ...) {
  if (length(list(...)) > 0L) {
    x <- modify_theme(x, ...)
  }
  x$ramp
}


#' @rdname as_palette
#' @exportS3Method midr::as_ramp
as_ramp.default <- function(x, ...) {
  if (is.character(x) && length(list(...)) == 0L && (length(x) > 1L || is_color(x))) {
    if (length(x) > 1L || !is_registered_theme(x)) {
      return(colors_to_ramp(x))
    }
  }
  colortheme(x, ...)$ramp
}

