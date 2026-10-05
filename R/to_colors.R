#' Map Values to Colors using Color Themes
#'
#' @description
#' \code{to.colors()} maps a vector of data values to a vector of hexadecimal color codes based on a specified color theme.
#' It automatically handles both discrete and continuous variables, appropriately scaling the values and handling missing data.
#'
#' @param x a vector of data values (discrete or continuous) to be mapped to colors.
#' @param theme a color theme name (e.g., "Viridis"), a character vector of color names, or a palette/ramp function. See \code{?color.theme} for more details.
#' @param middle a numeric value specifying the middle point for the diverging color themes. Default is \code{0}.
#' @param na.value a character string specifying the color for \code{NA} values. If \code{NULL} (default), it falls back to the theme's default \code{na.color} option.
#'
#' @returns a character vector of hexadecimal color codes.
#'
#' @examples
#' # Continuous mapping (Sequential)
#' plot(cars, col = to.colors(cars$speed, "Viridis"), pch = 19)
#'
#' # Discrete mapping (Qualitative)
#' plot(iris$Sepal.Length, iris$Sepal.Width,
#'      col = to.colors(iris$Species, "Set2"), pch = 19)
#' @seealso \code{\link{color.theme}}
#'
#' @export to.colors
#'
to.colors <- function(x, theme = NULL, middle = 0, na.value = NULL) {
  discrete <- is.discrete(x)
  theme <- theme %||% color.theme.defaults(if (discrete) "qual" else "seq")
  theme <- color.theme(theme)
  if (is.null(na.value))
    na.value <- theme$options$na.color %||% NA
  if (discrete) {
    x <- as.integer(as.factor(x))
    cols <- theme$palette(max(x, na.rm = TRUE))[x]
  } else {
    if (theme$type == "qualitative") {
      stop("qualitative color theme can't be used for continuous variable")
    } else if (theme$type == "sequential") {
      cols <- theme$ramp(rescale(x))
    } else if (theme$type == "diverging") {
      cols <- theme$ramp(rescale(x, middle = middle))
    } else
      cols <- rep.int(1L, length(x))
  }
  cols[is.na(cols)] <- na.value
  cols
}


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
