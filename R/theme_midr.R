#' Default Plotting Themes
#'
#' @description
#' \code{theme_midr()} returns a complete theme for "ggplot" objects, providing a consistent visual style for \strong{ggplot2} plots.
#'
#' @param grid_type the type of grid lines to display, one of "none", "x", "y" or "xy".
#' @param base_size base font size, given in pts.
#' @param base_family base font family.
#' @param base_line_size base size for line elements.
#' @param base_rect_size base size for rect elements.
#' @param ... other parameters passed on to \code{ggplot2::theme_light()}. \pkg{ggplot2} >= 4.0.0 accepts \code{ink}, \code{paper}, and \code{accent}.
#'
#' @examples
#' # Use theme_midr() with ggplot2
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
#' @returns
#' \code{theme_midr()} provides a \strong{ggplot2} theme customized for the \strong{midr} package.
#'
#' @export theme_midr
#'
theme_midr <- function(
    grid_type = c("none", "x", "y", "xy"),
    base_size = 11,
    base_family = "serif",
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22,
    ...
  ) {
  grid_type = match.arg(grid_type)
  grid_x <- any(grid_type == c("x", "xy"))
  grid_y <- any(grid_type == c("y", "xy"))
  e1 <- ggplot2::theme_light(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    ...
  )
  e2 <- ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(fill = NA,
                                         colour = "gray5",
                                         linewidth = ggplot2::rel(0.5)),
    panel.grid.major.x = if (!grid_x) ggplot2::element_blank() else NULL,
    panel.grid.minor.x = if (!grid_x) ggplot2::element_blank() else NULL,
    panel.grid.major.y = if (!grid_y) ggplot2::element_blank() else NULL,
    panel.grid.minor.y = if (!grid_y) ggplot2::element_blank() else NULL
  )
  e1 + e2
}
