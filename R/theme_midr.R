#' Default Plotting Themes
#'
#' @description
#' \code{theme_midr()} returns a complete theme for "ggplot" objects, providing a consistent visual style for ggplot2 plots.
#'
#' @param grid_type the type of grid lines to display, one of "none", "x", "y" or "xy".
#' @param base_size base font size, given in pts.
#' @param base_family base font family.
#' @param base_line_size base size for line elements.
#' @param base_rect_size base size for rect elements.
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
#'
#' # Use par.midr() for base R graphics
#' old.par <- par.midr()
#' plot(y ~ x, data = X)
#' par(old.par)
#' @returns
#' \code{theme_midr()} provides a ggplot2 theme customized for the \strong{midr} package.
#'
#' @export theme_midr
#'
theme_midr <- function(
    grid_type = c("none", "x", "y", "xy"),
    base_size = 11,
    base_family = "serif",
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22) {
  grid_type = match.arg(grid_type)
  grid_x <- any(grid_type == c("x", "xy"))
  grid_y <- any(grid_type == c("y", "xy"))
  e1 <- ggplot2::theme_light(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  )
  e2 <- ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(fill = NA,
                                         colour = "gray5",
                                         linewidth = ggplot2::rel(0.5)),
    panel.grid.major.x = if (!grid_x) ggplot2::element_blank(),
    panel.grid.minor.x = if (!grid_x) ggplot2::element_blank(),
    panel.grid.major.y = if (!grid_y) ggplot2::element_blank(),
    panel.grid.minor.y = if (!grid_y) ggplot2::element_blank(),
    complete = TRUE
  )
  e1[names(e2)] <- e2
  e1
}


#' @rdname theme_midr
#'
#' @description
#' \code{par.midr()} can be used to set graphical parameters for base R graphics.
#'
#' @param ... for \code{par.midr()}, optional arguments in \code{tag = value} form to be passed to \code{graphics::par()}.
#'
#' @returns
#' \code{par.midr()} returns the previous values of the changed parameters in an invisible named list.
#'
#' @export par.midr
#'
par.midr <- function(...) {
  dots <- list(...)
  args <- list(
    bg = "white", bty = "o", mar = c(4.1, 4.1, 2.1, 1.1), family = "serif",
    font = 1L, font.axis = 1L, font.lab = 1L, font.main = 1L, font.sub = 1L,
    col = "black", col.axis = "black", col.lab = "black", col.main ="black",
    col.sub = "black", cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1.2,
    cex.sub = .9, las = 0L, lty = "solid", lwd = 1, pch = 16L
  )
  args <- override(args = args, dots = dots,
                   params = names(graphics::par(no.readonly = TRUE)))
  do.call(what = graphics::par, args = args)
}


