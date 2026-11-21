# R/dualplot.R

check_ggplot2 <- function(call = sys.call(-1L)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "package 'ggplot2' is required for engine = 'ggplot2': ",
      "please install it or use engine = 'graphics'",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Draw Bar, Dot, and Waterfall Charts
#'
#' @param to numeric vector or matrix (\code{nrow} categories by \code{ncol} groups) of end values.
#' @param from numeric vector or matrix of start values (default \code{0}).
#' @param ticks character vector of category tick labels (length \code{nrow(to)}).
#' @param horizontal logical; if \code{TRUE}, bars/dots are drawn horizontally (top-to-bottom).
#' @param type one of \code{"bar"} or \code{"dot"}.
#' @param middle numeric midpoint for diverging color themes (default \code{0}).
#' @param limits optional numeric vector of length 2 for value axis limits.
#' @param width optional numeric bar/dodge width.
#' @param vline optional numeric intercept for a reference line.
#' @param connect logical; if \code{TRUE}, draws step lines connecting bars (for waterfall charts).
#' @param ... additional graphical parameters.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param theme.values optional vector or matrix mapped to colors via \code{theme}.
#' @param legend logical or character legend position (\code{FALSE}, \code{TRUE},
#'   \code{"topright"}, \code{"topleft"}, \code{"bottomright"}, \code{"bottomleft"},
#'   \code{"top"}, \code{"bottom"}, \code{"left"}, \code{"right"}).
#' @param legend.title optional title string for the legend.
#' @param engine graphics backend: \code{"graphics"} or \code{"ggplot2"}.
#'
#' @return Invisibly returns \code{NULL} for \code{"graphics"}, or a \code{"ggplot"} object for \code{"ggplot2"}.
#' @keywords internal
#'
draw_bars <- function(
    to, from = 0, ticks = NULL, horizontal = TRUE, type = c("bar", "dot"),
    middle = 0, limits = NULL, width = NULL, vline = NULL, connect = FALSE,
    ..., main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, theme.values = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% (engine == "ggplot2")
  fn <- if (engine == "graphics") draw_bars_base else { check_ggplot2(); draw_bars_gg }
  fn(
    to = to, from = from, ticks = ticks, horizontal = horizontal, type = type,
    theme = theme, theme.values = theme.values, legend = legend, legend.title = legend.title,
    middle = middle, limits = limits, width = width, vline = vline, connect = connect,
    main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' Draw Multi-Line Series
#'
#' @param x vector of x-axis values (numeric, factor, or character).
#' @param y numeric matrix or vector of y-values (\code{NROW(y) == length(x)}).
#' @param discrete logical; if \code{NULL}, inferred automatically from \code{x}.
#' @param limits optional numeric vector of length 2 for y-axis limits.
#' @param points optional \code{data.frame} with columns \code{x} and \code{y}
#'   (and optional \code{col}, \code{pch}, \code{cex}) for overlaying points.
#' @param ... additional graphical parameters.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param theme.values optional vector (length \code{ncol(y)}) mapped to line colors via \code{theme}.
#' @param legend logical or character legend position.
#' @param legend.title optional title string for the legend.
#' @param engine graphics backend: \code{"graphics"} or \code{"ggplot2"}.
#'
#' @return Invisibly returns \code{NULL} for \code{"graphics"}, or a \code{"ggplot"} object for \code{"ggplot2"}.
#' @keywords internal
#'
draw_lines <- function(
    x, y, discrete = NULL, limits = NULL, points = NULL,
    ..., main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, theme.values = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% (engine == "ggplot2")
  fn <- if (engine == "graphics") draw_lines_base else { check_ggplot2(); draw_lines_gg }
  fn(
    x = x, y = y, theme = theme, theme.values = theme.values,
    legend = legend, legend.title = legend.title,
    discrete = discrete, limits = limits, points = points,
    main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' Draw Scatter and Jitter Plots
#'
#' @param x,y numeric vectors of coordinates.
#' @param middle numeric midpoint for diverging color themes (default \code{0}).
#' @param xticks,yticks optional character vectors for discrete axis tick labels.
#' @param xlim,ylim optional axis limits.
#' @param overlay logical; if \code{TRUE} and \code{engine = "graphics"}, adds points to an existing plot.
#' @param ... additional graphical parameters.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param theme.values optional vector mapped to point colors via \code{theme}.
#' @param legend logical or character legend position.
#' @param legend.title optional title string for the legend.
#' @param engine graphics backend: \code{"graphics"} or \code{"ggplot2"}.
#'
#' @return Invisibly returns \code{NULL} for \code{"graphics"}, or a \code{"ggplot"} object for \code{"ggplot2"}.
#' @keywords internal
#'
draw_points <- function(
    x, y, middle = 0, xticks = NULL, yticks = NULL, xlim = NULL, ylim = NULL,
    overlay = FALSE, ..., main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, theme.values = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% (engine == "ggplot2")
  fn <- if (engine == "graphics") draw_points_base else { check_ggplot2(); draw_points_gg }
  fn(
    x = x, y = y, theme = theme, theme.values = theme.values,
    legend = legend, legend.title = legend.title,
    middle = middle, xticks = xticks, yticks = yticks, xlim = xlim, ylim = ylim,
    overlay = overlay, main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' Draw 2D Surface / Heatmap Plots
#'
#' @param x,y numeric vectors of grid coordinates.
#' @param z numeric matrix of surface values (\code{length(x)} by \code{length(y)}).
#' @param zlim optional numeric vector of length 2 for color scale limits.
#' @param middle numeric midpoint for diverging color themes (default \code{0}).
#' @param xticks,yticks optional character vectors for discrete axis tick labels.
#' @param points optional \code{data.frame} with \code{x} and \code{y} columns to overlay.
#' @param ... additional graphical parameters.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param legend logical or character legend position.
#' @param legend.title optional title string for the legend.
#' @param engine graphics backend: \code{"graphics"} or \code{"ggplot2"}.
#'
#' @return Invisibly returns \code{NULL} for \code{"graphics"}, or a \code{"ggplot"} object for \code{"ggplot2"}.
#' @keywords internal
#'
draw_surface <- function(
    x = seq(0, 1, length.out = nrow(z)),
    y = seq(0, 1, length.out = ncol(z)),
    z, zlim = NULL, middle = 0, xticks = NULL, yticks = NULL,
    points = NULL, ...,  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% (engine == "ggplot2")
  fn <- if (engine == "graphics") draw_surface_base else { check_ggplot2(); draw_surface_gg }
  fn(
    x = x, y = y, z = z, theme = theme, legend = legend, legend.title = legend.title,
    zlim = zlim, middle = middle, xticks = xticks, yticks = yticks, points = points,
    main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' Draw Matrix Heatmaps
#'
#' @param z numeric matrix of values to plot.
#' @param ticks character vector of row/column tick labels.
#' @param middle numeric midpoint for diverging color themes (default \code{0}).
#' @param ... additional graphical parameters.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param legend logical or character legend position.
#' @param legend.title optional title string for the legend.
#' @param engine graphics backend: \code{"graphics"} or \code{"ggplot2"}.
#'
#' @return Invisibly returns \code{NULL} for \code{"graphics"}, or a \code{"ggplot"} object for \code{"ggplot2"}.
#' @keywords internal
#'
draw_matrix <- function(
    z, ticks = rownames(z), middle = 0, ...,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% (engine == "ggplot2")
  fn <- if (engine == "graphics") draw_matrix_base else { check_ggplot2(); draw_matrix_gg }
  fn(
    z = z, ticks = ticks, theme = theme, legend = legend, legend.title = legend.title,
    middle = middle, main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' Draw Boxplots
#'
#' @param x a named list of numeric vectors (one per category).
#' @param horizontal logical; if \code{TRUE}, boxes are drawn horizontally.
#' @param ... additional graphical parameters.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param theme.values optional vector (length \code{length(x)}) mapped to box fills via \code{theme}.
#' @param legend logical or character legend position.
#' @param legend.title optional title string for the legend.
#' @param engine graphics backend: \code{"graphics"} or \code{"ggplot2"}.
#'
#' @return Invisibly returns \code{NULL} for \code{"graphics"}, or a \code{"ggplot"} object for \code{"ggplot2"}.
#' @keywords internal
#'
draw_boxes <- function(
    x, horizontal = TRUE, ..., main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, theme.values = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% (engine == "ggplot2")
  fn <- if (engine == "graphics") draw_boxes_base else { check_ggplot2(); draw_boxes_gg }
  fn(
    x = x, theme = theme, theme.values = theme.values,
    legend = legend, legend.title = legend.title,
    horizontal = horizontal, main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}

#' Set or Get Global Options for dualplot
#'
#' @param ... options specified as \code{name = value} pairs (e.g.,
#'   \code{engine = "ggplot2"}), character strings to query specific options,
#'   or empty to retrieve all current \code{dualplot.*} options.
#'
#' @return A named list of current or previous option values.
#' @keywords internal
#'
dualplot_options <- function(...) {
  args <- list(...)
  if (length(args) == 0L) {
    opts <- options()
    opts <- opts[grep("^dualplot\\.", names(opts))]
    if (length(opts) == 0L) return(list())
    names(opts) <- sub("^dualplot\\.", "", names(opts))
    return(opts)
  }
  if (length(args) == 1L && is.list(args[[1L]]) && !is.null(names(args[[1L]]))) {
    args <- args[[1L]]
  }
  if (is.null(names(args)) && all(vapply(args, is.character, logical(1L)))) {
    nms <- unlist(args)
    res <- lapply(paste0("dualplot.", nms), getOption)
    names(res) <- nms
    return(res)
  }
  if (is.null(names(args)) || any(names(args) == "")) {
    stop("options must be specified as name = value", call. = FALSE)
  }
  valid <- c("engine", "gg.theme")
  new <- list()
  for (nm in names(args)) {
    if (!nm %in% valid) {
      warning(
        sprintf("unknown option '%s': expected one of %s",
                nm, paste(valid, collapse = ", ")),
        call. = FALSE
      )
    }
    if (nm == "engine") {
      args[[nm]] <- match.arg(args[[nm]], c("graphics", "ggplot2"))
    }
    new[[paste0("dualplot.", nm)]] <- args[[nm]]
  }
  old <- options(new)
  names(old) <- sub("^dualplot\\.", "", names(old))
  invisible(old)
}

#' Monograph Visual Theme for ggplot2
#'
#' Returns a clean, publication-ready \strong{ggplot2} theme featuring a white
#' background, crisp dark border, optional grid lines, and serif typography.
#'
#' @param grid.type character string specifying which grid lines to display:
#'   one of \code{"none"} (default), \code{"x"}, \code{"y"}, or \code{"xy"}.
#' @param base_size base font size in pts (default \code{11}).
#' @param base_family base font family (default \code{"serif"}).
#' @param base_line_size base size for line elements.
#' @param base_rect_size base size for rectangle elements.
#' @param ... additional arguments passed to \code{ggplot2::theme_light()}.
#'
#' @return A \code{ggplot2} \code{theme} object.
#' @seealso \code{\link{par_monograph}}
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) +
#'     ggplot2::geom_point(size = 2) +
#'     ggplot2::labs(title = "cars") +
#'     theme_monograph(grid.type = "xy")
#' }
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(iris, ggplot2::aes(Petal.Length, Species)) +
#'     ggplot2::geom_boxplot() +
#'     ggplot2::labs(title = "iris") +
#'     theme_monograph(grid.type = "x")
#' }
#' @export
#'
theme_monograph <- function(
    grid.type = c("none", "x", "y", "xy"),
    base_size = 11,
    base_family = "serif",
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22,
    ...
) {
  check_ggplot2()
  grid.type <- match.arg(grid.type)
  grid_x <- grid.type %in% c("x", "xy")
  grid_y <- grid.type %in% c("y", "xy")

  e1 <- ggplot2::theme_light(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    ...
  )
  e2 <- ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      fill = NA,
      colour = "gray5",
      linewidth = ggplot2::rel(0.5)
    ),
    panel.grid.major.x = if (!grid_x) ggplot2::element_blank() else NULL,
    panel.grid.minor.x = if (!grid_x) ggplot2::element_blank() else NULL,
    panel.grid.major.y = if (!grid_y) ggplot2::element_blank() else NULL,
    panel.grid.minor.y = if (!grid_y) ggplot2::element_blank() else NULL
  )
  e1 + e2
}

#' Monograph Graphical Parameters for Base Graphics
#'
#' Wraps \code{\link[graphics]{par}()} to apply a clean, publication-ready base
#' graphics aesthetic matching \code{\link{theme_monograph}()}.
#'
#' @param ... graphical parameters in \code{name = value} form passed to
#'   \code{\link[graphics]{par}()} to override the monograph defaults.
#'
#' @return Invisibly returns a named list of the previous values of the modified
#'   graphical parameters.
#' @seealso \code{\link{theme_monograph}}
#' @examples
#' old_par <- par_monograph()
#' plot(cars, main = "cars")
#' boxplot(Petal.Length ~ Species, iris, horizontal = TRUE, main = "iris")
#' par(old_par)
#' @export
#'
par_monograph <- function(...) {
  dots <- list(...)
  dots <- dots[names(dots) %in% names(graphics::par(no.readonly = TRUE))]
  defaults <- list(
    bg = "white", bty = "o", mar = c(4.1, 4.1, 2.1, 1.1), family = "serif",
    font = 1L, font.axis = 1L, font.lab = 1L, font.main = 1L, font.sub = 1L,
    col = "black", col.axis = "black", col.lab = "black", col.main = "black",
    col.sub = "black", cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1.2,
    cex.sub = 0.9, las = 0L, lty = "solid", lwd = 1, pch = 16L
  )
  args <- utils::modifyList(defaults, dots)
  do.call(graphics::par, args)
}
