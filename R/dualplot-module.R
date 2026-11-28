# Generated from dualplot package — do not edit by hand
# Source: https://github.com/ryo-asashi/dualplot


# R/dualplot.R

`%||%` <- function(x, y) if (is.null(x)) y else x


is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}


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
#' @description
#' Renders vertical or horizontal bar charts, dot charts, and connected
#' waterfall charts using either base graphics or \pkg{ggplot2}.
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
#' @examples
#' vals <- c(Age = 0.42, Income = -0.28, Tenure = 0.65, Region = -0.15)
#'
#' # Horizontal bar chart with diverging color theme
#' draw_bars(vals, theme = "azurerose", xlab = "Contribution", vline = 0)
#'
#' # Dot chart
#' draw_bars(vals, type = "dot", theme = "azurerose", xlab = "Contribution")
#'
#' # Waterfall chart
#' draw_bars(
#'   to = c(1.0, 1.4, 1.1, 1.5),
#'   from = c(0.0, 1.0, 1.4, 1.1),
#'   ticks = c("Intercept", "Age", "Income", "Total"),
#'   connect = TRUE, vline = 0, theme = "azurerose"
#' )
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   draw_bars(vals, theme = "azurerose", xlab = "Contribution", engine = "ggplot2")
#' }
#' @noRd
draw_bars <- function(
    to, from = 0, ticks = NULL, horizontal = TRUE, type = c("bar", "dot"),
    middle = 0, limits = NULL, width = NULL, vline = NULL, connect = FALSE,
    ..., main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, theme.values = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% TRUE
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
#' @description
#' Plots one or more line series (with optional overlaid points) using either
#' base graphics or \pkg{ggplot2}.
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
#' @examples
#' x <- seq(0, 2 * pi, length.out = 50)
#' y <- cbind(Sine = sin(x), Cosine = cos(x))
#' obs <- data.frame(x = c(1, 3, 5), y = sin(c(1, 3, 5)))
#'
#' draw_lines(
#'   x, y, points = obs, theme = "Okabe-Ito",
#'   xlab = "x", ylab = "Value", legend= "bottom", legend.title = "Series"
#' )
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   draw_lines(x, y, points = obs, theme = "Okabe-Ito", engine = "ggplot2")
#' }
#' @noRd
draw_lines <- function(
    x, y, discrete = NULL, limits = NULL, points = NULL,
    ..., main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, theme.values = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% TRUE
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
#' @description
#' Plots 2D scatter points with optional color mapping and discrete axis
#' labels using either base graphics or \pkg{ggplot2}.
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
#' @examples
#' draw_points(
#'   mtcars$wt, mtcars$mpg,
#'   theme = "viridis", theme.values = mtcars$hp,
#'   xlab = "Weight", ylab = "MPG",
#'   legend= "bottom", legend.title = "Horsepower"
#' )
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   draw_points(
#'     mtcars$wt, mtcars$mpg,
#'     theme = "viridis", theme.values = mtcars$hp,
#'     xlab = "Weight", ylab = "MPG", legend.title = "Horsepower",
#'     engine = "ggplot2"
#'   )
#' }
#' @noRd
draw_points <- function(
    x, y, middle = 0, xticks = NULL, yticks = NULL, xlim = NULL, ylim = NULL,
    overlay = FALSE, ..., main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, theme.values = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% TRUE
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
#' @description
#' Visualizes a 2D numeric surface matrix over coordinates (with optional
#' overlaid points) using either base graphics or \pkg{ggplot2}.
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
#' @examples
#' gx <- seq(-2, 2, length.out = 25)
#' gy <- seq(-2, 2, length.out = 25)
#' gz <- outer(gx, gy, function(a, b) sin(a) * cos(b))
#' pts <- data.frame(x = c(-1, 0.5, 1.2), y = c(0.8, -0.5, 1.0))
#'
#' draw_surface(
#'   gx, gy, gz, points = pts,
#'   xlab = "X1", ylab = "X2", legend = "right", legend.title = "Effect"
#' )
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   draw_surface(
#'     gx, gy, gz, points = pts,
#'     xlab = "X1", ylab = "X2", legend.title = "Effect", engine = "ggplot2"
#'   )
#' }
#' @noRd
draw_surface <- function(
    x = seq(0, 1, length.out = nrow(z)),
    y = seq(0, 1, length.out = ncol(z)),
    z, zlim = NULL, middle = 0, xticks = NULL, yticks = NULL,
    points = NULL, ...,  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% TRUE
  fn <- if (engine == "graphics") draw_surface_base else { check_ggplot2(); draw_surface_gg }
  fn(
    x = x, y = y, z = z, theme = theme, legend = legend, legend.title = legend.title,
    zlim = zlim, middle = middle, xticks = xticks, yticks = yticks, points = points,
    main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}


#' Draw Matrix Heatmaps
#'
#' @description
#' Renders a 2D numeric matrix as a tiled heatmap. Following
#' \code{\link[graphics]{image}()}, matrix rows are mapped to the x-axis and
#' columns to the y-axis.
#'
#' @param z numeric matrix of values to plot (rows map to x, columns to y).
#' @param xticks,yticks optional character vectors of tick labels for the x-axis
#'   (rows) and y-axis (columns). Default to \code{rownames(z)} and \code{colnames(z)}.
#' @param zlim optional numeric vector of length 2 for color scale limits.
#' @param middle numeric midpoint for diverging color themes (default \code{0}).
#' @param ... additional graphical parameters.
#' @param main,sub,xlab,ylab optional plot titles and axis labels.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param legend logical or character legend position.
#' @param legend.title optional title string for the legend.
#' @param engine graphics backend: \code{"graphics"} or \code{"ggplot2"}.
#'
#' @return Invisibly returns \code{NULL} for \code{"graphics"}, or a \code{"ggplot"} object for \code{"ggplot2"}.
#' @examples
#' cm <- cor(mtcars[, 1:5])
#' draw_matrix(cm, theme = "azurerose", legend = "right", legend.title = "Corr")
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   draw_matrix(cm, theme = "azurerose", legend.title = "Corr", engine = "ggplot2")
#' }
#' @noRd
draw_matrix <- function(
    z, xticks = rownames(z), yticks = colnames(z), zlim = NULL, middle = 0, ...,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% TRUE
  fn <- if (engine == "graphics") draw_matrix_base else { check_ggplot2(); draw_matrix_gg }
  fn(
    z = z, xticks = xticks, yticks = yticks, zlim = zlim,
    theme = theme, legend = legend, legend.title = legend.title,
    middle = middle, main = main, sub = sub, xlab = xlab, ylab = ylab, ...
  )
}


#' Draw Boxplots
#'
#' @description
#' Draws vertical or horizontal box-and-whisker plots for a list of numeric
#' vectors using either base graphics or \pkg{ggplot2}.
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
#' @examples
#' groups <- split(iris$Sepal.Length, iris$Species)
#' draw_boxes(groups, theme = "Okabe-Ito", xlab = "Sepal Length")
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   draw_boxes(groups, theme = "Okabe-Ito", xlab = "Sepal Length", engine = "ggplot2")
#' }
#' @noRd
draw_boxes <- function(
    x, horizontal = TRUE, ..., main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    theme = NULL, theme.values = NULL, legend = NULL, legend.title = NULL,
    engine = getOption("dualplot.engine", "graphics")
) {
  engine <- match.arg(engine, c("graphics", "ggplot2"))
  legend <- legend %||% TRUE
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
#' @examples
#' old <- dualplot_options(engine = "ggplot2")
#' dualplot_options("engine")
#' dualplot_options(old)
#' @noRd
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


# R/dualplot_aesthetics.R

dict_to_base_params <- c(
  colour    = "col",
  color     = "col",
  size      = "cex",
  shape     = "pch",
  linetype  = "lty",
  linewidth = "lwd",
  title     = "main",
  subtitle  = "sub"
)


dict_to_gg_params <- c(
  col       = "colour",
  color     = "colour",
  fg        = "colour",
  bg        = "fill",
  cex       = "size",
  pch       = "shape",
  lty       = "linetype",
  lwd       = "linewidth"
)


#' Translate and Merge Parameters for Base Graphics
#'
#' Converts ggplot2-style parameter names in \code{dots} to base graphics equivalents,
#' merges them with \code{defaults}, and optionally applies alpha transparency.
#'
#' @param dots a named list of user-supplied parameters (typically \code{list(...)}).
#' @param allowed optional character vector of allowed parameter names.
#' @param defaults a named list of default base graphics parameters.
#' @param alpha.on character vector of parameter names (e.g., \code{"col"}, \code{"fill"})
#'   to which the \code{alpha} parameter should be applied. Set to \code{NULL} to leave \code{alpha} as-is.
#'
#' @return a sanitized named list ready for \code{do.call(graphics::..., args)}.
#' @noRd
cast_to_base_params <- function(
    dots = list(), allowed = NULL, defaults = list(), alpha.on = "col"
) {
  if (length(dots) > 0L && !is.null(names(dots))) {
    nms <- names(dots)
    mapped <- dict_to_base_params[nms]
    names(dots) <- ifelse(is.na(mapped), nms, unname(mapped))
    dots <- dots[!duplicated(names(dots), fromLast = TRUE)]
  }
  out <- utils::modifyList(defaults, dots, keep.null = TRUE)
  if (!is.null(alpha.on)) {
    alpha <- out$alpha
    out$alpha <- NULL
    if (!is.null(alpha)) {
      for (target in alpha.on) {
        if (!is.null(out[[target]])) {
          out[[target]] <- apply_alpha(out[[target]], alpha)
        }
      }
    }
  }
  if (!is.null(allowed)) {
    out <- out[names(out) %in% allowed]
  }
  out
}


#' Translate and Filter Parameters for ggplot2 Layers
#'
#' Converts base graphics parameter names in \code{dots} to standard ggplot2 aesthetics
#' without requiring ggplot2 to be loaded at package build time.
#'
#' @param dots a named list of user-supplied parameters (typically \code{list(...)}).
#' @param allowed optional character vector of allowed parameter names for a specific geom.
#' @param defaults optional named list of default parameters.
#'
#' @return a sanitized named list of parameters for a ggplot2 layer.
#' @noRd
cast_to_gg_params <- function(
    dots = list(), allowed = NULL, defaults = list()
) {
  if (length(dots) > 0L && !is.null(names(dots))) {
    nms <- names(dots)
    mapped <- dict_to_gg_params[nms]
    nms <- ifelse(is.na(mapped), nms, unname(mapped))
    nms <- sub("color$", "colour", nms)
    names(dots) <- nms
    dots <- dots[!duplicated(names(dots), fromLast = TRUE)]
  }
  out <- if (length(defaults) > 0L) {
    utils::modifyList(defaults, dots, keep.null = TRUE)
  } else {
    dots
  }
  if (!is.null(allowed)) {
    out <- out[names(out) %in% allowed]
  }
  out
}


# Extract title/subtitle/xlab/ylab into a ggplot2::labs() object
extract_gg_labs <- function(
    dots = list(), xlab = NULL, ylab = NULL, main = NULL, sub = NULL
) {
  check_ggplot2()
  title <- dots$title %||% dots$main %||% main
  subtitle <- dots$subtitle %||% dots$sub %||% sub
  x <- dots$xlab %||% xlab
  y <- dots$ylab %||% ylab
  args <- list()
  if (!is.null(title)) args$title <- title
  if (!is.null(subtitle)) args$subtitle <- subtitle
  if (!is.null(x)) args$x <- x
  if (!is.null(y)) args$y <- y
  do.call(ggplot2::labs, args)
}


#' Apply Alpha Transparency to Colors (Vectorized & NA-safe)
#'
#' @param col a vector of R colors (names, hex strings, or integer indices).
#' @param alpha a numeric vector of alpha values in \code{[0, 1]}.
#'
#' @return a character vector of hex colors with alpha channel.
#' @noRd
apply_alpha <- function(col, alpha = NULL) {
  if (is.null(col) || is.null(alpha) || all(is.na(col))) {
    return(col)
  }
  n <- max(length(col), length(alpha))
  col <- rep_len(col, n)
  alpha <- pmax(0, pmin(1, rep_len(alpha, n)))
  if (!any(alpha < 1, na.rm = TRUE)) {
    return(col)
  }
  is_na <- is.na(col) | is.na(alpha)
  out <- rep(NA_character_, n)
  if (any(!is_na)) {
    rgbs <- grDevices::col2rgb(col[!is_na], alpha = FALSE)
    out[!is_na] <- grDevices::rgb(
      red   = rgbs[1L, ],
      green = rgbs[2L, ],
      blue  = rgbs[3L, ],
      alpha = round(alpha[!is_na] * 255),
      maxColorValue = 255L
    )
  }
  out
}


# R/dualplot_base.R

adjust_left_margin <- function(ticks, mai = graphics::par("mai"), pad = 1 / 16) {
  if (is.null(ticks) || length(ticks) == 0L) return(mai)
  cex <- graphics::par("cex.axis")
  req <- max(
    graphics::strwidth(as.character(ticks), units = "inches", cex = cex),
    na.rm = TRUE
  )
  req <- 0.42 + req + pad
  if (mai[2L] < req) mai[2L] <- req
  mai
}


normalize_legend_pos <- function(legend) {
  if (isFALSE(legend) || is.null(legend)) return("none")
  if (isTRUE(legend)) return("right")
  .valid_pos <- c("topright", "topleft", "bottomright", "bottomleft",
                  "top", "bottom", "left", "right", "none")
  match.arg(as.character(legend)[1L], .valid_pos)
}


has_base_legend <- function(values, theme, legend = TRUE, limits = NULL) {
  pos <- normalize_legend_pos(legend)
  if (pos == "none" || is.null(theme) || is.null(values)) {
    return(FALSE)
  }
  ct <- colortheme::colortheme(theme)
  if (is_discrete(values) || ct$type == "qualitative") {
    f <- if (is.factor(values)) droplevels(values) else factor(values)
    return(length(levels(f)) > 0L)
  }
  rng <- limits %||% range(values, na.rm = TRUE, finite = TRUE)
  all(is.finite(rng)) && diff(rng) > 0
}


adjust_legend_margin <- function(legend, mai = graphics::par("mai"), space = 0.5) {
  if (isFALSE(legend) || is.null(legend)) return(mai)
  pos <- normalize_legend_pos(legend)
  if (pos == "bottom") mai[1L] <- max(mai[1L], 1.02 + space * 0.85)
  if (pos == "left")   mai[2L] <- max(mai[2L], 0.82 + space)
  if (pos == "top")    mai[3L] <- max(mai[3L], 0.82 + space * 0.85)
  if (pos == "right")  mai[4L] <- max(mai[4L], 0.42 + space)
  mai
}


as_param_matrix <- function(x, nrow, ncol) {
  if (is.matrix(x) && nrow(x) == nrow && ncol(x) == ncol) {
    return(x)
  } else if (length(x) == ncol && ncol > 1L) {
    matrix(x, nrow = nrow, ncol = ncol, byrow = TRUE)
  } else if (length(x) == nrow || length(x) == 1L) {
    matrix(x, nrow = nrow, ncol = ncol, byrow = FALSE)
  } else {
    matrix(rep_len(x, nrow * ncol), nrow = nrow, ncol = ncol)
  }
}


draw_base_legend <- function(
    values, theme, legend = TRUE, title = NULL,
    style = c("fill", "line", "point"), middle = 0, limits = NULL,
    pch = 16L, lty = 1L, lwd = 1L
) {
  pos <- normalize_legend_pos(legend)
  if (pos == "none" || is.null(theme) || is.null(values)) {
    return(invisible(NULL))
  }
  is_outside <- pos %in% c("top", "bottom", "left", "right")
  is_horiz <- pos %in% c("top", "bottom")
  style <- match.arg(style)
  ct <- colortheme::colortheme(theme)

  font_axis <- graphics::par("font.axis")
  font_lab  <- graphics::par("font.lab")

  if (is_outside) {
    opar <- graphics::par(xpd = NA)
    on.exit(graphics::par(opar), add = TRUE)
  }

  usr <- graphics::par("usr")
  dx <- diff(usr[1:2])
  dy <- diff(usr[3:4])

  plt <- graphics::par("plt")
  mai <- graphics::par("mai")
  ux_per_in <- dx / (graphics::par("pin")[1L])
  uy_per_in <- dy / (graphics::par("pin")[2L])

  # Discrete scale
  if (is_discrete(values) || ct$type == "qualitative") {
    f <- if (is.factor(values)) droplevels(values) else factor(values)
    lvls <- levels(f)
    if (length(lvls) == 0L) return(invisible(NULL))
    cols <- ct$palette(length(lvls))

    leg_args <- list(
      legend = lvls, title = title, horiz = is_horiz,
      bty = "n", bg = NA, cex = 0.85, y.intersp = 1.25,
      text.font = font_axis, title.font = font_lab
    )
    if (style == "fill") {
      leg_args$fill <- cols
      leg_args$border <- "gray50"
    } else if (style == "line") {
      leg_args$col <- cols
      leg_args$lty <- lty
      leg_args$lwd <- lwd
    } else {
      leg_args$col <- cols
      leg_args$pch <- pch
    }

    if (is_outside) {
      meas <- do.call(graphics::legend, c(list(x = usr[1L], y = usr[3L], plot = FALSE), leg_args))
      lw <- meas$rect$w
      lh <- meas$rect$h
      pad_in <- 0.12

      if (pos == "right") {
        leg_args$x <- usr[2L] + (mai[4L] - pad_in) * ux_per_in - lw
        leg_args$y <- mean(usr[3:4]) + lh / 2
      } else if (pos == "left") {
        leg_args$x <- usr[1L] - (mai[2L] - pad_in) * ux_per_in
        leg_args$y <- mean(usr[3:4]) + lh / 2
      } else if (pos == "top") {
        leg_args$x <- mean(usr[1:2]) - lw / 2
        leg_args$y <- usr[4L] + (mai[3L] - pad_in * 0.6) * uy_per_in
      } else if (pos == "bottom") {
        leg_args$x <- mean(usr[1:2]) - lw / 2
        leg_args$y <- usr[3L] - (mai[1L] - pad_in * 0.6) * uy_per_in + lh
      }
    } else {
      leg_args$x <- pos
    }
    return(do.call(graphics::legend, leg_args))
  }

  # Continuous scale (colorbar)
  rng <- limits %||% range(values, na.rm = TRUE, finite = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(invisible(NULL))
  if (ct$type == "diverging") {
    zmax <- max(abs(rng - middle))
    rng <- c(-zmax, zmax) + middle
  }

  pad_x <- dx * 0.035
  pad_y <- dy * 0.055
  has_title <- !is.null(title) && nzchar(title)
  ticks <- pretty(rng, n = 3L)
  ticks <- ticks[ticks >= rng[1L] & ticks <= rng[2L]]
  tick_labs <- format(ticks, digits = 3)
  pad_in <- 0.15

  if (is_horiz) {
    w <- dx * 0.45
    h <- dy * 0.045
    x1 <- mean(usr[1:2]) - w / 2
    x2 <- x1 + w
    if (pos == "bottom") {
      y1 <- usr[3L] - (mai[1L] - pad_in * 1.4) * uy_per_in
      y2 <- y1 + h
    } else {
      y2 <- usr[4L] + (mai[3L] - pad_in * 1.2) * uy_per_in
      y1 <- y2 - h
    }
    cols <- matrix(ct$ramp(seq.int(0, 1, length.out = 128L)), nrow = 1L)
    graphics::rasterImage(grDevices::as.raster(cols), x1, y1, x2, y2)

    tx <- x1 + (ticks - rng[1L]) / diff(rng) * w
    graphics::text(tx, y1 - pad_y * 0.35, labels = tick_labs, adj = c(0.5, 1), cex = 0.75, font = font_axis)
    if (has_title) {
      graphics::text(mean(c(x1, x2)), y2 + pad_y * 0.35, labels = title, adj = c(0.5, 0), cex = 0.8, font = font_lab)
    }
  } else {
    w <- dx * 0.045
    h <- if (is_outside) dy * 0.55 else dy * 0.28
    label_w <- max(graphics::strwidth(tick_labs, cex = 0.75), na.rm = TRUE) + pad_x * 0.4

    if (is_outside) {
      if (pos == "right") {
        x2 <- usr[2L] + (mai[4L] - pad_in) * ux_per_in - label_w
        x1 <- x2 - w
      } else {
        x1 <- usr[1L] - (mai[2L] - pad_in) * ux_per_in
        x2 <- x1 + w
      }
      y2 <- mean(usr[3:4]) + h / 2
      y1 <- y2 - h
    } else {
      x1 <- if (grepl("left", pos)) usr[1L] + pad_x else usr[2L] - pad_x - w - label_w
      x2 <- x1 + w
      y2 <- if (grepl("bottom", pos)) usr[3L] + pad_y + h else usr[4L] - pad_y
      y1 <- y2 - h
    }

    cols <- rev(ct$ramp(seq.int(0, 1, length.out = 128L)))
    graphics::rasterImage(grDevices::as.raster(cols), x1, y1, x2, y2)

    ty <- y1 + (ticks - rng[1L]) / diff(rng) * h
    graphics::text(x2 + pad_x * 0.25, ty, labels = tick_labs, adj = c(0, 0.5), cex = 0.75, font = font_axis)
    if (has_title) {
      graphics::text(x1, y2 + pad_y * 0.45, labels = title, adj = c(0, 0), cex = 0.8, font = font_lab)
    }
  }
  invisible(NULL)
}


draw_bars_base <- function(
    to, from = 0, ticks = NULL, horizontal = TRUE, type = c("bar", "dot"),
    theme = NULL, theme.values = NULL, legend = TRUE, legend.title = NULL,
    middle = 0, limits = NULL, width = NULL, vline = NULL, connect = FALSE,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  type <- match.arg(sub("plot$|chart$", "", type), c("bar", "dot"))
  dots <- cast_to_base_params(dots = list(...), defaults = list(), alpha.on = NULL)

  main <- dots$main %||% main
  sub  <- dots$sub  %||% sub
  xlab <- dots$xlab %||% xlab
  ylab <- dots$ylab %||% ylab

  to <- as.matrix(to)
  n <- nrow(to)
  m <- ncol(to)
  ticks <- ticks %||% rownames(to) %||% as.character(seq_len(n))
  g_names <- colnames(to) %||% as.character(seq_len(m))
  from <- as_param_matrix(from %||% 0, n, m)

  theme_cols <- NULL
  cval <- NULL
  if (!is.null(theme)) {
    ct <- colortheme::colortheme(theme)
    cval <- theme.values %||% if (m > 1L) {
      factor(rep(g_names, each = n), levels = g_names)
    } else {
      as.vector(to - from)
    }
    theme_cols <- colortheme::map_to_colors(cval, theme = ct, middle = middle)
  }

  if (type == "bar") {
    raw_fill <- dots$fill %||% theme_cols %||% "gray35"
    raw_col  <- dots$border %||% dots$col %||% NA
    fill_mat <- as_param_matrix(apply_alpha(raw_fill, dots$alpha), n, m)
    col_mat  <- as_param_matrix(raw_col, n, m)
  } else {
    raw_col  <- dots$col %||% theme_cols %||% "gray35"
    col_mat  <- as_param_matrix(apply_alpha(raw_col, dots$alpha), n, m)
  }
  pch_mat <- as_param_matrix(dots$pch %||% 16L, n, m)
  cex_val <- dots$cex %||% 1
  lty_val <- dots$lty %||% (if (type == "dot") 3L else 1L)
  lwd_val <- dots$lwd %||% 1L

  rng <- range(c(to, from, vline), na.rm = TRUE)
  mgn <- if (!is.finite(diff(rng)) || diff(rng) == 0) 0.5 else abs(diff(rng)) * 0.02
  limits <- limits %||% c(rng[1L] - mgn, rng[2L] + mgn)
  at <- if (horizontal) rev(seq_len(n)) else seq_len(n)

  opar <- graphics::par(c("mai", "las"))
  on.exit(graphics::par(opar), add = TRUE)
  mai <- if (horizontal) adjust_left_margin(ticks) else graphics::par("mai")
  if (has_base_legend(cval, theme, legend)) {
    mai <- adjust_legend_margin(legend, mai)
  }
  graphics::par(mai = mai, las = if (horizontal) 1L else graphics::par("las"))

  graphics::plot.new()
  graphics::plot.window(
    xlim = if (horizontal) limits else c(0.5, n + 0.5),
    ylim = if (horizontal) c(0.5, n + 0.5) else limits
  )
  graphics::box()
  graphics::title(main = main, sub = sub, xlab = xlab %||% "", ylab = ylab %||% "")
  graphics::axis(side = if (horizontal) 1L else 2L)
  graphics::axis(side = if (horizontal) 2L else 1L, at = at, labels = ticks)

  if (!is.null(vline)) {
    v_lty <- if (connect) 3L else 1L
    if (horizontal) {
      graphics::abline(v = vline, lwd = 0.5, lty = v_lty, col = graphics::par("fg"))
    } else {
      graphics::abline(h = vline, lwd = 0.5, lty = v_lty, col = graphics::par("fg"))
    }
  }

  if (type == "bar") {
    bw <- (width %||% (if (connect) 0.6 else 0.9)) / m
    offsets <- (seq_len(m) - (m + 1) / 2) * bw
    centers <- outer(at, offsets, "+")
    hw <- bw / 2

    if (horizontal) {
      graphics::rect(
        xleft = as.vector(from), xright = as.vector(to),
        ybottom = as.vector(centers - hw), ytop = as.vector(centers + hw),
        col = as.vector(fill_mat), border = as.vector(col_mat),
        lty = lty_val, lwd = lwd_val
      )
      if (connect && n > 1L && m == 1L) {
        seg_col <- if (all(is.na(col_mat))) graphics::par("fg") else col_mat[-n, 1L]
        graphics::segments(
          x0 = to[-n, 1L], y0 = at[-n] + hw,
          x1 = to[-n, 1L], y1 = at[-1L] - hw,
          col = seg_col, lty = lty_val, lwd = lwd_val
        )
      }
    } else {
      graphics::rect(
        xleft = as.vector(centers - hw), xright = as.vector(centers + hw),
        ybottom = as.vector(from), ytop = as.vector(to),
        col = as.vector(fill_mat), border = as.vector(col_mat),
        lty = lty_val, lwd = lwd_val
      )
      if (connect && n > 1L && m == 1L) {
        seg_col <- if (all(is.na(col_mat))) graphics::par("fg") else col_mat[-n, 1L]
        graphics::segments(
          y0 = to[-n, 1L], x0 = at[-n] - hw,
          y1 = to[-n, 1L], x1 = at[-1L] + hw,
          col = seg_col, lty = lty_val, lwd = lwd_val
        )
      }
    }
  } else {
    bw <- (width %||% 0.6) / m
    offsets <- (seq_len(m) - (m + 1) / 2) * bw
    centers <- outer(at, offsets, "+")
    lcol <- graphics::par("fg") %||% "black"

    if (horizontal) {
      graphics::segments(
        x0 = as.vector(from), y0 = as.vector(centers),
        x1 = as.vector(to),   y1 = as.vector(centers),
        col = lcol, lty = lty_val, lwd = lwd_val
      )
      graphics::points(
        x = as.vector(to), y = as.vector(centers),
        col = as.vector(col_mat), pch = as.vector(pch_mat), cex = cex_val
      )
    } else {
      graphics::segments(
        x0 = as.vector(centers), y0 = as.vector(from),
        x1 = as.vector(centers), y1 = as.vector(to),
        col = lcol, lty = lty_val, lwd = lwd_val
      )
      graphics::points(
        x = as.vector(centers), y = as.vector(to),
        col = as.vector(col_mat), pch = as.vector(pch_mat), cex = cex_val
      )
    }
  }

  if (!is.null(theme)) {
    draw_base_legend(
      values = cval, theme = theme, legend = legend, title = legend.title,
      style = if (type == "bar") "fill" else "point",
      middle = middle, pch = dots$pch %||% 16L
    )
  }
  invisible(NULL)
}


draw_lines_base <- function(
    x, y, theme = NULL, theme.values = NULL,
    legend = TRUE, legend.title = NULL,
    discrete = NULL, limits = NULL, points = NULL,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  y <- as.matrix(y)
  m <- ncol(y)
  discrete <- discrete %||% is_discrete(x)
  x_num <- if (discrete) seq_along(x) else as.numeric(x)
  ord <- if (discrete) seq_along(x_num) else order(x_num)

  dots <- list(...)
  g_names <- colnames(y) %||% paste0("series_", seq_len(m))
  theme_cols <- NULL
  cval <- NULL
  if (!is.null(theme)) {
    cval <- theme.values %||% factor(g_names, levels = g_names)
    theme_cols <- colortheme::map_to_colors(cval, theme = theme)
  }

  if (has_base_legend(cval, theme, legend)) {
    pos <- normalize_legend_pos(legend)
    if (pos %in% c("top", "bottom", "left", "right")) {
      opar <- graphics::par("mai")
      on.exit(graphics::par(mai = opar), add = TRUE)
      graphics::par(mai = adjust_legend_margin(pos, opar))
    }
  }

  defaults <- list(
    x = x_num[ord],
    y = y[ord, , drop = FALSE],
    type = if (discrete) "b" else "l",
    pch = 16L, lty = 1L, lwd = 1L,
    col = theme_cols %||% "black",
    main = main, sub = sub, xlab = xlab %||% "", ylab = ylab %||% "",
    ylim = limits %||% range(c(y, points$y), na.rm = TRUE),
    xaxt = if (discrete) "n" else "s"
  )
  args <- cast_to_base_params(dots = dots, defaults = defaults, alpha.on = "col")
  do.call(graphics::matplot, args)

  if (discrete) {
    graphics::axis(side = 1L, at = x_num, labels = as.character(x))
  }
  if (!is.null(points) && nrow(points) > 0L) {
    px <- if (discrete) {
      if (is.numeric(points$x)) points$x else match(as.character(points$x), as.character(x))
    } else {
      points$x
    }
    p_defaults <- list(
      x = px,
      y = points$y,
      pch = points$pch %||% args$pch %||% 16L,
      col = points$col %||% args$col,
      cex = points$cex %||% args$cex %||% 1
    )
    p_dots <- dots[!names(dots) %in% c("lty", "lwd", "type", "main", "sub", "xlab", "ylab")]
    p_args <- cast_to_base_params(
      dots = p_dots, defaults = p_defaults, alpha.on = "col",
      allowed = c("x", "y", "col", "pch", "cex", "bg")
    )
    do.call(graphics::points, p_args)
  }

  if (!is.null(theme)) {
    draw_base_legend(
      values = cval, theme = theme, legend = legend, title = legend.title,
      style = "line", lty = args$lty[1L], lwd = args$lwd[1L]
    )
  }
  invisible(NULL)
}


draw_points_base <- function(
    x, y, theme = NULL, theme.values = NULL,
    legend = TRUE, legend.title = NULL, middle = 0,
    xticks = NULL, yticks = NULL, xlim = NULL, ylim = NULL,
    overlay = FALSE, main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  theme_cols <- NULL
  if (!is.null(theme) && !is.null(theme.values)) {
    theme_cols <- colortheme::map_to_colors(theme.values, theme = theme, middle = middle)
  }

  if (overlay) {
    defaults <- list(
      x = x, y = y, col = theme_cols %||% 1L, pch = 16L, cex = 1
    )
    args <- cast_to_base_params(
      dots = list(...),
      defaults = defaults,
      allowed = c("x", "y", "col", "pch", "cex", "bg"),
      alpha.on = "col"
    )
    do.call(graphics::points.default, args)
    return(invisible(NULL))
  }

  if (has_base_legend(theme.values, theme, legend)) {
    pos <- normalize_legend_pos(legend)
    if (pos %in% c("top", "bottom", "left", "right")) {
      opar <- graphics::par("mai")
      on.exit(graphics::par(mai = opar), add = TRUE)
      graphics::par(mai = adjust_legend_margin(pos, opar))
    }
  }

  has_custom_axes <- !is.null(xticks) || !is.null(yticks)
  defaults <- list(
    x = x, y = y, type = "p",
    col = theme_cols %||% 1L, pch = 16L, cex = 1,
    xlim = xlim %||% (if (!is.null(xticks)) c(0.5, length(xticks) + 0.5) else NULL),
    ylim = ylim %||% (if (!is.null(yticks)) c(0.5, length(yticks) + 0.5) else NULL),
    xlab = xlab %||% "", ylab = ylab %||% "", main = main, sub = sub,
    axes = !has_custom_axes
  )
  args <- cast_to_base_params(dots = list(...), defaults = defaults, alpha.on = "col")
  do.call(graphics::plot.default, args)

  if (has_custom_axes) {
    graphics::box()
    if (!is.null(xticks)) {
      graphics::axis(1L, at = seq_along(xticks), labels = xticks)
    } else {
      graphics::axis(1L)
    }
    if (!is.null(yticks)) {
      graphics::axis(2L, at = seq_along(yticks), labels = yticks)
    } else {
      graphics::axis(2L)
    }
  }

  if (!is.null(theme) && !is.null(theme.values)) {
    draw_base_legend(
      values = theme.values, theme = theme, legend = legend, title = legend.title,
      style = "point", middle = middle, pch = args$pch[1L]
    )
  }
  invisible(NULL)
}


draw_surface_base <- function(
    x, y, z, theme = NULL, legend = TRUE, legend.title = NULL,
    zlim = NULL, middle = 0, xticks = NULL, yticks = NULL, points = NULL,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  theme <- theme %||% getOption("colortheme.diverging", "azurerose")
  ct <- colortheme::colortheme(theme)
  zlim <- zlim %||% range(z, na.rm = TRUE)
  if (ct$type == "diverging") {
    zmax <- max(abs(zlim - middle))
    zlim <- c(-zmax, zmax) + middle
  }

  if (has_base_legend(as.vector(z), theme, legend, limits = zlim)) {
    pos <- normalize_legend_pos(legend)
    if (pos %in% c("top", "bottom", "left", "right")) {
      opar <- graphics::par("mai")
      on.exit(graphics::par(mai = opar), add = TRUE)
      graphics::par(mai = adjust_legend_margin(pos, opar))
    }
  }

  dots <- cast_to_base_params(dots = list(...), defaults = list(), alpha.on = NULL)
  pal <- ct$ramp(seq.int(0, 1, length.out = 256))
  if (!is.null(dots$alpha)) {
    pal <- apply_alpha(pal, dots$alpha)
  }

  img_defaults <- list(
    x = x, y = y, z = z, zlim = zlim, col = pal,
    xlab = xlab %||% "", ylab = ylab %||% "", main = main, sub = sub,
    axes = FALSE
  )
  img_allowed <- c(
    "x", "y", "z", "col", "zlim", "xlim", "ylim",
    "xlab", "ylab", "main", "sub", "axes", "useRaster"
  )
  img_args <- cast_to_base_params(
    dots = dots, defaults = img_defaults, allowed = img_allowed, alpha.on = NULL
  )
  do.call(graphics::image.default, img_args)

  graphics::box()
  if (!is.null(xticks)) {
    graphics::axis(1L, at = seq_along(xticks), labels = xticks)
  } else {
    graphics::axis(1L)
  }
  if (!is.null(yticks)) {
    graphics::axis(2L, at = seq_along(yticks), labels = yticks)
  } else {
    graphics::axis(2L)
  }

  if (!is.null(points) && nrow(points) > 0L) {
    pt_args <- cast_to_base_params(
      dots = dots,
      defaults = list(
        x = points$x, y = points$y,
        pch = points$pch %||% 16L,
        col = points$col %||% 1L,
        cex = points$cex %||% 1
      ),
      allowed = c("x", "y", "col", "pch", "cex", "bg"),
      alpha.on = "col"
    )
    do.call(graphics::points.default, pt_args)
  }

  draw_base_legend(
    values = as.vector(z), theme = theme, legend = legend, title = legend.title,
    style = "fill", middle = middle, limits = zlim
  )
  invisible(NULL)
}


draw_matrix_base <- function(
    z, xticks = rownames(z), yticks = colnames(z), zlim = NULL, theme = NULL,
    legend = TRUE, legend.title = NULL, middle = 0,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  nr <- nrow(z)
  nc <- ncol(z)
  xticks <- xticks %||% as.character(seq_len(nr))
  yticks <- yticks %||% as.character(seq_len(nc))

  theme <- theme %||% getOption("colortheme.sequential", "grayscale")
  ct <- colortheme::colortheme(theme)
  palette <- ct$ramp(seq.int(0, 1, length.out = 256L))

  zlim <- zlim %||% range(z, na.rm = TRUE)
  if (ct$type == "diverging") {
    zmax <- max(abs(zlim - middle))
    zlim <- c(-zmax, zmax) + middle
  }

  opar <- graphics::par(c("mai", "las"))
  on.exit(graphics::par(opar), add = TRUE)
  mai <- adjust_left_margin(yticks)
  if (has_base_legend(as.vector(z), theme, legend, limits = zlim)) {
    mai <- adjust_legend_margin(legend, mai)
  }
  graphics::par(mai = mai, las = 1L)

  defaults <- list(
    x = seq_len(nr), y = seq_len(nc), z = z, zlim = zlim,
    fill = palette, col = NA, axes = FALSE,
    main = main, sub = sub, xlab = xlab %||% "", ylab = ylab %||% "",
    lty = 1L, lwd = 1L
  )
  args <- cast_to_base_params(dots = list(...), defaults = defaults, alpha.on = "fill")
  grid_col <- args$col[1L]
  args$col <- args$fill
  args$fill <- NULL

  img_allowed <- c("x", "y", "z", "col", "zlim", "xlim", "ylim", "axes", "xlab", "ylab", "main", "sub")
  do.call(graphics::image.default, args[names(args) %in% img_allowed])

  at_x <- seq_len(nr + 2L) - 1L
  at_y <- seq_len(nc + 2L) - 1L
  graphics::axis(1L, at = at_x, labels = c("", xticks, ""))
  graphics::axis(2L, at = at_y, labels = c("", yticks, ""))
  graphics::axis(3L, labels = FALSE, tick = TRUE, at = at_x, lwd.ticks = 0)
  graphics::axis(4L, labels = FALSE, tick = TRUE, at = at_y, lwd.ticks = 0)
  if (!is.na(grid_col)) {
    graphics::abline(
      v = seq_len(nr) - 0.5, h = seq_len(nc) - 0.5,
      col = grid_col, lty = args$lty, lwd = args$lwd
    )
  }

  draw_base_legend(
    values = as.vector(z), theme = theme, legend = legend, title = legend.title,
    style = "fill", middle = middle, limits = zlim
  )
  invisible(NULL)
}


draw_boxes_base <- function(
    x, theme = NULL, theme.values = NULL,
    legend = TRUE, legend.title = NULL, horizontal = TRUE,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  x <- as.list(x)
  keep <- vapply(x, function(v) is.numeric(v) || is.logical(v), logical(1L))
  x <- lapply(x[keep], as.numeric)
  nms <- names(x) %||% as.character(seq_along(x))
  cval <- if (!is.null(theme)) (theme.values %||% factor(nms, levels = nms)) else NULL
  if (horizontal) {
    x <- rev(x)
    if (!is.null(cval)) cval <- rev(cval)
  }
  opar <- graphics::par(c("mai", "las"))
  on.exit(graphics::par(opar), add = TRUE)
  mai <- if (horizontal) adjust_left_margin(nms) else graphics::par("mai")
  if (has_base_legend(cval, theme, legend)) {
    mai <- adjust_legend_margin(legend, mai)
  }
  graphics::par(mai = mai, las = if (horizontal) 1L else graphics::par("las"))
  theme_cols <- if (!is.null(theme)) colortheme::map_to_colors(cval, theme = theme) else NULL

  defaults <- list(
    x = x, fill = theme_cols %||% NA, col = "black", pch = 16L,
    main = main, sub = sub, xlab = xlab %||% "", ylab = ylab %||% "",
    horizontal = horizontal
  )
  args <- cast_to_base_params(dots = list(...), defaults = defaults, alpha.on = "fill")
  args$border <- args$col
  args$col <- args$fill
  args$fill <- NULL
  args$boxwex <- args$boxwex %||% args$width
  args$width <- NULL
  do.call(graphics::boxplot.default, args)

  if (!is.null(theme)) {
    draw_base_legend(
      values = cval, theme = theme, legend = legend, title = legend.title,
      style = "fill"
    )
  }
  invisible(NULL)
}


# R/dualplot_gg.R

if (getRversion() >= "2.15.1") {
  utils::globalVariables(".data")
}


gg_common_params <- c(
  "na.rm", "show.legend", "inherit.aes", "key_glyph", "position"
)


make_gg_layer <- function(geom_name, allowed_aes) {
  allowed <- unique(c(allowed_aes, gg_common_params))
  function(mapping = NULL, data = NULL, ...) {
    check_ggplot2()
    geom_fun <- getExportedValue("ggplot2", geom_name)
    dots <- cast_to_gg_params(list(...), allowed = allowed)
    do.call(geom_fun, c(list(mapping = mapping, data = data), dots))
  }
}

#' Low-Level Filtered ggplot2 Layer Constructors
#' @name gg_layers
#' @param mapping set of aesthetic mappings created by \code{ggplot2::aes()}.
#' @param data a data frame to be displayed in this layer.
#' @param ... additional parameters (both base and ggplot2 names supported).
#' @return a \code{ggplot2} layer object (or list of layers for compound geoms).
#' @noRd
layer_point <- make_gg_layer(
  "geom_point",
  c("colour", "alpha", "shape", "size", "fill", "stroke")
)

#' @rdname gg_layers
#' @noRd
layer_line <- make_gg_layer(
  "geom_line",
  c("colour", "alpha", "linetype", "linewidth", "lineend", "linejoin", "linemitre", "arrow")
)

#' @rdname gg_layers
#' @noRd
layer_path <- make_gg_layer(
  "geom_path",
  c("colour", "alpha", "linetype", "linewidth", "lineend", "linejoin", "linemitre", "arrow")
)

#' @rdname gg_layers
#' @noRd
layer_linerange <- make_gg_layer(
  "geom_linerange",
  c("colour", "alpha", "linetype", "linewidth", "orientation")
)

#' @rdname gg_layers
#' @noRd
layer_col <- make_gg_layer(
  "geom_col",
  c("colour", "fill", "alpha", "linewidth", "linetype", "width", "just")
)

#' @rdname gg_layers
#' @noRd
layer_rect <- make_gg_layer(
  "geom_rect",
  c("colour", "fill", "alpha", "linewidth", "linetype", "linejoin")
)

#' @rdname gg_layers
#' @noRd
layer_tile <- make_gg_layer(
  "geom_tile",
  c("colour", "fill", "alpha", "linewidth", "linetype", "width", "height")
)

#' @rdname gg_layers
#' @noRd
layer_raster <- make_gg_layer(
  "geom_raster",
  c("fill", "alpha", "interpolate", "hjust", "vjust")
)

#' @rdname gg_layers
#' @noRd
layer_boxplot <- make_gg_layer(
  "geom_boxplot",
  c("colour", "fill", "alpha", "linewidth", "linetype", "shape", "size", "width",
    "outlier.colour", "outlier.fill", "outlier.shape", "outlier.size",
    "outlier.stroke", "outlier.alpha", "notch", "notchwidth", "varwidth")
)

#' @rdname gg_layers
#' @noRd
layer_linepoint <- function(mapping = NULL, data = NULL, ...) {
  list(
    layer_line(mapping = mapping, data = data, ...),
    layer_point(mapping = mapping, data = data, ...)
  )
}

#' @rdname gg_layers
#' @noRd
layer_dotchart <- function(mapping = NULL, data = NULL, ...) {
  check_ggplot2()
  dots <- cast_to_gg_params(list(...))
  line_dots <- dots[names(dots) != "colour"]
  tli <- ggplot2::theme_get()$line
  line_defaults <- list(
    colour    = tli$colour %||% "black",
    linetype  = 3L,
    linewidth = tli$linewidth %||% 0.5
  )
  line_args <- c(
    list(mapping = mapping, data = data),
    utils::modifyList(line_defaults, line_dots)
  )
  pt_mapping <- mapping
  pt_mapping$xmin <- pt_mapping$xmax <- pt_mapping$ymin <- pt_mapping$ymax <- NULL
  pt_args <- c(list(mapping = pt_mapping, data = data), dots)

  list(
    do.call(layer_linerange, line_args),
    do.call(layer_point, pt_args)
  )
}

#' @rdname gg_layers
#' @noRd
layer_vline <- function(xintercept = 0, scale_lwd = 0.5, ...) {
  check_ggplot2()
  tli <- ggplot2::theme_get()$line
  defaults <- list(
    xintercept = xintercept,
    linewidth  = (tli$linewidth %||% 0.5) * scale_lwd,
    colour     = tli$colour %||% "black",
    linetype   = 1L
  )
  allowed <- c("xintercept", "colour", "linewidth", "linetype", "alpha", "na.rm", "show.legend")
  args <- cast_to_gg_params(list(...), allowed = allowed, defaults = defaults)
  do.call(ggplot2::geom_vline, args)
}

#' @rdname gg_layers
#' @noRd
layer_hline <- function(yintercept = 0, scale_lwd = 0.5, ...) {
  check_ggplot2()
  tli <- ggplot2::theme_get()$line
  defaults <- list(
    yintercept = yintercept,
    linewidth  = (tli$linewidth %||% 0.5) * scale_lwd,
    colour     = tli$colour %||% "black",
    linetype   = 1L
  )
  allowed <- c("yintercept", "colour", "linewidth", "linetype", "alpha", "na.rm", "show.legend")
  args <- cast_to_gg_params(list(...), allowed = allowed, defaults = defaults)
  do.call(ggplot2::geom_hline, args)
}

safe_col_name <- function(lbl, fallback, reserved = character(0L)) {
  if (!is.character(lbl) || length(lbl) != 1L || !nzchar(lbl) || lbl %in% reserved) {
    return(fallback)
  }
  lbl
}

apply_theme_and_legend <- function(
    pl, legend = TRUE, legend.on = "fill", legend.title = NULL
) {
  gg_th <- getOption("dualplot.gg.theme", NULL)
  if (!is.null(gg_th)) {
    pl <- pl + gg_th
  }
  lab_args <- list()
  if ("fill" %in% legend.on) lab_args["fill"] <- list(legend.title)
  if (any(c("color", "colour") %in% legend.on)) lab_args["color"] <- list(legend.title)
  if (length(lab_args) > 0L) {
    pl <- pl + do.call(ggplot2::labs, lab_args)
  }
  if (isFALSE(legend) || is.null(legend)) {
    return(pl + ggplot2::theme(legend.position = "none"))
  }
  if (is.character(legend) && length(legend) == 1L) {
    pos <- switch(
      legend,
      topright    = c(0.98, 0.98),
      topleft     = c(0.02, 0.98),
      bottomright = c(0.98, 0.02),
      bottomleft  = c(0.02, 0.02),
      legend
    )
    just <- switch(
      legend,
      topright    = c(1, 1),
      topleft     = c(0, 1),
      bottomright = c(1, 0),
      bottomleft  = c(0, 0),
      NULL
    )
    if (is.numeric(pos)) {
      return(pl + ggplot2::theme(
        legend.position = pos,
        legend.justification = just,
        legend.background = ggplot2::element_blank()
      ))
    }
    return(pl + ggplot2::theme(legend.position = pos))
  }
  pl
}

draw_bars_gg <- function(
    to, from = 0, ticks = NULL, horizontal = TRUE, type = c("bar", "dot"),
    theme = NULL, theme.values = NULL, legend = TRUE, legend.title = NULL,
    middle = 0, limits = NULL, width = NULL, vline = NULL, connect = FALSE,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  type <- match.arg(sub("plot$|chart$", "", type), c("bar", "dot"))
  dots <- cast_to_gg_params(list(...))

  to <- as.matrix(to)
  n <- nrow(to)
  m <- ncol(to)
  ticks <- ticks %||% rownames(to) %||% as.character(seq_len(n))
  from <- as_param_matrix(from %||% 0, n, m)
  g_names <- colnames(to) %||% as.character(seq_len(m))
  cat_levels <- if (horizontal) rev(ticks) else ticks

  cat_lbl <- if (horizontal) ylab else xlab
  val_lbl <- if (horizontal) xlab else ylab
  cat_col <- safe_col_name(cat_lbl, "category", c("group", "from", "diff", ".cval"))
  val_col <- safe_col_name(val_lbl, "to", c("group", "from", "diff", ".cval", cat_col))

  df <- data.frame(
    category = factor(rep(ticks, times = m), levels = cat_levels),
    group    = factor(rep(g_names, each = n), levels = g_names),
    from     = as.vector(from),
    to       = as.vector(to),
    diff     = as.vector(to - from),
    stringsAsFactors = FALSE
  )
  names(df)[c(1L, 4L)] <- c(cat_col, val_col)

  has_const_aes <- (type == "bar" && !is.null(dots$fill)) ||
                   (type == "dot" && !is.null(dots$colour))
  use_theme <- !is.null(theme) && !has_const_aes
  if (use_theme) {
    ct <- colortheme::colortheme(theme)
    cval <- theme.values %||% (if (m > 1L) df$group else df$diff)
    df$.cval <- as.vector(cval)
  }

  pl <- if (horizontal) {
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[val_col]], y = .data[[cat_col]]))
  } else {
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[cat_col]], y = .data[[val_col]]))
  }

  if (!is.null(vline)) {
    v_lty <- if (connect) 3L else 1L
    if (horizontal) {
      pl <- pl + layer_vline(xintercept = vline, linewidth = 0.25, linetype = v_lty)
    } else {
      pl <- pl + layer_hline(yintercept = vline, linewidth = 0.25, linetype = v_lty)
    }
  }

  if (type == "bar") {
    w <- width %||% (if (connect) 0.6 else 0.9)
    if (connect && m == 1L) {
      hw <- w / 2
      df$idx <- as.integer(df[[cat_col]])
      df$cmin <- df$idx - hw
      df$cmax <- df$idx + hw

      rect_aes <- if (horizontal) {
        ggplot2::aes(
          xmin = .data[["from"]], xmax = .data[[val_col]],
          ymin = .data[["cmin"]], ymax = .data[["cmax"]]
        )
      } else {
        ggplot2::aes(
          xmin = .data[["cmin"]], xmax = .data[["cmax"]],
          ymin = .data[["from"]], ymax = .data[[val_col]]
        )
      }
      rect_args <- list(mapping = rect_aes, data = df)
      if (use_theme) {
        rect_args$mapping$fill <- ggplot2::aes(fill = .data[[".cval"]])[[1L]]
      } else if (is.null(dots$fill)) {
        rect_args$fill <- "gray35"
      }
      pl <- pl + do.call(layer_rect, c(rect_args, list(...)))

      if (n > 1L) {
        seg_df <- df[-n, , drop = FALSE]
        tli <- ggplot2::theme_get()$line
        seg_col <- dots$colour %||% tli$colour %||% "black"
        seg_lty <- dots$linetype %||% 1L
        seg_lwd <- dots$linewidth %||% 0.5

        if (horizontal) {
          seg_df$c_next <- df$cmin[-1L]
          pl <- pl + layer_linerange(
            mapping = ggplot2::aes(
              x = .data[[val_col]], ymin = .data[["c_next"]], ymax = .data[["cmax"]]
            ),
            data = seg_df, colour = seg_col, linetype = seg_lty, linewidth = seg_lwd
          )
        } else {
          seg_df$c_next <- df$cmax[-1L]
          pl <- pl + ggplot2::geom_segment(
            mapping = ggplot2::aes(
              x = .data[["cmin"]], xend = .data[["c_next"]],
              y = .data[[val_col]], yend = .data[[val_col]]
            ),
            data = seg_df, colour = seg_col, linetype = seg_lty, linewidth = seg_lwd
          )
        }
      }
    } else {
      bar_aes <- if (m > 1L) ggplot2::aes(group = .data[["group"]]) else ggplot2::aes()
      if (use_theme) bar_aes$fill <- ggplot2::aes(fill = .data[[".cval"]])[[1L]]
      pos <- if (m > 1L) ggplot2::position_dodge(width = w) else "identity"
      col_args <- list(mapping = bar_aes, position = pos, width = w)
      if (!use_theme && is.null(dots$fill)) col_args$fill <- "gray35"
      pl <- pl + do.call(layer_col, c(col_args, list(...)))
    }
    if (use_theme) {
      pl <- pl + colortheme::scale_fill_theme(theme = ct, discrete = is_discrete(df$.cval), middle = middle)
    }
  } else {
    w <- width %||% 0.6
    pos <- if (m > 1L) ggplot2::position_dodge(width = w) else "identity"
    dot_aes <- if (horizontal) {
      ggplot2::aes(xmin = .data[["from"]], xmax = .data[[val_col]], y = .data[[cat_col]], group = .data[["group"]])
    } else {
      ggplot2::aes(x = .data[[cat_col]], ymin = .data[["from"]], ymax = .data[[val_col]], group = .data[["group"]])
    }
    if (use_theme) dot_aes$colour <- ggplot2::aes(colour = .data[[".cval"]])[[1L]]
    dot_args <- list(mapping = dot_aes, position = pos)
    if (!use_theme && is.null(dots$colour)) dot_args$colour <- "gray35"
    pl <- pl + do.call(layer_dotchart, c(dot_args, list(...)))
    if (use_theme) {
      pl <- pl + colortheme::scale_color_theme(theme = ct, discrete = is_discrete(df$.cval), middle = middle)
    }
  }

  if (!is.null(limits)) {
    pl <- if (horizontal) {
      pl + ggplot2::coord_cartesian(xlim = limits)
    } else {
      pl + ggplot2::coord_cartesian(ylim = limits)
    }
  }

  pl <- pl + ggplot2::labs(title = main, subtitle = sub, x = xlab, y = ylab)
  apply_theme_and_legend(
    pl,
    legend = legend,
    legend.on = if (use_theme) (if (type == "bar") "fill" else "color") else character(0L),
    legend.title = legend.title
  )
}

draw_lines_gg <- function(
    x, y, theme = NULL, theme.values = NULL,
    legend = TRUE, legend.title = NULL,
    discrete = NULL, limits = NULL, points = NULL,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  dots <- cast_to_gg_params(list(...))
  y <- as.matrix(y)
  m <- ncol(y)
  discrete <- discrete %||% is_discrete(x)
  g_names <- colnames(y) %||% paste0("series_", seq_len(m))
  x_col <- if (discrete) factor(x, levels = unique(as.character(x))) else x

  use_theme <- !is.null(theme) && is.null(dots$colour)
  cvals <- if (use_theme) (theme.values %||% factor(g_names, levels = g_names)) else NULL

  xcol <- safe_col_name(xlab, "x", c("group", ".cval"))
  ycol <- safe_col_name(ylab, "y", c("group", ".cval", xcol))
  df_list <- lapply(seq_len(m), function(j) {
    out <- data.frame(
      x = x_col,
      y = y[, j],
      group = factor(g_names[j], levels = g_names),
      stringsAsFactors = FALSE
    )
    names(out)[1:2] <- c(xcol, ycol)
    if (use_theme) out$.cval <- cvals[j]
    out
  })
  df <- do.call(rbind, df_list)

  line_aes <- ggplot2::aes(group = .data[["group"]])
  if (use_theme) line_aes$colour <- ggplot2::aes(colour = .data[[".cval"]])[[1L]]

  pl <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]])) +
    layer_line(mapping = line_aes, ...)

  if (discrete) {
    pt_aes <- ggplot2::aes(group = .data[["group"]])
    if (use_theme) pt_aes$colour <- ggplot2::aes(colour = .data[[".cval"]])[[1L]]
    pl <- pl + layer_point(mapping = pt_aes, ...)
  }

  if (!is.null(points) && nrow(points) > 0L) {
    pdf <- points
    if (discrete) pdf$x <- factor(pdf$x, levels = levels(x_col))
    names(pdf)[match(c("x", "y"), names(pdf))] <- c(xcol, ycol)
    pt_aes <- ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]])
    if (use_theme && ".cval" %in% names(pdf)) {
      pt_aes$colour <- ggplot2::aes(colour = .data[[".cval"]])[[1L]]
    }
    pl <- pl + layer_point(mapping = pt_aes, data = pdf, ...)
  }

  if (use_theme) {
    pl <- pl + colortheme::scale_color_theme(theme = theme, discrete = is_discrete(df$.cval))
  }
  if (!is.null(limits)) {
    pl <- pl + ggplot2::coord_cartesian(ylim = limits)
  }
  pl <- pl + ggplot2::labs(title = main, subtitle = sub, x = xlab, y = ylab)
  apply_theme_and_legend(
    pl,
    legend = legend,
    legend.on = if (use_theme) "color" else character(0L),
    legend.title = legend.title
  )
}

draw_points_gg <- function(
    x, y, theme = NULL, theme.values = NULL,
    legend = TRUE, legend.title = NULL, middle = 0,
    xticks = NULL, yticks = NULL, xlim = NULL, ylim = NULL,
    overlay = FALSE, main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  xcol <- safe_col_name(xlab, "x", ".cval")
  ycol <- safe_col_name(ylab, "y", c(".cval", xcol))
  dots <- cast_to_gg_params(list(...))
  df <- stats::setNames(data.frame(x, y), c(xcol, ycol))

  use_theme <- !is.null(theme) && !is.null(theme.values) && is.null(dots$colour)
  if (use_theme) df$.cval <- theme.values

  pt_aes <- if (use_theme) ggplot2::aes(colour = .data[[".cval"]]) else ggplot2::aes()
  pl <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]])) +
    layer_point(mapping = pt_aes, ...)

  if (!is.null(xticks)) {
    pl <- pl + ggplot2::scale_x_continuous(breaks = seq_along(xticks), labels = xticks)
    xlim <- xlim %||% c(0.5, length(xticks) + 0.5)
  }
  if (!is.null(yticks)) {
    pl <- pl + ggplot2::scale_y_continuous(breaks = seq_along(yticks), labels = yticks)
    ylim <- ylim %||% c(0.5, length(yticks) + 0.5)
  }
  if (use_theme) {
    pl <- pl + colortheme::scale_color_theme(theme = theme, discrete = is_discrete(df$.cval), middle = middle)
  }
  if (!is.null(xlim) || !is.null(ylim)) {
    pl <- pl + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  }
  pl <- pl + ggplot2::labs(title = main, subtitle = sub, x = xlab, y = ylab)
  apply_theme_and_legend(
    pl,
    legend = legend,
    legend.on = if (use_theme) "color" else character(0L),
    legend.title = legend.title
  )
}

draw_surface_gg <- function(
    x, y, z, theme = NULL, legend = TRUE, legend.title = NULL,
    zlim = NULL, middle = 0, xticks = NULL, yticks = NULL, points = NULL,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  xcol <- safe_col_name(xlab, "x")
  ycol <- safe_col_name(ylab, "y", xcol)
  zcol <- safe_col_name(legend.title, "z", c(xcol, ycol))
  grid <- expand.grid(x, y)
  names(grid) <- c(xcol, ycol)
  grid[[zcol]] <- as.vector(z)

  theme <- theme %||% getOption("colortheme.diverging", "azurerose")
  pl <- ggplot2::ggplot(grid, ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]])) +
    layer_raster(mapping = ggplot2::aes(fill = .data[[zcol]]), ...) +
    colortheme::scale_fill_theme(theme = theme, middle = middle, limits = zlim)

  if (!is.null(xticks)) {
    pl <- pl + ggplot2::scale_x_continuous(breaks = seq_along(xticks), labels = xticks)
  }
  if (!is.null(yticks)) {
    pl <- pl + ggplot2::scale_y_continuous(breaks = seq_along(yticks), labels = yticks)
  }
  if (!is.null(points) && nrow(points) > 0L) {
    pdf <- points
    names(pdf)[match(c("x", "y"), names(pdf))] <- c(xcol, ycol)
    pl <- pl + layer_point(
      mapping = ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]]),
      data = pdf, ...
    )
  }
  pl <- pl + ggplot2::labs(title = main, subtitle = sub, x = xlab, y = ylab)
  apply_theme_and_legend(pl, legend = legend, legend.on = "fill", legend.title = legend.title)
}

draw_matrix_gg <- function(
    z, xticks = rownames(z), yticks = colnames(z), zlim = NULL, theme = NULL,
    legend = TRUE, legend.title = NULL, middle = 0,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  nr <- nrow(z)
  nc <- ncol(z)
  xticks <- xticks %||% as.character(seq_len(nr))
  yticks <- yticks %||% as.character(seq_len(nc))
  xcol <- safe_col_name(xlab, "x")
  ycol <- safe_col_name(ylab, "y", xcol)
  zcol <- safe_col_name(legend.title, "z", c(xcol, ycol))
  grid <- expand.grid(
    factor(xticks, levels = xticks),
    factor(yticks, levels = yticks)
  )
  names(grid) <- c(xcol, ycol)
  grid[[zcol]] <- as.vector(z)
  theme <- theme %||% getOption("colortheme.sequential", "grayscale")

  pl <- ggplot2::ggplot(grid, ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]])) +
    layer_tile(mapping = ggplot2::aes(fill = .data[[zcol]]), ...) +
    colortheme::scale_fill_theme(theme = theme, middle = middle, limits = zlim) +
    ggplot2::labs(title = main, subtitle = sub, x = xlab, y = ylab)
  apply_theme_and_legend(pl, legend = legend, legend.title = legend.title)
}

draw_boxes_gg <- function(
    x, theme = NULL, theme.values = NULL,
    legend = TRUE, legend.title = NULL, horizontal = TRUE,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  x <- as.list(x)
  keep <- vapply(x, function(v) is.numeric(v) || is.logical(v), logical(1L))
  x <- lapply(x[keep], as.numeric)
  dots <- cast_to_gg_params(list(...))
  nms <- names(x) %||% as.character(seq_along(x))
  use_theme <- !is.null(theme) && is.null(dots$fill)
  cvals <- if (use_theme) (theme.values %||% factor(nms, levels = nms)) else NULL
  cat_levels <- if (horizontal) rev(nms) else nms

  cat_lbl <- if (horizontal) ylab else xlab
  val_lbl <- if (horizontal) xlab else ylab
  cat_col <- safe_col_name(cat_lbl, "category", ".cval")
  val_col <- safe_col_name(val_lbl, "value", c(".cval", cat_col))

  df_list <- lapply(seq_along(x), function(i) {
    out <- data.frame(
      category = factor(rep(nms[i], length(x[[i]])), levels = cat_levels),
      value = x[[i]]
    )
    names(out) <- c(cat_col, val_col)
    if (use_theme) out$.cval <- cvals[i]
    out
  })
  df <- do.call(rbind, df_list)

  box_aes <- if (use_theme) ggplot2::aes(fill = .data[[".cval"]]) else ggplot2::aes()
  pl <- if (horizontal) {
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[val_col]], y = .data[[cat_col]])) +
      layer_boxplot(mapping = box_aes, ...)
  } else {
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[cat_col]], y = .data[[val_col]])) +
      layer_boxplot(mapping = box_aes, ...)
  }

  if (use_theme) {
    pl <- pl + colortheme::scale_fill_theme(theme = theme, discrete = is_discrete(df$.cval))
  }
  pl <- pl + ggplot2::labs(title = main, subtitle = sub, x = xlab, y = ylab)
  apply_theme_and_legend(
    pl,
    legend = legend,
    legend.on = if (use_theme) "fill" else character(0L),
    legend.title = legend.title
  )
}

