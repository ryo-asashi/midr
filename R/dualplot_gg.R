# R/dualplot_gg.R
# Internal ggplot2 rendering primitives

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
#' @keywords internal
#'
layer_point <- make_gg_layer(
  "geom_point",
  c("colour", "alpha", "shape", "size", "fill", "stroke")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_line <- make_gg_layer(
  "geom_line",
  c("colour", "alpha", "linetype", "linewidth", "lineend", "linejoin", "linemitre", "arrow")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_path <- make_gg_layer(
  "geom_path",
  c("colour", "alpha", "linetype", "linewidth", "lineend", "linejoin", "linemitre", "arrow")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_linerange <- make_gg_layer(
  "geom_linerange",
  c("colour", "alpha", "linetype", "linewidth", "orientation")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_col <- make_gg_layer(
  "geom_col",
  c("colour", "fill", "alpha", "linewidth", "linetype", "width", "just")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_rect <- make_gg_layer(
  "geom_rect",
  c("colour", "fill", "alpha", "linewidth", "linetype", "linejoin")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_tile <- make_gg_layer(
  "geom_tile",
  c("colour", "fill", "alpha", "linewidth", "linetype", "width", "height")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_raster <- make_gg_layer(
  "geom_raster",
  c("fill", "alpha", "interpolate", "hjust", "vjust")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_boxplot <- make_gg_layer(
  "geom_boxplot",
  c("colour", "fill", "alpha", "linewidth", "linetype", "shape", "size", "width",
    "outlier.colour", "outlier.fill", "outlier.shape", "outlier.size",
    "outlier.stroke", "outlier.alpha", "notch", "notchwidth", "varwidth")
)

#' @rdname gg_layers
#' @keywords internal
#'
layer_linepoint <- function(mapping = NULL, data = NULL, ...) {
  list(
    layer_line(mapping = mapping, data = data, ...),
    layer_point(mapping = mapping, data = data, ...)
  )
}

#' @rdname gg_layers
#' @keywords internal
#'
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
#' @keywords internal
#'
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
#' @keywords internal
#'
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
  if (isFALSE(legend)) {
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
    ct <- colortheme(theme)
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
        rect_args$fill <- "gray65"
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
      if (!use_theme && is.null(dots$fill)) col_args$fill <- "gray65"
      pl <- pl + do.call(layer_col, c(col_args, list(...)))
    }
    if (use_theme) {
      pl <- pl + scale_fill_theme(theme = ct, middle = middle)
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
      pl <- pl + scale_color_theme(theme = ct, middle = middle)
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
    pl <- pl + scale_color_theme(theme = theme)
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
    pl <- pl + scale_color_theme(theme = theme, middle = middle)
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
    scale_fill_theme(theme = theme, middle = middle, limits = zlim)

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
    z, ticks = rownames(z), theme = NULL,
    legend = TRUE, legend.title = NULL, middle = 0,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  m <- nrow(z)
  ticks <- ticks %||% as.character(seq_len(m))
  xcol <- safe_col_name(xlab, "x")
  ycol <- safe_col_name(ylab, "y", xcol)
  zcol <- safe_col_name(legend.title, "z", c(xcol, ycol))
  grid <- expand.grid(
    factor(ticks, levels = ticks),
    factor(ticks, levels = ticks)
  )
  names(grid) <- c(xcol, ycol)
  grid[[zcol]] <- as.vector(z)
  theme <- theme %||% getOption("colortheme.sequential", "grayscale")

  pl <- ggplot2::ggplot(grid, ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]])) +
    layer_tile(mapping = ggplot2::aes(fill = .data[[zcol]]), ...) +
    scale_fill_theme(theme = theme, middle = middle) +
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
    pl <- pl + scale_fill_theme(theme = theme)
  }
  pl <- pl + ggplot2::labs(title = main, subtitle = sub, x = xlab, y = ylab)
  apply_theme_and_legend(
    pl,
    legend = legend,
    legend.on = if (use_theme) "fill" else character(0L),
    legend.title = legend.title
  )
}
