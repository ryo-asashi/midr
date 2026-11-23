# R/dualplot_base.R
# Internal Base graphics rendering primitives

adjust_left_margin <- function(ticks, mai = graphics::par("mai"), pad = 1 / 16) {
  if (is.null(ticks) || length(ticks) == 0L) return(mai)
  cex <- graphics::par("cex.axis")
  req <- max(
    graphics::strwidth(as.character(ticks), units = "inches", cex = cex),
    na.rm = TRUE
  )
  req <- mai[4L] + req + pad
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
  ct <- colortheme(theme)
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
  if (pos == "bottom") mai[1L] <- mai[1L] + space * 0.85
  if (pos == "left")   mai[2L] <- mai[2L] + space
  if (pos == "top")    mai[3L] <- mai[3L] + space * 0.85
  if (pos == "right")  mai[4L] <- mai[4L] + space
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
  ct <- colortheme(theme)

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
      leg_args$border <- "gray30"
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
    ct <- colortheme(theme)
    cval <- theme.values %||% if (m > 1L) {
      factor(rep(g_names, each = n), levels = g_names)
    } else {
      as.vector(to - from)
    }
    theme_cols <- map_to_colors(cval, theme = ct, middle = middle)
  }

  if (type == "bar") {
    raw_fill <- dots$fill %||% theme_cols %||% "gray65"
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
  if (has_base_legend(cval, theme, legend, limits = limits)) {
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
      middle = middle, limits = limits, pch = dots$pch %||% 16L
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
    theme_cols <- map_to_colors(cval, theme = theme)
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
    theme_cols <- map_to_colors(theme.values, theme = theme, middle = middle)
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
  ct <- colortheme(theme)
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
    z, ticks = rownames(z), theme = NULL,
    legend = TRUE, legend.title = NULL, middle = 0,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...
) {
  m <- nrow(z)
  ticks <- ticks %||% as.character(seq_len(m))

  theme <- theme %||% getOption("colortheme.sequential", "grayscale")
  ct <- colortheme(theme)
  palette <- ct$ramp(seq.int(0, 1, length.out = 256L))

  zlim <- range(z, na.rm = TRUE)
  if (ct$type == "diverging") {
    zmax <- max(abs(zlim - middle))
    zlim <- c(-zmax, zmax) + middle
  }

  opar <- graphics::par(c("mai", "las"))
  on.exit(graphics::par(opar), add = TRUE)
  mai <- adjust_left_margin(ticks)
  if (has_base_legend(as.vector(z), theme, legend, limits = zlim)) {
    mai <- adjust_legend_margin(legend, mai)
  }
  graphics::par(mai = mai, las = 1L)

  defaults <- list(
    x = seq_len(m), y = seq_len(m), z = z, zlim = zlim,
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

  at <- seq_len(m + 2L) - 1L
  graphics::axis(1L, at = at, labels = c("", ticks, ""))
  graphics::axis(2L, at = at, labels = c("", ticks, ""))
  graphics::axis(3L, labels = FALSE, tick = TRUE, at = at, lwd.ticks = 0)
  graphics::axis(4L, labels = FALSE, tick = TRUE, at = at, lwd.ticks = 0)
  if (!is.na(grid_col)) {
    graphics::abline(
      v = seq_len(m) - 0.5, h = seq_len(m) - 0.5,
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
  theme_cols <- if (!is.null(theme)) map_to_colors(cval, theme = theme) else NULL

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
