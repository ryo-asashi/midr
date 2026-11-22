# R/plot_midcon.R

#' Plot Conditional Expectations
#'
#' @description
#' Visualize Individual Conditional Expectation (ICE) curves for a \code{"midcon"} object.
#' \code{plot()} renders via base graphics, while \code{ggmid()} returns
#' \strong{ggplot2} objects.
#'
#' @param x,object a \code{"midcon"} object.
#' @param type visualization style: \code{"iceplot"} (default) or \code{"centered"}.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param var.alpha,var.color,var.linetype,var.linewidth optional variable names or
#'   expressions evaluated in \code{observed} to map aesthetics across curves.
#' @param reference integer index of the grid point used for centering when \code{type = "centered"}.
#' @param points logical. If \code{TRUE} (default), overlays observed predictions.
#' @param sample optional character vector of observation IDs (\code{.id}) to display.
#' @param ... additional graphical parameters.
#'
#' @returns
#' \code{plot()} returns \code{NULL} invisibly.
#' \code{ggmid()} returns a \code{"ggplot"} object.
#'
#' @seealso \code{\link{eval_conditional}}, \code{\link{plot.midcons}}
#'
#' @name plot.midcon
#' @rdname plot.midcon
#' @exportS3Method base::plot
#'
plot.midcon <- function(
    x, type = c("iceplot", "centered"), theme = NULL,
    var.alpha = NULL, var.color = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, points = TRUE, sample = NULL, ...
) {
  render_midcon(
    x = x, type = type, theme = theme,
    col_expr = substitute(var.color), alpha_expr = substitute(var.alpha),
    lty_expr = substitute(var.linetype), lwd_expr = substitute(var.linewidth),
    reference = reference, points = points, sample = sample,
    engine = "graphics", ...
  )
  invisible(NULL)
}


#' @rdname plot.midcon
#' @exportS3Method midr::ggmid
#'
ggmid.midcon <- function(
    object, type = c("iceplot", "centered"), theme = NULL,
    var.alpha = NULL, var.color = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, points = TRUE, sample = NULL, ...
) {
  render_midcon(
    x = object, type = type, theme = theme,
    col_expr = substitute(var.color), alpha_expr = substitute(var.alpha),
    lty_expr = substitute(var.linetype), lwd_expr = substitute(var.linewidth),
    reference = reference, points = points, sample = sample,
    engine = "ggplot2", ...
  )
}


#' @exportS3Method ggplot2::autoplot
#'
autoplot.midcon <- function(object, ...) {
  ggmid(object, ...)
}


#' Compare Conditional Expectations Across Models
#'
#' @description
#' Compare Individual Conditional Expectation (ICE) curves across a \code{"midcons"} collection.
#' \code{plot()} renders via base graphics, while \code{ggmid()} returns
#' \strong{ggplot2} objects.
#'
#' @param x,object a \code{"midcons"} collection object.
#' @param type visualization style: \code{"iceplot"} (default), \code{"centered"}, or \code{"series"}.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param var.alpha,var.linetype,var.linewidth optional variable names or
#'   expressions evaluated in \code{observed} to map aesthetics across curves.
#' @param reference integer index of the grid point used for centering when \code{type = "centered"}.
#' @param sample optional character vector of observation IDs (\code{.id}) to display.
#' @param labels optional numeric or character vector of model labels.
#' @param ... additional graphical parameters.
#'
#' @returns
#' \code{plot()} returns \code{NULL} invisibly.
#' \code{ggmid()} returns a \code{"ggplot"} object.
#'
#' @seealso \code{\link{eval_conditional}}, \code{\link{plot.midcon}}, \code{\link{midlist}}
#'
#' @name plot.midcons
#' @rdname plot.midcons
#' @exportS3Method base::plot
#'
plot.midcons <- function(
    x, type = c("iceplot", "centered", "series"), theme = NULL,
    var.alpha = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, sample = NULL, labels = NULL, ...
) {
  render_midcons(
    x = x, type = type, theme = theme,
    alpha_expr = substitute(var.alpha), lty_expr = substitute(var.linetype),
    lwd_expr = substitute(var.linewidth),
    reference = reference, sample = sample, labels = labels,
    engine = "graphics", ...
  )
  invisible(NULL)
}


#' @rdname plot.midcons
#' @exportS3Method midr::ggmid
#'
ggmid.midcons <- function(
    object, type = c("iceplot", "centered", "series"), theme = NULL,
    var.alpha = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, sample = NULL, labels = NULL, ...
) {
  render_midcons(
    x = object, type = type, theme = theme,
    alpha_expr = substitute(var.alpha), lty_expr = substitute(var.linetype),
    lwd_expr = substitute(var.linewidth),
    reference = reference, sample = sample, labels = labels,
    engine = "ggplot2", ...
  )
}


#' @exportS3Method ggplot2::autoplot
#'
autoplot.midcons <- function(object, ...) {
  ggmid(object, ...)
}


# internal rendering engines ----

expr_label <- function(expr) {
  if (is.null(expr)) return(NULL)
  if (is.character(expr) && length(expr) == 1L) expr else deparse(expr)[1L]
}

eval_var_expr <- function(expr, env_df) {
  if (is.null(expr)) return(NULL)
  if (is.character(expr) && length(expr) == 1L) {
    if (expr %in% names(env_df)) return(env_df[[expr]])
    expr <- str2lang(expr)
  }
  eval(expr, envir = env_df, enclos = parent.frame())
}

rescale01 <- function(x) {
  if (is_discrete(x)) x <- as.numeric(factor(x))
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1L]) / diff(rng)
}

render_midcon <- function(
    x, type, theme, col_expr, alpha_expr, lty_expr, lwd_expr,
    reference, points, sample, engine, ...
) {
  dots <- list(...)
  legend_dot <- dots$legend
  legend_title_dot <- dots$legend.title
  dots$legend <- NULL
  dots$legend.title <- NULL
  type <- match.arg(type, c("iceplot", "centered"))
  variable <- x$variable
  obs <- x$observed
  con <- x$conditional
  values <- x$values
  yvar <- "yhat"

  if (type == "centered") {
    if (reference < 0L) reference <- length(values)
    refval <- values[min(length(values), max(1L, as.integer(reference)))]
    ref <- con[con[[variable]] == refval, , drop = FALSE]
    ynew <- paste0("centered ", yvar)
    obs[[ynew]] <- obs[[yvar]] - ref[[yvar]][match(obs$.id, ref$.id)]
    con[[ynew]] <- con[[yvar]] - ref[[yvar]][match(con$.id, ref$.id)]
    yvar <- ynew
  }

  if (!is.null(sample)) {
    obs <- obs[obs$.id %in% sample, , drop = FALSE]
    con <- con[con$.id %in% sample, , drop = FALSE]
  }
  n <- nrow(obs)
  if (n == 0L) {
    message("no observations found")
    return(invisible(NULL))
  }

  col_val <- eval_var_expr(col_expr, obs)
  alpha_val <- eval_var_expr(alpha_expr, obs)
  lty_val <- eval_var_expr(lty_expr, obs)
  lwd_val <- eval_var_expr(lwd_expr, obs)

  if (!is.null(col_val) && is.null(theme)) {
    theme <- colortheme_default(if (is_discrete(col_val)) "qualitative" else "sequential")
  }

  if (engine == "ggplot2") {
    check_ggplot2()
    idx <- match(con$.id, obs$.id)
    con$.x <- con[[variable]]
    con$.y <- con[[yvar]]
    obs$.x <- obs[[variable]]
    obs$.y <- obs[[yvar]]

    aes_map <- ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]], group = .data[[".id"]])
    legend_on <- character(0L)
    ltitle <- NULL
    if (!is.null(col_val)) {
      con$.cval <- col_val[idx]
      obs$.cval <- col_val
      aes_map$colour <- ggplot2::aes(colour = .data[[".cval"]])[[1L]]
      legend_on <- "color"
      ltitle <- if (is.character(col_expr)) col_expr else deparse(col_expr)[1L]
    }
    extra_labs <- list()
    if (!is.null(alpha_val)) {
      con$.aval <- alpha_val[idx]
      aes_map$alpha <- ggplot2::aes(alpha = .data[[".aval"]])[[1L]]
      extra_labs$alpha <- expr_label(alpha_expr)
    }
    if (!is.null(lty_val)) {
      con$.lty <- if (is_discrete(lty_val)) lty_val[idx] else factor(lty_val[idx])
      aes_map$linetype <- ggplot2::aes(linetype = .data[[".lty"]])[[1L]]
      extra_labs$linetype <- expr_label(lty_expr)
    }
    if (!is.null(lwd_val)) {
      con$.lwd <- lwd_val[idx]
      aes_map$linewidth <- ggplot2::aes(linewidth = .data[[".lwd"]])[[1L]]
      extra_labs$linewidth <- expr_label(lwd_expr)
    }

    pl <- ggplot2::ggplot(con, aes_map) + layer_line(...)
    if (length(extra_labs) > 0L) {
      pl <- pl + do.call(ggplot2::labs, extra_labs)
    }
    if (isTRUE(points)) {
      pt_map <- ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]])
      if (!is.null(col_val)) pt_map$colour <- ggplot2::aes(colour = .data[[".cval"]])[[1L]]
      pl <- pl + layer_point(mapping = pt_map, data = obs, ...)
    }
    if (!is.null(theme) && !is.null(col_val)) {
      pl <- pl + scale_color_theme(theme = theme, discrete = is_discrete(con$.cval))
    }
    pl <- pl + extract_gg_labs(dots, xlab = variable, ylab = yvar)
    legend_opt <- legend_dot %||% (!is.null(col_val))
    return(apply_theme_and_legend(pl, legend = legend_opt, legend.on = legend_on, legend.title = ltitle))
  }

  # graphics backend
  mat <- matrix(NA_real_, nrow = length(values), ncol = n, dimnames = list(as.character(values), obs$.id))
  cols_idx <- match(con$.id, obs$.id)
  rows_idx <- match(con[[variable]], values)
  ok <- !is.na(rows_idx) & !is.na(cols_idx)
  mat[cbind(rows_idx[ok], cols_idx[ok])] <- con[[yvar]][ok]

  aes_col <- if (!is.null(col_val)) map_to_colors(col_val, theme = theme) else dots$col %||% 1L
  aes_alpha <- if (!is.null(alpha_val)) rescale01(alpha_val) * 0.75 + 0.25 else dots$alpha %||% 1
  aes_lty <- if (!is.null(lty_val)) pmin(floor(rescale01(lty_val) * 5) + 1L, 6L) else dots$lty %||% 1L
  aes_lwd <- if (!is.null(lwd_val)) rescale01(lwd_val) * 2.5 + 0.5 else dots$lwd %||% 1

  fv <- is_discrete(values)
  flvs <- if (fv) levels(factor(values)) else NULL
  xvals <- if (fv) seq_along(flvs) else values

  legend_opt <- legend_dot %||% FALSE
  opar <- graphics::par("mai")
  on.exit(graphics::par(mai = opar), add = TRUE)
  graphics::par(mai = adjust_legend_margin(legend_opt, opar))

  p_args <- cast_to_base_params(
    dots = dots,
    defaults = list(
      x = xvals, y = rep(NA_real_, length(xvals)),
      xlim = range(xvals, na.rm = TRUE), ylim = range(c(mat, obs[[yvar]]), na.rm = TRUE),
      xlab = variable, ylab = yvar, type = "n", xaxt = if (fv) "n" else "s"
    ),
    alpha.on = NULL
  )
  p_args$alpha <- NULL
  do.call(graphics::plot.default, p_args)
  if (fv) graphics::axis(side = 1L, at = seq_along(flvs), labels = flvs)

  m_args <- cast_to_base_params(
    dots = dots,
    defaults = list(x = xvals, y = mat, type = "l", add = TRUE, col = aes_col, lty = aes_lty, lwd = aes_lwd, alpha = aes_alpha),
    alpha.on = "col"
  )
  do.call(graphics::matplot, m_args)

  if (isTRUE(points)) {
    px <- if (fv) match(as.character(obs[[variable]]), flvs) else obs[[variable]]
    pt_args <- cast_to_base_params(
      dots = dots,
      defaults = list(x = px, y = obs[[yvar]], pch = 16L, col = aes_col, alpha = aes_alpha),
      allowed = c("x", "y", "col", "pch", "cex", "bg", "alpha"),
      alpha.on = "col"
    )
    do.call(graphics::points.default, pt_args)
  }
  if (!is.null(col_val) && !isFALSE(legend_opt)) {
    ltitle <- legend_title_dot %||% (if (is.character(col_expr)) col_expr else deparse(col_expr)[1L])
    draw_base_legend(values = col_val, theme = theme, legend = legend_opt, title = ltitle, style = "line")
  }
  invisible(NULL)
}


render_midcons <- function(
    x, type, theme, alpha_expr, lty_expr, lwd_expr,
    reference, sample, labels, engine, ...
) {
  dots <- list(...)
  legend_dot <- dots$legend
  legend_title_dot <- dots$legend.title
  dots$legend <- NULL
  dots$legend.title <- NULL
  type <- match.arg(type, c("iceplot", "centered", "series"))
  objs <- as.list(x)
  first <- objs[[1L]]
  variable <- first$variable
  obs <- first$observed
  values <- first$values
  con <- summarize_midlist(
    object = x, shape = "long",
    dfname = "conditional", keycol = ".id", valcol = "yhat",
    keys = c(".id", variable), targets = first$ids
  )

  olabs <- as.character(unique(con$label))
  labels <- labels %||% olabs
  if (length(labels) != length(olabs)) {
    stop("length of 'labels' must match the number of models in 'x'", call. = FALSE)
  }
  con$label <- labels[match(as.character(con$label), olabs)]
  nums <- suppressWarnings(as.numeric(labels))
  labels_parsed <- if (!anyNA(nums)) nums else factor(labels, levels = unique(labels))
  discrete <- is_discrete(labels_parsed)
  con$.lbl <- if (discrete) factor(con$label, levels = levels(labels_parsed)) else as.numeric(con$label)

  yvar <- "yhat"
  if (type == "centered") {
    nvals <- length(values)
    if (reference < 0L) reference <- nvals
    refval <- values[min(nvals, max(1L, as.integer(reference)))]
    ref <- con[con[[variable]] == refval, , drop = FALSE]
    key1 <- paste(con$.id, con$label, sep = "\r")
    key2 <- paste(ref$.id, ref$label, sep = "\r")
    ynew <- "centered yhat"
    con[[ynew]] <- con[[yvar]] - ref[[yvar]][match(key1, key2)]
    yvar <- ynew
  }

  if (!is.null(sample)) {
    obs <- obs[obs$.id %in% sample, , drop = FALSE]
    con <- con[con$.id %in% sample, , drop = FALSE]
  }
  n <- nrow(obs)
  if (n == 0L) {
    message("no observations found")
    return(invisible(NULL))
  }

  alpha_val <- eval_var_expr(alpha_expr, obs)
  lty_val <- eval_var_expr(lty_expr, obs)
  lwd_val <- eval_var_expr(lwd_expr, obs)
  fv <- is_discrete(values)

  if (engine == "ggplot2") {
    check_ggplot2()
    idx <- match(con$.id, obs$.id)
    con$.y <- con[[yvar]]
    if (type == "series") {
      th <- theme %||% colortheme_default(if (fv) "qualitative" else "sequential")
      con$.x <- con$.lbl
      con$.grp <- paste(con$.id, con[[variable]], sep = "\r")
      con$.cval <- con[[variable]]
      ltitle <- variable
      xlab <- ""
    } else {
      th <- theme %||% colortheme_default(if (discrete) "qualitative" else "sequential")
      con$.x <- con[[variable]]
      con$.grp <- paste(con$.id, con$.lbl, sep = "\r")
      con$.cval <- con$.lbl
      ltitle <- NULL
      xlab <- variable
    }
    gg_dots <- cast_to_gg_params(dots)
    use_col_aes <- is.null(gg_dots$colour)
    aes_map <- ggplot2::aes(
      x = .data[[".x"]], y = .data[[".y"]],
      group = .data[[".grp"]]
    )
    if (use_col_aes) {
      aes_map$colour <- ggplot2::aes(colour = .data[[".cval"]])[[1L]]
    }
    extra_labs <- list()
    if (!is.null(alpha_val)) {
      con$.aval <- alpha_val[idx]
      aes_map$alpha <- ggplot2::aes(alpha = .data[[".aval"]])[[1L]]
      extra_labs$alpha <- expr_label(alpha_expr)
    }
    if (!is.null(lty_val)) {
      con$.lty <- if (is_discrete(lty_val)) lty_val[idx] else factor(lty_val[idx])
      aes_map$linetype <- ggplot2::aes(linetype = .data[[".lty"]])[[1L]]
      extra_labs$linetype <- expr_label(lty_expr)
    }
    if (!is.null(lwd_val)) {
      con$.lwd <- lwd_val[idx]
      aes_map$linewidth <- ggplot2::aes(linewidth = .data[[".lwd"]])[[1L]]
      extra_labs$linewidth <- expr_label(lwd_expr)
    }

    pl <- ggplot2::ggplot(con, aes_map) + layer_line(...)
    if (length(extra_labs) > 0L) {
      pl <- pl + do.call(ggplot2::labs, extra_labs)
    }
    if (type == "series" && discrete) {
      pl <- pl + layer_point(...)
    }
    if (use_col_aes) {
      pl <- pl + scale_color_theme(theme = th, discrete = is_discrete(con$.cval))
    }
    gg_leg <- legend_dot %||% TRUE
    series_main <- if (type == "series" && isFALSE(gg_leg) && is.null(dots$main) && is.null(dots$title)) {
      variable
    } else {
      NULL
    }
    pl <- pl + extract_gg_labs(dots, xlab = xlab, ylab = yvar, main = series_main)
    return(apply_theme_and_legend(
      pl,
      legend = gg_leg,
      legend.on = if (use_col_aes) "color" else character(0L),
      legend.title = legend_title_dot %||% ltitle
    ))
  }

  # graphics backend
  nlabs <- length(labels)
  nvals <- length(values)
  aes_alpha <- if (!is.null(alpha_val)) rescale01(alpha_val) * 0.75 + 0.25 else rep(1, n)
  aes_lty <- if (!is.null(lty_val)) pmin(floor(rescale01(lty_val) * 5) + 1L, 6L) else rep(1L, n)
  aes_lwd <- if (!is.null(lwd_val)) rescale01(lwd_val) * 2.5 + 0.5 else rep(1, n)

  if (type == "series") {
    th <- theme %||% colortheme_default(if (fv) "qualitative" else "sequential")
    basecol <- dots$col %||% map_to_colors(values, theme = th)
    mat <- matrix(NA_real_, nrow = nlabs, ncol = n * nvals)
    r_idx <- match(as.character(con$label), as.character(labels))
    c_idx <- (match(con[[variable]], values) - 1L) * n + match(con$.id, obs$.id)
    ok <- !is.na(r_idx) & !is.na(c_idx)
    mat[cbind(r_idx[ok], c_idx[ok])] <- con[[yvar]][ok]

    ord <- if (discrete) seq_len(nlabs) else order(labels_parsed)
    xvals <- if (discrete) seq_len(nlabs) else labels_parsed
    xaxt_val <- if (discrete) "n" else "s"
    xlab <- ""
    col_vec <- rep(basecol, each = n)
    lty_vec <- rep(aes_lty, times = nvals)
    lwd_vec <- rep(aes_lwd, times = nvals)
    alp_vec <- rep(aes_alpha, times = nvals)
    leg_vals <- values
    leg_title <- variable
  } else {
    th <- theme %||% colortheme_default(if (discrete) "qualitative" else "sequential")
    basecol <- dots$col %||% map_to_colors(labels_parsed, theme = th)
    mat <- matrix(NA_real_, nrow = nvals, ncol = n * nlabs)
    r_idx <- match(con[[variable]], values)
    c_idx <- (match(as.character(con$label), as.character(labels)) - 1L) * n + match(con$.id, obs$.id)
    ok <- !is.na(r_idx) & !is.na(c_idx)
    mat[cbind(r_idx[ok], c_idx[ok])] <- con[[yvar]][ok]

    ord <- seq_len(nvals)
    xvals <- if (fv) seq_len(nvals) else values
    xaxt_val <- if (fv) "n" else "s"
    xlab <- variable
    col_vec <- rep(basecol, each = n)
    lty_vec <- rep(aes_lty, times = nlabs)
    lwd_vec <- rep(aes_lwd, times = nlabs)
    alp_vec <- rep(aes_alpha, times = nlabs)
    leg_vals <- labels_parsed
    leg_title <- NULL
  }

  legend_opt <- legend_dot %||% FALSE
  opar <- graphics::par("mai")
  on.exit(graphics::par(mai = opar), add = TRUE)
  graphics::par(mai = adjust_legend_margin(legend_opt, opar))

  series_main <- if (type == "series" && isFALSE(legend_opt) && is.null(dots$main) && is.null(dots$title)) {
    variable
  } else {
    NULL
  }
  p_args <- cast_to_base_params(
    dots = dots,
    defaults = list(
      x = xvals[ord], y = rep(NA_real_, length(xvals)),
      xlim = range(xvals, na.rm = TRUE), ylim = range(mat, na.rm = TRUE),
      main = series_main, xlab = xlab, ylab = yvar, type = "n", xaxt = xaxt_val
    ),
    alpha.on = NULL
  )
  p_args$alpha <- NULL
  do.call(graphics::plot.default, p_args)
  if (type == "series" && discrete) {
    graphics::axis(side = 1L, at = seq_len(nlabs), labels = as.character(labels))
  } else if (type != "series" && fv) {
    graphics::axis(side = 1L, at = seq_len(nvals), labels = as.character(values))
  }

  m_args <- cast_to_base_params(
    dots = dots,
    defaults = list(
      x = xvals[ord], y = mat[ord, , drop = FALSE], add = TRUE,
      type = if (type == "series" && discrete) "b" else "l",
      pch = 16L, col = col_vec, lty = lty_vec, lwd = lwd_vec, alpha = alp_vec
    ),
    alpha.on = "col"
  )
  do.call(graphics::matplot, m_args)

  if (!isFALSE(legend_opt)) {
    draw_base_legend(
      values = leg_vals, theme = th, legend = legend_opt,
      title = legend_title_dot %||% leg_title, style = "line"
    )
  }
  invisible(NULL)
}
