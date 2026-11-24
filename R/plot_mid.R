# R/plot_mid.R

#' Plot MID Component Functions
#'
#' @description
#' Visualize component functions of a fitted MID model (\code{"mid"}).
#' \code{plot()} renders via base graphics, while \code{ggmid()} returns
#' \strong{ggplot2} objects via the \code{dualplot} engine.
#'
#' @details
#' Each element of \code{terms} produces one plot.
#' A single element may be a term label (e.g., \code{"Wind"}, \code{"Wind:Temp"})
#' or a formula-like expression string (e.g., \code{"Wind * Temp + 1"} or
#' \code{"Wind + Temp"}), which is extracted via \code{\link{subset.mid}()}
#' and rendered as a 1D curve/bar chart or a 2D surface depending on the
#' number of constituent variables.
#' For 2D plots, the order in which variables appear in the term expression
#' determines the x- and y-axes (e.g., \code{"Wind:Temp"} vs \code{"Temp:Wind"}).
#' If a \code{formula} is passed (e.g., \code{~ Wind + Temp}), it is expanded
#' into individual term plots.
#'
#' The \code{type} argument selects the visualization style:
#' \itemize{
#'   \item \code{"effect"} (default): plots the component function(s) alone.
#'   \item \code{"data"}: plots observed points from \code{data} colored by the component effect.
#'   \item \code{"compound"}: overlays observed points on top of the component function.
#' }
#'
#' @param x,object a \code{"mid"} object to be visualized.
#' @param terms a character vector of term labels/expressions or a formula specifying what to plot. Defaults to all main effects.
#' @param type visualization style: \code{"effect"}, \code{"data"}, or \code{"compound"}.
#' @param theme a color theme name or \code{"colortheme"} object.
#' @param intercept logical. If \code{TRUE}, adds the intercept to the plotted effect (or inferred from each term expression when omitted).
#' @param main.effects logical. If \code{TRUE}, includes constituent main effects when plotting an interaction term.
#' @param data a data.frame used for \code{type = "data"} / \code{"compound"} or when plotting raw-variable expressions (e.g., \code{"x"} when \code{log(x)} is in the model).
#' @param limits optional numeric vector of length 2 for the effect axis/color limits (may contain \code{NA} for partial specification).
#' @param jitter numeric amount of jitter for discrete axes when plotting points.
#' @param resolution integer (or length-2 integer vector for 2D surfaces) specifying the evaluation grid density for continuous variables.
#' @param lumped logical. If \code{TRUE} (default), uses lumped factor levels when a variable appears only in main effects or only in interactions; automatically set to \code{FALSE} for any variable that mixes both.
#' @param ... additional graphical parameters passed to the underlying \code{dualplot} layer.
#'
#' @examples
#' data(trees, package = "datasets")
#' mid <- interpret(Volume ~ Girth * Height, data = trees, lambda = 0.2)
#'
#' plot(mid, terms = "Girth")
#' plot(mid, terms = "Girth * Height")
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggmid(mid, terms = "Girth * Height")
#' }
#' @returns
#' \code{plot()} returns \code{NULL} invisibly.
#' \code{ggmid()} returns a \code{"ggplot"} object for a single plot, or a named list of \code{"ggplot"} objects for multiple plots.
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.mids}}, \code{\link{subset.mid}}, \code{\link{predict.mid}}
#'
#' @name plot.mid
#' @rdname plot.mid
#' @exportS3Method base::plot
#'
plot.mid <- function(
    x, terms = NULL, type = c("effect", "data", "compound"), theme = NULL,
    intercept = FALSE, main.effects = FALSE, data = NULL, limits = c(NA, NA),
    jitter = NULL, resolution = c(100L, 100L), lumped = TRUE, ...
) {
  render_mid(
    x = x, terms = terms, type = type, theme = theme,
    intercept = intercept, missing_intercept = missing(intercept),
    main.effects = main.effects, data = data, limits = limits,
    jitter = jitter, resolution = resolution, lumped = lumped,
    engine = "graphics", ...
  )
  invisible(NULL)
}


#' @rdname plot.mid
#' @export ggmid
#'
ggmid <- function(object, ...)
  UseMethod("ggmid")


#' @rdname plot.mid
#' @exportS3Method midr::ggmid
#'
ggmid.mid <- function(
    object, terms = NULL, type = c("effect", "data", "compound"), theme = NULL,
    intercept = FALSE, main.effects = FALSE, data = NULL, limits = c(NA, NA),
    jitter = NULL, resolution = c(100L, 100L), lumped = TRUE, ...
) {
  render_mid(
    x = object, terms = terms, type = type, theme = theme,
    intercept = intercept, missing_intercept = missing(intercept),
    main.effects = main.effects, data = data, limits = limits,
    jitter = jitter, resolution = resolution, lumped = lumped,
    engine = "ggplot2", ...
  )
}


#' @exportS3Method ggplot2::autoplot
#'
autoplot.mid <- function(object, ...) {
  ggmid(object, ...)
}


#' Compare MID Component Functions Across Models
#'
#' @description
#' Compare main effects across a collection of MID models (\code{"mids"}).
#' \code{plot()} renders via base graphics, while \code{ggmid()} returns
#' \strong{ggplot2} objects via the \code{dualplot} engine.
#'
#' @details
#' The \code{type} argument selects the comparison layout:
#' \itemize{
#'   \item \code{"effect"} (default): overlays component curves/bars across models.
#'   \item \code{"series"}: transposes the comparison to show effect trends across models for each feature value.
#' }
#'
#' @param x,object a \code{"mids"} collection object.
#' @param terms optional character vector or formula specifying main-effect terms to plot.
#' @param type comparison style: \code{"effect"} (default) or \code{"series"}.
#' @param theme a color theme name or \code{"colortheme"} object.
#' @param intercept logical. If \code{TRUE}, adds the intercept to the plotted effect (or inferred from \code{terms} when omitted).
#' @param limits optional numeric vector of length 2 for the effect axis limits.
#' @param resolution optional integer specifying evaluation grid density for continuous variables.
#' @param labels optional numeric or character vector of model labels.
#' @param ... additional graphical parameters passed to the underlying \code{dualplot} layer.
#'
#' @examples
#' data(iris, package = "datasets")
#' mids <- interpret(
#'   cbind(Sepal.Length, Sepal.Width) ~ (Petal.Length + Petal.Width)^2,
#'   data = iris, lambda = 0.5
#' )
#'
#' plot(mids, terms = "Petal.Length", legend = "topleft")
#' plot(mids, terms = "Petal.Length", type = "series")
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggmid(mids, terms = "Petal.Length", type = "series")
#' }
#'
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   vet <- na.omit(survival::veteran)
#'   fit <- survival::coxph(survival::Surv(time, status) ~ trt + celltype + karno + age, data = vet)
#'   survmid <- interpret(~ trt + celltype + karno + age, data = vet, model = fit, lambda = .1)
#'   plot(survmid, terms = c("karno+1", "celltype+1"), type = "series", legend = "topright")
#' }
#' @returns
#' \code{plot()} returns \code{NULL} invisibly.
#' \code{ggmid()} returns a \code{"ggplot"} object for a single plot, or a named list of \code{"ggplot"} objects for multiple plots.
#'
#' @seealso \code{\link{plot.mid}}, \code{\link{midlist}}
#'
#' @name plot.mids
#' @rdname plot.mids
#' @exportS3Method base::plot
#'
plot.mids <- function(
    x, terms = NULL, type = c("effect", "series"), theme = NULL,
    intercept = FALSE, limits = c(NA, NA), resolution = NULL, labels = NULL, ...
) {
  render_mids(
    x = x, terms = terms, type = type, theme = theme,
    intercept = intercept, missing_intercept = missing(intercept),
    limits = limits, resolution = resolution, labels = labels,
    engine = "graphics", ...
  )
  invisible(NULL)
}


#' @rdname plot.mids
#' @exportS3Method midr::ggmid
#'
ggmid.mids <- function(
    object, terms = NULL, type = c("effect", "series"), theme = NULL,
    intercept = FALSE, limits = c(NA, NA), resolution = NULL, labels = NULL, ...
) {
  render_mids(
    x = object, terms = terms, type = type, theme = theme,
    intercept = intercept, missing_intercept = missing(intercept),
    limits = limits, resolution = resolution, labels = labels,
    engine = "ggplot2", ...
  )
}


#' @exportS3Method ggplot2::autoplot
#'
autoplot.mids <- function(object, ...) {
  ggmid(object, ...)
}


# internal rendering engines ----

render_mid <- function(
    x, terms, type, theme, intercept, missing_intercept,
    main.effects, data, limits, jitter, resolution, lumped, engine, ...
) {
  type <- match.arg(type, c("effect", "data", "compound"))
  terms <- terms %||% get_labels(filter_terms(x, order = 1L))
  specs <- if (inherits(terms, c("formula", "terms"))) {
    get_labels(as_terms(terms, data = data))
  } else {
    as.character(terms)
  }
  n <- length(specs)
  if (n == 0L) stop("none of the specified 'terms' are in 'x'", call. = FALSE)

  need_data <- (type %in% c("data", "compound"))
  if (need_data && is.null(data)) {
    data <- model_data(x, env = parent.frame())
    if (is.null(data)) {
      stop("'data' must be supplied for the '", type, "' plot", call. = FALSE)
    }
  }

  intercept_vec <- if (missing_intercept) rep(list(NULL), n) else as.list(rep_len(intercept, n))
  main_vec <- rep_len(isTRUE(main.effects), n)

  subs <- vector("list", n)
  vars_orders <- vector("list", n)
  for (i in seq_len(n)) {
    spec <- specs[i]
    if (is.null(data) && grepl("\\.", spec)) {
      data <- model_data(x, env = parent.frame())
    }
    parsed <- as_terms(spec, keep.order = TRUE, data = data)
    vars_orders[[i]] <- list(
      model = get_variables(parsed, raw = FALSE),
      raw   = get_variables(parsed, raw = TRUE)
    )
    sub <- subset(x, terms = spec, intercept = intercept_vec[[i]], data = data)
    if (main_vec[i]) {
      sub_labs <- get_labels(sub)
      sub_vars <- unique(unlist(lapply(sub_labs, get_variables)))
      if (length(sub_vars) > 0L) {
        int_flag <- intercept_vec[[i]] %||% has_intercept(stats::terms(sub))
        sub <- subset(x, terms = unique(c(sub_labs, sub_vars)), intercept = int_flag, data = data)
      }
    }
    subs[[i]] <- sub
  }

  # Sync limits across multiple plots when applicable
  if (n > 1L && !is.null(limits) && anyNA(limits)) {
    all_labs <- unique(unlist(lapply(subs, get_labels)))
    dfs <- c(x$main.effects, x$interactions)[all_labs]
    vals <- unlist(lapply(dfs, `[[`, ".effect"), use.names = FALSE)
    ints <- vapply(subs, function(s) has_intercept(stats::terms(s)), logical(1L))
    if (length(unique(ints)) == 1L && length(vals) > 0L) {
      shift <- if (ints[1L]) get_intercept(x) else 0
      if (is.na(limits[1L])) limits[1L] <- min(vals, na.rm = TRUE) + shift
      if (is.na(limits[2L])) limits[2L] <- max(vals, na.rm = TRUE) + shift
    }
  }
  if (!is.null(limits) && anyNA(limits)) limits <- NULL

  out <- vector("list", n)
  names(out) <- specs
  for (i in seq_len(n)) {
    out[[i]] <- render_submodel(
      sub = subs[[i]], full = x, type = type, theme = theme,
      data = data, limits = limits, jitter = jitter,
      resolution = resolution, lumped = isTRUE(lumped),
      vars_order = vars_orders[[i]], engine = engine, ...
    )
  }
  if (engine == "graphics") return(invisible(NULL))
  if (n == 1L) out[[1L]] else out
}


render_submodel <- function(
    sub, full, type, theme, data, limits, jitter, resolution, lumped,
    vars_order = NULL, engine, ...
) {
  dots <- list(...)
  mvars <- get_variables(sub, raw = FALSE)
  rvars <- get_variables(sub, raw = TRUE)
  use_raw <- FALSE
  if (length(mvars) <= 1L) {
    vars <- mvars
  } else if (length(rvars) <= 2L && length(rvars) < length(mvars)) {
    vars <- rvars
    use_raw <- TRUE
  } else {
    vars <- mvars
  }
  order_ref <- if (use_raw) vars_order$raw else vars_order$model
  if (!is.null(order_ref) && length(vars) > 1L) {
    vars <- unique(c(intersect(order_ref, vars), vars))
  }
  nvars <- length(vars)
  if (nvars == 0L || nvars > 2L) {
    stop(
      sprintf("cannot plot a submodel with %d variables; specify 1 or 2 variables", nvars),
      call. = FALSE
    )
  }
  if (use_raw && is.null(data)) {
    data <- model_data(full, env = parent.frame())
    if (is.null(data)) {
      stop("'data' must be supplied when plotting raw transformed variables", call. = FALSE)
    }
  }

  me_raw <- unique(unlist(lapply(names(sub$main.effects), get_variables, raw = TRUE)))
  ie_raw <- unique(unlist(lapply(names(sub$interactions), get_variables, raw = TRUE)))
  mixed_raw <- intersect(me_raw, ie_raw)

  has_int <- has_intercept(stats::terms(sub))
  middle <- if (has_int) get_intercept(sub) else 0
  labs <- get_labels(sub)
  is_single_term <- (length(labs) == 1L) && !use_raw

  if (nvars == 1L) {
    v <- vars[1L]
    vinfo <- resolve_var_info(sub, v, ie_raw, mixed_raw, lumped)
    enc <- vinfo$enc
    lumped_v <- vinfo$lumped

    obs_df <- NULL
    if (type %in% c("data", "compound")) {
      df_obs <- if (!use_raw && !(v %in% colnames(data))) {
        model_reframe(stats::terms(sub), data)
      } else data
      vx <- df_obs[[v]]
      vy <- stats::predict(sub, newdata = df_obs, type = "link")
      if (!is.null(enc) && enc$type == "factor") {
        vx <- enc$transform(vx, lumped = lumped_v)
      }
      obs_df <- data.frame(x = vx, y = as.numeric(vy))
    }

    if (is_single_term && !is.null(enc) && enc$type == "factor") {
      lvls <- encoder_levels(enc, original = !lumped_v)
      nd <- stats::setNames(data.frame(factor(lvls, levels = lvls)), v)
      eff <- as.numeric(stats::predict(sub, newdata = nd, type = "link"))
      if (type == "data") {
        jit <- jitter[1L] %||% 0.35
        x_num <- as.integer(obs_df$x) + stats::runif(nrow(obs_df), -jit, jit)
        return(call_dualplot(
          draw_points,
          list(
            x = x_num, y = obs_df$y, xticks = lvls, ylim = limits,
            middle = middle, theme = theme, theme.values = obs_df$y,
            legend.title = "effect", xlab = v, ylab = "effect", engine = engine
          ),
          dots
        ))
      }
      pl <- call_dualplot(
        draw_bars,
        list(
          to = eff, ticks = lvls, horizontal = FALSE, limits = limits,
          middle = middle, theme = theme, xlab = v, ylab = "effect", engine = engine
        ),
        dots
      )
      if (type == "compound") {
        jit <- jitter[1L] %||% 0.35
        if (engine == "graphics") {
          x_num <- as.integer(obs_df$x) + stats::runif(nrow(obs_df), -jit, jit)
          draw_points(x = x_num, y = obs_df$y, overlay = TRUE, engine = "graphics", ...)
        } else {
          obs_df$x <- as.integer(obs_df$x) + stats::runif(nrow(obs_df), -jit, jit)
          pl <- pl + layer_point(
            mapping = ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
            data = obs_df, ...
          )
        }
      }
      return(pl)
    }

    if (type == "data") {
      return(call_dualplot(
        draw_points,
        list(
          x = obs_df$x, y = obs_df$y, ylim = limits,
          middle = middle, theme = theme, theme.values = obs_df$y,
          legend.title = "effect", xlab = v, ylab = "effect", engine = engine
        ),
        dots
      ))
    }

    if (is_single_term && !is.null(enc) && enc$type == "constant") {
      br <- encoder_breaks(enc)
      eff <- sub$main.effects[[labs]]$.effect + middle
      x_grid <- as.numeric(rbind(br[-length(br)], br[-1L]))
      y_grid <- rep(eff, each = 2L)
    } else if (is_single_term && !is.null(enc) && enc$type == "linear") {
      x_grid <- encoder_knots(enc)
      nd <- stats::setNames(data.frame(x_grid), v)
      y_grid <- as.numeric(stats::predict(sub, newdata = nd, type = "link"))
    } else {
      rng <- if (!use_raw && !is.null(enc)) {
        encoder_range(enc)
      } else {
        range(data[[v]], na.rm = TRUE, finite = TRUE)
      }
      x_grid <- seq(rng[1L], rng[2L], length.out = resolution[1L])
      nd <- stats::setNames(data.frame(x_grid), v)
      y_grid <- as.numeric(stats::predict(sub, newdata = nd, type = "link"))
    }
    return(draw_lines(
      x = x_grid, y = y_grid, limits = limits, points = obs_df,
      theme = theme, xlab = v, ylab = "effect", engine = engine, ...
    ))
  }

  # 2D rendering (interaction or 2-variable additive expression)
  v1 <- vars[1L]
  v2 <- vars[2L]
  vinfo1 <- resolve_var_info(sub, v1, ie_raw, mixed_raw, lumped)
  vinfo2 <- resolve_var_info(sub, v2, ie_raw, mixed_raw, lumped)
  enc1 <- vinfo1$enc
  enc2 <- vinfo2$enc
  res <- if (length(resolution) == 1L) c(resolution, resolution) else resolution[1L:2L]

  use_theme <- theme %||% colortheme_default(if (type == "data") "sequential" else "diverging")

  g1 <- make_axis_grid(enc1, v1, data, use_raw, res[1L], vinfo1$lumped)
  g2 <- make_axis_grid(enc2, v2, data, use_raw, res[2L], vinfo2$lumped)

  obs_df <- NULL
  if (type %in% c("data", "compound")) {
    df_obs <- if (!use_raw && !all(vars %in% colnames(data))) {
      model_reframe(stats::terms(sub), data)
    } else data
    ox <- df_obs[[v1]]
    oy <- df_obs[[v2]]
    if (!is.null(enc1) && enc1$type == "factor") {
      jit1 <- jitter[1L] %||% 0.3
      ox <- as.integer(enc1$transform(ox, lumped = vinfo1$lumped)) +
        stats::runif(length(ox), -jit1, jit1)
    }
    if (!is.null(enc2) && enc2$type == "factor") {
      jit2 <- (if (length(jitter) > 1L) jitter[2L] else jitter[1L]) %||% 0.3
      oy <- as.integer(enc2$transform(oy, lumped = vinfo2$lumped)) +
        stats::runif(length(oy), -jit2, jit2)
    }
    oz <- as.numeric(stats::predict(sub, newdata = df_obs, type = "link"))
    obs_df <- data.frame(x = ox, y = oy, z = oz)
  }

  if (type == "data") {
    return(call_dualplot(
      draw_points,
      list(
        x = obs_df$x, y = obs_df$y, middle = middle,
        xticks = g1$ticks, yticks = g2$ticks,
        theme = use_theme, theme.values = obs_df$z,
        legend.title = "effect", xlab = v1, ylab = v2, engine = engine
      ),
      dots
    ))
  }

  nd <- expand.grid(g1$values, g2$values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  colnames(nd) <- c(v1, v2)
  zmat <- matrix(
    as.numeric(stats::predict(sub, newdata = nd, type = "link")),
    nrow = length(g1$coords), ncol = length(g2$coords)
  )
  call_dualplot(
    draw_surface,
    list(
      x = g1$coords, y = g2$coords, z = zmat, zlim = limits, middle = middle,
      xticks = g1$ticks, yticks = g2$ticks, points = obs_df,
      theme = use_theme, legend.title = "effect", xlab = v1, ylab = v2, engine = engine
    ),
    dots
  )
}


render_mids <- function(
    x, terms, type, theme, intercept, missing_intercept,
    limits, resolution, labels, engine, ...
) {
  dots <- list(...)
  type <- match.arg(type, c("effect", "series"))
  labels <- labels %||% base::labels(x)
  me_labs <- if (inherits(x, "midrib")) {
    get_labels(filter_terms(x, order = 1L))
  } else {
    unique(unlist(lapply(as.list(x), labels, order = 1L), use.names = FALSE))
  }
  terms <- terms %||% me_labs

  if (inherits(terms, c("formula", "terms"))) {
    parsed <- as_terms(terms)
    if (any(get_order(parsed) > 1L)) {
      message("interaction term plotting is not implemented for 'mids' objects")
    }
    tlab <- get_labels(filter_terms(parsed, order = 1L))
    mlab <- match_labels(tlab, me_labs, nomatch = "omit")
    int_vec <- rep(
      if (missing_intercept) has_intercept(parsed) else isTRUE(intercept),
      length(mlab)
    )
  } else {
    specs <- as.character(terms)
    mlab <- character(0L)
    int_vec <- logical(0L)
    for (spec in specs) {
      parsed <- as_terms(spec)
      if (any(get_order(parsed) > 1L)) {
        message("interaction term plotting is not implemented for 'mids' objects")
      }
      tlab <- get_labels(filter_terms(parsed, order = 1L))
      matched <- match_labels(tlab, me_labs, nomatch = "omit")
      if (length(matched) > 0L) {
        int_flag <- if (missing_intercept) has_intercept(parsed) else isTRUE(intercept)
        mlab <- c(mlab, matched)
        int_vec <- c(int_vec, rep(int_flag, length(matched)))
      }
    }
  }
  n <- length(mlab)
  if (n == 0L) {
    stop("none of the specified main effect 'terms' are in 'x'; ",
         "interaction 'terms' are ignored", call. = FALSE)
  }
  nums <- suppressWarnings(as.numeric(labels))
  labels_parsed <- if (!anyNA(nums)) nums else factor(labels, levels = unique(labels))
  discrete_labels <- is_discrete(labels_parsed)
  eff_legend <- dots$legend %||% TRUE

  prep <- vector("list", n)
  for (i in seq_len(n)) {
    term <- mlab[i]
    base <- if (inherits(x, "midrib")) x else {
      ok <- vapply(as.list(x), function(m) !is.null(get_encoder(m, term, order = 1L)), logical(1L))
      as.list(x)[[which(ok)[1L]]]
    }
    enc <- get_encoder(base, term, order = 1L)
    if (enc$type == "factor") {
      lvls <- encoder_levels(enc, original = TRUE)
      xvals <- factor(lvls, levels = lvls)
    } else {
      rng <- encoder_range(enc)
      res <- resolution %||% (if (type == "series") 25L else min(max(1e4L %/% length(labels), 10L), 500L))
      xvals <- seq(rng[1L], rng[2L], length.out = res)
    }
    nd <- stats::setNames(data.frame(xvals), term)
    t_spec <- if (int_vec[i]) term else c(term, "-1")
    fmat <- stats::predict(x, newdata = nd, terms = t_spec, type = "link")
    if (!is.matrix(fmat)) fmat <- matrix(fmat, ncol = 1L)
    colnames(fmat) <- as.character(labels)
    prep[[i]] <- list(term = term, enc = enc, xvals = xvals, fmat = fmat)
  }

  # Sync limits across multiple plots (or fill partial NA limits)
  sync_ok <- (n == 1L && !all(is.na(limits))) ||
    (n > 1L && (length(unique(int_vec)) == 1L || !all(is.na(limits))))
  if (!is.null(limits) && anyNA(limits) && sync_ok) {
    vals <- unlist(lapply(prep, `[[`, "fmat"), use.names = FALSE)
    has_bars <- (type == "effect") &&
      any(vapply(prep, function(p) p$enc$type == "factor", logical(1L)))
    if (has_bars) vals <- c(0, vals)
    if (length(vals) > 0L && any(is.finite(vals))) {
      rng_all <- range(vals, na.rm = TRUE, finite = TRUE)
      if (is.na(limits[1L])) limits[1L] <- rng_all[1L]
      if (is.na(limits[2L])) limits[2L] <- rng_all[2L]
    }
  }
  if (!is.null(limits) && anyNA(limits)) limits <- NULL

  out <- vector("list", n)
  names(out) <- mlab
  for (i in seq_len(n)) {
    term <- prep[[i]]$term
    enc <- prep[[i]]$enc
    xvals <- prep[[i]]$xvals
    fmat <- prep[[i]]$fmat

    if (type == "effect") {
      th <- theme %||% colortheme_default(if (discrete_labels) "qualitative" else "sequential")
      if (enc$type == "factor") {
        out[[i]] <- call_dualplot(
          draw_bars,
          list(
            to = fmat, ticks = as.character(xvals), horizontal = FALSE,
            limits = limits, theme = th,
            theme.values = rep(labels_parsed, each = nrow(fmat)),
            xlab = term, ylab = "effect", engine = engine
          ),
          dots
        )
      } else {
        out[[i]] <- call_dualplot(
          draw_lines,
          list(
            x = xvals, y = fmat, limits = limits, theme = th,
            theme.values = labels_parsed,
            xlab = term, ylab = "effect", engine = engine
          ),
          dots
        )
      }
    } else {
      th <- theme %||% colortheme_default(if (is_discrete(xvals)) "qualitative" else "sequential")
      ymat <- t(fmat)
      colnames(ymat) <- as.character(xvals)
      s_args <- list(
        x = labels_parsed, y = ymat, discrete = discrete_labels,
        limits = limits, theme = th, theme.values = xvals,
        xlab = "", ylab = "effect", engine = engine
      )
      if (isFALSE(eff_legend)) {
        if (is.null(dots$main) && is.null(dots$title)) s_args$main <- term
      } else {
        s_args$legend.title <- term
      }
      out[[i]] <- call_dualplot(draw_lines, s_args, dots)
    }
  }
  if (engine == "graphics") return(invisible(NULL))
  if (n == 1L) out[[1L]] else out
}


# helpers ----

call_dualplot <- function(fn, args, dots = list()) {
  if (length(dots) > 0L) {
    args <- utils::modifyList(args, dots, keep.null = TRUE)
  }
  do.call(fn, args)
}


resolve_var_info <- function(sub, v, ie_raw, mixed_raw, lumped) {
  v_raw <- get_variables(v, raw = TRUE)
  is_mixed <- any(v_raw %in% mixed_raw)
  lumped_v <- if (is_mixed) FALSE else isTRUE(lumped)
  ord <- if (is_mixed) NULL else if (any(v_raw %in% ie_raw)) 2L else 1L
  enc <- get_encoder(sub, v, order = ord) %||% get_encoder(sub, v, order = NULL)
  list(enc = enc, lumped = lumped_v)
}


make_axis_grid <- function(enc, var, data, use_raw, res, lumped) {
  if (!use_raw && !is.null(enc) && enc$type == "factor") {
    lvls <- encoder_levels(enc, original = !lumped)
    vals <- factor(lvls, levels = lvls)
    list(values = vals, coords = seq_along(lvls), ticks = lvls)
  } else {
    rng <- if (!use_raw && !is.null(enc)) {
      encoder_range(enc)
    } else {
      range(data[[var]], na.rm = TRUE, finite = TRUE)
    }
    vals <- seq(rng[1L], rng[2L], length.out = res)
    list(values = vals, coords = vals, ticks = NULL)
  }
}
