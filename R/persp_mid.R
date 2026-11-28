# R/persp_mid.R

#' Perspective Plots of MID Component Functions
#'
#' @description
#' Visualizes the combined effect of two variables from a \code{"mid"} object
#' using a 3D perspective plot via \code{\link[graphics]{persp}()}.
#'
#' @details
#' Each element of \code{terms} produces one 3D perspective plot.
#' A single element may be a two-variable interaction label (e.g., \code{"wt:hp"})
#' or a formula-like expression string involving two variables (e.g.,
#' \code{"wt * hp + 1"} or \code{"wt + hp"}), which is extracted via
#' \code{\link{subset.mid}()}.
#' When \code{terms = NULL}, all second-order interaction terms in \code{x} are
#' plotted in sequence.
#'
#' By default (\code{theme = NULL}), surface coloring is left to
#' \code{graphics::persp()}. When a \code{theme} is supplied, surface facets are
#' colored according to their mean effect values using \code{\link[colortheme]{map_to_colors}()}.
#'
#' @param x a \code{"mid"} object, typically the result of \code{\link{interpret}()}.
#' @param terms a character vector of term labels/expressions or a formula
#'   specifying 2-variable effects to plot. Defaults to all second-order interactions.
#' @param theme optional color theme name or \code{"colortheme"} object used to
#'   color surface facets. Defaults to \code{NULL} (standard \code{persp()} appearance).
#' @param intercept logical. If \code{TRUE}, adds the intercept to the plotted
#'   effect (or inferred from each term expression when omitted).
#' @param main.effects logical. If \code{TRUE}, includes constituent main effects
#'   when plotting an interaction term.
#' @param data optional data.frame used when plotting raw-variable expressions
#'   (e.g., \code{"x"} when \code{log(x)} is in the model).
#' @param limits optional numeric vector of length 2 for \code{zlim} (may
#'   contain \code{NA} for partial specification).
#' @param resolution integer (or length-2 integer vector) specifying the
#'   evaluation grid density for continuous axes. Defaults to \code{c(50L, 50L)}.
#' @param lumped logical. If \code{TRUE} (default), uses lumped factor levels
#'   when a variable appears only in interactions; automatically set to
#'   \code{FALSE} for any variable that mixes main effects and interactions.
#' @param ... additional arguments passed on to \code{\link[graphics]{persp}()}.
#'
#' @returns
#' \code{persp.mid()} invisibly returns the viewing transformation matrix for a
#' single plot, or a named list of such matrices when multiple plots are drawn.
#'
#' @seealso \code{\link[graphics]{persp}}, \code{\link{plot.mid}}, \code{\link{subset.mid}}
#'
#' @examples
#' mid <- interpret(mpg ~ wt * hp + factor(am), data = mtcars, lambda = 0.5)
#'
#' # Plot only the interaction effect
#' persp(mid, terms = "wt:hp", theta = 225, phi = 45)
#'
#' # Include constituent main effects
#' persp(mid, terms = "wt * hp", theme = "azurerose", theta = 135, phi = 20)
#'
#' # Plot additive main effects with higher resolution
#' persp(mid, terms = "wt + hp", theme = "steps", resolution = 80L, theta = 135, phi = 20)
#' @exportS3Method graphics::persp
#'
persp.mid <- function(
    x, terms = NULL, theme = NULL, intercept = FALSE, main.effects = FALSE,
    data = NULL, limits = c(NA, NA), resolution = c(50L, 50L), lumped = TRUE, ...
) {
  if (!inherits(x, "mid")) {
    stop("'x' must be a 'mid' object", call. = FALSE)
  }
  terms <- terms %||% get_labels(filter_terms(x, order = 2L))
  specs <- if (inherits(terms, c("formula", "terms"))) {
    get_labels(as_terms(terms, data = data))
  } else {
    as.character(terms)
  }
  n <- length(specs)
  if (n == 0L) {
    stop("no 2-variable terms found in 'x'; specify 'terms' explicitly", call. = FALSE)
  }

  res <- if (length(resolution) == 1L) c(resolution, resolution) else resolution[1L:2L]
  missing_intercept <- missing(intercept)
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
  dots <- list(...)
  for (i in seq_len(n)) {
    out[[i]] <- render_persp_submodel(
      sub = subs[[i]], full = x, theme = theme, data = data,
      limits = limits, res = res, lumped = isTRUE(lumped),
      vars_order = vars_orders[[i]], dots = dots
    )
  }
  invisible(if (n == 1L) out[[1L]] else out)
}


# internal helpers ----

render_persp_submodel <- function(
    sub, full, theme, data, limits, res, lumped, vars_order, dots
) {
  mvars <- get_variables(sub, raw = FALSE)
  rvars <- get_variables(sub, raw = TRUE)
  use_raw <- FALSE
  if (length(mvars) == 2L) {
    vars <- mvars
  } else if (length(rvars) == 2L && length(rvars) < length(mvars)) {
    vars <- rvars
    use_raw <- TRUE
  } else {
    vars <- mvars
  }
  order_ref <- if (use_raw) vars_order$raw else vars_order$model
  if (!is.null(order_ref) && length(vars) > 1L) {
    vars <- unique(c(intersect(order_ref, vars), vars))
  }
  if (length(vars) != 2L) {
    stop(
      sprintf("persp.mid() requires exactly 2 variables per plot, got %d", length(vars)),
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

  v1 <- vars[1L]
  v2 <- vars[2L]
  vinfo1 <- resolve_var_info(sub, v1, ie_raw, mixed_raw, lumped)
  vinfo2 <- resolve_var_info(sub, v2, ie_raw, mixed_raw, lumped)

  g1 <- make_persp_axis(vinfo1$enc, v1, data, use_raw, res[1L], vinfo1$lumped)
  g2 <- make_persp_axis(vinfo2$enc, v2, data, use_raw, res[2L], vinfo2$lumped)

  nd <- expand.grid(g1$values, g2$values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  colnames(nd) <- c(v1, v2)
  z <- matrix(
    as.numeric(stats::predict(sub, newdata = nd, type = "link")),
    nrow = length(g1$coords), ncol = length(g2$coords)
  )
  zrng <- range(z, na.rm = TRUE)
  if (!all(is.finite(zrng))) {
    stop("z values contain only NA/NaN/Inf", call. = FALSE)
  }

  args <- dots
  args$x <- g1$coords
  args$y <- g2$coords
  args$z <- z
  args$xlab <- args$xlab %||% v1
  args$ylab <- args$ylab %||% v2
  args$zlab <- args$zlab %||% "effect"
  if (!is.null(limits)) {
    args$zlim <- args$zlim %||% limits
  }
  if (is.null(args$zlim) && zrng[1L] == zrng[2L]) {
    args$zlim <- zrng[1L] + c(-1, 1)
  }
  if (!is.null(theme) && is.null(args$col)) {
    nx <- nrow(z)
    ny <- ncol(z)
    z_facet <- (z[-nx, -ny] + z[-1L, -ny] + z[-nx, -1L] + z[-1L, -1L]) / 4
    middle <- if (has_intercept(stats::terms(sub))) get_intercept(sub) else 0
    args$col <- colortheme::map_to_colors(as.vector(z_facet), theme = theme, middle = middle)
  }

  do.call(graphics::persp, args)
}


make_persp_axis <- function(enc, var, data, use_raw, res, lumped, mar = 0.499) {
  if (!use_raw && !is.null(enc) && enc$type == "factor") {
    lvls <- encoder_levels(enc, original = !lumped)
    vals <- factor(rep(lvls, each = 2L), levels = lvls)
    coords <- rep(seq_along(lvls), each = 2L) + c(-mar, mar)
    return(list(values = vals, coords = coords))
  }
  if (use_raw && !is.null(data) && is_discrete(data[[var]])) {
    lvls <- levels(factor(data[[var]]))
    vals <- factor(rep(lvls, each = 2L), levels = lvls)
    coords <- rep(seq_along(lvls), each = 2L) + c(-mar, mar)
    return(list(values = vals, coords = coords))
  }
  rng <- if (!use_raw && !is.null(enc)) {
    encoder_range(enc)
  } else {
    range(data[[var]], na.rm = TRUE, finite = TRUE)
  }
  vals <- seq(rng[1L], rng[2L], length.out = res)
  list(values = vals, coords = vals)
}


#' @exportS3Method graphics::persp
#'
persp.mids <- function(x, ...) {
  stop(
    "'persp()' is not supported for 'mids' objects; ",
    "subset a single model first (e.g., persp(x[[1L]], ...))",
    call. = FALSE
  )
}
