#' Set Evaluation Grids
#'
#' @description
#' \code{set_grid()} constructs and attaches an evaluation grid (\code{gma.grid}) to a fitted model object for subsequent model analysis.
#'
#' @param object a fitted model object.
#' @param ... further arguments passed to methods.
#'
#' @export set_grid
set_grid <- function(object, ...)
UseMethod("set_grid")


#' @rdname set_grid
#'
#' @param axis a character vector or formula specifying the primary variables of interest.
#' @param method character string specifying how to construct the axis grid: "raster" (uniform mesh over the domain), "data" (observed points), or "knots" (encoder frame points weighted by \code{.distribution}).
#' @param data a data.frame to be referred to for creating the grid.
#' @param weights a numeric vector of sample weights corresponding to \code{data}.
#' @param resolution an integer specifying the grid density for continuous axis variables when \code{method = "raster"}.
#' @param lumped logical. If \code{TRUE}, lumped factor levels are used for the grid.
#'
#' @exportS3Method midr::set_grid
set_grid.mid <- function(
    object, axis = NULL, method = c("raster", "data", "knots"),
    data = NULL, weights = NULL, resolution = 100L, lumped = FALSE, ...
) {
  method <- match.arg(method)
  domain <- get_variables(object)
  axis <- get_variables(axis)
  nuisance <- setdiff(domain, axis)
  if (method == "data" && length(axis) > 0L || (length(nuisance) > 0L)) {
    if (is.null(data)) {
      data <- model_data(object, env = parent.frame())
      weights <- weights %||% object$weights
    }
    if (is.null(data)) {
      stop("'data' is required when method = 'data' or when nuisance variables remain outside 'axis'")
    }
    if (!all(domain %in% colnames(data))) {
      data <- model_reframe(object, data)
    }
    data.weights <- weights %||% rep.int(1, nrow(data))
    if (nrow(data) != length(data.weights)) {
      stop("length of 'weights' doesn't match the number of rows in 'data'")
    }
    data.weights <- data.weights / sum(data.weights)
  }
  if (length(axis) == 0L) {
    axis_grid <- NULL
    axis.weights    <- 1
  } else if (method == "data") {
    axis_grid <- data[, axis, drop = FALSE]
    axis.weightseights    <- data.weights
  } else if (method == "knots") {
    knot_list <- lapply(axis, function(v) {
      ef <- object$main.effects[[v]]
      if (!is.null(ef) && ".distribution" %in% names(ef)) {
        return(list(vals = ef[[v]], w = ef[[".distribution"]]))
      }
      enc <- object$encoders$interactions[[v]] %||% object$encoders$main.effects[[v]]
      vals <- .extract_encoder_nodes(enc, lumped = lumped)
      list(vals = vals, w = rep.int(1 / length(vals), length(vals)))
    })
    vals_list <- setNames(lapply(knot_list, `[[`, "vals"), axis)
    w_list <- lapply(knot_list, `[[`, "w")
    axis_grid <- expand.grid(vals_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    axis.weights <- apply(expand.grid(w_list), 1L, prod)
    axis.weights <- axis.weights / sum(axis.weights)
  } else if (method == "raster") {
    encs <- object$encoders
    vals_list <- lapply(axis, function(v) {
      enc <- if (length(axis) == 1L) {
        encs$main.effects[[v]] %||% encs$interactions[[v]]
      } else {
        encs$interactions[[v]] %||% encs$main.effects[[v]]
      }
      .make_raster_seq(enc, resolution = resolution, lumped = lumped)
    })
    names(vals_list) <- axis
    axis_grid <- expand.grid(vals_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    axis.weights    <- rep.int(1 / nrow(axis_grid), nrow(axis_grid))
  }
  if (length(nuisance) == 0L) {
    grid       <- axis_grid
    final_w    <- axis.weights
  } else if (is.null(axis_grid)) {
    grid       <- data[, nuisance, drop = FALSE]
    grid$.gma.id <- seq_len(nrow(grid))
    final_w    <- data.weights
  } else {
    bg_sub <- data[, nuisance, drop = FALSE]
    bg_sub$.gma.id <- seq_len(nrow(bg_sub))
    grid    <- merge(axis_grid, bg_sub, by = NULL)
    final_w <- as.numeric(outer(axis.weights, data.weights, FUN = "*"))
  }
  rownames(grid) <- NULL
  attr(grid, "gma.axis")     <- axis
  attr(grid, "gma.nuisance") <- nuisance
  attr(grid, "gma.weights")  <- final_w
  class(grid) <- unique(c("gma_grid", class(grid)))
  object$gma.grid <- grid
  object
}


.resolve_axis_names <- function(axis, domain) {
  if (is.null(axis))
    return(NULL)
  matched <- intersect(get_variables(axis), domain)
  if (length(matched) > 0L)
    return(matched)
  vars <- all.vars(as_terms(axis))
  matched <- vapply(
    domain, function(d) {any(all.vars(str2lang(d)) %in% vars)}, logical(1L)
  )
  domain[matched]
}

.make_raster_seq <- function(enc, resolution = 100L, lumped = FALSE) {
  if (is.null(enc))
    return(0)
  if (enc$type == "factor") {
    enc$envir[[if (isTRUE(lumped)) "flvs" else "olvs"]]
  } else if (enc$type == "constant") {
    ref <- unlist(enc$frame[, 2L:3L], use.names = FALSE)
    ref <- ref[is.finite(ref)]
    res <- seq(min(ref), max(ref), length.out = resolution)
    unique(sort(c(res, ref)))
  } else if (enc$type == "linear") {
    ref <- enc$frame[, 1L]
    res <- seq(ref[1L], ref[length(ref)], length.out = resolution)
    unique(sort(c(res, ref)))
  } else {
    0
  }
}

.extract_encoder_nodes <- function(enc, lumped = FALSE) {
  if (is.null(enc))
    return(0)
  if (enc$type == "factor") {
    enc$envir[[if (isTRUE(lumped)) "flvs" else "olvs"]]
  } else {
    enc$frame[, 1L]
  }
}
