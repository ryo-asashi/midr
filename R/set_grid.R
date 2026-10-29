#' Set Evaluation Grids
#'
#' @description
#' \code{set_grid()} constructs and attaches an evaluation grid (\code{ema.grid}) to a fitted model object for subsequent model analysis.
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
#' @param method character string specifying how to construct the axis grid: "raster" (uniform mesh over the domain), "data" (observed points), or "frame" (encoder frame points weighted by \code{.distribution}).
#' @param data a data.frame to be referred to for creating the grid.
#' @param weights a numeric vector of sample weights corresponding to \code{data}.
#' @param resolution an integer specifying the grid density for continuous axis variables when \code{method = "raster"}.
#' @param lumped logical. If \code{TRUE}, lumped factor levels are used for the grid.
#'
#' @exportS3Method midr::set_grid
set_grid.mid <- function(
    object, axis = NULL, method = c("raster", "data"),
    data = NULL, weights = NULL, resolution = 100L, lumped = FALSE,
    max.nrow = Inf, ...
) {
  method <- match.arg(method)
  Vaxis <- get_variables(axis)
  Vfull <- get_variables(object, raw = TRUE)
  if (!all(Vaxis %in% Vfull))
    Vfull <- get_variables(object, raw = FALSE)
  if (!all(Vaxis %in% Vfull))
    stop("invalid 'axis' variables found")
  Vnuis <- setdiff(Vfull, Vaxis)
  if (method == "data" && length(Vaxis) > 0L || (length(Vnuis) > 0L)) {
    if (is.null(data)) {
      data <- model_data(object, env = parent.frame())
      weights <- weights %||% object$weights
    }
    if (is.null(data)) {
      stop("'data' is required when method = 'data' or when nuisance variables remain outside 'axis'")
    }
    if (!all(Vfull %in% colnames(data))) {
      data <- try(model_reframe(object, data), silent = TRUE)
      if (inherits(data, "try-error") || !all(Vfull %in% colnames(data)))
        stop("'data' does not contain all required variables")
    }
    data.weights <- weights %||% rep.int(1, nrow(data))
    if (nrow(data) != length(data.weights)) {
      stop("length of 'weights' doesn't match the number of rows in 'data'")
    }
  }
  if (length(Vaxis) == 0L) {
    axis.grid <- NULL
    axis.weights <- 1
  } else if (method == "data") {
    axis.grid <- data[, Vaxis, drop = FALSE]
    axis.weights <- data.weights
  } else {
    encs <- object$encoders
    vals.list <- lapply(Vaxis, function(v) {
      enc <- encs$main.effects[[v]] %||% encs$interactions[[v]]
      .make_raster_seq(enc, resolution = resolution, lumped = lumped)
    })
    names(vals.list) <- axis
    axis.grid <- expand.grid(
      vals.list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    )
    axis.weights <- rep.int(1, nrow(axis.grid))
  }
  if (length(Vnuis) == 0L) {
    grid <- axis.grid
    grid.weights <- axis.weights
  } else if (is.null(axis.grid)) {
    grid <- data[, Vnuis, drop = FALSE]
    grid$.id <- seq_len(nrow(grid))
    grid.weights <- data.weights
  } else {
    nuis.grid <- data[, Vnuis, drop = FALSE]
    nuis.grid$.id <- seq_len(nrow(nuis.grid))
    grid <- merge(axis.grid, nuis.grid, by = NULL)
    grid.weights <- as.numeric(outer(axis.weights, data.weights, FUN = "*"))
  }
  rownames(grid) <- NULL
  attr(grid, "axis") <- Vaxis
  attr(grid, "nuisance") <- Vnuis
  attr(grid, "weights") <- grid.weights
  object$ema.grid <- grid
  object
}


#' @exportS3Method midr::set_grid
#'
set_grid.midrib <- function(object, ...) {
  set_grid.mid(object, ...)
}


.make_raster_seq <- function(enc, resolution = 100L, lumped = FALSE) {
  if (is.null(enc))
    return(0)
  if (enc$type == "factor") {
    enc$envir[[if (isTRUE(lumped)) "flvs" else "olvs"]]
  } else if (enc$type == "constant") {
    ref <- unique(sort(unlist(enc$frame[, 2L:3L], use.names = FALSE)))
    ref <- ref[is.finite(ref)]
    eps <- (max(ref) - min(ref)) * 1e-7
    pts <- c(ref - eps, ref, ref + eps)
    pts <- pts[pts >= min(ref) & pts <= max(ref)]
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
