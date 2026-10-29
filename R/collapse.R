#' Collapse Evaluation Arrays
#'
#' @param object a \code{gma_evaluation} object returned by \code{evaluate()}.
#' @param direction a character string specifying the dimension to collapse: \code{"none"}, \code{"row"} (across grid points), \code{"column"} (across terms/variables), or \code{"layer"} (across models).
#' @param method a character string or function specifying the method to collapse (e.g., \code{"mean"}, \code{"sum"}, \code{"mean_abs"}, \code{"sd"}).
#'
#'
#' @export collapse
collapse <- function(
    object, direction = c("row", "column", "layer"), method = "mean",
    group = NULL, weights = NULL, ...
) {
  direction <- match.arg(direction)
  dims <- dim(object)
  is3D <- length(dims) == 3L
  grid <- attr(object, "grid")
  collapse.fun <- .get_collapse_fun(method)
  if (direction == "row") {
    w <- weights %||% attr(grid, "weights") %||% rep.int(1, dims[1L])
    if (length(w) == 1L)
      w <- rep.int(w, dims[1L])
    if (length(w) != dims[1L])
      stop("length of 'weights' doesn't match the number of evaluation points")
    axis <- attr(grid, "axis")
    by <- unique(c(axis, get_variables(group)))
    if (is.null(by) || length(by) == 0L) {
      margin <- if (is3D) 2L:3L else 2L
      res <- apply(
        X = object, MARGIN = margin,
        FUN = function(x) collapse.fun(x, weights = w, ...))
      return(res)
    }
    if (!all(by %in% colnames(grid)))
      stop("invalid 'axis' or 'group' variables specified")
    res.grid <- grid[, by, drop = FALSE]
    group <- interaction(res.grid, drop = FALSE, lex.order = FALSE)
    first <- vapply(
      X = split(seq_len(dims[1L]), group),
      FUN = `[`, FUN.VALUE = integer(1L), 1L
    )
    res.grid <- res.grid[first, , drop = FALSE]
    rownames(res.grid) <- NULL
    if (!is3D) {
      res.list <- lapply(X = split, FUN = function(idx) {
        apply(
          X = object[idx, , drop = FALSE], MARGIN = 2L,
          FUN = function(x) collapse.fun(x, weights = w[idx], ...))
      })
      res.mat <- do.call(rbind, res.list)
      if (ncol(object) == 1L) {
        res.mat <- matrix(res.mat, ncol = 1L)
        colnames(res.mat) <- colnames(object)
      }
      colnames(res.mat) <- paste0(".", colnames(res.mat))
      return(cbind(res.grid, res.mat))
    } else {
      res.arr <- array(
        NA_real_,
        dim = c(length(split), dims[2L], dims[3L]),
        dimnames = list(NULL, dimnames(object)[[2L]], dimnames(object)[[3L]])
      )
      for (g in seq_along(split)) {
        idx <- split[[g]]
        res_arr[g, , ] <- apply(
          X = object[idx, , , drop = FALSE], 2L:3L,
          FUN = function(x) collapse.fun(x, weights = w[idx], ..)
        )
      }
      attr(res.grid, "axis") <- by
    }
  } else if (direction == "column") {
    weights <- weights %||% attr(object, "weights")
    if (length(weights) == 1L)
      weights <- rep.int(weights, ncol(object))
    if (length(weights) != nrow(object))
      stop("length of 'weights' doesn't match the number of evaluation points")
    object <- apply(
      X = object,
      MARGIN = 2L,
      FUN = method,
      weight = weight,
      simplify = TRUE
    )
    colnames(object) <- paste0(".", colnames(object))
    return(object)

  }
}



.get_collapse_fun <- function(method) {
  if (is.function(method)) {
    if ("w" %in% names(formals(method)) || "..." %in% names(formals(method))) {
      return(method)
    }
    return(function(x, w, ...) method(x, ...))
  }
  method <- match.arg(method, c("mean", "sum", "mean_abs", "sd", "var"))
  switch(
    method,
    mean = function(x, w, na.rm = FALSE, ...) stats::weighted.mean(x, w = w, na.rm = na.rm, ...),
    sum = function(x, w, na.rm = FALSE, ...) sum(x * w, na.rm = na.rm),
    mean_abs = function(x, w, na.rm = FALSE, ...) stats::weighted.mean(abs(x), w = w, na.rm = na.rm, ...),
    var = function(x, w, na.rm = FALSE, ...) {
      if (na.rm) { keep <- !is.na(x); x <- x[keep]; w <- w[keep] }
      w <- w / sum(w)
      mu <- sum(x * w)
      sum(w * (x - mu)^2) / (1 - sum(w^2)) # 不偏重み付き分散
    },
    sd = function(x, w, na.rm = FALSE, ...) {
      if (na.rm) { keep <- !is.na(x); x <- x[keep]; w <- w[keep] }
      w <- w / sum(w)
      mu <- sum(x * w)
      sqrt(sum(w * (x - mu)^2) / (1 - sum(w^2)))
    }
  )
}
