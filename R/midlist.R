#' Create a Collection of MID Models
#'
#' @description
#' Combines multiple "mid", "midlist", or "midrib" objects into a single "midlist" object.
#' This is useful for grouping models together for comparison, summary, or visualization.
#'
#' @details
#' A "midlist" is a standard R list containing only "mid" objects.
#' When multiple arguments are passed to \code{midlist()}, they are coerced and flattened into a single list of models.
#'
#' If a single "midrib" object is provided, it is returned as-is, preserving its optimized struct-of-arrays format.
#' However, if a "midrib" object is combined with other objects via \code{...}, it is automatically coerced into a pure list (array of structures) to ensure structural consistency before concatenation.
#'
#' @param ... "mid", "midlist", or "midrib" objects to be combined.
#'
#' @returns
#' A "midlist" object, which is a list of "mid" objects. If a single "midrib" object is provided in \code{...}, the original object is returned as-is.
#'
#' @export
#'
midlist <- function(...) {
  x <- list(...)
  if (length(x) == 0L)
    return(structure(list(), class = "midlist"))
  if (length(x) == 1L && inherits(x[[1]], "midrib"))
    return(x[[1L]])
  structure(do.call(c, lapply(x, as.midlist)), class = "midlist")
}

as.midlist <- function(x, force = TRUE) {
  if (force && inherits(x, "midrib"))
    return(structure(as.list.midrib(x), class = "midlist"))
  if (inherits(x, "midlist"))
    return(x)
  if (inherits(x, "mid"))
    return(structure(list(x), class = "midlist"))
  if (is.list(x) && all(vapply(x, inherits, logical(1L), "mid"))) {
    class(x) <- unique(c("midlist", class(x)))
    return(x)
  }
  stop("'x' cannot be coerced to a 'midlist' object")
}


#' @exportS3Method base::as.list
#'
as.list.midrib <- function(x, ...) {
  nm <- labels(x)
  out <- lapply(stats::setNames(nm, nm), function(nm) x[[nm]])
  structure(out, class = "midlist")
}

#' @exportS3Method stats::formula
#'
formula.midrib <- function(x, ...) {
  formula.mid(x, ...)
}

#' @exportS3Method stats::model.frame
#'
model.frame.midrib <- function(object, ...) {
  model.frame.mid(object, ...)
}

#' @exportS3Method base::summary
#'
summary.midlist.importance <- function(
    object, shape = c("wide", "long"), terms = attr(object, "term.labels"), ...
) {
  shape <- match.arg(shape)
  terms <- terms %||% mid.terms(object[[1L]])
  if (shape == "wide") {
    fun <- function(x) {
      res <- x$importance$importance[match(terms, x$importance$term)]
      replace(res, is.na(res), 0)
    }
    if (is.null(names(object))) names(object) <- labels(object)
    out <- data.frame(
      term = terms, as.data.frame(lapply(object, fun)), check.names = FALSE
    )
  } else {
    nms <- labels(object)
    fun.long <- function(x, nm) {
      res <- x$importance[x$importance$term %in% terms, ]
      res$label <- nm
      res
    }
    out <- Map(fun.long, object, nms)
    out <- do.call(rbind, out)
  }
  rownames(out) <- NULL
  out
}

#' @exportS3Method base::summary
#'
summary.midlist.breakdown <- function(
    object, shape = c("wide", "long"), terms = attr(object, "term.labels"), ...
) {
  shape <- match.arg(shape)
  terms <- terms %||% mid.terms(object[[1L]])
  if (shape == "wide") {
    fun <- function(x) {
      res <- x$breakdown$mid[match(terms, x$breakdown$term)]
      replace(res, is.na(res), 0)
    }
    if (is.null(names(object))) names(object) <- labels(object)
    out <- data.frame(
      term = terms, as.data.frame(lapply(object, fun)), check.names = FALSE
    )
  } else {
    nms <- labels(object)
    fun.long <- function(x, nm) {
      res <- x$breakdown[x$breakdown$term %in% terms, ]
      res$label <- nm
      res
    }
    out <- Map(fun.long, object, nms)
    out <- do.call(rbind, out)
  }
  rownames(out) <- NULL
  out
}

#' @exportS3Method base::summary
#'
summary.midlist.conditional <- function(
    object, shape = c("long", "wide"), ids = attr(object, "ids"), ...
) {
  shape <- match.arg(shape)
  ids <- ids %||% object[[1L]]$ids
  variable <- attr(object, "variable") %||% object[[1L]]$variable
  if (shape == "wide") {
    values <- attr(object, "values") %||% object[[1L]]$values
    info <- object[[1L]]$conditional
    info <- info[info$.id %in% ids, c(".id", variable), drop = FALSE]
    fun <- function(x) {
      res <- x$conditional$yhat[x$conditional$.id %in% ids]
      replace(res, is.na(res), 0)
    }
    if (is.null(names(object))) names(object) <- labels(object)
    out <- data.frame(
      info, as.data.frame(lapply(X = object, FUN = fun)), check.names = FALSE
    )
  } else {
    nms <- labels(object)
    fun.long <- function(x, nm) {
      res <- x$conditional[
        x$conditional$.id %in% ids, c(".id", variable, "yhat"), drop = FALSE
      ]
      res$label <- nm
      res
    }
    out <- Map(fun.long, object, nms)
    out <- do.call(rbind, out)
  }
  rownames(out) <- NULL
  out
}
