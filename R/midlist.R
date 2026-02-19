#' @export
#'
`[.midlist` <- function(
    x, i, drop = if (missing(i)) TRUE else length(i) == 1L
  ) {
  nm <- names(x$intercept)
  if (is.null(nm)) {
    x <- unclass(x)
    x <- if (length(i) == 1L && drop) x[[i]] else
      structure(x[i], class = "midlist")
    return(x)
  }
  if (missing(i)) {
    i <- seq_along(nm)
  } else if (is.character(i)) {
    i <- match(i, nm)
  } else if (is.logical(i)) {
    i <- which(rep(i, length.out = length(nm)))
  } else if (is.numeric(i)) {
    i <- as.integer(i)
  }
  if (anyNA(i))
    stop("undefined item selected")
  if (is.numeric(i) && any(i > length(nm) | i < 1L))
    stop("subscript out of bounds")
  x$intercept <- x$intercept[i]
  me <- x$main.effects
  if (!is.null(me)) x$main.effects <- lapply(me, extract.effects, i, drop)
  ie <- x$interactions
  if (!is.null(ie)) x$interactions <- lapply(ie, extract.effects, i, drop)
  x$fitted.values <- x$fitted.values[, i, drop = drop]
  x$residuals <- x$residuals[, i, drop = drop]
  if (!is.null(x$linear.predictors))
    x$linear.predictors <- x$linear.predictors[, i, drop = drop]
  if (!is.null(x$response.residuals))
    x$response.residuals <- x$response.residuals[, i, drop = drop]
  x$ratio <- if (is.matrix(x$ratio))
    x$ratio[, i, drop = drop] else x$ratio[i]
  if (drop && length(i) == 1L) {
    names(x$intercept) <- NULL
    if (length(x$ratio) == 1L) names(x$ratio) <- NULL
    class(x) <- "mid"
  } else {
    class(x) <- "midlist"
  }
  x
}

#' @export
#'
`[[.midlist` <- function(x, i, exact = TRUE) {
  if (length(i) != 1L)
    stop("attempt to select more than one element in vectorIndex")
  if (is.character(i) && !exact) {
    i <- pmatch(i, names(x$intercept))
  }
  x[i, drop = TRUE]
}

extract.effects <- function(x, i, drop = FALSE) {
  x$mid <- if (drop && length(i) == 1L)
    as.numeric(x$mid[, i]) else I(x$mid[, i, drop = FALSE])
  x
}

#' @exportS3Method base::as.list
#'
as.list.midlist <- function(x, ...) {
  nm <- names(x$intercept)
  if (is.null(nm)) return(x)
  lapply(stats::setNames(nm, nm), function(name) x[[name]])
}

#' @exportS3Method stats::formula
#'
formula.midlist <- function(x, ...) {
  formula.mid(x, ...)
}

#' @exportS3Method stats::model.frame
#'
model.frame.midlist <- function(object, ...) {
  model.frame.mid(object, ...)
}


#' @export
#'
`[.midlist.importance` <- function(x, i, ...) {
  x <- unclass(x)[i, ...]
  if (!is.object(x)) class(x) <- "midlist.importance"
  x
}

#' @export
#'
`[.midlist.conditional` <- function(x, i, ...) {
  x <- unclass(x)[i, ...]
  if (!is.object(x)) class(x) <- "midlist.conditional"
  x
}

#' @export
#'
`[.midlist.breakdown` <- function(x, i, ...) {
  x <- unclass(x)[i, ...]
  if (!is.object(x)) class(x) <- "midlist.breakdown"
  x
}


#' @exportS3Method base::summary
#'
summary.midlist.importance <- function(
    object, shape = c("wide", "long"), terms = attr(object, "term.labels"), ...
  ) {
  shape <- match.arg(shape)
  if (shape == "wide") {
    info <- data.frame(term = terms)
    fun <- function(x) {
      res <- x$importance
      idx <- match(terms, res$term)
      res <- res$importance[idx]
      res[is.na(res)] <- 0
      res
    }
    out <- vapply(
      X = object, FUN = fun, FUN.VALUE = numeric(length(terms))
    )
    out <- cbind(info, out)
    rownames(out) <- NULL
  } else {
    nms <- names(object) %||% as.character(seq_along(object))
    fun <- function(nm) {
      res <- object[[nm]]$importance
      res <- res[res$term %in% terms, ]
      res$label <- nm
      res
    }
    out <- lapply(X = nms, FUN = fun)
    out <- do.call(rbind, out)
  }
  out
}

#' @exportS3Method base::summary
#'
summary.midlist.breakdown <- function(
    object, shape = c("wide", "long"), terms = attr(object, "term.labels"), ...
) {
  shape <- match.arg(shape)
  if (shape == "wide") {
    info <- data.frame(term = terms)
    fun <- function(x) {
      res <- x$breakdown
      idx <- match(terms, res$term)
      res <- res$mid[idx]
      res[is.na(res)] <- 0
      res
    }
    out <- vapply(
      X = object, FUN = fun, FUN.VALUE = numeric(length(terms))
    )
    out <- cbind(info, out)
    rownames(out) <- NULL
  } else {
    nms <- names(object) %||% as.character(seq_along(object))
    fun <- function(nm) {
      res <- object[[nm]]$breakdown
      res <- res[res$term %in% terms, ]
      res$label <- nm
      res
    }
    out <- lapply(X = nms, FUN = fun)
    out <- do.call(rbind, out)
  }
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
  values <- attr(object, "values") %||% object[[1L]]$values
  if (shape == "wide") {
    info <- object[[1L]]$conditional
    info <- info[info$.id %in% ids, c(".id", variable), drop = FALSE]
    fun <- function(x) {
      res <- x$conditional
      res <- res$yhat[res$.id %in% ids]
      res[is.na(res)] <- 0
      res
    }
    out <- vapply(
      X = object, FUN = fun, FUN.VALUE = numeric(length(ids) * length(values))
    )
    out <- cbind(info, out)
    rownames(out) <- NULL
  } else {
    nms <- names(object) %||% as.character(seq_along(object))
    fun <- function(nm) {
      res <- object[[nm]]$conditional
      res <- res[res$.id %in% ids, c(".id", variable, "yhat"), drop = FALSE]
      res$label <- nm
      res
    }
    out <- lapply(X = nms, FUN = fun)
    out <- do.call(rbind, out)
    rownames(out) <- NULL
  }
  out
}
