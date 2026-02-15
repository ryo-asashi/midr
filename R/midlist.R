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
    x$label <- names(x$intercept)
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
    object, terms = NULL, ...
  ) {
  terms <- (terms %||% attr(object, "term.labels")) %||% mid.terms(object[[1L]])
  fun <- function(x, terms) {
    dat <- x$importance
    idx <- match(terms, dat$term)
    res <- dat$importance[idx]
    res[is.na(res)] <- 0
    res
  }
  out <- vapply(
    X = object, FUN = fun, FUN.VALUE = numeric(length(terms)), terms = terms
  )
  rownames(out) <- terms
  out
}

#' @exportS3Method base::summary
#'
summary.midlist.breakdown <- function(
    object, terms = NULL, ...
) {
  terms <- (terms %||% attr(object, "term.labels")) %||% mid.terms(object[[1L]])
  fun <- function(x, terms) {
    dat <- x$breakdown
    idx <- match(terms, dat$term)
    res <- dat$mid[idx]
    res[is.na(res)] <- 0
    res
  }
  out <- vapply(
    X = object, FUN = fun, FUN.VALUE = numeric(length(terms)), terms = terms
  )
  rownames(out) <- terms
  out
}

#' @exportS3Method base::summary
#'
summary.midlist.conditional <- function(
    object, ...
) {
  variable <- attr(object, "variable") %||% attr(object[[1L]], "variable")
  nms <- names(object)
  if (is.null(nms)) nms <- as.character(seq_along(object))
  fun <- function(nm) {
    dat <- object[[nm]]$conditional[, c(".id", "yhat", variable)]
    dat$.name <- nm
    dat
  }
  out <- lapply(X = nms, FUN = fun)
  do.call(rbind, out)
}
