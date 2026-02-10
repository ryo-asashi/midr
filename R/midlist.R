#'
#' @export
#'
`[.midlist` <- function(
    x, i, drop = if (missing(i)) TRUE else length(i) == 1L
  ) {
  nm <- names(x$intercept)
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
    stop("undefined target(s) selected")
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

#'
#' @export
#'
`[[.midlist` <- function(x, i, exact = TRUE) {
  if (length(i) != 1L)
    stop("attempt to select more than one element in vectorIndex")
  if (is.character(i) && !exact)
    i <- pmatch(i, names(x))
  x[i, drop = TRUE]
}

extract.effects <- function(x, i, drop = FALSE) {
  x$mid <- if (drop && length(i) == 1L)
    as.numeric(x$mid[, i]) else I(x$mid[, i, drop = FALSE])
  x
}

#'
#' @exportS3Method base::as.list
#'
as.list.midlist <- function(x, ...) {
  nm <- names(x$intercept)
  lapply(stats::setNames(nm, nm), function(x) x[[x]])
}

#'
#' @exportS3Method stats::formula
#'
formula.midlist <- function(x, ...) {
  formula.mid(x, ...)
}
