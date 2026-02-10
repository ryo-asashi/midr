#'
#' @exportS3Method base::names
#'
names.midlist <- function(x)
  names(x$intercept)

#'
#' @exportS3Method base::`[`
#'
`[.midlist` <- function(
    x, i, drop = if (missing(i)) TRUE else length(i) == 1L
  ) {
  nm <- names(x)
  if (missing(i)) {
    i <- seq_along(nm)
  } else if (is.character(i)) {
    i <- pmatch(i, nm, duplicates.ok = TRUE)
    if (anyNA(i)) stop("undefined targets selected")
  } else if (is.logical(i)) {
    i <- which(rep(i, length.out = length(nm)))
  }
  x$intercept <- x$intercept[i]
  me <- x$main.effects
  if (!is.null(me)) x$main.effects <- lapply(me, extract.effects, i, drop)
  ie <- x$interactions
  if (!is.null(ie)) x$interactions <- lapply(ie, extract.effects, i, drop)
  x$fitted.values <- x$fitted.values[, i, drop = drop]
  x$residuals <- x$residuals[, i, drop = drop]
  x$linear.predictors <- x$linear.predictors[, i, drop = drop]
  x$response.residuals <- x$response.residuals[, i, drop = drop]
  x$ratio <- if (is.matrix(x$ratio))
    x$ratio[, i, drop = drop] else x$ratio[i]
  if (drop && length(i) == 1L) {
    names(x$intercept) <- NULL
    names(x$ratio) <- NULL
  }
  x
}

#'
#' @exportS3Method base::`[[`
#'
`[[.midlist` <- function(x, i, exact = TRUE) {
  if (length(i) != 1L)
    stop("attempt to select more than one element in vectorIndex")
  if (is.character(i) && !exact)
    i <- pmatch(i, names(x))
  x[i, drop = TRUE]
}

extract.effects <- function(x, i, drop = FALSE) {
  x$mid <- if (drop && length(i) == 1L) as.numeric(x$mid[, i]) else I(x$mid[, i])
  x
}
