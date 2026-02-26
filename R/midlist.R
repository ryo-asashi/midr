#' Combine MID Objects
#'
##' @description
#' Combines multiple MID models ("mid") or their interpretation results ("midimp", "midcon", "midbrk") into a unified collection object.
#' This is useful for grouping models and their explanations together for seamless comparison, summary, and visualization.
#'
#' @details
#' The \code{midlist()} function acts as a polymorphic constructor for collection objects.
#' Depending on the class of the input objects, it automatically assigns the appropriate classes (e.g., "mid" objects become a "mids"-"midlist" collection; "midimp" objects become a "midimps"-"midlist" collection).
#' All objects provided in \code{...} must belong to the same base class.
#'
#' If a single "midrib" object is provided, it is returned as-is, preserving its optimized struct-of-arrays format.
#' However, if a "midrib" object is combined with other objects via \code{...}, it is automatically coerced into a pure list (array of structures) to ensure structural consistency before concatenation.
#'
#' @param ... objects to be combined. All inputs must inherit from exactly one of the supported base classes: "mid", "midimp", "midcon", or "midbrk". Collection classes (e.g., "mids"-"midrib", "midimps"-"midlist") are also accepted and will be flattened appropriately.
#'
#' @examples
#' # Fit models using the built-in anscombe dataset
#' fit_1 <- lm(cbind(y1, y2, y3) ~ x1, data = anscombe)
#' fit_2 <- lm(y4 ~ x4, data = anscombe)
#'
#' # Create interpretation objects
#' # mid_1 is a "midrib" collection containing 3 models
#' mid_1 <- interpret(cbind(y1, y2, y3) ~ x1, data = anscombe, model = fit_1)
#' # mid_2 is a single "mid" object
#' mid_2 <- interpret(y4 ~ x4, data = anscombe, model = fit_2)
#'
#' # Combine a "midrib" and a "mid" into a single "midlist" collection.
#' mid_collection <- midlist(mid_1, y4 = mid_2)
#'
#' # Check the labels of the combined collection
#' labels(mid_collection)
#'
#' # The resulting object is a flat list of models
#' class(mid_collection)
#' @returns
#' \code{midlist()} returns a list-based collection object inheriting from "midlist" and the appropriate collection class (e.g., "midcons"-"midlist").
#' If a single "midrib" object is provided, the original object is returned as-is.
#'
#' @export
#'
midlist <- function(...) {
  x <- list(...)
  if (length(x) == 0L)
    return(structure(list(), class = "midlist"))
  if (length(x) == 1L && inherits(x[[1]], "midrib"))
    return(x[[1L]])
  x <- do.call(c, lapply(x, as.midlist))
  for (what in c("mid", "midimp", "midcon", "midbrk")) {
    cls <- c(paste0(what, "s"), "midlist")
    if (all(vapply(x, inherits, logical(1L), what)))
      return(structure(x, class = cls))
  }
  stop("'x' cannot be coerced to a 'midlist' object")
}

as.midlist <- function(x, force = TRUE) {
  if (force && inherits(x, "midrib"))
    return(as.list.midrib(x))
  if (inherits(x, "midlist"))
    return(x)
  for (what in c("mid", "midimp", "midcon", "midbrk")) {
    cls <- c(paste0(what, "s"), "midlist")
    if (inherits(x, what))
      return(structure(list(x), class = cls))
    if (is.list(x) && all(vapply(x, inherits, logical(1L), what)))
      return(structure(x, class = cls))
  }
  stop("'x' cannot be coerced to a 'midlist' object")
}

#' @exportS3Method base::as.list
#'
as.list.midrib <- function(x, ...) {
  nm <- labels(x)
  out <- lapply(stats::setNames(nm, nm), function(nm) x[[nm]])
  cls <- unique(c(setdiff(class(x), "midrib"), "midlist"))
  structure(out, class = cls)
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


.summarize.collection <- function(
  object, shape, dfname, keycol, valcol, keys, targets
  ) {
  shape <- match.arg(shape, c("wide", "long"))
  if (shape == "wide") {
    fun <- function(x) {
      inner <- x[[dfname]]
      res <- inner[[valcol]][match(targets, inner[[keycol]])]
      replace(res, is.na(res), 0)
    }
    info <- object[[1L]][[dfname]]
    info <- info[match(targets, info[[keycol]]), keys, drop = FALSE]
    if (is.null(names(object))) names(object) <- labels(object)
    out <- data.frame(
      info, as.data.frame(lapply(object, fun)), check.names = FALSE
    )
  } else {
    nms <- labels(object)
    fun_long <- function(x, nm) {
      inner <- x[[dfname]]
      res <- inner[inner[[keycol]] %in% targets, c(keys, valcol), drop = FALSE]
      res$label <- nm
      res
    }
    out <- do.call(rbind, Map(fun_long, object, nms))
  }
  rownames(out) <- NULL
  return(out)
}

#' @exportS3Method base::summary
#'
summary.midimps <- function(
    object, shape = c("wide", "long"), terms = NULL, ...
) {
  shape <- match.arg(shape)
  terms <- terms %||% mid.terms(object[[1L]])
  .summarize.collection(
    object = object, shape = shape,
    dfname = "importance", keycol = "term", valcol = "importance",
    keys = "term", targets = terms
  )
}

#' @exportS3Method base::summary
#'
summary.midbrks <- function(
    object, shape = c("wide", "long"), terms = NULL, ...
) {
  shape <- match.arg(shape)
  terms <- terms %||% mid.terms(object[[1L]])
  .summarize.collection(
    object = object, shape = shape,
    dfname = "breakdown", keycol = "term", valcol = "mid",
    keys = "term", targets = terms
  )
}

#' @exportS3Method base::summary
#'
summary.midcons <- function(
    object, shape = c("long", "wide"), ids = NULL, ...
) {
  shape <- match.arg(shape)
  ids <- ids %||% object[[1L]]$ids
  variable <- object[[1L]]$variable
  .summarize.collection(
    object = object, shape = shape,
    dfname = "conditional", keycol = ".id", valcol = "yhat",
    keys = c(".id", variable), targets = ids
  )
}
