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
#' @param ... objects to be combined, possibly named. All inputs must inherit from exactly one of the supported base classes: "mid", "midimp", "midcon", or "midbrk". Collection classes (e.g., "mids"-"midrib", "midimps"-"midlist") are also accepted and will be flattened appropriately.
#'
#' @examples
#' # Fit models using the built-in anscombe dataset
#' fit1 <- lm(cbind(y1, y2, y3) ~ x1, data = anscombe)
#' fit2 <- lm(y4 ~ x4, data = anscombe)
#'
#' # Create interpretation objects
#' # mid1 is a "midrib" collection containing 3 models
#' mid1 <- interpret(cbind(y1, y2, y3) ~ x1, data = anscombe, model = fit1)
#' class(mid1)
#' # mid2 is a single "mid" object
#' mid2 <- interpret(y4 ~ x4, data = anscombe, model = fit2)
#'
#' # Combine a "midrib" and a "mid" into a single "midlist" collection.
#' collection <- midlist(mid1, y4 = mid2)
#'
#' # Check the labels of the combined collection
#' labels(collection)
#'
#' # The resulting object is a flat list of models
#' class(collection)
#' @returns
#' \code{midlist()} returns a list-based collection object inheriting from "midlist" and the appropriate collection class (e.g., "midcons"-"midlist").
#' If a single "midrib" object is provided, the original object is returned as-is.
#'
#' \code{as.midlist()} returns a "midlist" object with a type-class "mids", "midimps", "midbrks", or "midcons".
#'
#' @seealso \code{\link{extract.midlist}}, \code{\link{labels.midlist}}
#'
#' @export
#'
midlist <- function(...) {
  x <- list(...)
  if (length(x) == 0L)
    return(structure(list(), class = c("mids", "midlist")))
  if (length(x) == 1L && inherits(x[[1]], "midrib"))
    return(x[[1L]])
  x <- do.call(c, lapply(X = x, FUN = as.midlist))
  for (what in c("mid", "midimp", "midcon", "midbrk")) {
    cls <- c(paste0(what, "s"), "midlist")
    if (all(vapply(x, inherits, logical(1L), what)))
      return(structure(x, class = cls))
  }
  stop("objects cannot be combined into a 'midlist' object")
}


#' @rdname midlist
#' @param x object to be coerced or tested.
#' @export
#'
as.midlist <- function(x) {
  if (inherits(x, "midrib"))
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
