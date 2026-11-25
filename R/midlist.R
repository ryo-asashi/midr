# R/midlist.R

.midlist_classes <- c("mid", "midimp", "midcon", "midbrk")

#' Combine MID Objects
#'
#' @description
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
#' @param ... objects to be combined, possibly named. All inputs must inherit from exactly one of the supported base classes: "mid", "midimp", "midcon", or "midbrk". Collection classes (e.g., "midrib"-"mids", "midimps"-"midlist") are also accepted and will be flattened appropriately.
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
  if (length(x) == 1L && inherits(x[[1L]], c("midrib", "midlist")))
    return(x[[1L]])
  x <- do.call(c, lapply(X = x, FUN = function(el) unclass(as.midlist(el))))
  for (what in .midlist_classes) {
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
  for (what in .midlist_classes) {
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
  out <- lapply(stats::setNames(nm, nm), function(name) x[[name]])
  cls <- unique(c(setdiff(class(x), "midrib"), "midlist"))
  structure(out, class = cls)
}


#' Label MID Collection Objects
#'
#' @description
#' S3 methods to get or set the labels (names) of a "midrib" or "midlist" object.
#'
#' @details
#' While a "midlist" object is a standard R list containing only one of a single base class, a "midrib" object stores multiple MID models in an optimized struct-of-arrays format.
#'
#' Because of the internal struct-of-arrays ("AsIs") structure, using \code{names()} on a "midrib" object returns internal component names (e.g., "intercept", "main.effects").
#' To safely access or modify the names of the models, always use \code{labels()} and \code{labels<-}.
#'
#' @param object a collection object of class "midlist" or "midrib".
#' @param value a character vector of the same length as the number of base objects in the collection object.
#' @param ... optional parameters passed to other methods.
#'
#' @examples
#' # Fit a multivariate linear model
#' fit <- lm(cbind(y1, y2, y3) ~ x1 + I(x1^2), data = anscombe)
#'
#' # Interpret the linear models
#' collection <- interpret(cbind(y1, y2, y3) ~ x1, data = anscombe, model = fit)
#'
#' # Check the default labels
#' labels(collection)
#'
#' # Rename the models in the collection
#' labels(collection) <- letters[1L:3L]
#' labels(collection)
#'
#' # Extract a single base "mid" object by its new name using [[
#' mid <- collection[["a"]]
#' class(mid)
#'
#' # Subset the collection to keep only the first two models using [
#' sub <- collection[1:2]
#' class(sub) # Maintains the collection class (e.g., "midrib"-"mids")
#' @returns
#' \code{labels()} returns a character vector of labels of the stored base objects.
#'
#' \code{labels<-} returns the updated collection object with new labels.
#'
#' @seealso \code{\link{midlist}}, \code{\link{extract.midlist}}
#'
#' @exportS3Method base::labels
#'
labels.midlist <- function(object, ...) {
  names(object) %||% as.character(seq_along(object))
}

#' @rdname labels.midlist
#' @exportS3Method base::labels
#'
labels.midrib <- function(object, ...) {
  names(object$intercept) %||% as.character(seq_along(object$intercept))
}


#' @rdname labels.midlist
#' @export
#'
`labels<-` <- function(object, value)
UseMethod("labels<-")

#' @rdname labels.midlist
#' @export
#'
`labels<-.midlist` <- function(object, value) {
  if (length(value) != length(object))
    stop("length of 'value' must match the number of models")
  names(object) <- as.character(value)
  object
}

#' @rdname labels.midlist
#' @export
#'
`labels<-.midrib` <- function(object, value) {
  nlab <- length(object$intercept)
  if (length(value) != nlab) {
    stop("length of 'value' must match the number of labels (", nlab, ")")
  }
  value <- as.character(value)
  names(object$intercept) <- value
  if (!is.null(object$main.effects)) {
    for (j in seq_along(object$main.effects))
      colnames(object$main.effects[[j]]$.effect) <- value
  }
  if (!is.null(object$interactions)) {
    for (j in seq_along(object$interactions))
      colnames(object$interactions[[j]]$.effect) <- value
  }
  colnames(object$fitted.values) <- value
  colnames(object$residuals) <- value
  if (!is.null(object$linear.predictors))
    colnames(object$linear.predictors) <- value
  if (!is.null(object$response.residuals))
    colnames(object$response.residuals) <- value
  if (is.matrix(object$ratio)) {
    colnames(object$ratio) <- value
  } else {
    names(object$ratio) <- value
  }
  object
}


#' Subset MID Collection Objects
#'
#' @description
#' S3 methods to extract parts of a "midlist" or "midrib" collection object.
#'
#' @details
#' A "midlist" or "midrib" object stores multiple objects of the same single base class: "mid", "midimp", "midcon", or "midbrk".
#'
#' When extracting items using \code{[}, it returns a subsetted "midlist" or "midrib" object, preserving its collection class (e.g., "mids", "midimps") by default (\code{drop = FALSE}).
#' If \code{drop = TRUE} and a single base object is extracted (\code{length(i) == 1L}), the object is simplified to a single base object (e.g., "mid", "midimp").
#' \code{[[} always extracts a single base object.
#'
#' @param x a collection object of class "midlist" or "midrib".
#' @param i indices specifying elements to extract. Can be numeric, character, or logical vectors.
#' @param drop logical. If \code{TRUE} and a single element is selected, the result is simplified to a base object (e.g., "mid"). Defaults to \code{FALSE}.
#' @param exact logical. If \code{TRUE}, exact matching is used for character strings.
#'
#' @examples
#' # Fit a multivariate linear model
#' fit <- lm(cbind(y1, y2, y3) ~ x1 + I(x1^2), data = anscombe)
#'
#' # Interpret the linear models
#' collection <- interpret(cbind(y1, y2, y3) ~ x1, data = anscombe, model = fit)
#'
#' # Check the default labels
#' labels(collection)
#'
#' # Rename the models in the collection
#' labels(collection) <- letters[1L:3L]
#' labels(collection)
#'
#' # Extract a single base "mid" object by its new name using [[
#' mid <- collection[["a"]]
#' class(mid)
#'
#' # Subset the collection to keep only the first two models using [
#' sub <- collection[1:2]
#' class(sub) # Maintains the collection class (e.g., "midrib"-"mids")
#' @returns
#' \code{[} returns a subsetted collection object or a single base object if \code{drop = TRUE}.
#'
#' \code{[[} returns a single base object.
#'
#' @seealso \code{\link{midlist}}, \code{\link{labels.midlist}}
#'
#' @name extract.midlist
#' @rdname extract.midlist
#' @export
#'
`[.midlist` <- function(
    x, i, drop = FALSE
) {
  out <- NextMethod("[")
  if (isTRUE(drop) && length(out) == 1L) return(out[[1L]])
  class(out) <- class(x)
  out
}


#' @rdname extract.midlist
#' @export
#'
`[.midrib` <- function(
    x, i, drop = FALSE
) {
  nm <- labels(x)
  if (missing(i)) {
    i <- seq_along(nm)
  } else if (is.character(i) && !anyNA(i)) {
    i <- match(i, nm)
  } else if (is.logical(i) && !anyNA(i)) {
    i <- which(rep(i, length.out = length(nm)))
  } else if (is.numeric(i) && !anyNA(i)) {
    i <- as.integer(i)
    if (any(i < 0L)) {
      if (any(i > 0L))
        stop("only 0's may be mixed with negative subscripts")
      i <- seq_along(nm)[i]
    }
  }
  if (anyNA(i)) stop("undefined item selected")
  if (is.numeric(i) && any(i > length(nm) | i < 0L))
    stop("subscript out of bounds")
  if (is.numeric(i)) {
    i <- i[i != 0L]
  }
  if (length(i) == 0L) return(NULL)
  drop <- drop && (length(i) == 1L)
  x$intercept <- x$intercept[i]
  if (drop) names(x$intercept) <- NULL
  if (!is.null(x$main.effects)) {
    for (j in seq_along(x$main.effects)) {
      x$main.effects[[j]]$.effect <- if (drop) {
        as.numeric(x$main.effects[[j]]$.effect[, i])
      } else {
        I(x$main.effects[[j]]$.effect[, i, drop = FALSE])
      }
    }
  }
  if (!is.null(x$interactions)) {
    for (j in seq_along(x$interactions)) {
      x$interactions[[j]]$.effect <- if (drop) {
        as.numeric(x$interactions[[j]]$.effect[, i])
      } else {
        I(x$interactions[[j]]$.effect[, i, drop = FALSE])
      }
    }
  }
  x$fitted.values <- x$fitted.values[, i, drop = drop]
  x$residuals <- x$residuals[, i, drop = drop]
  if (!is.null(x$linear.predictors))
    x$linear.predictors <- x$linear.predictors[, i, drop = drop]
  if (!is.null(x$response.residuals))
    x$response.residuals <- x$response.residuals[, i, drop = drop]
  if (is.matrix(x$ratio)) {
    x$ratio <- x$ratio[, i, drop = drop]
  } else {
    x$ratio <- if (drop) as.numeric(x$ratio[i]) else x$ratio[i]
  }
  class(x) <- if (drop) "mid" else c("mids", "midrib")
  x
}


#' @rdname extract.midlist
#' @export
#'
`[[.midrib` <- function(x, i, exact = TRUE) {
  if (length(i) != 1L)
    stop("attempt to select more than one element in vectorIndex")
  if (is.character(i) && !exact) {
    i <- pmatch(i, labels(x))
  }
  x[i, drop = TRUE]
}


# internal helpers ----

summarize_midlist <- function(
    object, shape = c("wide", "long"), dfname, keycol, valcol,
    keys = keycol, targets = NULL, fill = NA_real_
) {
  shape <- match.arg(shape)
  objs <- as.list(object)
  labs <- labels(object)
  if (length(objs) == 0L) return(data.frame())

  if (is.null(targets)) {
    targets <- unique(unlist(
      lapply(objs, function(o) as.character(o[[dfname]][[keycol]])),
      use.names = FALSE
    ))
  } else {
    targets <- as.character(targets)
  }

  if (shape == "wide") {
    first <- objs[[1L]][[dfname]]
    if (length(keys) == 1L) {
      info <- data.frame(
        stats::setNames(list(factor(targets, levels = targets)), keys),
        check.names = FALSE
      )
      mat <- vapply(objs, function(o) {
        inner <- o[[dfname]]
        vals <- inner[[valcol]][match(targets, as.character(inner[[keycol]]))]
        if (!is.na(fill)) vals[is.na(vals)] <- fill
        vals
      }, numeric(length(targets)))
    } else {
      keep0 <- as.character(first[[keycol]]) %in% targets
      info <- first[keep0, keys, drop = FALSE]
      ref_id <- do.call(paste, c(info, sep = "\r"))
      mat <- vapply(objs, function(o) {
        inner <- o[[dfname]]
        cur_id <- do.call(paste, c(inner[keys], sep = "\r"))
        vals <- inner[[valcol]][match(ref_id, cur_id)]
        if (!is.na(fill)) vals[is.na(vals)] <- fill
        vals
      }, numeric(nrow(info)))
    }
    if (!is.matrix(mat)) mat <- matrix(mat, nrow = nrow(info))
    out <- data.frame(info, mat, check.names = FALSE)
    colnames(out)[seq.int(ncol(info) + 1L, ncol(out))] <- labs
    rownames(out) <- NULL
    return(out)
  }

  dfs <- lapply(seq_along(objs), function(i) {
    inner <- objs[[i]][[dfname]]
    keep <- as.character(inner[[keycol]]) %in% targets
    sub <- inner[keep, c(keys, valcol), drop = FALSE]
    if (length(keys) == 1L) {
      sub[[keycol]] <- factor(as.character(sub[[keycol]]), levels = targets)
    }
    sub$label <- labs[i]
    sub[, c(keys, "label", valcol), drop = FALSE]
  })
  out <- do.call(rbind, dfs)
  out$label <- factor(out$label, levels = labs)
  rownames(out) <- NULL
  out
}
