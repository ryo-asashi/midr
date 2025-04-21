#' Extract Components from MID Models
#'
#' \code{mid.extract()} returns a component of a MID model.
#'
#' @param object a "mid" object.
#' @param component a literal character string or name. The name of the component to extract, such as "frames", "encoding.scheme" and "uninterpreted.rate".
#' @param ... optional parameters to be passed to the function used to extract the component.
#' @examples
#' data(trees, package = "datasets")
#' mid <- interpret(Volume ~ .^2, trees, k = 10)
#' mid.extract(mid, encoding.scheme)
#' mid.extract(mid, ur)
#' mid.extract(mid, frames)
#' mid.extract(mid, Girth)
#' mid.extract(mid, intercept)
#' @returns
#' \code{mid.extract()} returns the \code{component} extracted from the \code{object},
#' \code{mid.encoding.scheme()} returns a data frame containing the information about encoding schemes,
#' \code{mid.frames()} returns a list of the encoding frames,
#' \code{mid.terms()} returns a character vector of the term labels, and
#' \code{mid.ur()} returns the uninterpreted rate of the MID model.
#' @export mid.extract
#'
mid.extract <- function(object, component, ...) {
  component <- as.character(substitute(component))
  if (any(component == object$terms)) {
    if (grepl(":", component))
      return(object$interactions[[component]])
    return(object$main.effects[[component]])
  }
  rt <- switch(component,
    breakdown = mid.breakdown(object, ...),
    conditional = mid.conditional(object, ...),
    encoding.scheme = mid.encoding.scheme(object, ...),
    frames = mid.frames(object, ...),
    importance = mid.importance(object, ...),
    plots = mid.plots(object, ...),
    terms = mid.terms(object, ...),
    ur = mid.ur(object, ...)
  )
  ifnot.null(rt, object[[component]])
}

#' @rdname mid.extract
#' @export mid.encoding.scheme
#'
mid.encoding.scheme <- function(object, ...) {
  fun <- function(enc) paste0(enc$type, "(", enc$n, ")")
  mencs <- sapply(object$encoders[["main.effects"]], fun)
  iencs <- sapply(object$encoders[["interactions"]], fun)
  tags <- unique(c(mtags <- names(mencs), itags <- names(iencs)))
  df <- data.frame(row.names = tags)
  if (length(mtags) > 0L) {
    df[["main.effect"]] <- character(1L)
    for (tag in mtags)
      df[tag, "main.effect"] <- mencs[tag]
  }
  if (length(itags) > 0L) {
    df[["interaction"]] <- character(1L)
    for (tag in itags)
      df[tag, "interaction"] <- iencs[tag]
  }
  df
}


#' @rdname mid.extract
#' @export mid.frames
#'
mid.frames <- function(object, ...) {
  mfl <- lapply(object$encoders[["main.effects"]], function(enc) enc$frame)
  ifl <- lapply(object$encoders[["interactions"]], function(enc) enc$frame)
  tags <- unique(c(names(mfl), names(ifl)))
  res <- list()
  for (tag in tags) {
    if (identical(mfl[[tag]], ifl[[tag]])) {
      res[[tag]] <- mfl[[tag]]
    } else {
      res[[paste0("|", tag)]] <- mfl[[tag]]
      res[[paste0(":", tag)]] <- ifl[[tag]]
    }
  }
  res
}


#' @rdname mid.extract
#' @export mid.ur
#'
mid.ur <- function(object, ...) {
  object$uninterpreted.rate
}


#' @rdname mid.extract
#' @param main.effect logical. If \code{FALSE}, the main effect terms are excluded.
#' @param interaction logical. If \code{FALSE}, the interaction terms are excluded.
#' @param require a character vector of variable names. The terms that are not related to any of the specified names are excluded.
#' @param remove a character vector of variable names. The terms that are related to at least one of the specified names are excluded.
#' @export mid.terms
#'
mid.terms <- function(
    object, main.effect = TRUE, interaction = TRUE,
    require = NULL, remove = NULL, ...) {
  dots <- list(...)
  if (missing(main.effect) && !is.null(dots$me))
    main.effect <- dots$me
  if (missing(interaction) && !is.null(dots$ie))
    interaction <- dots$ie
  terms <- ifnot.null(object$terms, attr(object, "terms"))
  if (!main.effect)
    terms <- terms[grepl(":", terms)]
  if (!interaction)
    terms <- terms[!grepl(":", terms)]
  if (!is.null(require)) {
    tlist <- strsplit(terms, ":")
    terms <- terms[sapply(tlist, function(x) any(require %in% x))]
  }
  if (!is.null(remove)) {
    tlist <- strsplit(terms, ":")
    terms <- terms[!sapply(tlist, function(x) any(remove %in% x))]
  }
  terms
}

#' @rdname mid.extract
#' @param x a "mid" or "mid.importance" object.
#' @exportS3Method stats::terms
#'
terms.mid <- function(x, ...) {
  mid.terms(object = x, ...)
}


#' @rdname mid.extract
#' @exportS3Method stats::terms
#'
terms.mid.importance <- function(x, ...) {
  mid.terms(object = x, ...)
}
