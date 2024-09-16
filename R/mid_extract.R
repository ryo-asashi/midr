#' Extract Information from a mid Object
#'
#' Returns information on the mid object including a summary for encoders, uninterpreted rates, or other special components.
#'
#' @param x a mid object.
#' @param component a literal character string that specifies the name of the component to extract. Any component of the mid object can be extracted. In addition, some special values such as "importance", "UR", "encoders", and names of any functional decomposition terms like "x1:x2" can be used.
#' @param ... optional arguments to be passed to the specific functions used to extract information.
#'
#' @examples
#' data(trees, package = "datasets")
#' mid <- interpret(Volume ~ ., trees, k = 10)
#' mid.extract(mid, encoders)
#' mid.extract(mid, intercept)
#' mid.extract(mid, uninterpreted.rate)
#' mid.extract(mid, Girth)
#' @export mid.extract
#'
mid.extract <- function(x, component, ...) {
  component <- as.character(substitute(component))
  if (component %in% x$terms) {
    if (grepl(":", component))
      return(x$interactions[[component]])
    return(x$main.effects[[component]])
  }
  rt <- switch(component,
    encoding.info = mid.encoding.info(x, ...),
    frames = mid.frames(x, ...),
    importance = mid.importance(x, ...),
    plots = mid.plots(x, ...),
    terms = mid.terms(x, ...)
  )
  if (is.null(rt))
    rt <- x[[component]]
  rt
}

#' @rdname mid.extract
#' @export mid.encoding.info
#'
mid.encoding.info <- function(x, ...) {
  fun <- function(enc) paste0(enc$type, "(", enc$n, ")")
  mencs <- sapply(x$me.encoders, fun)
  iencs <- sapply(x$ie.encoders, fun)
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
mid.frames <- function(x, ...) {
  mfl <- lapply(x$me.encoders, function(enc) enc$frame)
  ifl <- lapply(x$ie.encoders, function(enc) enc$frame)
  tags <- unique(c(names(mfl), names(ifl)))
  frs <- list()
  for (tag in tags) {
    frs[[tag]] <-
      if (is.null(ifl[[tag]]) || identical(mfl[[tag]], ifl[[tag]])) {
        mfl[[tag]]
      } else if (is.null(mfl[[tag]])) {
        ifl[[tag]]
      } else {
        list(mfl[[tag]], ifl[[tag]])
    }
  }
  frs
}


#' @rdname mid.extract
#'
#' @param main.effect logical. If FALSE, all main effects are excluded.
#' @param interaction logical. If FALSE, all interactions are excluded.
#' @param require a character vector of feature names. Only terms related to at least one of the specified features will be returned.
#' @param remove a character vector of feature names. All terms related to at least one of the specified features will not be returned.
#'
#' @export mid.terms
#'
mid.terms <- function(
    x, main.effect = TRUE, interaction = TRUE,
    require = NULL, remove = NULL, ...) {
  dots <- list(...)
  if (missing(main.effect) && !is.null(dots$me))
    main.effect <- dots$me
  if (missing(interaction) && !is.null(dots$ie))
    interaction <- dots$ie
  terms <- x$terms
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
#' @exportS3Method stats::terms
#'
terms.mid <- function(x, ...) {
  mid.terms(x, ...)
}
