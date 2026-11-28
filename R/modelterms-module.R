# Generated from modelterms package -- do not edit by hand
# Source: https://github.com/ryo-asashi/modelterms

# R/modelterms.R

`%||%` <- function(x, y) if (is.null(x)) y else x


#' Coerce, Inspect, Filter, and Match Model Terms and Labels
#'
#' @description
#' A standalone, zero-dependency suite of functions to seamlessly manipulate
#' \code{\link[stats]{terms}} objects, formulas, fitted models, and character
#' vectors of term labels.
#'
#' @details
#' \subsection{Three Levels of Model Components}{
#' For a model formula such as \code{y ~ x1 + log(x2) + x1:log(x2) + I(x1 * x2)},
#' this package distinguishes three structural levels:
#' \enumerate{
#'   \item \strong{Term Labels} (\code{get_labels(x)}):
#'     The individual additive components of the model:
#'     \code{c("x1", "log(x2)", "x1:log(x2)", "I(x1 * x2)")}.
#'   \item \strong{Model Variables} (\code{get_variables(x, raw = FALSE)}):
#'     The distinct predictor expressions forming the terms (rows of the \code{"factors"} matrix):
#'     \code{c("x1", "log(x2)", "I(x1 * x2)")}.
#'   \item \strong{Raw Variables} (\code{get_variables(x, raw = TRUE)}):
#'     The underlying column names required in \code{data}:
#'     \code{c("x1", "x2")}.
#' }
#' }
#'
#' \subsection{Character Input Rules in \code{as_terms()}}{
#' When \code{x} is a character vector, \code{as_terms()} distinguishes between
#' a vector of strict term labels and a length-1 formula shorthand:
#' \itemize{
#'   \item \strong{Strict Term Labels (\code{character(n)})}: If every element (excluding
#'     intercept tokens like \code{"1"} or \code{"-1"}) represents strictly a single valid
#'     term (e.g., \code{c("b:a", "a", "I(x^2)")}), input order and exact interaction notation
#'     (\code{"b:a"}) are preserved by default (\code{keep.order = TRUE}).
#'   \item \strong{Length-1 Formula Shorthand (\code{character(1)})}: If \code{x} is a single
#'     string containing formula operators or \code{.} (e.g., \code{".^2"}, \code{"."},
#'     or \code{"a * b + c + 1"}), it is expanded as a right-hand side formula expression
#'     (using \code{data} if needed) and sorted by interaction order by default
#'     (\code{keep.order = FALSE}).
#'   \item \strong{Invalid Mixed Vectors (\code{length(x) >= 2})}: If \code{x} has length 2 or
#'     more and contains any non-single-term expression (such as \code{c("a * b", "c")} or
#'     \code{c(".", "a")}), an informative error is thrown.
#' }
#' }
#'
#' \subsection{Subsetting and Filtering with \code{filter_terms()}}{
#' \code{filter_terms()} serves as a declarative, crash-safe replacement for
#' \code{\link[stats]{drop.terms}}, \code{\link[stats]{delete.response}}, and \code{[.terms}:
#' \itemize{
#'   \item \strong{Direct Subsetting}: Pass character labels or integer indices to \code{keep}
#'     (the second argument) or \code{drop} to extract or exclude specific terms (order-invariantly)
#'     while preserving the intercept and response.
#'   \item \strong{Structural Filtering}: Use \code{order}, \code{require}, and \code{remove}
#'     to filter terms by interaction order or constituent variables.
#'   \item \strong{Response & Intercept Control}: In both \code{as_terms()} and
#'     \code{filter_terms()}, set \code{response = FALSE} to remove the response variable
#'     (like \code{stats::delete.response(x)}), pass a new variable name to replace it, or set
#'     \code{intercept = TRUE} / \code{FALSE} without crashing even when all predictor terms are dropped.
#' }
#' }
#'
#' @param x,y an object representing model terms: a \code{\link[stats]{terms}} object,
#'   a \code{\link[stats]{formula}}, a fitted model object, or a character vector.
#'   In \code{match_labels()}, \code{y} is the target object whose term labels are matched against.
#' @param response optional response specification for \code{as_terms()} and \code{filter_terms()}.
#'   If \code{NULL} (default) or \code{TRUE}, retains the existing response of \code{x} (or no
#'   response if \code{x} is a character vector). If \code{FALSE}, removes the response variable.
#'   If a character string or language object, sets or replaces the response variable.
#' @param intercept optional logical for \code{as_terms()} and \code{filter_terms()}.
#'   If \code{NULL} (default), retains the existing intercept status of \code{x} (or defaults to
#'   no intercept for character vectors unless explicit tokens like \code{"1"} or \code{"+1"} are
#'   present in \code{x}). If \code{TRUE} or \code{FALSE}, explicitly forces or removes the intercept.
#' @param data a \code{data.frame} used to resolve \code{.} in formulas or shorthand strings.
#' @param keep.order logical. Should terms be kept in their input order rather than sorted by
#'   interaction order? Defaults to \code{TRUE} for strict term label vectors and \code{FALSE}
#'   for formulas or length-1 formula shorthands.
#' @param env an environment to attach to the resulting \code{terms} object when coercing
#'   from a character vector. Defaults to \code{parent.frame()}.
#' @param keep,drop a character vector of term labels (matched order-invariantly via
#'   \code{match_labels()}) or an integer vector of term indices to explicitly retain
#'   (\code{keep}) or exclude (\code{drop}) in \code{filter_terms()}.
#' @param order an integer vector specifying interaction orders to retain in \code{filter_terms()}
#'   (e.g., \code{1L} for main effects, \code{2L} for two-way interactions).
#' @param require a character vector of variable names. Only terms containing at least one
#'   of these variables are retained.
#' @param remove a character vector of variable names. Any terms containing these variables
#'   are excluded.
#' @param raw logical. If \code{FALSE} (default in \code{get_variables()} and
#'   \code{get_response()}), returns model expressions (e.g., \code{"log(x2)"}).
#'   If \code{TRUE} (default in \code{filter_terms()}), extracts or filters by underlying
#'   raw column names (e.g., \code{"x2"}).
#' @param by.term logical. If \code{FALSE} (default), \code{get_variables()} returns a flat
#'   character vector across the whole model. If \code{TRUE}, returns a named list mapping
#'   each term label to its constituent variables.
#' @param nomatch character string specifying how to handle unmatched labels in
#'   \code{match_labels()}: \code{"omit"} (default; drops unmatched elements),
#'   \code{"pass"} (returns \code{NA}), or \code{"fail"} (throws an error).
#' @param use.names logical. If \code{TRUE} (default), \code{match_labels()} names the output
#'   vector with the input labels from \code{x}, and \code{get_order()} names the integer
#'   vector with the model's term labels.
#' @param ... additional arguments passed to \code{\link[stats]{terms}} or \code{as_terms()}.
#'
#' @returns
#' \itemize{
#'   \item \code{as_terms()}, \code{filter_terms()}: a \code{\link[stats]{terms}} object.
#'   \item \code{get_labels()}, \code{match_labels()}: a character vector (named by input
#'     labels in \code{match_labels()} when \code{use.names = TRUE}).
#'   \item \code{get_order()}: an integer vector of term orders (named when \code{use.names = TRUE}).
#'   \item \code{get_variables()}: a character vector (when \code{by.term = FALSE}) or
#'     a named list of character vectors (when \code{by.term = TRUE}).
#'   \item \code{get_response()}: a character string (when \code{raw = FALSE}), a character
#'     vector of variable names (when \code{raw = TRUE}), or \code{NULL} if no response exists.
#'   \item \code{has_response()}, \code{has_intercept()}: a scalar logical (\code{TRUE} or \code{FALSE}).
#' }
#'
#' @examples
#' # Coercion with as_terms()
#' t1 <- as_terms(c("b:a", "a", "1"))
#' get_labels(t1)
#' has_intercept(t1)
#'
#' t2 <- as_terms(".^2", response = "Ozone", data = airquality[, 1:4])
#' get_labels(t2)
#'
#' # Extracting labels, variables, and response at three levels
#' fml <- log(y) ~ x1 + log(x2) + x1:log(x2) + I(x1 * x2)
#' get_response(fml, raw = FALSE)
#' get_response(fml, raw = TRUE)
#' get_labels(fml)
#' get_variables(fml, raw = FALSE)
#' get_variables(fml, raw = TRUE)
#' get_variables(fml, raw = TRUE, by.term = TRUE)
#'
#' # Subsetting, filtering, and modifying response/intercept
#' filter_terms(fml, "log(x2):x1")
#' get_labels(filter_terms(fml, order = 1))
#' get_labels(filter_terms(fml, require = "x2", remove = "x1", raw = TRUE))
#' filter_terms(fml, drop = "log(x2):x1", response = FALSE)
#' filter_terms(fml, order = 1, response = "yhat", intercept = FALSE)
#'
#' # Order-invariant label matching
#' match_labels(c("x1", "log(x2):x1", "I(x2 * x1)"), fml, nomatch = "pass")
#' @noRd
NULL


is_strict_label <- function(str) {
  if (is.na(str) || !nzchar(trimws(str))) return(FALSE)
  expr <- tryCatch(str2lang(str), error = function(e) NULL)
  if (is.null(expr) || "." %in% all.vars(expr)) return(FALSE)
  fml <- tryCatch(
    stats::reformulate(c("-1", str), intercept = TRUE),
    error = function(e) NULL
  )
  if (is.null(fml)) return(FALSE)
  obj <- tryCatch(stats::terms(fml), error = function(e) NULL)
  !is.null(obj) &&
    length(attr(obj, "term.labels")) == 1L &&
    attr(obj, "intercept") == 0L
}


format_labels <- function(x, ...) {
  if (!is.character(x)) {
    vlist <- get_variables(x, raw = FALSE, by.term = TRUE, ...)
    if (length(vlist) == 0L) return(character(0L))
    return(unname(vapply(vlist, function(v) {
      paste(sort.int(v), collapse = ":")
    }, character(1L))))
  }
  if (length(x) == 0L) return(character(0L))
  unname(vapply(x, function(lab) {
    if (!is_strict_label(lab)) return(NA_character_)
    obj <- tryCatch(as_terms(lab), error = function(e) NULL)
    if (is.null(obj)) return(NA_character_)
    vars <- sort.int(get_variables(obj, raw = FALSE))
    if (length(vars) == 0L) return(NA_character_)
    paste(vars, collapse = ":")
  }, character(1L)))
}


as_terms <- function(
    x, response = NULL, intercept = NULL, data = NULL, keep.order = NULL,
    env = parent.frame(), ...
) {
  # as_terms.default
  if (isTRUE(response)) {
    response <- NULL
  }
  if (!is.character(x)) {
    if (inherits(x, "terms") && is.null(keep.order)) {
      obj <- x
    } else {
      kpo <- keep.order %||% FALSE
      obj <- tryCatch(
        stats::terms(x, keep.order = kpo, data = data, ...),
        error = function(e) NULL
      )
    }
    if (inherits(obj, "terms")) {
      if (is.null(response) && is.null(intercept)) {
        return(obj)
      }
      if (missing(env)) {
        env <- environment(obj) %||% env
      }
      x <- attr(obj, "term.labels") %||% character(0L)
      keep.order <- TRUE
      response <- response %||% (
        if (isTRUE(attr(obj, "response") > 0L)) {
          deparse1(attr(obj, "variables")[[2L]], backtick = TRUE)
        } else NULL
      )
      intercept <- intercept %||% isTRUE(attr(obj, "intercept") > 0L)
    } else {
      x <- attr(x, "term.labels") %||% (if (is.list(x)) x$term.labels else NULL)
    }
    if (!is.character(x)) {
      stop("cannot coerce 'x' to a 'terms' object", call. = FALSE)
    }
  }
  # as_terms.character
  if (anyNA(x)) {
    stop("'x' must not contain NA values", call. = FALSE)
  }
  x <- trimws(x)
  if (any(!nzchar(x))) {
    stop("'x' must not contain empty strings", call. = FALSE)
  }
  is_int_tok <- grepl("^[+-]?\\s*[01]$", x)
  term_part <- x[!is_int_tok]
  all_strict <- all(vapply(term_part, is_strict_label, logical(1L)))
  if (!all_strict && length(x) >= 2L) {
    stop(
      "when 'x' has length >= 2, each element must be a single valid term label",
      call. = FALSE
    )
  }
  rhs <- if (is.null(intercept)) {
    c("-1", x)
  } else {
    c(if (isTRUE(intercept)) "1" else "-1", term_part)
  }
  lhs <- if (isFALSE(response)) NULL else response
  fml <- tryCatch(
    stats::reformulate(
      termlabels = rhs, response = lhs, intercept = TRUE, env = env
    ),
    error = function(e) {
      stop("failed to construct a valid formula from 'x'", call. = FALSE)
    }
  )
  kpo <- keep.order %||% all_strict
  obj <- tryCatch(
    stats::terms(fml, keep.order = kpo, data = data, ...),
    error = function(e) stop(conditionMessage(e), call. = FALSE)
  )
  if (kpo && all_strict && length(term_part) > 0L) {
    keys <- vapply(term_part, function(lbl) {
      ti <- stats::terms(stats::reformulate(c("-1", lbl), intercept = TRUE))
      paste(sort.int(rownames(attr(ti, "factors"))), collapse = "\r")
    }, character(1L))
    orig_unique <- term_part[!duplicated(keys)]
    labs <- attr(obj, "term.labels")
    if (length(labs) == length(orig_unique)) {
      attr(obj, "term.labels") <- orig_unique
      if (!is.null(attr(obj, "factors"))) {
        colnames(attr(obj, "factors")) <- orig_unique
      }
    }
  }
  obj
}


filter_terms <- function(
    x, keep = NULL, drop = NULL, order = NULL, require = NULL, remove = NULL,
    raw = TRUE, response = NULL, intercept = NULL, ...
) {
  obj <- as_terms(x, ...)
  labs <- get_labels(obj)
  ok <- rep(TRUE, length(labs))
  if (length(labs) > 0L) {
    if (!is.null(order)) {
      ok <- ok & (attr(obj, "order") %in% order)
    }
    if (!is.null(require) || !is.null(remove)) {
      vlist <- get_variables(obj, raw = raw, by.term = TRUE)
      if (!is.null(require)) {
        ok <- ok & vapply(vlist, function(v) any(v %in% require), logical(1L))
      }
      if (!is.null(remove)) {
        ok <- ok & !vapply(vlist, function(v) any(v %in% remove), logical(1L))
      }
    }
    if (!is.null(keep)) {
      keep_labs <- if (is.numeric(keep)) {
        labs[keep]
      } else {
        match_labels(keep, labs, nomatch = "omit", ...)
      }
      ok <- ok & (labs %in% keep_labs)
    }
    if (!is.null(drop)) {
      drop_labs <- if (is.numeric(drop)) {
        labs[drop]
      } else {
        match_labels(drop, labs, nomatch = "omit", ...)
      }
      ok <- ok & !(labs %in% drop_labs)
    }
  }
  if (all(ok) && (is.null(response) || isTRUE(response)) && is.null(intercept)) {
    return(obj)
  }
  as_terms(
    x = labs[ok],
    keep.order = TRUE,
    response = (if (isTRUE(response)) NULL else response) %||% (
      if (isTRUE(attr(obj, "response") > 0L)) {
        deparse1(attr(obj, "variables")[[2L]], backtick = TRUE)
      } else NULL
    ),
    intercept = intercept %||% isTRUE(attr(obj, "intercept") > 0L),
    env = environment(obj) %||% parent.frame()
  )
}


get_labels <- function(x, ...) {
  attr(as_terms(x, ...), "term.labels") %||% character(0L)
}


match_labels <- function(
    x, y, nomatch = c("omit", "pass", "fail"), use.names = TRUE, ...
) {
  nomatch <- match.arg(nomatch)
  xlabs <- if (is.character(x)) x else get_labels(x, ...)
  ylabs <- if (is.character(y)) y else get_labels(y, ...)
  xkeys <- format_labels(xlabs)
  ykeys <- format_labels(ylabs)
  idx <- match(xkeys, ykeys, nomatch = NA_integer_, incomparables = NA_character_)
  out <- ylabs[idx]
  if (use.names) {
    names(out) <- xlabs
  }
  if (nomatch != "pass" && anyNA(out)) {
    ng <- is.na(out)
    if (nomatch == "fail") {
      n_ng <- sum(ng)
      stop(
        n_ng, if (n_ng == 1L) " invalid label" else " invalid labels",
        " found: ", paste0(xlabs[ng], collapse = ", "),
        call. = FALSE
      )
    }
    out <- out[!ng]
  }
  out
}


get_order <- function(x, use.names = TRUE, ...) {
  obj <- as_terms(x, ...)
  out <- attr(obj, "order") %||% integer(0L)
  if (use.names) {
    names(out) <- attr(obj, "term.labels")
  }
  out
}


get_variables <- function(x, raw = FALSE, by.term = FALSE, ...) {
  facs <- attr(as_terms(x, ...), "factors")
  if (is.null(facs) || length(facs) == 0L) {
    return(if (by.term) stats::setNames(list(), character(0L)) else character(0L))
  }
  vars <- rownames(facs)
  if (raw) {
    raw_map <- lapply(vars, function(v) all.vars(str2lang(v)))
  }
  if (by.term) {
    return(apply(facs > 0L, MARGIN = 2L, FUN = function(col) {
      if (raw) {
        unique(unlist(raw_map[col], use.names = FALSE))
      } else {
        vars[col]
      }
    }, simplify = FALSE))
  }
  active <- rowSums(facs) > 0L
  if (raw) {
    unique(unlist(raw_map[active], use.names = FALSE))
  } else {
    vars[active]
  }
}


get_response <- function(x, raw = FALSE, ...) {
  obj <- as_terms(x, ...)
  if (!has_response(obj)) return(NULL)
  expr <- attr(obj, "variables")[[2L]]
  if (raw) {
    return(all.vars(expr))
  }
  deparse1(expr, backtick = TRUE)
}


has_response <- function(x, ...) {
  isTRUE(attr(as_terms(x, ...), "response") > 0L)
}


has_intercept <- function(x, ...) {
  isTRUE(attr(as_terms(x, ...), "intercept") > 0L)
}
