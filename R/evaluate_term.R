#' Evaluate Single Component Functions of Additive Models
#'
#' @description
#' \code{evaluate_term()} calculates the contribution of a single component function of a fitted additive model.
#' It serves as a low-level helper function for making predictions or for direct analysis of a term effect.
#'
#' @details
#' \code{evaluate_term()} is a generic function designed to calculate the contribution of a single component function.
#' Unlike \code{\link[stats]{predict}()}, it accepts vectors, as well as matrices or data frames, as input for \code{...}.
#' This makes it useful for visualizing a component's effect in combination with standard plotting functions, such as \code{graphics::curve()}.
#'
#' @param object an additive model object (e.g., "lm", "glm") including "mid" and "mids".
#' @param term a character string specifying the component function (term) to evaluate.
#' @param ... vectors, a matrix, or a data frame containing values for the variables in the term. If not named, the values are assigned for variables according to its order in the \code{term} argument.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#'
#' # Visualize the main effect of "Wind"
#' curve(evaluate_term(mid, term = "Wind", x), from = 0, to = 25)
#'
#' # Visualize the interaction of "Wind" and "Temp"
#' curve(evaluate_term(mid, "Wind:Temp", x, 50), 0, 25)
#' curve(evaluate_term(mid, "Wind:Temp", x, 60), add = TRUE, lty = 2)
#' curve(evaluate_term(mid, "Wind:Temp", x, 70), add = TRUE, lty = 3)
#' @returns
#' \code{evaluate_term()} returns a numeric vector of the calculated term contributions, with the same length as \code{x}.
#'
#' For a collection of models ("mids"), \code{evaluate_term()} returns a numeric matrix where each column corresponds to a model.
#'
#' @export evaluate_term
#'
evaluate_term <- function(object, ...)
UseMethod("evaluate_term")

#' @rdname evaluate_term
#' @exportS3Method midr::evaluate_term
#'
evaluate_term.mid <- function(object, term, ...) {
  term <- as_terms(term)
  if (!is_single_term(term))
    stop("'term' must be a single term such as 'X1' or 'X1:X2'")
  data <- data.frame(..., check.names = FALSE, fix.empty.names = FALSE)
  vars <- get_variables(term)
  lacked <- vars[!(vars %in% names(data))]
  if (length(lacked) > 0L) {
    unnamed <- which(names(data) == "")
    n <- max(length(unnamed), length(lacked))
    names(data)[unnamed[seq_len(n)]] <- lacked[seq_len(n)]
  }
  out <- stats::predict(object, newdata = data, terms = term)
  if (inherits(object, "midrib")) {
    if (!is.matrix(out)) out <- as.matrix(out)
    colnames(out) <- base::labels(object)
  } else {
    out <- as.numeric(out)
  }
  out
}

#' @rdname evaluate_term
#' @exportS3Method midr::evaluate_term
#'
evaluate_term.mids <- function(object, term, ...) {
  if (inherits(object, "midrib")) {
    evaluate_term.mid(object, term = term, ...)
  } else {
    res <- sapply(as.list(object), evaluate_term, term = term, ...)
    if (!is.matrix(res)) {
      res <- matrix(res, nrow = NROW(x), ncol = length(object))
    }
    colnames(res) <- base::labels(object)
    res
  }
}

#' @rdname evaluate_term
#' @param data an optional data frame containing the original training data. Required for additive models if the data cannot be automatically extracted from the model object.
#' @exportS3Method midr::evaluate_term
#'
evaluate_term.default <- function(object, term, x, y = NULL, data = NULL, ...) {
  term <- as.terms(term)
  if (!is.single.term(term))
    stop("'term' must be a single term")
  vars <- get.variables(term)
  tlab <- get.labels(term)
  ie <- length(vars) > 1L
  if (ie && grepl(":", tlab)) {
    tvars <- strsplit(tlab, ":")[[1L]]
    if (length(tvars) == 2L && all(tvars %in% vars)) {
      vars <- tvars
    }
  }
  if (is.matrix(x)) {
    if (ie) y <- x[, vars[2L]]
    x <- x[, vars[1L]]
  } else if (is.data.frame(x)) {
    if (ie) y <- x[[vars[2L]]]
    x <- x[[vars[1L]]]
  }
  if (ie && is.null(y))
    stop("'y' is missing and can't be extracted from 'x'")
  nx <- length(x)
  ny <- length(y)
  n <- if (ie) max(nx, ny) else nx
  if (ie && nx != ny) {
    if (nx == 1L) {
      x <- rep.int(x, ny)
    } else if (ny == 1L) {
      y <- rep.int(y, nx)
    } else {
      stop("'x' and 'y' must have the same length")
    }
  }
  if (is.null(data)) {
    data <- tryCatch(model.data(object), error = function(e) NULL)
    if (is.null(data)) stop("'data' must be supplied for this model class")
  }
  newdata <- data[rep(1L, n), , drop = FALSE]
  newdata[[vars[1L]]] <- x
  if (ie) newdata[[vars[2L]]] <- y
  preds <- tryCatch(
    stats::predict(object, newdata = newdata, type = "terms", na.action = "na.pass"),
    error = function(e) stop("failed to compute term effects using stats::predict")
  )
  mlab <- match.labels(tlab, colnames(preds))
  if (is.na(mlab)) {
    return(numeric(n))
  }
  as.numeric(preds[, mlab])
}


#' @rdname evaluate_term
#'
#' @description
#' \code{term.f()} is a convenient shorthand for \code{evaluate_term()}.
#'
#' @export term.f
#'
term.f <- evaluate_term

term.effect <- function(...) evaluate_term(...)
