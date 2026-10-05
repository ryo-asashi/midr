#' Evaluate Single Component Functions of Additive Models
#'
#' @description
#' \code{term.effect()} calculates the contribution of a single component function of a fitted additive models.
#' It serves as a low-level helper function for making predictions or for direct analysis of a term effect.
#'
#' @details
#' \code{term.effect()} is a low-level S3 generic function designed to calculate the contribution of a single component function.
#' Unlike \code{predict.mid()}, which is designed to return total model predictions, \code{term.effect()} is more flexible.
#' It accepts vectors, as well as matrices or data frames, as input for \code{x} and \code{y}. If \code{x} is a data frame, the necessary columns are automatically extracted.
#' This makes it particularly useful for visualizing a component's effect in combination with standard plotting functions, such as \code{graphics::curve()}.
#'
#' For a main effect, the function evaluates the component function \eqn{f_j(x_j)} for a vector of values \eqn{x_j}.
#' For an interaction, it evaluates \eqn{f_{jk}(x_j, x_k)} using vectors \eqn{x_j} and \eqn{x_k}.
#' The assignment of \code{x} and \code{y} strictly follows the order of variables specified in the \code{term} argument.
#' For example, if \code{term = "Temp:Wind"}, \code{x} is assigned to \code{Temp} and \code{y} is assigned to \code{Wind}.
#'
#' @param object a "mid" object, a collection of models ("mids"), or other compatible additive model objects (e.g., "lm", "glm").
#' @param term a character string specifying the component function (term) to evaluate.
#' @param x a vector of values for the first variable in the term. If a matrix or data frame is provided, values of the related variables are automatically extracted from it.
#' @param y a vector of values for the second variable in an interaction term. Ignored if \code{x} is a data frame containing both variables.
#' @param ... optional arguments to be passed to methods.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#'
#' # Visualize the main effect of "Wind"
#' curve(term.effect(mid, term = "Wind", x), from = 0, to = 25)
#'
#' # Visualize the interaction of "Wind" and "Temp"
#' curve(term.f(mid, "Wind:Temp", x, 50), 0, 25)
#' curve(term.f(mid, "Wind:Temp", x, 60), 0, 25, add = TRUE, lty = 2)
#' curve(term.f(mid, "Wind:Temp", x, 70), 0, 25, add = TRUE, lty = 3)
#' @returns
#' \code{term.effect()} returns a numeric vector of the calculated term contributions, with the same length as \code{x}.
#'
#' For a collection of models ("mids"), \code{term.effect()} returns a numeric matrix where each column corresponds to a model.
#'
#' @export term.effect
#'
term.effect <- function(object, ...)
UseMethod("term.effect")

#' @rdname term.effect
#' @exportS3Method midr::term.effect
#'
term.effect.mid <- function(object, term, x, y = NULL, ...) {
  term <- as.terms(term)
  if (!is.single.term(term))
    stop("'term' must be a single term")
  vars <- get.variables(term)
  tlab <- get.labels(term)
  mlab <- match.labels(tlab, term.labels(object))
  ie <- length(vars) > 1L
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
  if (is.na(mlab)) {
    if (inherits(object, "midrib")) {
      k <- length(object$intercept)
      res <- matrix(0, nrow = n, ncol = k)
      colnames(res) <- base::labels(object)
      return(res)
    }
    return(numeric(n))
  }
  if (ie && nx != ny) {
    if (nx == 1L) {
      x <- rep.int(x, ny)
      nx <- ny
    } else if (ny == 1L) {
      y <- rep.int(y, nx)
      ny <- nx
    } else {
      stop("'x' and 'y' must have the same length")
    }
  }
  if (!ie) {
    bmat <- object$main.effects[[mlab]]$mid
    mmat <- object$encoders$main.effects[[mlab]]$encode(x)
    out <- mmat %*% bmat
  } else {
    if (tlab != mlab) {
      vars <- rev(vars)
      temp <- x; x <- y; y <- temp
    }
    bmat <- object$interactions[[mlab]]$mid
    xmat <- object$encoders$interactions[[vars[1L]]]$encode(x)
    ymat <- object$encoders$interactions[[vars[2L]]]$encode(y)
    mx <- ncol(xmat)
    my <- ncol(ymat)
    if (NCOL(bmat) == 1L) {
      W <- matrix(as.numeric(bmat), nrow = mx, ncol = my)
      W[is.na(W)] <- 0
      out <- rowSums((xmat %*% W) * ymat)
    } else {
      imat <- xmat[, rep(seq_len(mx), times = my), drop = FALSE] *
        ymat[, rep(seq_len(my), each = mx), drop = FALSE]
      out <- imat %*% bmat
    }
  }
  if (inherits(object, "midrib")) {
    if (!is.matrix(out)) out <- as.matrix(out)
    colnames(out) <- base::labels(object)
  } else {
    out <- as.numeric(out)
  }
  out
}

#' @rdname term.effect
#' @exportS3Method midr::term.effect
#'
term.effect.mids <- function(object, term, x, y = NULL, ...) {
  if (inherits(object, "midrib")) {
    term.effect.mid(object, term = term, x = x, y = y, ...)
  } else {
    res <- sapply(as.list(object), term.effect, term = term, x = x, y = y, ...)
    if (!is.matrix(res)) {
      res <- matrix(res, nrow = NROW(x), ncol = length(object))
    }
    colnames(res) <- base::labels(object)
    res
  }
}

#' @rdname term.effect
#' @param data an optional data frame containing the original training data. Required for additive models if the data cannot be automatically extracted from the model object.
#' @exportS3Method midr::term.effect
#'
term.effect.default <- function(object, term, x, y = NULL, data = NULL, ...) {
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
    data <- tryCatch(stats::model.frame(object), error = function(e) NULL)
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


#' @rdname term.effect
#'
#' @description
#' \code{term.f()} is a convenient shorthand for \code{term.effect()}.
#'
#' @export term.f
#'
term.f <- term.effect
