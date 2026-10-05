#' @exportS3Method stats::formula
#'
formula.mid <- function(x, ...) {
  fm <- x$call$formula
  if (!is.null(fm)) {
    res <- stats::formula(stats::terms(x))
    environment(res) <- environment(fm)
    res
  } else {
    stats::formula(stats::terms(x))
  }
}

#' @exportS3Method stats::formula
#'
formula.midrib <- function(x, ...) {
  formula.mid(x, ...)
}


#' @exportS3Method stats::model.frame
#'
model.frame.mid <- function(object, ...) {
  model.reframe(object, data = model.data(object))
}

#' @exportS3Method stats::model.frame
#'
model.frame.midrib <- function(object, ...) {
  model.frame.mid(object, ...)
}


#' @exportS3Method stats::nobs
#'
nobs.mid <- function(object, ...) {
  NROW(object$fitted.values)
}

#' @exportS3Method stats::nobs
#'
nobs.midrib <- function(object, ...) {
  nobs.mid(object, ...)
}


#' @exportS3Method stats::variable.names
#'
variable.names.mid <- function(object, ...) {
  get.variables(object)
}

#' @exportS3Method stats::variable.names
#'
variable.names.midrib <- function(object, ...) {
  variable.names.mid(object, ...)
}


#' @exportS3Method stats::coef
#'
coef.mid <- function(object, ...) {
  stop("raw coefficients are not meaningful: ",
       "please see '$main.effects' or '$interactions' instead", call. = FALSE)
}

#' @exportS3Method stats::coef
#'
coef.midrib <- function(object, ...) {
  coef.mid(object, ...)
}
