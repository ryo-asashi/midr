#' Predict Method for Fitted MID Models
#'
#' @description
#' \code{predict()} methods for obtaining predictions from a fitted MID model ("mid") or a collection of MID models ("mids").
#' It can be used to predict on new data or to retrieve the fitted values from the original data.
#'
#' @details
#' The \code{type} argument allows you to specify the scale of the prediction.
#' By default (\code{type = "response"}), the function returns predictions on the original scale of the response variable.
#' Alternatively, you can obtain predictions on the scale of the linear predictor by setting \code{type = "link"}.
#' For a detailed breakdown, setting \code{type = "terms"} returns a matrix where each column represents the contribution of a specific model term on the linear predictor scale.
#'
#' The \code{terms} argument allows for predictions based on a subset of the model's component functions, excluding others.
#'
#' @param object a fitted model object of class "mid", or a collection object ("mids") to be used for prediction.
#' @param newdata a data frame of the new observations. If not provided, data is extracted based on the function call.
#' @param type the type of prediction required. One of "response", "link", or "terms".
#' @param terms a character vector of term labels or an object coercible to a \code{\link[stats]{terms.object}}, specifying a subset of component functions to use for predictions.
#' @param na.action a function or character string specifying what should happen when the data contain \code{NA} values.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' data(airquality, package = "datasets")
#' test <- 1:10
#' mid <- interpret(Ozone ~ .^2, airquality[-test, ], lambda = 1, link = "log")
#'
#' # Predict on new data
#' predict(mid, airquality[test, ])
#'
#' # Get predictions on the link scale
#' predict(mid, airquality[test, ], type = "link")
#'
#' # Get the contributions of specific terms
#' predict(mid, airquality[test, ], terms = c("Temp", "Wind"), type = "terms")
#' @returns
#' For a single "mid" object, \code{predict.mid()} returns a numeric vector if \code{type} is "response" or "link", or a numeric matrix if \code{type = "terms"}.
#'
#' For a collection ("mids"), \code{predict.mids()} returns a numeric matrix where each column corresponds to a model if \code{type} is "response" or "link", or a list of numeric matrices if \code{type = "terms"}.
#'
#' @seealso \code{\link{interpret}}, \code{\link{get_yhat}}
#'
#' @exportS3Method stats::predict
#'
predict.mid <- function(
    object, newdata = NULL, type = c("response", "link", "terms"),
    terms = NULL, na.action = stats::na.pass, ...) {
  type <- match.arg(type)
  newdata <- newdata %||% model_data(object, env = parent.frame())
  if (!is.null(terms)) {
    terms <- as_terms(terms, data = newdata)
    mlabs <- unique(match_labels(terms, object, nomatch = 0L))
  } else {
    terms <- as_terms(object)
    mlabs <- get_labels(terms)
  }
  if (length(mlabs) == 0L)
    stop("none of the specified 'terms' are in 'object'")
  if (is.null(newdata))
    stop("'newdata' must be provided")
  vars <- get_variables(terms)
  if (!all(vars %in% colnames(newdata)))
    newdata <- model_reframe(terms, newdata)
  attr(newdata, "na.action") <- NULL
  newdata <- do.call(na.action, list(newdata))
  naa <- stats::na.action(newdata)
  n <- nrow(newdata)
  m <- length(mlabs)
  ints <- get_intercept(object) * has_intercept(terms %||% "1")
  if (type == "terms") {
    preds <- matrix(0, nrow = n, ncol = m, dimnames = list(NULL, mlabs))
  } else {
    k <- length(ints)
    preds <- matrix(ints, nrow = n, ncol = k, byrow = TRUE)
  }
  vlis <- lapply(mlabs, get_variables)
  vlen <- lengths(vlis)
  imat <- list()
  for (tag in unique(unlist(vlis[vlen == 2L]))) {
    imat[[tag]] <- encode(object, tag, 2L, newdata)
  }
  for (i in seq_len(m)) {
    tags <- vlis[[i]]
    if (length(tags) == 1L) {
      bmat <- object$main.effects[[tags]]$.effect
      mmat <- encode(object, tags, 1L, newdata)
      term_preds <- mmat %*% bmat
      mmat <- NULL
    } else if (length(tags) == 2L) {
      bmat <- object$interactions[[mlabs[i]]]$.effect
      mat1 <- imat[[tags[1L]]]
      mat2 <- imat[[tags[2L]]]
      m1 <- ncol(mat1)
      m2 <- ncol(mat2)
      if (NCOL(bmat) == 1L) {
        W <- matrix(as.numeric(bmat), nrow = m1, ncol = m2)
        W[is.na(W)] <- 0
        term_preds <- rowSums((mat1 %*% W) * mat2)
      } else {
        mat3 <- mat1[, rep(seq_len(m1), times = m2), drop = FALSE] *
          mat2[, rep(seq_len(m2), each = m1), drop = FALSE]
        term_preds <- mat3 %*% bmat
      }
    }
    if (type == "terms") {
      preds[, i] <- term_preds
    } else {
      preds <- preds + term_preds
    }
  }
  if (type == "response" && !is.null(object$link)) {
    preds <- object$link$linkinv(preds)
  } else if (type == "terms") {
    attr(preds, "constant") <- ints
  }
  if (inherits(naa, "exclude")) {
    preds <- stats::napredict(naa, preds)
  }
  if (type != "terms" && ncol(preds) == 1L)
    preds <- as.vector(preds)
  return(structure(preds, na.action = naa))
}


#' @rdname predict.mid
#' @exportS3Method stats::predict
#'
predict.mids <- function(
    object, newdata = NULL, type = c("response", "link", "terms"),
    terms = NULL, na.action = stats::na.pass, ...
) {
  type <- match.arg(type)
  if (type == "terms") {
    lapply(
      X = object, FUN = predict.mid, newdata = newdata, type = type,
      terms = terms, na.action = na.action, ...
    )
  } else if (inherits(object, "midrib")) {
    predict.mid(
      object = object, newdata = newdata, type = type,
      terms = terms, na.action = na.action, ...
    )
  } else {
    sapply(
      X = object, FUN = predict.mid, newdata = newdata, type = type,
      terms = terms, na.action = na.action, ...
    )
  }
}
