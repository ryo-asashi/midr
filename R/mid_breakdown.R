#' Calculate Term Contribution
#'
#' @description
#' \code{term.contribution()} calculates the contribution of each component function of a fitted model to a single prediction.
#' It breaks down the total prediction into the effects of the intercept, main effects, and interactions (if any).
#'
#' @details
#' This function provides local interpretability for a specific observation by decomposing its prediction into the individual contributions of the model's components.
#' For a target observation \eqn{\mathbf{x}}, the total prediction is represented as the sum of all estimated tlabs:
#'
#' \deqn{g(\mathbf{x}) = g_\emptyset + \sum_{j} g_j(x_j) + \sum_{j<k} g_{jk}(x_j, x_k)}
#'
#' The output data frame itemizes the numerical value of each main effect \eqn{g_j(x_j)} and interaction effect \eqn{g_{jk}(x_j, x_k)}, along with the intercept \eqn{g_\emptyset}.
#' This decomposition makes the model's decision for a single instance fully transparent and easy to attribute to specific features or their combinations.
#'
#' @param object a fitted model object (e.g., "mid", "lm", "glm", "gam").
#' @param data a data frame containing one or more observations for which to calculate the breakdown. If not provided, data is automatically extracted based on the function call.
#' @param row an optional numeric value or character string specifying the row of \code{data} to be used for the breakdown. If \code{NULL}, and the \code{data} contains two or more observations, only the first observation is used.
#' @param sort logical. If \code{TRUE}, the output data frame is sorted by the absolute contribution of each effect in descending order.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#'
#' # Calculate the contribution for the first observation in the data
#' brk <- term.contribution(mid, data = airquality, row = 1)
#' print(brk)
#' @returns
#' \code{term.contribution()} returns an object of class "termcon" (and "midbrk" for compatibility). This is a list with the following components:
#' \item{contribution}{a data frame containing the term-by-term contribution of the prediction.}
#' \item{data}{the data frame containing the predictor variable values used for the prediction.}
#' \item{intercept}{the intercept of the model.}
#' \item{prediction}{the predicted value from the model (on the response scale).}
#' \item{linear.predictor}{the predicted value on the link scale (only present if a link function is used).}
#'
#' @seealso \code{\link{interpret}}, \code{\link{plot.midbrk}}, \code{\link{ggmid.midbrk}}
#'
#' @export term.contribution
#'
term.contribution <- function(object, ...)
UseMethod("term.contribution")

#' @rdname term.contribution
#'
#' @export mid.breakdown
#'
mid.breakdown <- function(object, ...) {
  warning("'mid.breakdown' is deprecated: please use 'term.contribution' instead")
  term.contribution(object, ...)
}


#' @rdname term.contribution
#'
#' @exportS3Method midr::term.contribution
#'
term.contribution.default <- function(
    object, data = NULL, row = NULL, sort = TRUE, ...
) {
  if (is.null(data)) {
    data <- tryCatch(
      model.data(object, env = parent.frame()),
      error = function(e) NULL
    )
    if (is.null(data))
      stop("'data' could not be automatically extracted from the model object: please provide it directly")
  }
  if (!is.data.frame(data))
    data <- as.data.frame(data)
  if (!is.null(row))
    data <- data[row, , drop = FALSE]
  if (nrow(data) != 1L) {
    message("'data' contains multiple observations: the first observation is used")
    data <- data[1L, , drop = FALSE]
  }
  if (nrow(data) == 0L)
    stop("'data' contains no observations to be used")
  preds <- stats::predict(
    object, newdata = data, type = "terms", na.action = stats::na.pass
  )
  brk <- preds[1L, ]
  tlabs <- names(if (sort) base::sort(abs(brk), decreasing = TRUE) else preds)
  df <- data.frame(
    term = factor(tlabs, levels = rev(tlabs)),
    contribution = brk[tlabs],
    order = as.factor(lengths(lapply(tlabs, get.variables)))
  )
  rownames(df) <- NULL
  ints <- attr(preds, "constant") %||% get.intercept(object) %||% 0
  out <- list()
  out$contribution <- df
  out$data <- tryCatch(model.reframe(object, data), error = function(e) data)
  out$intercept <- ints
  linp <- ints + sum(preds)
  link <- object$family %||% object$link %||% attr(object, "link")
  if (!is.null(link$linkinv)) {
    out$linear.predictor <- linp
    out$prediction <- link$linkinv(linp)
  } else {
    out$prediction <- linp
  }
  attr(out, "term.labels") <- as.character(df$term)
  class(out) <- c("termcon", "midbrk")
  out
}

#' @rdname term.contribution
#' @export
term.contribution.mids <- function(
    object, data = NULL, row = NULL, sort = TRUE, ...
) {
  out <- suppressMessages(lapply(
    X = object, FUN = term.contribution, data = data, row = row, sort = sort, ...
  ))
  class(out) <- c("termcons", "midbrks", "midlist")
  return(out)
}
