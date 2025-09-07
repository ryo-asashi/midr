#' Evaluate Single MID Component Functions
#'
#' @description
#' \code{mid.effect()} calculates the contribution of a single component function of a fitted MID model.
#' It serves as a low-level helper function for making predictions or for direct analysis of a term's effect.
#'
#' @details
#' \code{mid.effect()} is a low-level function designed to calculate the contribution of a single component function.
#' Unlike \code{predict.mid()}, which is designed to return total model predictions, \code{mid.effect()} is more flexible.
#' It accepts vectors, as well as data frames, as input for \code{x} and \code{y}, making it particularly useful for visualizing a component's effect in combination with other functions, such as \code{graphics::curve()}.
#'
#' For a main effect, the function evaluates the component function \eqn{f_j(x_j)} for a vector of values \eqn{x_j}.
#' For an interaction, it evaluates \eqn{f_{jk}(x_j, x_k)} using vectors \eqn{x_j} and \eqn{x_k}.
#'
#' @param object a "mid" object.
#' @param term a character string specifying the component function (term) to evaluate.
#' @param x a vector of values for the first variable in the term. If a matrix or data frame is provided, values of the related variables are extracted from it.
#' @param y a vector of values for the second variable in an interaction term.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, data = airquality, lambda = 1)
#'
#' # Visualize the main effect of "Wind"
#' curve(mid.effect(mid, term = "Wind", x), from = 0, to = 25)
#'
#' # Visualize the interaction of "Wind" and "Temp"
#' curve(mid.f(mid, "Wind:Temp", x, 50), 0, 25)
#' curve(mid.f(mid, "Wind:Temp", x, 60), 0, 25, add = TRUE, lty = 2)
#' curve(mid.f(mid, "Wind:Temp", x, 70), 0, 25, add = TRUE, lty = 3)
#' @returns
#' \code{mid.effect()} returns a numeric vector of the calculated term contributions, with the same length as \code{x} and \code{y}.
#'
#' @seealso \code{\link{interpret}}, \code{\link{predict.mid}}
#'
#' @export mid.effect
#'
mid.effect <- function(object, term, x, y = NULL) {
  tags <- term.split(term)
  ie <- length(tags) == 2L
  if (is.matrix(x) || is.data.frame(x)) {
    if (ie)
      y <- x[, tags[2L]]
    x <- x[, tags[1L]]
  }
  n <- length(x)
  .term <- term.check(term, object$terms, stop = FALSE)
  if (is.na(.term))
    return(numeric(n))
  if (!ie) {
    enc <- object$encoders[["main.effects"]][[.term]]
    X <- enc$encode(x)
    mid <- object$main.effects[[.term]]$mid
  } else {
    ny <- length(y)
    if (ny != n) {
      if (n == 1L) {
        x <- rep.int(x, ny)
        n <- ny
      } else if (ny == 1L) {
        y <- rep.int(y, n)
      } else {
        stop("x and y must have the same length")
      }
    }
    encs <- list(object$encoders[["interactions"]][[tags[1L]]],
                 object$encoders[["interactions"]][[tags[2L]]])
    mats <- list(encs[[1L]]$encode(x), encs[[2L]]$encode(y))
    r <- if (term == .term) 0L else 1L
    uni <- as.matrix(expand.grid(1L:encs[[1L + r]]$n, 1L:encs[[2L - r]]$n))
    X <- matrix(0, nrow = n, ncol = nrow(uni))
    for (j in seq_len(nrow(uni))) {
      X[, j] <- as.numeric(mats[[1L + r]][, uni[j, 1L]] *
                             mats[[2L - r]][, uni[j, 2L]])
    }
    mid <- object$interactions[[.term]]$mid
  }
  mid[is.na(mid)] <- 0
  as.numeric(X %*% mid)
}

#' @rdname mid.effect
#'
#' @description
#' \code{mid.f()} is a convenient shorthand for \code{mid.effect()}.
#'
#' @export mid.f
#'
mid.f <- mid.effect
