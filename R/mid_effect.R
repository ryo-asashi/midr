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
#' For "midlist", \code{mid.effect()} returns a matrix of the term contributions.
#'
#' @seealso \code{\link{interpret}}, \code{\link{predict.mid}}
#'
#' @export mid.effect
#'
mid.effect <- function(object, term, x, y = NULL) {
  if (!inherits(object, "mid") && !inherits(object, "midrib"))
    stop("'object' must be 'mid' or 'midrib'")
  checked <- term.check(term, mid.terms(object), stop = FALSE)
  tags <- term.split(term)
  ie <- (length(tags) == 2L)
  if (is.matrix(x) || is.data.frame(x)) {
    if (ie) y <- x[, tags[2L]]
    x <- x[, tags[1L]]
  }
  if (ie && is.null(y))
    stop("'y' is missing and can't be extracted from 'x'")
  nx <- length(x)
  ny <- length(y)
  n <- if (ie) max(nx, ny) else nx
  if (is.na(checked)) {
    if (inherits(object, "midlist")) {
      k <- length(object$intercept)
      return(matrix(0, nrow = n, ncol = k))
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
    bmat <- object$main.effects[[checked]]$mid
    mmat <- object$encoders$main.effects[[checked]]$encode(x)
    out <- mmat %*% bmat
    if (ncol(out) == 1L)
      out <- as.numeric(out)
  } else {
    if (term != checked) {
      tags <- rev(tags)
      temp <- x; x <- y; y <- temp
    }
    bmat <- object$interactions[[checked]]$mid
    xmat <- object$encoders$interactions[[tags[1L]]]$encode(x)
    ymat <- object$encoders$interactions[[tags[2L]]]$encode(y)
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
  out
}

#' @rdname mid.effect
#'
#' @description
#' \code{mid.f()} is a convenient shorthand for \code{mid.effect()}.
#'
#' @export mid.f
#'
mid.f <- mid.effect
