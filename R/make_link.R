#' Extended Parametric Link Functions
#'
#' \code{make_link()} creates a link function object (inheriting from "link-glm") capable of handling parametric transformations such as Box-Cox, Yeo-Johnson, and shifted logarithms.
#' This function serves as a wrapper and extension to \code{\link[stats]{make.link}()}.
#'
#' @details
#' The available links and their parameters are:
#' \itemize{
#'   \item "log1p": shifted log link \eqn{\eta = \log(\mu + 1)}.
#'   \item "shifted.log": shifted log link \eqn{\eta = \log(\mu + h)} with a shift parameter \code{h} (default 1).
#'   \item "shifted.identity": shifted identity link \eqn{\eta = \mu + h} with a shift parameter \code{h} (default 1).
#'   \item "robit": robit link using the Student's t-distribution CDF. \eqn{\eta = F_{t}^{-1}(\mu, \nu)} with a degrees of freedom parameter \code{df} (or alias \code{nu}, default 7).
#'   \item "asinh": inverse hyperbolic sine transformation \eqn{\eta = \text{asinh}(\lambda \mu)} with a scale parameter \code{lambda} (default 1).
#'   \item "scobit": skewed logit link \eqn{\eta = \text{logit}(\mu^{1/\alpha})}. The parameter \code{alpha} (default 1) controls the asymmetry of the tails. \code{alpha=1} corresponds to standard logistic regression.
#'   \item "box-cox": Box-Cox transformation \eqn{\eta = (\mu^\lambda - 1)/\lambda} with a power parameter \code{lambda} (default 0).
#'   \item "box-cox2": two-parameter Box-Cox transformation \eqn{\eta = ((\mu + \lambda_2)^{\lambda_1} - 1)/\lambda_1} with parameters \code{lambda1} (power, default 0) and \code{lambda2} (shift, default 1).
#'   \item "yeo-johnson": Yeo-Johnson transformation with a parameter \code{lambda} (default 0). Handles negative values.
#' }
#'
#' @param link a character string naming the link function: "log1p", "shifted.log", "shifted.identity", "robit", "asinh", "scobit", "box-cox", "box-cox2", or "yeo-johnson". Standard links (e.g., "logit", "probit", "log") are passed to \code{stats::make.link()}.
#' @param ... named arguments passed to the specific link generation logic. See Details for available parameters and defaults.
#' @param simplify logical. If \code{TRUE} (default), the function returns a standard link object or recursively calls \code{make_link} when parameters equate to a simpler and more computationally efficient form (e.g., \code{box-cox} with \code{lambda=0} becomes \code{log}).
#'
#' @examples
#' # Standard Box-Cox with lambda = 0.5 (Square root-like)
#' lk <- make_link("box-cox", lambda = 0.5)
#' plot(x <- seq(1, 100, length.out = 50), lk$linkfun(x), type = "l")
#'
#' # Yeo-Johnson with lambda = 1.5 (Handles negative values)
#' lk <- make_link("yeo-johnson", lambda = 1.5)
#' plot(x <- seq(-100, 100, length.out = 50), lk$linkfun(x), type = "l")
#'
#' # Robit link with df=2 (Heavier tails than probit)
#' lk <- make_link("robit", df = 2)
#' print(lk)
#' plot(x <- seq(-5, 5, length.out = 50), lk$linkinv(x), type = "l")
#' lk <- make_link("robit", df = 1)
#' cat(lk$name) # cauchit
#' points(x, lk$linkinv(x), type = "l", lty = 2L)
#' lk <- make_link("robit", df = Inf)
#' cat(lk$name) # probit
#' points(x, lk$linkinv(x), type = "l", lty = 3L)
#'
#' # Scobit link with alpha=0.5 (Skewed)
#' lk <- make_link("scobit", alpha = 0.5)
#' plot(x <- seq(-5, 5, length.out = 50), lk$linkinv(x), type = "l")
#'
#' # Inverse Hyperbolic Sine (Alternative to log for zero-inflated data)
#' lk <- make_link("asinh", lambda = 10)
#' plot(x <- seq(0, 5, length.out = 50), lk$linkfun(x), type = "l")
#' lk <- make_link("log1p")
#' points(x, lk$linkfun(x), type = "l", lty = 2L)
#'
#' # Shifted log simplifies to log1p when h=1
#' make_link("shifted.log", h = 1) # Returns link="log1p"
#'
#' # Box-Cox with lambda=1 simplifies to shifted identity
#' make_link("box-cox", lambda = 1)
#' @returns
#' \code{make_link()} returns an object of class "link-glm" and "parametric_link" containing:
#' \item{linkfun}{link function \eqn{g(\mu)}.}
#' \item{linkinv}{inverse link function \eqn{g^{-1}(\eta)}.}
#' \item{mu.eta}{derivative \eqn{d\mu/d\eta}.}
#' \item{valideta}{a function checking validity of linear predictors.}
#' \item{name}{name of the link.}
#'
#' @seealso \code{\link[stats]{make.link}}
#'
#' @export make_link
#'
make_link <- function(link, ..., simplify = TRUE) {
  link <- as.character(link)
  dots <- list(...)
  pass <- NULL
  args <- list()
  switch(
    link,
    "log1p" = {
      params <- list()
      linkfun <- function(mu) log1p(mu)
      linkinv <- function(eta) pmax(expm1(eta), .Machine$double.eps - 1)
      mu.eta <- function(eta) pmax(exp(eta), .Machine$double.eps)
      valideta <- function(eta) TRUE
    }, "shifted.log" = {
      params <- do.call(function(h = 1, ...) list(h = h), dots)
      h <- params$h
      if (simplify && h == 1) {
        pass <- make_link
        args <- list(link = "log1p")
      } else if (simplify && h == 0) {
        pass <- stats::make.link
        args <- list(link = "log")
      } else {
        linkfun <- function(mu) log(mu + h)
        linkinv <- function(eta) pmax(exp(eta) - h, .Machine$double.eps - h)
        mu.eta <- function(eta) pmax(exp(eta), .Machine$double.eps)
        valideta <- function(eta) TRUE
      }
    }, "shifted.identity" = {
      params <- do.call(function(h = 1, ...) list(h = h), dots)
      h <- params$h
      if (simplify && h == 0) {
        pass <- stats::make.link
        args <- list(link = "identity")
      } else {
        linkfun <- function(mu) mu + h
        linkinv <- function(eta) eta - h
        mu.eta <- function(eta) rep.int(1, times = length(eta))
        valideta <- function(eta) TRUE
      }
    }, "robit" = {
      params <- do.call(
        function(df = 7, ...) {
          df <- if (missing(df)) list(...)$nu %||% df else df
          list(df = df)
        }, dots
      )
      df <- params$df
      if (simplify && abs(df - 1) < sqrt(.Machine$double.eps)) {
        pass <- stats::make.link
        args <- list(link = "cauchit")
      } else if (simplify && is.infinite(df)) {
        pass <- stats::make.link
        args <- list(link = "probit")
      } else {
        if (df <= 0)
          stop("'df' must be positive for robit link")
        linkfun <- function(mu) stats::qt(mu, df = df)
        linkinv <- function(eta) stats::pt(eta, df = df)
        mu.eta <- function(eta) stats::dt(eta, df = df)
        valideta <- function(eta) TRUE
      }
    }, "asinh" = {
      params <- do.call(function(lambda = 1, ...) list(lambda = lambda), dots)
      lambda <- params$lambda
      if (abs(lambda) < .Machine$double.eps)
        stop("'lambda' cannot be zero for inverse hyperbolic sine (asinh) link")
      linkfun <- function(mu) asinh(lambda * mu)
      linkinv <- function(eta) {
        thresh <- asinh(.Machine$double.xmax)
        eta <- pmin(pmax(eta, -thresh), thresh)
        sinh(eta) / lambda
      }
      mu.eta <- function(eta) {
        thresh <- asinh(.Machine$double.xmax)
        eta <- pmin(pmax(eta, -thresh), thresh)
        cosh(eta) / lambda
      }
      valideta <- function(eta) TRUE
    }, "scobit" = {
      params <- do.call(function(alpha = 1, ...) list(alpha = alpha), dots)
      alpha <- params$alpha
      if (simplify && alpha == 1) {
        pass <- stats::make.link
        args <- list(link = "logit")
      } else {
        if (alpha <= 0)
          stop("'alpha' must be positive for scobit link")
        linkfun <- function(mu) {
          mu <- pmin(pmax(mu, .Machine$double.eps), 1 - .Machine$double.eps)
          stats::qlogis(mu ^ (1 / alpha))
        }
        linkinv <- function(eta) stats::plogis(eta) ^ alpha
        mu.eta <- function(eta) {
          p <- stats::plogis(eta)
          alpha * (p ^ alpha) * (1 - p)
        }
        valideta <- function(eta) TRUE
      }
    }, "box-cox" = {
      params <- do.call(function(lambda = 0, ...) list(lambda = lambda), dots)
      lambda <- params$lambda
      if (simplify && lambda == 0) {
        pass <- stats::make.link
        args <- list(link = "log")
      } else if (simplify && lambda == 1) {
        pass <- make_link
        args <- list(link = "shifted.identity", h = -1)
      } else {
        if (lambda == 0)
          stop("'lambda' cannot be zero for box-cox link")
        linkfun <- function(mu) (mu ^ lambda - 1) / lambda
        linkinv <- function(eta) {
          pmax(lambda * eta + 1, .Machine$double.eps) ^ (1 / lambda)
        }
        mu.eta <- function(eta) {
          pmax(lambda * eta + 1, .Machine$double.eps) ^ ((1 - lambda) / lambda)
        }
        valideta <- function(eta) all(lambda * eta + 1 > 0)
      }
    }, "box-cox2" = {
      params <- do.call(
        function(lambda1 = 0, lambda2 = 1, ...) {
          list(lambda1 = lambda1, lambda2 = lambda2)
        }, dots
      )
      lambda1 <- params$lambda1
      lambda2 <- params$lambda2
      if (simplify && lambda2 == 0) {
        pass <- make_link
        args <- list(link = "box-cox", lambda = lambda1)
      } else if (simplify && lambda1 == 0) {
        pass <- make_link
        args <- list(link = "shifted.log", h = lambda2)
      } else if (simplify && lambda1 == 1) {
        pass <- make_link
        args <- list(link = "shifted.identity", h = lambda2 - 1)
      } else {
        if (lambda1 == 0)
          stop("'lambda1' cannot be zero for box-cox2 link")
        linkfun <- function(mu) ((mu + lambda2) ^ lambda1 - 1) / lambda1
        linkinv <- function(eta) {
          pmax(lambda1 * eta + 1, .Machine$double.eps) ^ (1 / lambda1) - lambda2
        }
        mu.eta <- function(eta) {
          pmax(lambda1 * eta + 1, .Machine$double.eps) ^ ((1 - lambda1) / lambda1)
        }
        valideta <- function(eta) all(lambda1 * eta + 1 > 0)
      }
    }, "yeo-johnson" = {
      params <- do.call(function(lambda = 0, ...) list(lambda = lambda), dots)
      lambda <- params$lambda
      if (simplify && lambda == 1) {
        pass <- stats::make.link
        args <- list(link = "identity")
      } else {
        linkfun <- function(mu) {
          pos <- (mu >= 0)
          out <- mu
          out[pos] <- if (lambda != 0) {
            ((mu[pos] + 1) ^ lambda - 1) / lambda
          } else {
            log1p(mu[pos])
          }
          out[!pos] <- if (lambda != 2) {
            -((-mu[!pos] + 1) ^ (2 - lambda) - 1) / (2 - lambda)
          } else {
            -log1p(-mu[!pos])
          }
          out
        }
        linkinv <- function(eta) {
          eps <- .Machine$double.eps
          pos <- (eta >= 0)
          out <- eta
          out[pos] <- (
            if (lambda != 0) {
              pmax(lambda * eta[pos] + 1, eps) ^ (1 / lambda) - 1
            } else {
              expm1(eta[pos])
            }
          )
          out[!pos] <-(
            if (lambda != 2) {
              1 - (pmax(1 - (2 - lambda) * eta[!pos], eps)) ^ (1 / (2 - lambda))
            } else {
              -expm1(-eta[!pos])
            }
          )
          out
        }
        mu.eta <- function(eta) {
          eps <- .Machine$double.eps
          pos <- (eta >= 0)
          out <- eta
          out[pos] <- (
            if (lambda != 0) {
              pmax(lambda * eta[pos] + 1, eps) ^ ((1 - lambda) / lambda)
            } else {
              exp(eta[pos])
            }
          )
          out[!pos] <- (
            if (lambda != 2) {
              pmax(1 - (2 - lambda) * eta[!pos], eps) ^ ((lambda - 1) / (2 - lambda))
            } else {
              exp(-eta[!pos])
            }
          )
          out
        }
        valideta <- function(eta) TRUE
      }
    }, {
      pass <- stats::make.link
      args <- list(link = link)
    }
  )
  if (!is.null(pass)) {
    res <- do.call(pass, args)
    return(structure(res, class = c("parametric_link", "link-glm")))
  }
  envir <- list2env(params, parent = getNamespace("stats"))
  environment(linkfun) <- environment(linkinv) <-
    environment(mu.eta) <- environment(valideta) <- envir
  structure(
    list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta,
         valideta = valideta, name = link, envir = envir),
    class = c("parametric_link", "link-glm")
  )
}


#' @exportS3Method base::print
#'
print.parametric_link <- function(x, ...) {
  nm <- x$name %||% attr(x, "name", exact = TRUE)
  cat(paste0("Link Function: ", nm %||% "custom"))
  envir <- x$envir
  if (!is.null(envir) &&
      !tryCatch(rlang::is_namespace(envir), error = function(e) FALSE)) {
    args <- Filter(function(v) is.numeric(v) || is.character(v), as.list(envir))
    if (length(args) > 0L) {
      cat("(", paste(names(args), args, sep = " = ", collapse = ", "), ")", sep = "")
    }
  }
  cat("\n\n")
  print.function(x$linkfun)
  invisible(x)
}
