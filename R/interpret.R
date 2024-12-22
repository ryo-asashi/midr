#' Create an Interpretable Surrogate of Black-Box ML Models
#'
#' Construct a predictive model consisting of a set of functions, each with up to two variables.
#'
#' @param object a fitted model object to be interpreted.
#' @examples
#' data(cars, package = "datasets")
#' model <- lm(dist ~ I(speed^2) + speed, cars)
#' mid <- interpret(dist ~ speed, cars, model)
#' plot(mid, "speed", add.intercept = TRUE) +
#'   points(cars)
#' summary(mid)
#'
#' data(Nile, package = "datasets")
#' mid <- interpret(x = 1L:100L, y = Nile, k = 100L)
#' plot(mid, "x", add.intercept = TRUE, ylim = c(600L, 1300L)) +
#'   points(x = 1L:100L, y = Nile)
#' # reduce number of knots by k parameter
#' mid <- interpret(x = 1L:100L, y = Nile, k = 10L)
#' plot(mid, "x", add.intercept = TRUE, ylim = c(600L, 1300L)) +
#'   points(x = 1L:100L, y = Nile)
#' # pseudo-smoothing by lambda parameter
#' mid <- interpret(x = 1L:100L, y = Nile, k = 100L, lambda = 100L)
#' plot(mid, "x", add.intercept = TRUE, ylim = c(600L, 1300L)) +
#'   points(x = 1L:100L, y = Nile)
#'
#' data(airquality, package = "datasets")
#' airquality$Month <- factor(airquality$Month)
#' model <- glm(Ozone ~ .^2, Gamma(log), airquality)
#' mid <- interpret(Ozone ~ .^2, na.omit(airquality), model, lambda = .1)
#' summary(mid)
#' plot(mid, "Wind")
#' plot(mid, "Temp")
#' plot(mid, "Wind:Month", include.main.effects = TRUE)
#' @returns
#' \code{interpret()} returns an object of class "mid", which is a list containing the following components:
#' \item{weights}{a numeric vector of the weights.}
#' \item{call}{the matched call.}
#' \item{terms}{a character vector of decomposition term names.}
#' \item{link}{a list of class "mid-link", specifying the link function used.}
#' \item{intercept}{the fitted zeroth-order effect.}
#' \item{main.effects}{a list of data frames representing the fitted first-order (main) effects of each variable.}
#' \item{me.encoders}{a list of encoders for the first-order decomposition.}
#' \item{interacions}{a list of data frames representing the fitted second-order interactions.}
#' \item{ie.encoders}{a list of encoders for the second-order decomposition.}
#' \item{uninterpreted.rate}{the ratio of the interpretation loss to the original variance of model predictions (yhat).}
#' \item{fitted.matrix}{a matrix containing the breakdown of the fitted values into the functional decomposition terms.}
#' \item{linear.predictors}{a numeric vector of the linear predictors.}
#' \item{fitted.values}{a numeric vector of the fitted values.}
#' \item{residuals}{a numeric vector of the working residuals.}
#' \item{na.action}{information on the special handlings of NAs.}
#' @export interpret
#'
interpret <- function(object, ...)
UseMethod("interpret")

#'
#' @rdname interpret
#' @param x a matrix or a data frame of predictor variables to be used to interpret the model prediction. The response variable should not be included.
#' @param y an optional numeric vector representing of the model prediction or the response variable.
#' @param weights optional. a numeric vector indicating weights for each row in the data.
#' @param pred.fun a function that takes two arguments, 'X.model' and 'newdata' to be used to make a prediction. Default is \code{get.yhat()}, which uses \code{DALEX::yhat()} if the DALEX package is installed.
#' @param link name of the link function. One of "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse".
#' @param k an integer or a numeric vector of length two for main effects and interactions, specifying the maximum number of sample points for each numeric predictor variable. If an integer is passed, k is used for main effect terms and the square root of k is used for interaction terms. If not positive, all unique values are used as sample points.
#' @param type an integer or a vector of length two, specifying the type of piecewise functions to be fit on numeric variables. '0' is for step functions on discretized intervals, and '1' is for piecewise linear functions connecting at representative values.
#' @param frames a named list of encoding frames, which specifies bins for quantitative features or levels for qualitative features.
#' @param interaction logical. If TRUE, and if \code{terms} and \code{formula} are not supplied, all second order interaction effects for each pair of features in \code{x} are calculated.
#' @param terms a character vector of term labels, specifying the set of decomposition terms. If not passed, all main effects (and all second order interactions if \code{interaction} is TRUE) of \code{x} are used.
#' @param singular.ok logical. If FALSE, a singular fit is an error.
#' @param mode an integer specifying the general method of calculation. When \code{mode} is set to 1, centralization constraints are treated as penalties for the least squares problem. If \code{mode} is 2, the centralization constraints are used to reduce the number of unknown parameters, making the calculation safer and more robust.
#' @param method an integer or a vector of length two, specifying the methods to be used to solve the least squares problem. A non-negative value will be passed to RcppEigen::fastLmPure and if a negative value is passed, stats::lm.fit will be used.
#' @param lambda a numeric parameter for the penalty of weighted ridge regularization.
#' @param kappa a numeric parameter for the penalty of the centralization constraints. Only used if \code{mode} is 1.
#' @param na.action a function or a character which indicates what should happen when the data contain missing values (NAs). The default is na.omit.
#' @param encoding.digits an integer specifying the rounding digits for encoding numeric variables when \code{type} is 1 (piecewise linear functions).
#' @param use.catchall logical. If TRUE, less frequent levels are dropped and replaced with the catchall level.
#' @param catchall a character used as the name of "catchall" level for unused levels of each factor variable.
#' @param max.ncol an integer which indicates the maximum number of columns of the design matrix.
#' @param nil a threshold for the intercept and coefficients to be treated as zero. Default is 1e-7.
#' @param tol a tolerance for the singular value decomposition. Default is 1e-7.
#' @param ... special aliases for some arguments, including 'ie' for 'interaction' and 'ok' for 'singular.ok'.
#' @exportS3Method midr::interpret
#'
interpret.default <- function(
    object, x, y = NULL, weights = NULL, pred.fun = get.yhat, link = NULL,
    k = c(NA, NA), type = c(1L, 1L), frames = list(), interaction = FALSE,
    terms = NULL, singular.ok = FALSE, mode = 1L, method = NULL,
    lambda = 0, kappa = 1e6, na.action = getOption("na.action"),
    encoding.digits = 3L, use.catchall = FALSE, catchall = "(others)",
    max.ncol = 3000L, nil = 1e-7, tol = 1e-7, ...
) {

  cl <- match.call()
  dots <- list(...)
  if (missing(interaction) && !is.null(dots$ie)) interaction <- dots$ie
  if (missing(singular.ok) && !is.null(dots$ok)) singular.ok <- dots$ok
  fit.intercept <- ifnot.null(dots$fit.intercept, FALSE)
  interpolate.beta <- ifnot.null(dots$interpolate.beta, TRUE)
  weighted.norm <- ifnot.null(dots$weighted.norm, singular.ok)
  weighted.encoding <- ifnot.null(dots$weighted.encoding, FALSE)

  # preprocess data --------
  if (!is.data.frame(x))
    x <- as.data.frame(x)
  naai <- list(n.init = nrow(x), ids = 1L:nrow(x))
  if (is.null(colnames(x)))
    colnames(x) <- paste0("x", 1L:ncol(x))
  if (any("mid" == colnames(x)))
    colnames(x)[colnames(x) == "mid"] <- ".mid"
  tags <- colnames(x)
  if(missing(weights))
    weights <- attr(x, "weights")
  attr(x, "na.action") <- NULL
  x <- do.call(na.action, list(x))
  if (!is.null(naa.x <- stats::na.action(x))) {
    y <- y[-naa.x]
    weights <- weights[-naa.x]
    naai$ids <- naai$ids[-naa.x]
    attr(x, "na.action") <- NULL
  }
  if (is.null(y)) {
    if (missing(object))
      stop("either 'object' or 'y' must be supplied")
    y <- pred.fun(X.model = object, newdata = x)
  }
  if (is.character(y))
    y <- as.factor(y)
  target.level <- NULL
  if (is.factor(y)) {
    target.level <- levels(y)[1L]
    y <- (y == target.level)
  }
  if (!is.numeric(y))
    y <- as.numeric(y)
  if (length(y) != nrow(x))
    stop("length of 'y' doesn't match the number of rows in 'x'")
  if (!is.null(link)){
    link <- stats::make.link(as.character(substitute(link)))
    class(link) <- "link-mid"
    yres <- y
    y <- link$linkfun(y)
  }
  attr(y, "na.action") <- NULL
  y <- do.call(na.action, list(y))
  if (anyNA(y))
    stop("NA values in 'y' are not allowed")
  if (!is.null(naa.y <- stats::na.action(y))) {
    x <- x[-naa.y, , drop = FALSE]
    weights <- weights[-naa.y]
    naai$ids <- naai$ids[-naa.y]
    attr(y, "na.action") <- NULL
  }
  if (any(is.infinite(as.matrix(x))) || any(is.infinite(y)))
    stop("'Inf' and '-Inf' are not allowed")
  if (is.null(weights))
    weights <- rep.int(1, nrow(x))
  if (!is.numeric(weights) || anyNA(weights))
    stop("'weights' must be a numeric vector without missing values")
  if (length(weights) != nrow(x))
    stop("length of 'weights' doesn't match the number of rows in 'x'")
  if (any(weights < 0))
    stop("negative weights not allowed")
  wsum <- sum(weights)
  nuvs <- sapply(x, is.numeric)
  orvs <- nuvs | sapply(x, is.ordered)
  if (is.null(terms)) {
    mts <- tags
    its <- NULL
    if (interaction)
      its <- utils::combn(mts, 2L, function(x) paste0(x, collapse = ":"))
  } else {
    spl <- sapply(strsplit(terms, ":"), length)
    mts <- unique(terms[spl == 1L])
    its <- unique(terms[spl == 2L])
  }
  terms <- c(mts, its)
  n <- nrow(x) # sample size
  p <- length(mts)
  q <- length(its)
  if (length(k) == 1L)
    k <- c(k, ceiling(sqrt(max(k, 0L))))
  if (is.na(k[1L]))
    k[1L] <- min(25L, max(2L, if (lambda > 0) 25L else n %/% p))
  if (is.na(k[2L]))
    k[2L] <- min(5L, max(2L, if (lambda > 0) 5L else floor(sqrt(n / q))))
  if (length(type) == 1L)
    type <- c(type, type)
  f <- function(tag, d) {
    ifnot.null(frames[[paste0(switch(d, "|", ":"), tag)]], frames[[tag]])
  }
  if (is.null(method))
    method <- if (!singular.ok) 0L else 5L
  if (!singular.ok && any(method == 1L:2L))
    message("when 'method' is set to 1 or 2, singular fits cannot be detected")

  # get encoders for the calculation of the main effects --------
  u <- 0L
  if (me <- (p > 0L)) {
    mencs <- list()
    mmats <- list()
    for (tag in mts) {
      mencs[[tag]] <-
        if (nuvs[tag]) {
          numeric.encoder(x = x[, tag], k = k[1L], type = type[1L], tag = tag,
                          encoding.digits = encoding.digits, frame = f(tag, 1L),
                          weights = if (weighted.encoding) weights)
        } else {
          factor.encoder(x = x[, tag], k = k[1L], use.catchall = use.catchall,
                         catchall = catchall, tag = tag, frame = f(tag, 1L),
                         weights = if (weighted.encoding) weights)
        }
      mmats[[tag]] <- mencs[[tag]]$encode(x[, tag])
    }
    mlens <- sapply(mencs, function(x) x$n)
    mcumlens <- structure(cumsum(c(0L, mlens)), names = c(mts, NA))
    u <- mcumlens[length(mcumlens)] # total number of unique values
  }

  # get encoders for the calculation of the interactions --------
  v <- mi <- 0L
  if (ie <- (q > 0L)) {
    iencs <- list()
    imats <- list()
    for (tag in unique(term.split(its))) {
      iencs[[tag]] <-
        if (nuvs[tag]) {
          numeric.encoder(x = x[, tag], k = k[2L], type = type[2L], tag = tag,
                          encoding.digits = encoding.digits, frame = f(tag, 2L),
                          weights = if (weighted.encoding) weights)
        } else {
          factor.encoder(x = x[, tag], k = k[2L], use.catchall = use.catchall,
                         catchall = catchall, tag = tag, frame = f(tag, 2L),
                         weights = if (weighted.encoding) weights)
        }
      imats[[tag]] <- iencs[[tag]]$encode(x[, tag])
    }
    ilens <- sapply(iencs, function(x) x$n)
    plens <- structure(integer(q), names = its)
    for (i in seq_len(q)) {
      pcl <- term.split(its[i])
      plens[i] <- ilens[pcl[1L]] * ilens[pcl[2L]]
      mi <- mi + ilens[pcl[1L]] + ilens[pcl[2L]]
    }
    pcumlens <- structure(cumsum(c(0L, plens)), names = c(its, NA))
    v <- pcumlens[length(pcumlens)] # total number of unique pairs
  }

  # create the design matrix and the constraints matrix --------
  fi <- as.integer(fit.intercept)
  ncol <- fi + u + v
  ncon <- p + mi
  if (ncol > max.ncol)
    stop(paste0("number of columns of the design matrix (", ncol, ") exceeded the limit (", max.ncol, ")"))
  X <- matrix(0, nrow = n, ncol = ncol)
  M <- matrix(0, nrow = ncon, ncol = ncol)
  Y <- y
  w <- sqrt(weights)
  D <- rep.int(1, ncol)
  dens <- numeric(ncol)
  lemp <- list()
  ladj <- list()
  vnil <- logical(ncol)
  ## intercept
  if (fit.intercept) {
    X[, 1L] <- 1
  } else {
    intercept <- stats::weighted.mean(Y, weights)
    intercept <- attract(intercept, nil)
    Y <- Y - intercept
  }
  ## main effects
  for (i in seq_len(p)) {
    mcl <- mts[i]
    ord <- orvs[mcl]
    for (j in 1L:mlens[[i]]) {
      m <- fi + mcumlens[i] + j
      X[, m] <- mmats[[i]][, j]
      vsum <- sum(X[, m] * weights)
      if (vsum == 0) {
        lemp[[length(lemp) + 1L]] <- c(
          m,
          if (ord && j > 1L) m - 1L,
          if (ord && j < mlens[[i]]) m + 1L
        )
        vnil[m] <- TRUE
        next
      }
      M[i, m] <- vsum
      D[m] <- sqrt(vsum)
      if (weighted.norm) {
        X[, m] <- X[, m] / D[m]
        M[i, m] <- M[i, m] / D[m]
      }
      dens[m] <- vsum / wsum
      if (lambda > 0 && ord)
        ladj[[length(ladj) + 1L]] <- c(
          m,
          if (j > 1L) m - 1L,
          if (j < mlens[[i]]) m + 1L
        )
    }
  }
  ## interactions
  ofs <- p
  for (i in seq_len(q)) {
    pcl <- term.split(its[i])
    nval <- c(ilens[[pcl[1L]]], ilens[[pcl[2L]]])
    vals <- as.matrix(expand.grid(1L:nval[1L], 1L:nval[2L]))
    ords <- orvs[c(pcl[1L], pcl[2L])]
    for (j in 1L:plens[[i]]) {
      val <- vals[j, ]
      m <- fi + u + pcumlens[i] + j
      X[, m] <- imats[[pcl[1L]]][, val[1L]] * imats[[pcl[2L]]][, val[2L]]
      vsum <- sum(X[, m] * weights)
      if (vsum == 0) {
        lemp[[length(lemp) + 1L]] <- c(
          m,
          if (ords[1L] && val[1L] > 1L) m - 1L,
          if (ords[1L] && val[1L] < nval[1L]) m + 1L,
          if (ords[2L] && val[2L] > 1L) m - nval[1L],
          if (ords[2L] && val[2L] < nval[2L]) m + nval[1L]
        )
        vnil[m] <- TRUE
        next
      }
      M[ofs + val[1L], m] <- vsum
      M[ofs + nval[1L] + val[2L], m] <- vsum
      D[m] <- sqrt(vsum)
      if (weighted.norm) {
        X[, m] <- X[, m] / D[m]
        M[, m] <- M[, m] / D[m]
      }
      dens[m] <- vsum / wsum
      if (lambda > 0 && ords[1L])
        ladj[[length(ladj) + 1L]] <- c(
          m,
          if (val[1L] > 1L) m - 1L,
          if (val[1L] < nval[1L]) m + 1L
        )
      if (lambda > 0 && ords[2L])
        ladj[[length(ladj) + 1L]] <- c(
          m,
          if (val[2L] > 1L) m - nval[1L],
          if (val[2L] < nval[2L]) m + nval[1L]
        )
    }
    ofs <- ofs + nval[1L] + nval[2L]
  }
  ## ridge regularization
  nreg <- 0L
  if (lambda > 0) {
    nreg <- length(ladj)
    wreg <- rep.int(sqrt(lambda), nreg)
    R <- matrix(0, nrow = nreg, ncol = ncol)
    for (i in seq_len(nreg)) {
      a <- ladj[[i]][-1L]
      a <- a[!vnil[a]]
      if (length(a) == 0L)
        next
      m <- ladj[[i]][1L]
      R[i, a] <- - (if (weighted.norm) 1 / D[a] else 1)
      R[i, m] <- (if (weighted.norm) 1 / D[m] else 1) * length(a)
      wreg[i] <- wreg[i] * D[m]
    }
    X <- rbind(X, R)
    Y <- c(Y, numeric(nreg))
    w <- c(w, wreg)
  }
  ## additional constraints for columns filled with zero
  nemp <- length(lemp)
  if (nemp > 0L) {
    Mnil <- matrix(0, nrow = nemp, ncol = ncol)
    for (i in 1L:nemp)
      Mnil[i, lemp[[i]][1L]] <- 1
    M <- rbind(M, Mnil)
  }

  # get the least squares solution --------
  if (mode == 1L) {
    X <- rbind(X, M)
    Y <- c(Y, numeric(nrow(M)))
    w <- c(w, rep.int(sqrt(kappa), nrow(M)))
    r <- 0L
    if (method >= 0L) {
      z <- try(RcppEigen::fastLmPure(X * w, Y * w, method), silent = TRUE)
    }
    if (method < 0L || inherits(z, "try-error")) {
      method <- -1L
      z <- stats::lm.fit(X * w, Y * w)
    }
    beta <- z$coefficients
    beta[is.na(beta)] <- 0
    rsd <- z$residuals[1L:n] / w[1L:n]
    crsd <- z$residuals[(n + nreg + 1L):(n + nreg + ncon)]
    if (any(abs(crsd) > (nil * sqrt(kappa) * wsum))) {
      message(paste0("not strictly centralized: max absolute expected effect is ",
                     format(max(abs(crsd)) / sqrt(kappa) / wsum, digits = 6L)))
    }
  } else {
    Msvd <- svd(M, nv = ncol)
    r <- sum(Msvd$d > tol)
    if (r == dim(Msvd$v)[2L])
      stop("no coefficients to evaluate found")
    vr <- as.matrix(Msvd$v[, (r + 1L):ncol])
    if (method >= 0L)
      z <- try(RcppEigen::fastLmPure((X * w) %*% vr, Y * w, method),
               silent = TRUE)
    if (method < 0L || inherits(z, "try-error")) {
      method <- -1L
      z <- stats::lm.fit((X * w) %*% vr, Y * w)
    }
    coef <- z$coefficients
    coef[is.na(coef)] <- 0
    beta <- as.numeric(vr %*% coef)
    rsd <- z$residuals[1L:n] / w[1L:n]
  }
  if (!(any(method == 1L:2L)) && z$rank < ncol - r) {
    if (!singular.ok)
      stop("singular fit encountered")
    message("singular fit encountered")
  }
  if (weighted.norm) {
    gamm <- beta
    beta <- beta / D
  }
  lemp <- lemp[vapply(lemp, length, 0L) > 1L]
  nemp <- length(lemp)
  if (interpolate.beta && nemp > 0L) {
    B <- diag(1, ncol)
    for (i in seq_len(nemp)) {
      a <- lemp[[i]][-1L]
      m <- lemp[[i]][1L]
      B[m, a] <- - 1
      B[m, m] <- length(a)
    }
    beta <- try(RcppEigen::fastLmPure(B, beta, 0L)$coefficients,
                silent = TRUE)
    if (inherits(beta, "try-error"))
      beta <- as.numeric(stats::lm.fit(B, beta)$coefficients)
    beta[is.na(beta)] <- 0
  }
  beta[abs(beta) <= nil] <- 0

  # summarize results of the decomposition --------
  fm <- matrix(0, nrow = n, ncol = p + q)
  colnames(fm) <- terms
  ## intercept
  if(fit.intercept)
    intercept <- beta[1L]
  attr(fm, "constant") <- intercept
  ## main effects
  if (me) {
    main.effects <- list()
    for (i in seq_len(p)) {
      dat <- mencs[[mts[i]]]$frame
      lt <- fi + mcumlens[i] + 1L
      rt <- fi + mcumlens[i + 1L]
      dat$density <- dens[lt:rt]
      dat$mid <- beta[lt:rt]
      main.effects[[mts[i]]] <- dat
      xmat <- mmats[[mts[i]]]
      fm[, i] <- as.numeric(xmat %*% dat$mid)
    }
  }
  ## interactions
  if (ie) {
    interactions <- list()
    for (i in seq_len(q)) {
      pcl <- term.split(its[i])
      nval <- c(ilens[[pcl[1L]]], ilens[[pcl[2L]]])
      vals <- expand.grid(1L:nval[1L], 1L:nval[2L])
      dat <- cbind(iencs[[pcl[1L]]]$frame[vals[, 1L], ],
                   iencs[[pcl[2L]]]$frame[vals[, 2L], ])
      lt <- fi + u + pcumlens[i] + 1L
      rt <- fi + u + pcumlens[i + 1L]
      dat$density <- dens[lt:rt]
      dat$mid <- beta[lt:rt]
      rownames(dat) <- NULL
      interactions[[its[i]]] <- dat
      xmat <- X[1L:n, lt:rt]
      mult <- if (!weighted.norm) dat$mid else gamm[lt:rt]
      fm[, p + i] <- as.numeric(xmat %*% mult)
    }
  }

  # calculate the uninterpreted rate --------
  tot <- stats::weighted.mean((y - intercept) ^ 2, weights)
  uiq <- stats::weighted.mean(rsd ^ 2, weights)
  uir <- attract(uiq / tot, nil)

  # output the result --------
  obj <- list()
  class(obj) <- c("mid")
  cl[[1L]] <- as.name("interpret")
  obj$weights <- weights
  obj$call <- cl
  obj$target.level <- target.level
  obj$terms <- terms
  obj$link <- link
  obj$intercept <- intercept
  if (me) {
    obj$main.effects <- main.effects
    obj$me.encoders <- mencs
  }
  if (ie) {
    obj$interactions <- interactions
    obj$ie.encoders <- iencs
  }
  obj$uninterpreted.rate <- uir
  obj$fitted.matrix <- fm
  obj$fitted.values <- rowSums(fm) + intercept
  obj$residuals <- rsd
  if (!is.null(link)) {
    obj$linear.predictors <- obj$fitted.values
    obj$fitted.values <- link$linkinv(obj$fitted.values)
    obj$response.residuals <- yres - obj$fitted.values
    mu <- stats::weighted.mean(yres, weights)
    rtot <- stats::weighted.mean((yres - mu) ^ 2, weights)
    ruiq <- stats::weighted.mean(obj$response.residuals ^ 2, weights)
    ruir <- attract(ruiq / rtot, nil)
    obj$uninterpreted.rate <- c(working = uir, response = ruir)
  }
  if (length(naai$ids) < naai$n.init) {
    naacl <- attr(attr(do.call(na.action, list(NA)), "na.action"), "class")
    obj$na.action <- structure((1L:naai$n.init)[-naai$ids], class = naacl)
  }
  return(obj)
}

#'
#' @rdname interpret
#' @param formula a symbolic description of the decomposition model to be fit.
#' @param data a data frame containing the variables in the formula. If not found in data, the variables are taken from environment(formula).
#' @param model a model object to be interpreted.
#' @param subset an index vector specifying the rows to be used in the training sample.
#' @param drop.unused.levels logical. If TRUE, unused levels of factors will be dropped.
#' @exportS3Method midr::interpret
#'
interpret.formula <- function(
    formula, data = NULL, model = NULL, pred.fun = get.yhat, weights = NULL,
    subset = NULL, na.action = getOption("na.action"), mode = 1L,
    drop.unused.levels = FALSE, ...
) {
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  mf$... <- NULL
  mf$model <- NULL
  mf$pred.fun <- NULL
  mf$mode <- NULL
  if (use.model <- !missing(model)) {
    if (is.null(data))
      stop("'data' is required to get predicted values from the 'model'")
    if (!is.data.frame(data))
      data <- as.data.frame(data)
    naai <- list(n.init = nrow(data), ids = 1L:nrow(data))
    if (is.null(weights))
      weights <- attr(data, "weights")
    yvar <- if (any("yhat" == colnames(data))) ".yhat" else "yhat"
    rvar <- deparse(formula[[2L]])
    attr(data, "na.action") <- NULL
    data <- do.call(na.action, list(data[names(data) != rvar]))
    if (!is.null(naa.x <- stats::na.action(data))) {
      weights <- weights[-naa.x]
      naai$ids <- naai$ids[-naa.x]
      attr(data, "na.action") <- NULL
    }
    yhat <- pred.fun(X.model = model, newdata = data)
    attr(yhat, "na.action") <- NULL
    yhat <- do.call(na.action, list(yhat))
    if (anyNA(yhat))
      stop("NA values found in the model predictions")
    if (!is.null(naa.y <- stats::na.action(yhat))) {
      tags <- colnames(data)
      data <- data[-naa.y, , drop = FALSE]
      weights <- weights[-naa.y]
      naai$ids <- naai$ids[-naa.y]
      attr(yhat, "na.action") <- NULL
    }
    data[[yvar]] <- yhat
    formula[[2L]] <- as.name(yvar)
    cl$formula <- mf$formula <- formula
    mf$data <- data
    mf$weights <- weights
  } else {
    message("'model' is not passed: the response variable in the data is used")
    if (is.matrix(eval(mf$data)))
      mf$data <- as.data.frame(eval(mf$data))
    if (is.null(eval(mf$weights)))
      mf$weights <- attr(eval(mf$data), "weights")
  }
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  naa.data <- stats::na.action(mf)
  if (!use.model) {
    naai <- list(n.init = nrow(mf) + length(naa.data))
    naai$ids <- 1L:naai$n.init
  }
  if (!is.null(naa.data))
    naai$ids <- naai$ids[-naa.data]
  x <- structure(as.data.frame(mf), na.action = NULL)
  y <- stats::model.response(mf, "any")
  w <- as.vector(stats::model.weights(mf))
  tl <- attr(attr(mf, "terms"), "term.labels")
  ret <- interpret.default(model = model, x = x, y = y, weights = w,
                           terms = tl, mode = mode, na.action = na.action, ...)
  cl[[1L]] <- as.name("interpret")
  ret$call <- cl
  if (!is.null(naa.ret <- ret$na.action))
    naai$ids <- naai$ids[-naa.ret]
  if (length(naai$ids) < naai$n.init) {
    naacl <- attr(attr(do.call(na.action, list(NA)), "na.action"), "class")
    ret$na.action <- structure((1L:naai$n.init)[-naai$ids], class = naacl)
  }
  return(ret)
}
