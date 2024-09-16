#' Create an Interpretable Surrogate of Black-Box ML Models
#'
#' Construct a predictive model consisting of a set of functions, each with up to two variables.
#'
#' @param object a fitted model object to be interpreted.
#' @examples
#' model <- lm(Volume ~ . + I(Girth^2) + Girth:Height, trees)
#' mid <- interpret(Volume ~ . ^ 2, trees, model, k = 10)
#' print(mid)
#' plot(mid, "Girth")
#' plot(mid, "Height")
#' plot(mid, "Girth:Height")
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
#' @param method an integer or a vector of length two, specifying the methods to be used to solve the least squares problem. A non-negative value will be passed to RcppEigen::fastLmPure and if a negative value is passed, stats::lm.fit will be used.
#' @param lambda a numeric parameter for the weighted ridge regularization.
#' @param na.action a function or a character which indicates what should happen when the data contain missing values (NAs). The default is na.omit.
#' @param encoding.digits an integer specifying the rounding digits for encoding numeric variables when \code{type} is 1 (piecewise linear functions).
#' @param use.catchall logical. If TRUE, less frequent levels are dropped and replaced with the catchall level.
#' @param catchall a character used as the name of "catchall" level for unused levels of each factor variable.
#' @param max.ncol an integer which indicates the maximum number of columns of the design matrix.
#' @param nil a threshold for the intercept and coefficients to be treated as zero. Default is 1e-7.
#' @param tol a tolerance for the singular value decomposition. Default is 1e-7.
#' @param ... special aliases for some arguments, including 'ie' for 'interaction' and 'ok' for 'singular.ok'.
#'
#' @rdname interpret
#' @exportS3Method midr::interpret
#'
interpret.default <- function(
    object, x, y = NULL, weights = NULL, pred.fun = get.yhat, link = NULL,
    k = c(NA, NA), type = c(1L, 1L), frames = list(), interaction = FALSE,
    terms = NULL, singular.ok = FALSE, method = NULL, lambda = 0L,
    na.action = getOption("na.action"),
    encoding.digits = 3L, use.catchall = FALSE, catchall = "(others)",
    max.ncol = 3000L, nil = 1e-7, tol = 1e-7, ...
) {

  cl <- match.call()
  dots <- list(...)
  if (missing(interaction) && !is.null(dots$ie))
    interaction <- dots$ie
  if (missing(singular.ok) && !is.null(dots$ok))
    singular.ok <- dots$ok
  expand.matrix <-
    ifelse(!is.null(dots$expand.matrix), dots$expand.matrix, FALSE)
  kappa <-
    ifelse(!is.null(dots$kappa), dots$kappa, 1L)
  weighted.norm <-
    ifelse(!is.null(dots$weighted.norm), dots$weighted.norm, singular.ok)
  fit.intercept <-
    ifelse(!is.null(dots$fit.intercept), dots$fit.intercept, FALSE)
  interpolate.beta <-
    ifelse(!is.null(dots$interpolate.beta), dots$interpolate.beta, TRUE)
  weighted.encoding <-
    ifelse(!is.null(dots$weighted.encoding), dots$weighted.encoding, FALSE)

  # preprocess data --------
  inn <- nrow(x)
  ids <- 1L:inn
  if (!is.data.frame(x))
    x <- as.data.frame(x)
  if (is.null(colnames(x)))
    colnames(x) <- paste0("x", 1L:ncol(x))
  if ("mid" %in% colnames(x))
    colnames(x)[colnames(x) == "mid"] <- ".mid"
  if(missing(weights))
    weights <- attr(x, "weights")
  x <- do.call(na.action, list(x))
  if (!is.null(naa.x <- stats::na.action(x))) {
    y <- y[-naa.x]
    weights <- weights[-naa.x]
    ids <- ids[-naa.x]
    naa.class <- attr(naa.x, "class")
  }
  if (is.null(y)) {
    if (missing(object))
      stop("either 'object' or 'y' must be supplied")
    y <- pred.fun(X.model = object, newdata = x)
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
  y <- do.call(na.action, list(y))
  if (anyNA(y))
    stop("NA values in 'y' are not allowed")
  if (!is.null(naa.y <- stats::na.action(y))) {
    x <- x[-naa.y, ]
    weights <- weights[-naa.y]
    ids <- ids[-naa.y]
    naa.class <- attr(naa.y, "class")
  }
  if (is.null(weights))
    weights <- rep.int(1, nrow(x))
  if (!is.numeric(weights) || anyNA(weights))
    stop("'weights' must be a numeric vector without missing values")
  if (length(weights) != nrow(x))
    stop("length of 'weights' doesn't match the number of rows in 'x'")
  if (any(weights < 0))
    stop("negative weights not allowed")
  wsum <- sum(weights)
  tags <- colnames(x)
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
    k[1L] <- min(25L, max(2L, ifelse(singular.ok, 25L, n %/% p)))
  if (is.na(k[2L]))
    k[2L] <- min(5L, max(2L, floor(sqrt(n / q))))
  if (length(type) == 1L)
    type <- c(type, type)
  f <- function(tag, d) {
    frm <- frames[[tag]]
    if (inherits(frm, "list"))
      return(frm[[min(d, length(frm))]])
    frm
  }
  if (is.null(method))
    method <- ifelse(!singular.ok, 0L, 5L)
  if (!singular.ok && method %in% 1L:2L)
    warning("when 'method' is set to 1 or 2, singular fits cannot be detected")

  # get encoders for the calculation of the main effects --------
  u <- 0L
  if (me <- (p > 0L)) {
    mencs <- list()
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
    }
    mlens <- sapply(mencs, function(x) x$n)
    mcumlens <- structure(cumsum(c(0L, mlens)), names = c(mts, NA))
    u <- mcumlens[length(mcumlens)] # total number of unique values
  }

  # get encoders for the calculation of the interactions --------
  v <- mi <- 0L
  if (ie <- (q > 0L)) {
    iencs <- list()
    for (tag in unique(unlist(strsplit(its, ":")))) {
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
    }
    ilens <- sapply(iencs, function(x) x$n)
    plens <- structure(integer(q), names = its)
    for (it in its) {
      pcl <- unlist(strsplit(it, ":"))
      plens[it] <- ilens[pcl[1L]] * ilens[pcl[2L]]
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
    stop("number of columns of the design matrix exceeded the limit")
  X <- matrix(0, nrow = n, ncol = ncol)
  M <- matrix(0, nrow = ncon, ncol = ncol)
  Y <- y
  w <- sqrt(weights)
  Ddiag <- rep.int(1, ncol)
  dns <- numeric(ncol)
  emp <- list()
  ## intercept
  if (fit.intercept) {
    X[, 1L] <- 1
  } else {
    intercept <- stats::weighted.mean(Y, weights)
    intercept <- ifelse(abs(intercept) < nil, 0, intercept)
    Y <- Y - intercept
  }
  ## main effects
  for (i in seq_len(p)) {
    mcl <- mts[i]
    for (j in 1:mlens[[i]]) {
      m <- fi + mcumlens[i] + j
      X[, m] <- mencs[[i]]$encode(x[, mcl], j)
      vsum <- sum(X[, m] * weights)
      if (vsum == 0) {
        ctp <- c(ifelse(orvs[i] && j > 1, m - 1, NA),
                 ifelse(orvs[i] && j < mlens[[i]], m + 1, NA))
        ctp <- stats::na.omit(ctp)
        emp[[length(emp) + 1]] <-
          list(m = m, ctp = ctp, len = max(1, length(ctp)))
        next
      }
      M[i, m] <- vsum
      Ddiag[m] <- sqrt(vsum)
      if (weighted.norm) {
        X[, m] <- X[, m] / Ddiag[m]
        M[i, m] <- M[i, m] / Ddiag[m]
      }
      dns[m] <- vsum / wsum
    }
  }
  ## interactions
  ofs <- p
  for (i in seq_len(q)) {
    pcl <- unlist(strsplit(its[i], ":"))
    nval <- c(ilens[[pcl[1]]], ilens[[pcl[2]]])
    vals <- as.matrix(expand.grid(1:nval[1], 1:nval[2]))
    for (j in 1:plens[[i]]) {
      val <- vals[j, ]
      m <- fi + u + pcumlens[i] + j
      X[, m] <- as.numeric(iencs[[pcl[1]]]$encode(x[, pcl[1]], val[1]) *
                           iencs[[pcl[2]]]$encode(x[, pcl[2]], val[2]))
      vsum <- sum(X[, m] * weights)
      if (vsum == 0) {
        ctp <- c(ifelse(orvs[pcl[1]] && val[1] > 1, m - 1, NA),
                 ifelse(orvs[pcl[1]] && val[1] < nval[1], m + 1, NA),
                 ifelse(orvs[pcl[2]] && val[2] > 1, m - nval[1], NA),
                 ifelse(orvs[pcl[2]] && val[2] < nval[2], m + nval[1], NA))
        ctp <- stats::na.omit(ctp)
        emp[[length(emp) + 1]] <-
          list(m = m, ctp = ctp, len = max(1, length(ctp)))
        next
      }
      M[ofs + val[1], m] <- vsum
      M[ofs + nval[1] + val[2], m] <- vsum
      Ddiag[m] <- sqrt(vsum)
      if (weighted.norm) {
        X[, m] <- X[, m] / Ddiag[m]
        M[, m] <- M[, m] / Ddiag[m]
      }
      dns[m] <- vsum / wsum
    }
    ofs <- ofs + nval[1] + nval[2]
  }
  ## additional constraints for columns filled with zero
  if ((nemp <- length(emp)) > 0) {
    Memp <- matrix(0, nrow = nemp, ncol = ncol)
    for (i in 1:nemp)
      Memp[i, emp[[i]]$m] <- 1
    M <- rbind(M, Memp)
  }
  ## ridge regularization
  nreg <- 0L
  if (lambda > 0) {
    nreg <- u + v
    R <- matrix(0, nrow = nreg, ncol = ncol)
    for (i in seq_len(nreg))
      R[i, fi + i] <- ifelse(weighted.norm, 1, Ddiag[fi + i])
    X <- rbind(X, R)
    Y <- c(Y, numeric(nreg))
    w <- c(w, rep.int(lambda, nreg))
  }

  # get the least squares solution --------
  if (expand.matrix) {
    X <- rbind(X, M)
    Y <- c(Y, numeric(nrow(M)))
    w <- c(w, rep.int(kappa, nrow(M)))
    r <- 0L
    if (method >= 0L) {
      z <- try(RcppEigen::fastLmPure(X * w, Y * w, method), silent = TRUE)
    }
    if (method < 0L || inherits(z, "try-error")) {
      method <- -1L
      z <- stats::lm.fit(X * w, Y * w)
    }
    beta <- z$coefficients
    rsd <- z$residuals[1:n] / w[1:n]
    crsd <- z$residuals[(n + nreg + 1L):(n + nreg + ncon)]
    if (any(abs(crsd) > nil)) {
      message("centralization is not strictly achieved")
    }
  } else {
    Msvd <- svd(M, nv = ncol)
    r <- sum(Msvd$d > tol)
    if (r == dim(Msvd$v)[2])
      stop("no coefficients to evaluate found")
    vr <- as.matrix(Msvd$v[, (r + 1):ncol])
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
    rsd <- z$residuals[1:n] / w[1:n]
  }
  if (!(method %in% 1:2) && z$rank < ncol - r) {
    if (!singular.ok)
      stop("singular fit encountered")
    message("singular fit encountered")
  }
  if (weighted.norm)
    beta <- beta / Ddiag
  if (interpolate.beta && nemp > 0) {
    bemp <- diag(1, ncol)
    for (i in 1:nemp) {
      bemp[emp[[i]]$m, emp[[i]]$ctp] <- 1
      bemp[emp[[i]]$m, emp[[i]]$m] <- - emp[[i]]$len
    }
    beta <- try(RcppEigen::fastLmPure(bemp, beta, 0L)$coefficients,
                silent = TRUE)
    if (inherits(beta, "try-error"))
      beta <- as.numeric(stats::lm.fit(Memp, beta)$coefficients)
  }
  beta[abs(beta) < nil] <- 0

  # summarize results of the decomposition --------
  ## intercept
  if(fit.intercept)
    intercept <- beta[1L]
  ## main effects
  if (me) {
    main.effects <- list()
    for (i in seq_len(p)) {
      dat <- mencs[[mts[i]]]$frame
      dat$density <- dns[(fi + mcumlens[i] + 1):(fi + mcumlens[i + 1])]
      dat$mid <- beta[(fi + mcumlens[i] + 1):(fi + mcumlens[i + 1])]
      main.effects[[mts[i]]] <- dat
    }
  }
  ## interactions
  if (ie) {
    interactions <- list()
    for (i in seq_len(q)) {
      pcl <- unlist(strsplit(its[i], ":"))
      nval <- c(ilens[[pcl[1]]], ilens[[pcl[2]]])
      vals <- expand.grid(1:nval[1], 1:nval[2])
      dat <- cbind(iencs[[pcl[1]]]$frame[vals[, 1], ],
                   iencs[[pcl[2]]]$frame[vals[, 2], ])
      dat$density <-
        dns[(fi + u + pcumlens[i] + 1):(fi + u + pcumlens[i + 1])]
      dat$mid <-
        beta[(fi + u + pcumlens[i] + 1):(fi + u + pcumlens[i + 1])]
      rownames(dat) <- NULL
      interactions[[its[i]]] <- dat
    }
  }

  # calculate the uninterpreted rate --------
  tot <- stats::weighted.mean((y - intercept) ^ 2, weights)
  uiq <- stats::weighted.mean(rsd ^ 2, weights)
  uir <- ifelse(abs(uir <- uiq / tot) < nil, 0, uir)

  # output the result --------
  obj <- list()
  class(obj) <- c("mid")
  cl[[1L]] <- as.name("interpret")
  obj$weights <- weights
  obj$call <- cl
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
  obj$fitted.matrix <- predict.mid(obj, x, na.action, type = "terms")
  obj$fitted.values <- predict.mid(obj, NULL, na.action, type = "response")
  obj$residuals <- rsd
  if (!is.null(link)) {
    obj$linear.predictors <- predict.mid(obj, NULL, na.action, type = "link")
    mu <- stats::weighted.mean(yres, weights)
    rtot <- stats::weighted.mean((yres - mu) ^ 2, weights)
    ruiq <- stats::weighted.mean((rr <- yres - obj$fitted.values) ^ 2, weights)
    ruir <- ifelse(abs(ruir <- ruiq / rtot) < nil, 0, ruir)
    obj$response.residuals <- as.numeric(rr)
    obj$uninterpreted.rate <- c(working = uir, response = ruir)
  }
  if (length(ids) < inn)
    obj$na.action <- structure((1L:inn)[-ids], class = naa.class)
  return(obj)
}

#'
#' @param formula a symbolic description of the decomposition model to be fit.
#' @param data a data frame containing the variables in the formula. If not found in data, the variables are taken from environment(formula).
#' @param model a model object to be interpreted.
#' @param subset an index vector specifying the rows to be used in the training sample.
#' @param drop.unused.levels logical. If TRUE, unused levels of factors will be dropped.
#'
#' @rdname interpret
#' @exportS3Method midr::interpret
#'
interpret.formula <- function(
    formula, data = NULL, model = NULL, pred.fun = get.yhat, weights = NULL,
    subset = NULL, na.action = getOption("na.action"),
    drop.unused.levels = FALSE, ...
) {
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  mf$... <- NULL
  mf$model <- NULL
  mf$pred.fun <- NULL
  if (!missing(model)) {
    if (is.null(data))
      stop("'data' is required to get predicted values from the 'model'")
    inn <- nrow(data)
    ids <- 1L:inn
    if (!is.data.frame(data))
      data <- as.data.frame(data)
    if (is.null(weights))
      weights <- attr(data, "weights")
    yvar <- ifelse("yhat" %in% colnames(data), ".yhat", "yhat")
    rvar <- deparse(formula[[2L]])
    data <- do.call(na.action, list(data[names(data) != rvar]))
    if (!is.null(naa.x <- stats::na.action(data))) {
      weights <- weights[-naa.x]
      ids <- ids[-naa.x]
      naa.class <- attr(naa.x, "class")
    }
    attr(data, "na.action") <- NULL
    yhat <- pred.fun(X.model = model, newdata = data)
    yhat <- do.call(na.action, list(yhat))
    if (anyNA(yhat))
      stop("NA values found in the model predictions")
    if (!is.null(naa.y <- stats::na.action(yhat))) {
      data <- data[-naa.y, ]
      weights <- weights[-naa.y]
      ids <- ids[-naa.y]
      naa.class <- attr(naa.y, "class")
    }
    attr(yhat, "na.action") <- NULL
    data[[yvar]] <- yhat
    formula[[2L]] <- as.name(yvar)
    cl$formula <- mf$formula <- formula
    mf$data <- data
    mf$weights <- weights
  } else {
    message("'model' is not passed: the response variable in the data is used.")
    inn <- NULL
    if (is.matrix(eval(mf$data)))
      mf$data <- as.data.frame(eval(mf$data))
    if (is.null(eval(mf$weights)))
      mf$weights <- attr(eval(mf$data), "weights")
  }
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if (is.null(inn)) {
    inn <- nrow(mf)
    ids <- 1L:inn
  }
  if (!is.null(naa.data <- stats::na.action(mf))) {
    ids <- ids[-naa.data]
    naa.class <- attr(naa.data, "class")
  }
  x <- structure(as.data.frame(mf), na.action = NULL)
  y <- stats::model.response(mf, "numeric")
  w <- as.vector(stats::model.weights(mf))
  tl <- attr(attr(mf, "terms"), "term.labels")
  ret <- interpret.default(model = model, x = x, y = y, weights = w,
                           terms = tl, na.action = na.action, ...)
  cl[[1L]] <- as.name("interpret")
  ret$call <- cl
  if (!is.null(naa.ret <- ret$na.action)) {
    ids <- ids[-naa.ret]
    naa.class <- attr(naa.ret, "class")
  }
  if (length(ids) < inn)
    ret$na.action <- structure((1L:inn)[-ids], class = naa.class)
  return(ret)
}
