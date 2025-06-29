#' Fit MID Models
#'
#' \code{interpret()} is used to fit a MID model specifically as an interpretable surrogate for black-box predictive models.
#' A fitted MID model consists of a set of component functions, each with up to two variables.
#'
#' \code{interpret()} returns a global surrogate model of the target predictive model.
#' The prediction function of this surrogate model is derived from Maximum Interpretation
#' Decomposition (MID) applied to the prediction function of the target model
#' (denoted \eqn{f(\mathbf{x})}).
#'
#' The prediction function of the global surrogate model, denoted \eqn{\mathcal{F}(\mathbf{x})}, has the following structure:
#' \deqn{\mathcal{F}(\mathbf{x}) = f_\phi + \sum_{j} f_{j}(x_j) + \sum_{j<k} f_{jk}(x_j, x_k)}
#' where \eqn{f_\phi} is the intercept, \eqn{f_{j}(x_j)} is the main effect of feature \eqn{j},
#' and \eqn{f_{jk}(x_j, x_k)} is the second-order interaction effect between features \eqn{j} and \eqn{k}.
#'
#' To ensure the identifiability (uniqueness) of these decomposed components, they are subject to centering constraints during the fitting process.
#' Specifically, each main effect function \eqn{f_j(x_j)} is constrained such that its average over the data distribution of feature \eqn{X_j} is zero.
#' Similarly, each second-order interaction effect function \eqn{f_{jk}(x_j, x_k)} is constrained such that its conditional average over \eqn{X_j} (for any fixed value \eqn{x_k}) is zero, and its conditional average over \eqn{X_k} (for any fixed value \eqn{x_j}) is also zero.
#'
#' The surrogate model is fitted using the least squares method, which minimizes the squared error between the predictions of the target model \eqn{f(\mathbf{x})} and the surrogate model \eqn{\mathcal{F}(\mathbf{x})} (typically evaluated on a representative dataset).
#'
#' @param object a fitted model object to be interpreted.
#' @examples
#' # fit a MID model as a surrogate model
#' data(cars, package = "datasets")
#' model <- lm(dist ~ I(speed^2) + speed, cars)
#' mid <- interpret(dist ~ speed, cars, model)
#' plot(mid, "speed", intercept = TRUE)
#' points(cars)
#'
#' # customize the flexibility of a MID model
#' data(Nile, package = "datasets")
#' mid <- interpret(x = 1L:100L, y = Nile, k = 100L)
#' plot(mid, "x", intercept = TRUE, limits = c(600L, 1300L))
#' points(x = 1L:100L, y = Nile)
#' # reduce the number of knots by setting the 'k' parameter
#' mid <- interpret(x = 1L:100L, y = Nile, k = 10L)
#' plot(mid, "x", intercept = TRUE, limits = c(600L, 1300L))
#' points(x = 1L:100L, y = Nile)
#' # perform a pseudo smoothing by setting the 'lambda' parameter
#' mid <- interpret(x = 1L:100L, y = Nile, k = 100L, lambda = 100L)
#' plot(mid, "x", intercept = TRUE, limits = c(600L, 1300L))
#' points(x = 1L:100L, y = Nile)
#'
#' # fit a MID model as a predictive model
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, na.omit(airquality), lambda = .4)
#' plot(mid, "Wind")
#' plot(mid, "Temp")
#' plot(mid, "Wind:Temp", theme = "RdBu")
#' plot(mid, "Wind:Temp", main.effects = TRUE)
#' @returns
#' \code{interpret()} returns a "mid" object with the following components:
#' \item{weights}{a numeric vector of the sample weights.}
#' \item{call}{the matched call.}
#' \item{terms}{the term labels.}
#' \item{link}{a "link-glm" or "link-midr" object containing the link function.}
#' \item{intercept}{the intercept.}
#' \item{encoders}{a list of variable encoders.}
#' \item{main.effects}{a list of data frames representing the main effects.}
#' \item{interacions}{a list of data frames representing the interactions.}
#' \item{ratio}{the ratio of the sum of squared error between the target model predictions and the fitted MID values, to the sum of squared deviations of the target model predictions.}
#' \item{fitted.matrix}{a matrix showing the breakdown of the predictions into the effects of the component functions.}
#' \item{linear.predictors}{a numeric vector of the linear predictors.}
#' \item{fitted.values}{a numeric vector of the fitted values.}
#' \item{residuals}{a numeric vector of the working residuals.}
#' \item{na.action}{information about the special handlings of \code{NA}s.}
#' @export interpret
#'
interpret <- function(object, ...)
UseMethod("interpret")

#'
#' @rdname interpret
#' @param x a matrix or data.frame of predictor variables to be used in the fitting process. The response variable should not be included.
#' @param y an optional numeric vector of the model predictions or the response variable.
#' @param weights a numeric vector of sample weights for each observation in \code{x}.
#' @param pred.fun a function to obtain predictions from a fitted model, where the first argument is for the fitted model and the second argument is for new data. The default is \code{get.yhat()}.
#' @param link a character string specifying the link function: one of "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse", "translogit", "transprobit", "identity-logistic" and "identity-gaussian", or an object containing two functions \code{linkfun()} and \code{linkinv()}. See \code{help(make.link)}.
#' @param k an integer or integer-valued vector of length two. The maximum number of sample points for each variable. If a vector is passed, \code{k[1L]} is used for main effects and \code{k[2L]} is used for interactions. If an integer is passed, \code{k} is used for main effects and \code{sqrt(k)} is used for interactions. If not positive, all unique values are used as sample points.
#' @param type an integer or integer-valued vector of length two. The type of encoding. The effects of quantitative variables are modeled as piecewise linear functions if \code{type} is \code{1}, and as step functions if \code{type} is \code{0}. If a vector is passed, \code{type[1L]} is used for main effects and \code{type[2L]} is used for interactions.
#' @param frames a named list of encoding frames ("numeric.frame" or "factor.frame" objects). The encoding frames are used to encode the variable of the corresponding name. If the name begins with "|" or ":", the encoding frame is used only for main effects or interactions, respectively.
#' @param interaction logical. If \code{TRUE} and if \code{terms} and \code{formula} are not supplied, all interactions for each pair of variables are modeled and calculated.
#' @param terms a character vector of term labels specifying the set of component functions to be modeled. If not passed, \code{terms} includes all main effects, and all interactions if \code{interaction} is \code{TRUE}.
#' @param singular.ok logical. If \code{FALSE}, a singular fit is an error.
#' @param mode an integer specifying the method of calculation. If \code{mode} is \code{1}, the centralization constraints are treated as penalties for the least squares problem. If \code{mode} is \code{2}, the constraints are used to reduce the number of free parameters.
#' @param method an integer specifying the method to be used to solve the least squares problem. A non-negative value will be passed to \code{RcppEigen::fastLmPure()}. If negative, \code{stats::lm.fit()} is used.
#' @param lambda the penalty factor for pseudo smoothing. The default is \code{0}.
#' @param kappa the penalty factor for centering constraints. Used only when \code{mode} is \code{1}. The default is \code{1e+6}.
#' @param na.action a function or character string specifying the method of \code{NA} handling. The default is "na.omit".
#' @param verbosity the level of verbosity. \code{0}: fatal, \code{1}: warning (default), \code{2}: info or \code{3}: debug.
#' @param encoding.digits an integer. The rounding digits for encoding numeric variables. Used only when \code{type} is \code{1}.
#' @param use.catchall logical. If \code{TRUE}, less frequent levels of qualitative variables are dropped and replaced by the catchall level.
#' @param catchall a character string specifying the catchall level.
#' @param max.ncol integer. The maximum number of columns of the design matrix.
#' @param nil a threshold for the intercept and coefficients to be treated as zero. The default is \code{1e-7}.
#' @param tol a tolerance for the singular value decomposition. The default is \code{1e-7}.
#' @param pred.args optional parameters other than the fitted model and new data to be passed to \code{pred.fun()}.
#' @param ... for \code{interpret.default()}, optional arguments can be provided, including \code{fit.intercept}, \code{interpolate.beta}, \code{weighted.norm}, and \code{weighted.encoding}. Special character aliases are also supported, such as \code{ok} for \code{singular.ok} and \code{ie} for \code{interaction}. For \code{interpret.formula()}, any arguments to be passed on to \code{interpret.default()}.
#' @exportS3Method midr::interpret
#'
interpret.default <- function(
    object, x, y = NULL, weights = NULL, pred.fun = get.yhat, link = NULL,
    k = c(NA, NA), type = c(1L, 1L), frames = list(), interaction = FALSE,
    terms = NULL, singular.ok = FALSE, mode = 1L, method = NULL, lambda = 0,
    kappa = 1e6, na.action = getOption("na.action"), verbosity = 1L,
    encoding.digits = 3L, use.catchall = FALSE, catchall = "(others)",
    max.ncol = 1e4L, nil = 1e-7, tol = 1e-7, pred.args = list(), ...
) {
  cl <- match.call()
  dots <- list(...)
  if (is.null(dots$internal.call) || !dots$internal.call)
    verbose("model fitting started", verbosity, 2L, TRUE)
  if (missing(interaction) && !is.null(dots$ie)) interaction <- dots$ie
  if (missing(singular.ok) && !is.null(dots$ok)) singular.ok <- dots$ok
  fit.intercept <- ifnot.null(dots$fit.intercept, FALSE)
  interpolate.beta <- ifnot.null(dots$interpolate.beta, TRUE)
  weighted.norm <- ifnot.null(dots$weighted.norm, singular.ok)
  weighted.encoding <- ifnot.null(dots$weighted.encoding, FALSE)
  # preprocess data --------
  if (missing(object)) object <- NULL
  if (missing(weights)) weights <- attr(x, "weights")
  if (!is.matrix(x) && !is.data.frame(x)) {
    x <- try(as.data.frame(x), silent = TRUE)
    if (inherits(x, "try-error"))
      stop("'x' must be a matrix or data.frame, or an object that can be converted to a data.frame")
  }
  if (is.null(colnames(x)))
    colnames(x) <- paste0("x", seq_len(ncol(x)))
  if (any("mid" == colnames(x)))
    stop("'mid' can't be used as a column name")
  tags <- colnames(x)
  naai <- list(n.init = nrow(x), ids = seq_len(nrow(x)))
  attr(x, "na.action") <- NULL
  x <- do.call(na.action, list(x))
  if (!is.null(naa.x <- stats::na.action(x))) {
    verbose(paste(length(naa.x), "observations with NAs in 'x' are omitted"),
            verbosity, 3L)
    y <- y[-naa.x]
    weights <- weights[-naa.x]
    naai$ids <- naai$ids[-naa.x]
    attr(x, "na.action") <- NULL
  }
  if (is.null(y)) {
    y <- try(do.call(pred.fun, c(list(object, x), pred.args)), silent = TRUE)
    if (inherits(y, "try-error"))
      stop("'y' is not supplied and the model prediction failed")
    verbose(paste0(length(y), " predictions obtained from 'object': ",
                   examples(y, 3L)), verbosity, 3L)
  }
  if (length(y) != nrow(x))
    stop("length of 'y' doesn't match the number of rows in 'x'")
  if (is.character(y))
    y <- as.factor(y)
  if (is.factor(y))
    y <- (y != levels(y)[1L])
  if (!is.numeric(y))
    y <- as.numeric(y)
  if (!is.null(link)){
    if (is.character(link))
      link <- get.link(link)
    yres <- y
    y <- link$linkfun(y)
    verbose(paste0("'y' values are transformed by 'link': ", examples(y, 3L)),
            verbosity, 3L)
  }
  attr(y, "na.action") <- NULL
  y <- do.call(na.action, list(y))
  if (anyNA(y))
    stop("NA values in 'y' are not allowed")
  if (!is.null(naa.y <- stats::na.action(y))) {
    verbose(paste(length(naa.y), "NA values in 'y' are omitted"), verbosity, 3L)
    x <- x[-naa.y, , drop = FALSE]
    weights <- weights[-naa.y]
    naai$ids <- naai$ids[-naa.y]
    attr(y, "na.action") <- NULL
  }
  if (any(is.infinite(y)))
    stop("'Inf' and '-Inf' in 'y' are not allowed")
  if (is.null(weights))
    weights <- rep.int(1, nrow(x))
  if (!is.numeric(weights) || anyNA(weights))
    stop("'weights' must be a numeric vector without missing values")
  if (length(weights) != nrow(x))
    stop("length of 'weights' doesn't match the number of rows in 'x'")
  if (any(weights < 0))
    stop("negative weights not allowed")
  if (is.matrix(x)) x <- as.data.frame(x)
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
  verbose(sprintf("'terms' include %s main effect%s and %s interaction%s",
                  length(mts), if (length(mts) != 1L) "s" else "", length(its),
                  if (length(its) != 1L) "s" else ""), verbosity, 3L)
  n <- nrow(x)
  if (n == 0L)
    stop("no observations found")
  p <- length(mts)
  q <- length(its)
  if (length(k) == 1L)
    k <- c(k, ceiling(sqrt(max(k, 0L))))
  if (is.na(k[1L]))
    k[1L] <- min(25L, max(2L, if (lambda > 0) 25L else n %/% (p + q)))
  if (is.na(k[2L]))
    k[2L] <- min(5L, max(2L, if (lambda > 0) 5L else floor(sqrt(n / (p + q)))))
  verbose(sprintf("'k' is set to %s for main effects and %s for interactions",
                  k[1L], k[2L]), verbosity, 3L)
  if (length(type) == 1L)
    type <- c(type, type)
  f <- function(tag, d) {
    ifnot.null(frames[[paste0(switch(d, "|", ":"), tag)]], frames[[tag]])
  }
  if (is.null(method))
    method <- if (!singular.ok) 0L else 5L
  if (!singular.ok && any(method == 1L:2L))
    verbose("when 'method' is set to 1 or 2, singular fits cannot be detected",
            verbosity, level = 1L)
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
  if (!is.null(max.ncol) && ncol > max.ncol) {
    title <- paste0("number of columns of the design matrix (", ncol,
                    ") exceeded 'max.ncol' (", max.ncol, ")")
    if (verbosity < 1L)
      stop(title)
    choices <- c("exit", "continue")
    sel <- try(utils::select.list(choices, title = title), silent = TRUE)
    if (inherits(sel, "try-error") || sel == "exit")
      stop("execution halted")
  }
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
  verbose(paste0("least squares estimation initiated with 'mode' ", mode,
                 " and 'method' ", method), verbosity, 2L, FALSE)
  verbose(paste0(
    ncol, " parameters", if (nemp > 0L) paste0(" (", nemp, " unestimables)"),
    ", ", n, " observations, ", ncon, " centering constraints",
    if (nreg > 0L) paste0(", ", nreg, " smoothing constraints")
  ), verbosity, 3L, FALSE)
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
      verbose(paste0("not strictly centered: max absolute average effect = ",
                     format(max(abs(crsd)) / sqrt(kappa) / wsum, digits = 6L)),
              verbosity, level = 1L)
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
    if (inherits(z, "try-error")) {
      verbose("'RcppEigen::fastLmPure' failed: 'lm.fit' is used", verbosity, 1L)
      method <- -1L
    }
    if (method < 0L)
      z <- stats::lm.fit((X * w) %*% vr, Y * w)
    coef <- z$coefficients
    coef[is.na(coef)] <- 0
    beta <- as.numeric(vr %*% coef)
    rsd <- z$residuals[1L:n] / w[1L:n]
  }
  if (!(any(method == 1L:2L)) && z$rank < ncol - r) {
    if (!singular.ok) {
      title <- "singular fit encountered"
      if (verbosity < 1L)
        stop(title)
      choices <- c("exit", "continue")
      sel <- try(utils::select.list(choices, title = title), silent = TRUE)
      if (inherits(sel, "try-error") || sel == "exit")
        stop("execution halted")
    }
    verbose("singular fit encountered", verbosity, level = 1L)
  }
  if (weighted.norm) {
    gamm <- beta
    beta <- beta / D
  }
  lemp <- lemp[vapply(lemp, length, 0L) > 1L]
  nemp <- length(lemp)
  if (interpolate.beta && nemp > 0L) {
    verbose("interpolating unestimable parameters lacking observation weights",
            verbosity, 3L)
    B <- diag(1, ncol)
    for (i in seq_len(nemp)) {
      a <- lemp[[i]][-1L]
      m <- lemp[[i]][1L]
      B[m, a] <- - 1
      B[m, m] <- length(a)
    }
    beta <- try(RcppEigen::fastLmPure(B, beta, 0L)$coefficients, silent = TRUE)
    if (inherits(beta, "try-error")) {
      verbose("'RcppEigen::fastLmPure' failed: 'lm.fit' is used", verbosity, 1L)
      beta <- as.numeric(stats::lm.fit(B, beta)$coefficients)
    }
    beta[is.na(beta)] <- 0
  }
  beta[abs(beta) <= nil] <- 0
  verbose("least squares estimation completed", verbosity, 2L, FALSE)
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
      xmat <- X[seq_len(n), lt:rt, drop = FALSE]
      mult <- if (!weighted.norm) dat$mid else gamm[lt:rt]
      fm[, p + i] <- as.numeric(xmat %*% mult)
    }
  }
  # calculate the uninterpreted variation ratio --------
  tot <- stats::weighted.mean((y - intercept) ^ 2, weights)
  uiq <- stats::weighted.mean(rsd ^ 2, weights)
  uir <- attract(uiq / tot, nil)
  verbose(paste0("uninterpreted variation ratio: ", format(uir)), verbosity, 3L)
  # output the result --------
  obj <- list()
  class(obj) <- c("mid")
  obj$model.class <- attr(object, "class")
  cl[[1L]] <- as.name("interpret")
  obj$weights <- weights
  obj$call <- cl
  obj$terms <- terms
  obj$link <- link
  obj$intercept <- intercept
  obj$encoders <- list()
  if (me) {
    obj$main.effects <- main.effects
    obj$encoders[["main.effects"]] <- mencs
  }
  if (ie) {
    obj$interactions <- interactions
    obj$encoders[["interactions"]] <- iencs
  }
  obj$ratio <- uir
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
    verbose(paste0("uninterpreted variation ratio (response): ", format(ruir)),
            verbosity, 3L)
    obj$ratio <- c(working = uir, response = ruir)
  }
  if (length(naai$ids) < naai$n.init) {
    naacl <- attr(attr(do.call(na.action, list(NA)), "na.action"), "class")
    obj$na.action <- structure((1L:naai$n.init)[-naai$ids], class = naacl)
  }
  if (is.null(dots$internal.call) || !dots$internal.call)
    verbose("model fitting successfully finished", verbosity, 2L, TRUE)
  return(obj)
}

#'
#' @rdname interpret
#' @param formula a symbolic description of the MID model to be fit.
#' @param data a data.frame, list or environment containing the variables in \code{formula}. If not found in data, the variables are taken from \code{environment(formula)}.
#' @param model a fitted model object to be interpreted.
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process.
#' @param drop.unused.levels logical. If \code{TRUE}, unused levels of factors will be dropped.
#' @exportS3Method midr::interpret
#'
interpret.formula <- function(
    formula, data = NULL, model = NULL, pred.fun = get.yhat, weights = NULL,
    subset = NULL, na.action = getOption("na.action"), verbosity = 1L,
    mode = 1L, drop.unused.levels = FALSE, pred.args = list(), ...
) {
  verbose("model fitting started", verbosity, 2L, TRUE)
  cl <- match.call()
  cl[[1L]] <- as.symbol("interpret")
  mf <- match.call(expand.dots = FALSE)
  mf[c("model", "pred.fun", "verbosity", "mode", "pred.args", "...")] <- NULL
  mf[[1L]] <- quote(stats::model.frame.default)
  env <- ifnot.null(environment(formula), parent.frame())
  if (!missing(model)) {
    if (is.null(data))
      data <- environment(formula)
    if (!is.data.frame(data) && !is.matrix(data))
      data <- stats::model.frame(formula, data, na.action = stats::na.pass)
    if (is.null(weights))
      weights <- attr(data, "weights")
    naai <- list(n.init = nrow(data), ids = seq_len(nrow(data)))
    attr(data, "na.action") <- NULL
    data <- do.call(na.action, list(data))
    if (!is.null(naa.x <- stats::na.action(data))) {
      verbose(paste(length(naa.x), "observations with NAs in 'data' are omitted"),
              verbosity, 3L)
      weights <- weights[-naa.x]
      naai$ids <- naai$ids[-naa.x]
      attr(data, "na.action") <- NULL
    }
    y <- do.call(pred.fun, c(list(model, data), pred.args))
    verbose(paste0(length(y), " predictions obtained from 'model': ",
                   examples(y, 3L)), verbosity, 3L)
    attr(y, "na.action") <- NULL
    y <- do.call(na.action, list(y))
    if (anyNA(y))
      stop("NA values found in the model predictions")
    if (!is.null(naa.y <- stats::na.action(y))) {
      verbose(paste(length(naa.y), "NA values in predictions are omitted"),
              verbosity, 3L)
      data <- data[-naa.y, , drop = FALSE]
      weights <- weights[-naa.y]
      naai$ids <- naai$ids[-naa.y]
      attr(y, "na.action") <- NULL
    }
    if (!is.data.frame(data))
      data <- as.data.frame(data)
    ftest <- try(stats::model.frame.default(formula, data[NULL, ]),
                 silent = TRUE)
    if (inherits(ftest, "try-error")) {
      formula[[2L]] <- NULL
      mf$formula <- formula
    }
    mf$data <- data
    mf$weights <- weights
    mf <- eval(mf, envir = env)
    ytag <- if (any(colnames(data) == "yhat")) "predicted" else "yhat"
    if (length(formula) == 2L)
      formula[[3L]] <- formula[[2L]]
    formula[[2L]] <- as.symbol(ytag)
    cl$formula <- formula
  } else {
    verbose("'model' not passed: response variable in 'data' is used",
            verbosity, level = 1L)
    if (length(formula) < 3L)
      stop("invalid formula found")
    if (is.matrix(data))
      mf$data <- as.data.frame(data)
    if (is.null(weights))
      mf$weights <- attr(data, "weights")
    mf <- eval(mf, envir = env)
    if (!is.null(naa.data <- stats::na.action(mf)))
      verbose(paste(length(naa.data), "observations with NA in 'data' are omitted"),
              verbosity, 3L)
    naai <- list(n.init = nrow(mf) + length(naa.data))
    naai$ids <- seq_len(naai$n.init)
    if (!is.null(naa.data))
      naai$ids <- naai$ids[-naa.data]
    y <- stats::model.response(mf, "any")
  }
  w <- as.vector(stats::model.weights(mf))
  attr(mf, "na.action") <- NULL
  tl <- attr(attr(mf, "terms"), "term.labels")
  ret <- interpret.default(object = model, x = mf, y = y, weights = w,
                           terms = tl, mode = mode, na.action = na.action,
                           verbosity = verbosity, internal.call = TRUE, ...)
  ret$call <- cl
  if (!is.null(naa.ret <- ret$na.action))
    naai$ids <- naai$ids[-naa.ret]
  if (length(naai$ids) < naai$n.init) {
    naacl <- attr(attr(do.call(na.action, list(NA)), "na.action"), "class")
    ret$na.action <- structure((1L:naai$n.init)[-naai$ids], class = naacl)
  }
  verbose("model fitting successfully finished", verbosity, 2L, TRUE)
  return(ret)
}
