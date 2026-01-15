#' Fit MID Models
#'
#' @description
#' \code{interpret()} is used to fit a Maximum Interpretation Decomposition (MID) model.
#' MID models are additive, highly interpretable models composed of functions, each with up to two variables.
#'
#' @details
#' The MID model approximates a target model's prediction function \eqn{f(\mathbf{x})}, or values of the response variable \eqn{\mathbf{y}}.
#' This model, denoted as \eqn{\mathcal{F}(\mathbf{x})}, has the following structure: \deqn{\mathcal{F}(\mathbf{x}) = f_\phi + \sum_{j} f_{j}(x_j) + \sum_{j<k} f_{jk}(x_j, x_k)}
#' where \eqn{f_\phi} is the intercept, \eqn{f_{j}(x_j)} is the main effect of feature \eqn{j}, and \eqn{f_{jk}(x_j, x_k)} is the second-order interaction effect between features \eqn{j} and \eqn{k}.
#'
#' To ensure that the decomposed components are unique, they are fitted under the \emph{centering constraints}: each main effect's average is constrained to be zero, and each interaction effect's conditional averages are also constrained to be zero.
#' The model is fitted by minimizing the squared error between the target, \eqn{f(\mathbf{x})} or \eqn{\mathbf{y}}, and the surrogate \eqn{\mathcal{F}(\mathbf{x})}, which is typically evaluated on a representative dataset.
#'
#' @section Advanced Fitting Options:
#' The \code{...} argument can be used to pass several advanced fitting options:
#' \describe{
#'   \item{fit.intercept}{logical. If \code{TRUE}, the intercept term is fitted as part of the least squares problem. If \code{FALSE} (default), it is calculated as the weighted mean of the response.}
#'   \item{interpolate.beta}{a character string specifying the method for interpolating unestimable coefficients (betas) that arise from sparse data regions. Can be "iterative" for an iterative smoothing process, "direct" for solving a linear system, or "none" to disable interpolation.}
#'   \item{maxit}{an integer specifying the maximum number of iterations for the "iterative" interpolation method.}
#'   \item{weighted.norm}{logical. If \code{TRUE}, the columns of the design matrix are normalized by the square root of their weighted sum. This is required to ensure the minimum-norm least squares solution obtained by appropriate methods (i.e., \code{4} or \code{5}) of \code{fastLmPure()} is the minimum-norm solution in a \emph{weighted} sense.}
#'   \item{weighted.encoding}{logical. If \code{TRUE}, sample weights are used during the encoding process (e.g., for calculating quantiles to determine knots).}
#' }
#'
#' @param object a fitted model object to be interpreted.
#' @param ... optional arguments. For \code{interpret.formula()}, arguments to be passed on to \code{interpret.default()}. For \code{interpret.default()}, \code{...} can include convenient aliases (e.g., "ok" for \code{singular.ok}, "ie" for \code{interactions}) as well as several advanced fitting options (see the "Advanced Fitting Options" section for details).
#'
#' @examples
#' # Fit a MID model as a surrogate for another model
#' data(cars, package = "datasets")
#' model <- lm(dist ~ I(speed^2) + speed, cars)
#' mid <- interpret(dist ~ speed, cars, model)
#' plot(mid, "speed", intercept = TRUE)
#' points(cars)
#'
#' # Fit a MID model as a standalone predictive model
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ .^2, data = airquality, lambda = .5)
#' plot(mid, "Wind")
#' plot(mid, "Temp")
#' plot(mid, "Wind:Temp", main.effects = TRUE)
#'
#' data(Nile, package = "datasets")
#' nile <- data.frame(time = 1:length(Nile), flow = as.numeric(Nile))
#'
#' # A flexible fit with many knots
#' mid <- interpret(flow ~ time, data = nile, k = 100L)
#' plot(mid, "time", intercept = TRUE, limits = c(600L, 1300L))
#' points(x = 1L:100L, y = Nile)
#'
#' # A smoother fit with fewer knots
#' mid <- interpret(flow ~ time, data = nile, k = 10L)
#' plot(mid, "time", intercept = TRUE, limits = c(600L, 1300L))
#' points(x = 1L:100L, y = Nile)
#'
#' # A pseudo-smoothed fit using a penalty
#' mid <- interpret(flow ~ time, data = nile, k = 100L, lambda = 100L)
#' plot(mid, "time", intercept = TRUE, limits = c(600L, 1300L))
#' points(x = 1L:100L, y = Nile)
#' @returns
#' \code{interpret()} returns an object of class "mid". This is a list with the following components:
#' \item{weights}{a numeric vector of the sample weights.}
#' \item{call}{the matched call.}
#' \item{terms}{the \code{\link[stats]{terms.object}} used.}
#' \item{link}{a "link-glm" or "link-midr" object containing the link function.}
#' \item{intercept}{the intercept.}
#' \item{encoders}{a list of variable encoders.}
#' \item{main.effects}{a list of data frames representing the main effects.}
#' \item{interacions}{a list of data frames representing the interactions.}
#' \item{ratio}{the ratio of the sum of squared error between the target model predictions and the fitted MID values, to the sum of squared deviations of the target model predictions.}
#' \item{linear.predictors}{a numeric vector of the linear predictors.}
#' \item{fitted.values}{a numeric vector of the fitted values.}
#' \item{residuals}{a numeric vector of the working residuals.}
#' \item{na.action}{information about the special handling of \code{NA}s.}
#'
#' @seealso \code{\link{print.mid}}, \code{\link{summary.mid}}, \code{\link{predict.mid}}, \code{\link{plot.mid}}, \code{\link{ggmid}}, \code{\link{mid.plots}}, \code{\link{mid.effect}}, \code{\link{mid.terms}}, \code{\link{mid.importance}}, \code{\link{mid.conditional}}, \code{\link{mid.breakdown}}
#'
#' @references Asashiba R, Kozuma R, Iwasawa H (2025). “midr: Learning from Black-Box Models by Maximum Interpretation Decomposition.” 2506.08338, \url{https://arxiv.org/abs/2506.08338}.
#'
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
#' @param k an integer or a vector of two integers specifying the maximum number of sample points for main effects (\code{k[1]}) and interactions (\code{k[2]}). If a single integer is provided, it is used for main effects while the value for interactions is automatically determined. Any \code{NA} value will also trigger this automatic determination. With non-positive values, all unique data points are used as sample points.
#' @param type an integer or integer-valued vector of length two. The type of encoding. The effects of quantitative variables are modeled as piecewise linear functions if \code{type} is \code{1}, and as step functions if \code{type} is \code{0}. If a vector is passed, \code{type[1L]} is used for main effects and \code{type[2L]} is used for interactions.
#' @param frames a named list of encoding frames ("numeric.frame" or "factor.frame" objects). The encoding frames are used to encode the variable of the corresponding name. If the name begins with "|" or ":", the encoding frame is used only for main effects or interactions, respectively.
#' @param interactions logical. If \code{TRUE} and if \code{terms} and \code{formula} are not supplied, all interactions for each pair of variables are modeled and calculated.
#' @param terms a character vector of term labels or formula, specifying the set of component functions to be modeled. If not passed, \code{terms} includes all main effects, and all second-order interactions if \code{interactions} is \code{TRUE}.
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
#' @param max.nelements an integer specifying the maximum number of elements of the design matrix. Defaults to \code{1e9}.
#' @param nil a threshold for the intercept and coefficients to be treated as zero. The default is \code{1e-7}.
#' @param tol a tolerance for the singular value decomposition. The default is \code{1e-7}.
#' @param pred.args optional parameters other than the fitted model and new data to be passed to \code{pred.fun()}.
#' @exportS3Method midr::interpret
#'
interpret.default <- function(
    object, x, y = NULL, weights = NULL, pred.fun = get.yhat, link = NULL,
    k = c(NA, NA), type = c(1L, 1L), frames = list(), interactions = FALSE,
    terms = NULL, singular.ok = FALSE, mode = 1L, method = NULL, lambda = 0,
    kappa = 1e6, na.action = getOption("na.action"), verbosity = 1L,
    encoding.digits = 3L, use.catchall = FALSE, catchall = "(others)",
    max.nelements = 1e9L, nil = 1e-7, tol = 1e-7, pred.args = list(), ...
) {
  cl <- match.call()
  cl[[1L]] <- as.name("interpret")
  dots <- list(...)
  if (is.null(dots$internal.call) || !dots$internal.call)
    verbose("model fitting started", verbosity, 2L, TRUE)
  if (missing(interactions) && !is.null(dots$ie)) interactions <- dots$ie
  if (missing(singular.ok) && !is.null(dots$ok)) singular.ok <- dots$ok
  fit.intercept <- ifnot.null(dots$fit.intercept, FALSE)
  interpolate.beta <- ifnot.null(dots$interpolate.beta, TRUE)
  maxit <- ifnot.null(dots$maxit, 1e4L)
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
    if (!is.null(link))
      yres <- yres[-naa.y]
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
  n <- nrow(x)
  if (n == 0L)
    stop("no observations found")
  wsum <- sum(weights)
  nuvs <- sapply(x, is.numeric)
  orvs <- nuvs | sapply(x, is.ordered)
  if (is.null(terms)) {
    mts <- tags
    its <- NULL
    if (interactions)
      its <- utils::combn(mts, 2L, function(x) paste0(x, collapse = ":"))
  } else {
    if (inherits(terms, "formula")) {
      terms <- attr(stats::terms(terms), "term.labels")
    }
    spl <- strsplit(terms, ":")
    if (!all(unique(unlist(spl)) %in% tags)) {
      stop("'terms' contains term labels that are not found in 'x'")
    }
    spl <- sapply(spl, length)
    mts <- unique(terms[spl == 1L])
    its <- unique(terms[spl == 2L])
  }
  terms <- c(mts, its)
  p <- length(mts)
  q <- length(its)
  verbose(text = paste0(collapse = "",
    c("'terms' include ", if (p) c(p, " main effect", if (p > 1L) "s"),
    if (p > 0L && q > 0L) " and ", if (q) c(q, " interaction", if (q > 1L) "s"))
  ), verbosity, 3L)
  if (length(k) == 1L)
    k <- c(k, NA)
  k[2L] <- ifnot.null(dots$k2, k[2L])
  if (is.na(k[1L]))
    k[1L] <- min(25L, max(2L, if (lambda > 0) 25L else n %/% (p + q)))
  if (is.na(k[2L]))
    k[2L] <- min(5L, max(2L, if (lambda > 0) 5L else floor(sqrt(n / (p + q)))))
  verbose(text = paste0(collapse = "",
    c("'k' is set to ", if (p > 0L) c(k[1L], " for main effects"),
    if (p > 0L && q > 0L) " and ", if (q > 0L) c(k[2L], " for interactions"))
    ), verbosity, 3L)
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
          numeric.encoder(x = x[[tag]], k = k[1L], type = type[1L], tag = tag,
                          encoding.digits = encoding.digits, frame = f(tag, 1L),
                          weights = if (weighted.encoding) weights)
        } else {
          factor.encoder(x = x[[tag]], k = k[1L], use.catchall = use.catchall,
                         catchall = catchall, tag = tag, frame = f(tag, 1L),
                         weights = if (weighted.encoding) weights)
        }
      mmats[[tag]] <- mencs[[tag]]$encode(x[[tag]])
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
          numeric.encoder(x = x[[tag]], k = k[2L], type = type[2L], tag = tag,
                          encoding.digits = encoding.digits, frame = f(tag, 2L),
                          weights = if (weighted.encoding) weights)
        } else {
          factor.encoder(x = x[[tag]], k = k[2L], use.catchall = use.catchall,
                         catchall = catchall, tag = tag, frame = f(tag, 2L),
                         weights = if (weighted.encoding) weights)
        }
      imats[[tag]] <- iencs[[tag]]$encode(x[[tag]])
    }
    ilens <- sapply(iencs, function(x) x$n)
    plens <- structure(integer(q), names = its)
    for (i in seq_len(q)) {
      itag <- term.split(its[i])
      plens[i] <- ilens[itag[1L]] * ilens[itag[2L]]
      mi <- mi + ilens[itag[1L]] + ilens[itag[2L]]
    }
    pcumlens <- structure(cumsum(c(0L, plens)), names = c(its, NA))
    v <- pcumlens[length(pcumlens)] # total number of unique pairs
  }
  # create the design matrix and the constraints matrix --------
  fi <- as.integer(fit.intercept)
  ncol <- fi + u + v
  ncon <- p + mi
  tot.elements <- n * ncol
  if (!is.null(max.nelements) && tot.elements > max.nelements) {
    title <- sprintf("estimated design matrix size: %.2f GB (%d elements)",
                     tot.elements * 8 / (1024 ^ 3), tot.elements)
    if (verbosity < 1L)
      stop(title)
    choices <- c("exit", "continue")
    sel <- try(utils::select.list(choices, title = title), silent = TRUE)
    if (inherits(sel, "try-error") || sel == "exit")
      stop("number of elements in the design matrix exceeded 'max.nelements'")
  }
  X <- matrix(0, nrow = n, ncol = ncol)
  M <- matrix(0, nrow = ncon, ncol = ncol)
  Y <- y
  w <- sqrt(weights)
  D <- rep.int(1, ncol)
  dens <- numeric(ncol)
  vnil <- logical(ncol)
  lnil <- list()
  lreg <- list()
  ## intercept
  if (fit.intercept) {
    D[1L] <- sqrt(wsum)
    X[, 1L] <- if (weighted.norm) 1 / D[1L] else 1
  } else {
    intercept <- stats::weighted.mean(Y, weights)
    intercept <- attract(intercept, nil)
    Y <- Y - intercept
  }
  ## main effects
  for (i in seq_len(p)) {
    mtag <- mts[i]
    cols <- fi + mcumlens[i] + seq_len(mlens[i])
    Xsub <- mmats[[i]]
    vsum <- colSums(Xsub * weights)
    D[cols] <- sqrt(vsum)
    vfil <- (vsum != 0)
    vnil[cols] <- !vfil
    dens[cols] <- vsum / wsum
    if (any(vfil)) {
      if (weighted.norm) {
        X[, cols[vfil]] <- sweep(
          Xsub[, vfil, drop = FALSE], 2L, STATS = D[cols[vfil]], FUN = "/"
        ) * w
        M[i, cols[vfil]] <- D[cols[vfil]]
      } else {
        X[, cols[vfil]] <- Xsub[, vfil, drop = FALSE] * w
        M[i, cols[vfil]] <- vsum[vfil]
      }
    }
    ordr <- orvs[mtag]
    for (j in seq_len(mlens[i])) {
      m <- cols[j]
      if (vnil[m]) {
        lnil[[length(lnil) + 1L]] <- c(
          m,
          if (ordr && j > 1L) m - 1L,
          if (ordr && j < mlens[[i]]) m + 1L
        )
      } else if (lambda > 0 && ordr) {
        lreg[[length(lreg) + 1L]] <- c(
          m,
          if (j > 1L) m - 1L,
          if (j < mlens[[i]]) m + 1L
        )
      }
    }
  }
  ## interactions
  ofset <- p
  for (i in seq_len(q)) {
    itag <- term.split(its[i])
    cols <- fi + u + pcumlens[i] + seq_len(plens[i])
    nval <- ilens[itag]
    Xsub <- (
      imats[[itag[1L]]][, rep(seq_len(nval[1L]), times = nval[2L]), drop = FALSE] *
      imats[[itag[2L]]][, rep(seq_len(nval[2L]), each = nval[1L]), drop = FALSE]
    )
    vsum <- colSums(Xsub * weights)
    D[cols] <- sqrt(vsum)
    vfil <- (vsum != 0)
    vnil[cols] <- !vfil
    dens[cols] <- vsum / wsum
    if (any(vfil)) {
      if (weighted.norm) {
        X[, cols[vfil]] <- sweep(
          Xsub[, vfil, drop = FALSE], 2L, D[cols[vfil]], "/"
        ) * w
      } else {
        X[, cols[vfil]] <- Xsub[, vfil, drop = FALSE] * w
      }
    }
    ordr <- orvs[itag]
    for (j in seq_len(plens[i])) {
      m <- cols[j]
      npos <- c((j - 1) %% nval[1L] + 1L, (j - 1) %/% nval[1L] + 1L)
      if (vfil[j]) {
        value <- if (weighted.norm) vsum[j] / D[m] else vsum[j]
        M[ofset + npos[1L], m] <- value
        M[ofset + nval[1L] + npos[2L], m] <- value
      }
      if (vnil[m]) {
        lnil[[length(lnil) + 1L]] <- c(
          m,
          if (ordr[1L] && npos[1L] > 1L) m - 1L,
          if (ordr[1L] && npos[1L] < nval[1L]) m + 1L,
          if (ordr[2L] && npos[2L] > 1L) m - nval[1L],
          if (ordr[2L] && npos[2L] < nval[2L]) m + nval[1L]
        )
      } else if (lambda > 0) {
        if (ordr[1L])
          lreg[[length(lreg) + 1L]] <- c(
            m,
            if (npos[1L] > 1L) m - 1L,
            if (npos[1L] < nval[1L]) m + 1L
          )
        if (ordr[2L])
          lreg[[length(lreg) + 1L]] <- c(
            m,
            if (npos[2L] > 1L) m - nval[1L],
            if (npos[2L] < nval[2L]) m + nval[1L]
          )
      }
    }
    ofset <- ofset + nval[1L] + nval[2L]
  }
  ## ridge regularization
  nreg <- 0L
  if (lambda > 0) {
    nreg <- length(lreg)
    wreg <- rep.int(sqrt(lambda), nreg)
    R <- matrix(0, nrow = nreg, ncol = ncol)
    for (i in seq_len(nreg)) {
      a <- lreg[[i]][-1L]
      a <- a[!vnil[a]]
      if (length(a) == 0L)
        next
      m <- lreg[[i]][1L]
      R[i, a] <- - (if (weighted.norm) 1 / D[a] else 1)
      R[i, m] <- (if (weighted.norm) 1 / D[m] else 1) * length(a)
      wreg[i] <- wreg[i] * D[m]
    }
    X <- rbind(X, R)
    Y <- c(Y, numeric(nreg))
    w <- c(w, wreg)
  }
  ## additional constraints for columns filled with zero
  nnil <- length(lnil)
  if (nnil > 0L) {
    Mnil <- matrix(0, nrow = nnil, ncol = ncol)
    for (i in 1L:nnil)
      Mnil[i, lnil[[i]][1L]] <- 1
    M <- rbind(M, Mnil)
  }
  # clean up RAM --------
  remove(Xsub)
  if (!is.null(max.nelements) && tot.elements > max.nelements / 10) {
    verbose(paste0("collecting garbage..."), verbosity, 3L, FALSE)
    gc(verbose = FALSE, full = FALSE)
  }
  # get the least squares solution --------
  verbose(paste0("least squares estimation initiated with 'mode' ", mode,
                 " and 'method' ", method), verbosity, 2L, FALSE)
  verbose(paste0(
    ncol, " parameters", if (nnil > 0L) paste0(" (", nnil, " unestimables)"),
    ", ", n, " observations, ", ncon, " centering constraints",
    if (nreg > 0L) paste0(", ", nreg, " smoothing constraints")
  ), verbosity, 3L, FALSE)
  if (mode == 1L) {
    X <- rbind(X, M * sqrt(kappa))
    Y <- c(Y, numeric(nrow(M)))
    w <- c(w, rep.int(sqrt(kappa), nrow(M)))
    r <- 0L
    z <- if (method >= 0L) {
      try(RcppEigen::fastLmPure(X, Y * w, method), silent = TRUE)
    } else {
      try(stats::lm.fit(X, Y * w))
    }
    if (inherits(z, "try-error"))
      stop("failed to solve the least squares problem")
    beta <- z$coefficients
    beta[is.na(beta)] <- 0
    crsd <- z$residuals[(n + nreg + 1L):(n + nreg + ncon)]
    if (any(abs(crsd) > (nil * sqrt(kappa) * wsum))) {
      verbose(paste0("not strictly centered: max absolute average effect = ",
                     format(max(abs(crsd)) / sqrt(kappa) / wsum, digits = 6L)),
              verbosity, level = 1L)
    }
  } else if (mode == 2L) {
    Msvd <- svd(M, nv = ncol)
    r <- sum(Msvd$d > tol)
    if (r == dim(Msvd$v)[2L])
      stop("no coefficients to evaluate found")
    vr <- as.matrix(Msvd$v[, (r + 1L):ncol])
    z <- if (method >= 0L) {
      try(RcppEigen::fastLmPure(X %*% vr, Y * w, method), silent = TRUE)
    } else {
      try(stats::lm.fit(X %*% vr, Y * w))
    }
    if (inherits(z, "try-error"))
      stop("failed to solve the least squares problem")
    coef <- z$coefficients
    coef[is.na(coef)] <- 0
    beta <- as.numeric(vr %*% coef)
  } else {
    stop("'mode' must be 1 or 2")
  }
  if (!(any(method == 1L:2L)) && z$rank < ncol - r) {
    if (!singular.ok) {
      title <- "singular fit encountered"
      if (verbosity < 1L)
        stop(title)
      choices <- c("exit", "continue")
      sel <- try(utils::select.list(choices, title = title), silent = TRUE)
      if (inherits(sel, "try-error") || sel == "exit")
        stop("execution halted: singular fit encountered")
    }
    verbose("singular fit encountered", verbosity, level = 1L)
  }
  lnil <- lnil[vapply(lnil, length, 0L) > 1L]
  nnil <- length(lnil)
  gamma <- beta
  if (weighted.norm)
    beta[!vnil] <- beta[!vnil] / D[!vnil]
  if (!(interpolate.beta == "none" || isFALSE(interpolate.beta)) && nnil > 0L) {
    verbose("interpolating unestimable parameters...",
            verbosity, 3L)
    if (interpolate.beta == "iterative" || isTRUE(interpolate.beta)) {
      midx <- vapply(lnil, `[`, 0, 1L)
      aidx <- lapply(lnil, `[`, -1)
      pntr <- cumsum(c(1L, vapply(aidx, length, 0)))
      aidx <- unlist(aidx)
      res <- cpp_interpolate_beta(beta, midx, aidx, pntr, tol, maxit)
      verbose(text = paste0(
        "interpolation ", if (res$converged) "converged" else "stopped",
        " after ", res$iter," iterations"), verbosity, 3L)
      beta <- res$beta
    } else {
      B <- diag(1, ncol)
      for (i in seq_len(nnil)) {
        a <- lnil[[i]][-1L]
        m <- lnil[[i]][1L]
        B[m, a] <- - 1
        B[m, m] <- length(a)
      }
      beta <- try(RcppEigen::fastLmPure(B, beta, 0L)$coefficients, silent = TRUE)
      if (inherits(beta, "try-error")) {
        verbose("'RcppEigen::fastLmPure' failed: 'lm.fit' is used", verbosity, 1L)
        beta <- as.numeric(stats::lm.fit(B, beta)$coefficients)
      }
    }
    beta[is.na(beta)] <- 0
  }
  indices <- (abs(beta) <= nil)
  gamma[indices] <- 0
  beta[indices] <- 0
  verbose("least squares estimation completed", verbosity, 2L, FALSE)
  # summarize results of the decomposition --------
  ## intercept
  if(fit.intercept)
    intercept <- beta[1L]
  lp <- rep(intercept, n)
  ## main effects
  if (me) {
    ret.main.effects <- list()
    for (i in seq_len(p)) {
      dat <- mencs[[mts[i]]]$frame
      indices <- (fi + mcumlens[i] + 1L):(fi + mcumlens[i + 1L])
      dat$density <- dens[indices]
      dat$mid <- beta[indices]
      ret.main.effects[[mts[i]]] <- dat
      lp <- lp + as.numeric(mmats[[mts[i]]] %*% beta[indices])
    }
  }
  ## interactions
  if (ie) {
    ret.interactions <- list()
    for (i in seq_len(q)) {
      itag <- term.split(its[i])
      nval <- c(ilens[[itag[1L]]], ilens[[itag[2L]]])
      vals <- expand.grid(1L:nval[1L], 1L:nval[2L])
      dat <- cbind(iencs[[itag[1L]]]$frame[vals[, 1L], ],
                   iencs[[itag[2L]]]$frame[vals[, 2L], ])
      rownames(dat) <- NULL
      indices <- (fi + u + pcumlens[i] + 1L):(fi + u + pcumlens[i + 1L])
      dat$density <- dens[indices]
      dat$mid <- beta[indices]
      ret.interactions[[its[i]]] <- dat
      A1 <- imats[[itag[1L]]]
      A2 <- imats[[itag[2L]]]
      W <- matrix(beta[indices], nrow = nval[1L], ncol = nval[2L])
      lp <- lp + rowSums((A1 %*% W) * A2)
    }
  }
  # output the result --------
  obj <- list()
  class(obj) <- c("mid")
  obj$model.class <- attr(object, "class")
  obj$call <- cl
  obj$terms <- stats::terms(make.formula(terms, "..y", env = globalenv()))
  obj$link <- link
  obj$intercept <- intercept
  obj$encoders <- list()
  if (me) {
    obj$main.effects <- ret.main.effects
    obj$encoders[["main.effects"]] <- mencs
  }
  if (ie) {
    obj$interactions <- ret.interactions
    obj$encoders[["interactions"]] <- iencs
  }
  obj$weights <- weights
  obj$fitted.values <- lp
  obj$residuals <- y - obj$fitted.values
  tot <- stats::weighted.mean((y - intercept) ^ 2, weights)
  uiq <- stats::weighted.mean(obj$residuals ^ 2, weights)
  uir <- attract(uiq / tot, nil)
  verbose(paste0("uninterpreted variation ratio: ", format(uir)), verbosity, 3L)
  obj$ratio <- uir
  if (!is.null(link)) {
    obj$linear.predictors <- lp
    obj$fitted.values <- link$linkinv(lp)
    obj$response.residuals <- yres - obj$fitted.values
    mu <- stats::weighted.mean(yres, weights)
    tot <- stats::weighted.mean((yres - mu) ^ 2, weights)
    uiq <- stats::weighted.mean(obj$response.residuals ^ 2, weights)
    uir <- attract(uiq / tot, nil)
    verbose(paste0("uninterpreted variation ratio (response): ",
                   format(uir)), verbosity, 3L)
    obj$ratio <- c(working = obj$ratio, response = uir)
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
  use.yhat <- !is.null(model) && !is.null(pred.fun)
  ystr <- if (use.yhat) "predictions" else "response variable"
  if (use.yhat) {
    y <- do.call(pred.fun, c(list(model, data), pred.args))
    verbose(paste0(length(y), " predictions obtained from 'model': ",
                   examples(y, 3L)), verbosity, 3L)
  }
  if (is.matrix(data))
    data <- as.data.frame(data)
  args <- list(formula = formula, data = data, subset = numeric())
  ftry <- try(do.call(stats::model.frame.default, args), silent = TRUE)
  if (inherits(ftry, "try-error")) {
    formula[[2L]] <- NULL
    args$formula <- formula
  }
  args$subset <- eval(substitute(subset), data, parent.frame())
  args$na.action <- na.action
  args$drop.unused.levels <- drop.unused.levels
  if (use.yhat) args$.yhat <- y
  args$weights <- eval(substitute(weights), data, parent.frame())
  if (is.null(args$weights)) args$weights <- attr(data, "weights")
  data <- do.call(stats::model.frame.default, args)
  naa <- na.action(data)
  n <- nrow(data) + length(naa)
  naai <- list(n.init = n, ids = seq_len(n))
  if (!is.null(naa)) {
    verbose(paste(length(naa), "observations with NAs in 'data' are omitted"),
            verbosity, 3L)
    naai$ids <- naai$ids[-naa]
    attr(data, "na.action") <- NULL
  }
  verbose(paste("model frame with", nrow(data), "observations created"),
          verbosity, 3L)
  if (!use.yhat) {
    verbose(paste0(if (is.null(model)) "'model'" else "'pred.fun'",
                   " not passed: response variable in 'data' is used"),
            verbosity, level = 1L)
    y <- stats::model.response(data, "any")
    if (is.null(y))
      stop("response variable can't be extracted from 'data'")
  } else {
    y <- data[["(.yhat)"]]
    data[["(.yhat)"]] <- NULL
  }
  if (anyNA(y))
    stop("NA values found in ", ystr)
  attr(y, "na.action") <- NULL
  weights <- stats::model.weights(data)
  data[["(weights)"]] <- NULL
  mt <- attr(data, "terms")
  tl <- attr(mt, "term.labels")
  ret <- interpret.default(object = model, x = data, y = y, weights = weights,
                           terms = tl, mode = mode, na.action = na.action,
                           verbosity = verbosity, internal.call = TRUE, ...)
  cl$formula <- formula
  ret$call <- cl
  ret$terms <- mt
  if (!is.null(naa.ret <- ret$na.action))
    naai$ids <- naai$ids[-naa.ret]
  if (length(naai$ids) < naai$n.init) {
    naacl <- attr(attr(do.call(na.action, list(NA)), "na.action"), "class")
    ret$na.action <- structure((1L:naai$n.init)[-naai$ids], class = naacl)
  }
  verbose("model fitting successfully finished", verbosity, 2L, TRUE)
  return(ret)
}
