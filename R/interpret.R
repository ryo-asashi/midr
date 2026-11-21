# R/interpret.R

#' Fit MID Models
#'
#' @description
#' \code{interpret()} is used to fit a Maximum Interpretation Decomposition (MID) model.
#' MID models are additive, highly interpretable models composed of functions, each with up to two variables.
#'
#' @details
#' Maximum Interpretation Decomposition (MID) is a functional decomposition framework designed to serve as a faithful surrogate for complex, black-box models.
#' It deconstructs a target prediction function \eqn{f(\mathbf{X})} into a set of highly interpretable components:
#'
#' \deqn{f(\mathbf{X}) = g_\emptyset + \sum_{j} g_j(X_j) + \sum_{j<k} g_{jk}(X_j, X_k) + g_D(\mathbf{X})}
#'
#' where \eqn{g_\emptyset} is the intercept, \eqn{g_j(X_j)} represents the main effect of feature \eqn{j}, \eqn{g_{jk}(X_j, X_k)} represents the second-order interaction between features \eqn{j} and \eqn{k}, and \eqn{g_D(\mathbf{X})} is the residual.
#'
#' The components \eqn{g_j} and \eqn{g_{jk}} are modeled as a linear expansion of basis functions, resulting in piecewise linear or piecewise constant functions.
#' The estimation is performed by minimizing a penalized squared residual objective over a representative dataset:
#'
#' \deqn{\text{minimize } \mathbf{E}[g_D(\mathbf{X})^2] + \lambda R(g;\mathbf{X})}
#'
#' where \eqn{\lambda \ge 0} is a regularization parameter that controls the smoothness of the components by penalizing the second-order differences of adjacent coefficients (a discrete roughness penalty).
#'
#' To ensure the uniqueness and identifiability of the decomposition, MID imposes the centering constraints: for any feature \eqn{j}, \eqn{\mathbf{E}[g_j(X_j)] = 0}; and for any feature pair \eqn{(j, k)}, \eqn{\mathbf{E}[g_{jk}(X_j, X_k) \mid X_j = x_j] = 0} for all \eqn{x_j} and \eqn{\mathbf{E}[g_{jk}(X_j, X_k) \mid X_k = x_k] = 0} for all \eqn{x_k}.
#' In cases where the least-squares solution is still not unique due to collinearity, an additional probability-weighted minimum-norm constraint is applied to the coefficients to ensure a stable and unique solution.
#'
#' @section Advanced Fitting Options:
#' The \code{...} argument can be used to pass several advanced fitting options:
#' \describe{
#'   \item{fit.intercept}{logical. If \code{TRUE}, the intercept term is fitted as part of the least squares problem. If \code{FALSE} (default), it is calculated as the weighted mean of the response.}
#'   \item{interpolation}{a character string specifying the method for interpolating inestimable coefficients (betas) that arise from sparse data regions. Can be "iterative" for an iterative smoothing process, "direct" for solving a linear system, or "none" to disable interpolation.}
#'   \item{max.niterations}{an integer specifying the maximum number of iterations for the "iterative" interpolation method.}
#'   \item{save.memory}{an integer (0, 1, or 2) specifying the memory-saving level. Higher values reduce memory usage at the cost of increased computation time.}
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
#' \item{interactions}{a list of data frames representing the interactions.}
#' \item{ratio}{the ratio of the sum of squared error between the target model predictions and the fitted MID values, to the sum of squared deviations of the target model predictions.}
#' \item{linear.predictors}{a numeric vector of the linear predictors.}
#' \item{fitted.values}{a numeric vector of the fitted values.}
#' \item{residuals}{a numeric vector of the working residuals.}
#' \item{na.action}{information about the special handling of \code{NA}s.}
#'
#' If a matrix is provided for \code{y}, \code{interpret()} returns a "midrib" and "mids" object.
#'
#' @seealso \code{\link{print.mid}}, \code{\link{summary.mid}}, \code{\link{predict.mid}}, \code{\link{plot.mid}}, \code{\link{ggmid}}, \code{\link{eval_importance}}, \code{\link{eval_conditional}}, \code{\link{eval_breakdown}}
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
#' @param y an optional vector or matrix of the model predictions or the response variables.
#' @param weights a numeric vector of sample weights for each observation in \code{x}.
#' @param pred.fun a function to obtain predictions from a fitted model, where the first argument is for the fitted model and the second argument is for new data. The default is \code{get_yhat()}.
#' @param link a character string specifying the link function. This can be one of the links from \code{\link[stats]{make.link}()} (e.g., "log", "logit", "probit", "cauchit"), one of the links from \code{\link{make_link}()} (e.g., "log1p", "robit", "scobit", "box-cox"), or a custom object containing at least the \code{linkfun()} and \code{linkinv()} functions.
#' @param k an integer or a vector of two integers specifying the maximum number of sample points for main effects (\code{k[1]}) and interactions (\code{k[2]}). If a single integer is provided, it is used for main effects while the value for interactions is automatically determined. Any \code{NA} value will also trigger this automatic determination. With non-positive values, all unique data points are used as sample points.
#' @param type a character string, an integer, or a vector of length two specifying the encoding type. Can be integer (\code{1} for linear, \code{0} for step) or character (\code{"linear"}, \code{"constant"}). If a vector is passed, \code{type[1L]} is used for main effects and \code{type[2L]} is used for interactions.
#' @param encoders a named list of atomic vectors or "encoder" objects. Atomic vectors are automatically converted into encoders, and each encoder is used to encode the variable of the corresponding name. If a nested list named \code{main.effects} or \code{interactions} is provided, the enclosed encoders are used exclusively for main effects or interactions, respectively.
#' @param interactions logical. If \code{TRUE} and if \code{terms} and \code{formula} are not supplied, all interactions for each pair of variables are modeled and calculated.
#' @param terms a character vector of term labels or formula, specifying the set of component functions to be modeled. If not passed, \code{terms} includes all main effects, and all second-order interactions if \code{interactions} is \code{TRUE}.
#' @param singular.ok logical. If \code{FALSE}, a singular fit is an error.
#' @param mode an integer specifying the method of calculation. If \code{mode} is \code{1}, the centering constraints are treated as penalties for the least squares problem. If \code{mode} is \code{2}, the constraints are used to reduce the number of free parameters.
#' @param method an integer or a character string specifying the algorithm to solve the core least squares problem. Built-in options include \code{0} or "qr" (column-pivoted QR), \code{1} or "unpivoted.qr", \code{2} or "llt" (LLT Cholesky), \code{3} or "ldlt" (LDLT Cholesky), \code{4} or "svd" (singular value decomposition), and \code{5} or "eigen" (eigenvalue-eigenvector decomposition). For multi-response targets (matrix \code{y}), the computation automatically utilizes base R equivalents or safely falls back to "qr" with \code{stats::.lm.fit()}. External custom solvers can also be injected by setting \code{options(midr.solver.<method_name> = function(x, y) ...)}.
#' @param lambda the penalty factor for pseudo smoothing. The default is \code{0}.
#' @param kappa the penalty factor for centering constraints. Used only when \code{mode} is \code{1}. The default is \code{1e+6}.
#' @param na.action a function or character string specifying the method of \code{NA} handling. The default is "na.omit".
#' @param verbosity the level of verbosity. \code{0}: fatal, \code{1}: warning (default), \code{2}: info or \code{3}: debug.
#' @param split a character string specifying the splitting strategy for numeric variables: \code{"quantile"} or \code{"uniform"}.
#' @param digits an integer. The rounding digits for encoding numeric variables. Used only when \code{type} is \code{1} or \code{"linear"}.
#' @param lump a character string specifying the lumping strategy for factor variables: \code{"none"}, \code{"rank"}, \code{"order"}, or \code{"auto"}.
#' @param others a character string specifying the others level.
#' @param sep a character string used to separate levels when merging ordered factors or creating interaction terms.
#' @param max.nelements an integer specifying the maximum number of elements of the design matrix. Defaults to \code{1e9}.
#' @param nil a threshold for the intercept and coefficients to be treated as zero. The default is \code{1e-7}.
#' @param tol a tolerance for the singular value decomposition. The default is \code{1e-7}.
#' @param pred.args optional parameters other than the fitted model and new data to be passed to \code{pred.fun()}.
#' @exportS3Method midr::interpret
#'
interpret.default <- function(
    object, x = NULL, y = NULL, weights = NULL, pred.fun = get_yhat, link = NULL,
    k = c(NA, NA), type = c(1L, 1L), interactions = FALSE, terms = NULL,
    singular.ok = FALSE, mode = 1L, method = NULL, lambda = 0, kappa = 1e6,
    na.action = getOption("na.action"), verbosity = NULL, encoders = list(),
    split = "quantile", digits = NULL, lump = "none", others = "others", sep = ">",
    max.nelements = 1e9L, nil = 1e-7, tol = 1e-7, pred.args = list(), ...
) {
  cl <- match.call()
  cl[[1L]] <- as.name("interpret")
  dots <- list(...)
  verbosity <- verbosity %||% getOption("midr.verbosity", 1L)
  internal_call <- isTRUE(dots$internal.call)
  if (!internal_call) {
    verbose("model fitting started", verbosity, 2L, TRUE)
  }

  # normalize aliases, advanced options, and missing inputs at the boundary
  if (missing(object)) object <- NULL
  if (missing(interactions)) interactions <- dots$ie %||% interactions
  if (missing(singular.ok)) singular.ok <- dots$ok %||% singular.ok
  if (length(k) == 1L) k <- c(k, NA)
  if (!is.null(dots$k2)) k[2L] <- dots$k2
  if (length(type) == 1L) type <- c(type, type)
  if (!any(mode == 1L:2L)) {
    verbose("invalid 'mode' found: defaulted to 1", verbosity, 3L, FALSE)
    mode <- 1L
  }
  method <- method %||% (if (!singular.ok) 0L else 5L)
  fit.intercept <- dots$fit.intercept %||% FALSE
  interpolation <- dots$interpolation %||% "iterative"
  max.niterations <- dots$max.niterations %||% 1e4L
  save.memory <- dots$save.memory %||% 1L
  weighted.norm <- dots$weighted.norm %||% singular.ok
  weighted.encoding <- dots$weighted.encoding %||% FALSE
  penalize.edges <- !isFALSE(dots$penalize.edges)

  if (!is.null(object) && is.null(x)) {
    x <- try(model_data(object, env = parent.frame()), silent = TRUE)
    if (inherits(x, "try-error")) {
      stop("'x' could not be extracted from the model object: please provide it directly")
    }
    terms <- terms %||% tryCatch(
      get_variables(object, raw = TRUE), error = function(e) NULL
    )
  }
  if (missing(weights)) {
    weights <- attr(x, "weights")
  }

  prep <- preprocess_data(
    object = object,
    x = x,
    y = y,
    weights = weights,
    pred.fun = pred.fun,
    pred.args = pred.args,
    link = link,
    na.action = na.action,
    verbosity = verbosity
  )

  meta <- prepare_metadata(
    x = prep$x,
    weights = prep$weights,
    terms = terms,
    interactions = interactions,
    k = k,
    type = type,
    encoders = encoders,
    split = split,
    digits = digits,
    lump = lump,
    others = others,
    sep = sep,
    lambda = lambda,
    mode = mode,
    fit.intercept = fit.intercept,
    save.memory = save.memory,
    weighted.encoding = weighted.encoding,
    nil = nil,
    max.nelements = max.nelements,
    verbosity = verbosity
  )

  dm <- build_design_matrix(
    x = prep$x,
    y = prep$y,
    weights = prep$weights,
    meta = meta,
    fit.intercept = fit.intercept,
    mode = mode,
    lambda = lambda,
    kappa = kappa,
    weighted.norm = weighted.norm,
    penalize.edges = penalize.edges
  )

  if (!is.null(max.nelements) && meta$nelements > max.nelements / 10) {
    verbose("collecting garbage ...", verbosity, 3L, FALSE)
    gc(verbose = FALSE, full = FALSE)
  }

  fit <- solve_mid(
    dm = dm,
    meta = meta,
    mode = mode,
    method = method,
    singular.ok = singular.ok,
    weighted.norm = weighted.norm,
    interpolation = interpolation,
    max.niterations = max.niterations,
    fit.intercept = fit.intercept,
    kappa = kappa,
    nil = nil,
    tol = tol,
    verbosity = verbosity
  )
  delt <- dm$delt
  intercept <- dm$intercept
  dm <- NULL

  obj <- assemble_object(
    object = object,
    cl = cl,
    prep = prep,
    meta = meta,
    fit = fit,
    delt = delt,
    intercept = intercept,
    na.action = na.action,
    verbosity = verbosity
  )
  if (!internal_call) {
    verbose("model fitting successfully finished", verbosity, 2L, TRUE)
  }
  obj
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
    formula, data = NULL, model = NULL, pred.fun = get_yhat, weights = NULL,
    subset = NULL, na.action = getOption("na.action"), verbosity = NULL,
    mode = 1L, drop.unused.levels = FALSE, pred.args = list(), ...
) {
  verbosity <- verbosity %||% getOption("midr.verbosity", 1L)
  verbose("model fitting started", verbosity, 2L, TRUE)
  cl <- match.call()
  cl[[1L]] <- as.symbol("interpret")
  use.yhat <- !is.null(model) && !is.null(pred.fun)
  ystr <- if (use.yhat) "predictions" else "response variable"
  if (use.yhat) {
    data <- data %||% model_data(model, env = parent.frame())
    y <- do.call(pred.fun, c(list(model, data), pred.args))
    if (is.matrix(y) || is.data.frame(y)) {
      verbose(paste0(nrow(y), " x ", ncol(y), " predictions obtained from 'model': ",
                     examples(y, 3L)), verbosity, 3L)
    } else {
      verbose(paste0(length(y), " predictions obtained from 'model': ",
                     examples(y, 3L)), verbosity, 3L)
    }
  }
  mf <- match.call(expand.dots = FALSE)
  m <- match(
    c("formula", "data", "subset", "weights", "na.action", "drop.unused.levels"),
    names(mf), nomatch = 0L
  )
  mf <- mf[c(1L, m)]
  if (!is.null(data)) {
    if (is.matrix(data)) data <- as.data.frame(data)
    mf$data <- data
  }
  if (use.yhat) {
    mf$.yhat <- y
  }
  if (is.null(mf$weights) && !is.null(attr(data, "weights"))) {
    mf$weights <- attr(data, "weights")
  }
  mf[[1L]] <- quote(stats::model.frame)
  data <- try(eval(mf, envir = parent.frame()), silent = TRUE)
  if (inherits(data, "try-error")) {
    f <- stats::as.formula(mf$formula)
    if (use.yhat && length(f) == 3L) {
      f[[2L]] <- NULL
      mf$formula <- f
      data <- eval(mf, envir = parent.frame())
    } else {
      stop(attr(data, "condition")$message, call. = FALSE)
    }
  }
  naa <- stats::na.action(data)
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
  ret <- interpret.default(object = model, x = data, y = y, weights = weights,
                           terms = mt, mode = mode, na.action = na.action,
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
  ret
}
