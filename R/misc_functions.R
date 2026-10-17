#' Internal OLS Solver
#'
#' @description
#' An OLS solver utilized in \code{\link{interpret}()} supporting multiple computational methods.
#' It utilizes \code{\link[RcppEigen]{fastLmPure}()} for single-response variables to optimize speed independently of the environment, and uses base BLAS computations for multiple responses.
#'
#' @param x a design matrix.
#' @param y a numeric vector or matrix of the response variable.
#' @param tol the tolerance used in solvers. Defaults to \code{1e-7}.
#' @param method an integer or character string specifying the computational method. If a character string (e.g., \code{"FUNC"}) is passed, the function registered in the global option \code{"midr.solver.FUNC"} is used.
#' @param ... additional arguments.
#'
#' @returns
#' \code{solve_ols()} returns a list containing at least the \code{coefficients}, and potentially \code{residuals} or \code{rank} depending on the computational method.
#'
#' @keywords internal
#'
solve_ols <- function(x, y, tol = 1e-7, method = 0L, ...) {
  map <- c(
    qr = 0L, unpivoted.qr = 1L, llt = 2L, ldlt = 3L, svd = 4L, eigen = 5L
  )
  custom.solver <- NULL
  method <- method[1L]
  if (is.character(method)) {
    name <- method
    method <- if (any(name == names(map))) map[[name]] else -1L
    custom.solver <- getOption(paste0("midr.solver.", name))
  } else if (is.numeric(method)) {
    method <- as.integer(method)
    name <- names(map)[match(method, map)]
    if (is.na(name)) name <- as.character(method)
  } else {
    name <- "qr"
    method <- -1L
  }
  fmsg <- "falling back to 'qr' with stats::.lm.fit"
  if (!is.null(custom.solver) && is.function(custom.solver)) {
    z <- tryCatch(custom.solver(x, y), error = function(e) e)
    if (inherits(z, "error")) {
      emsg <- gsub("\n", " ", conditionMessage(z))
      msg <- sprintf("custom solver '%s' failed (%s): %s", name, emsg, fmsg)
      z <- stats::.lm.fit(x, y, tol = tol)
      return(structure(z, method = "qr", message = msg))
    } else if (is.null(z$coefficients)) {
      msg <- sprintf("custom solver '%s' did not return 'coefficients': %s",
                     name, fmsg)
      z <- stats::.lm.fit(x, y, tol = tol)
      return(structure(z, method = "qr", message = msg))
    } else {
      msg <- sprintf("custom solver '%s' is used", name)
      return(structure(z, method = name, message = msg))
    }
  }
  if (NCOL(y) == 1L && any(method == 0L:5L)) {
    return(structure(RcppEigen::fastLmPure(x, y, method), method = name))
  }
  z <- tryCatch(switch(
    name,
    "qr" = {
      stats::.lm.fit(x, y, tol = tol)
    }, "llt" = {
      XtX <- crossprod(x)
      XtY <- crossprod(x, y)
      R <- chol(XtX)
      coef <- backsolve(R, forwardsolve(t(R), XtY))
      list(coefficients = coef, residuals = y - x %*% coef)
    }, "svd" = {
      s <- svd(x)
      dinv <- ifelse(s$d > tol * s$d[1L], 1 / s$d, 0)
      coef <- s$v %*% (dinv * crossprod(s$u, y))
      list(coefficients = coef, residuals = y - x %*% coef, rank = sum(dinv > 0))
    },
    NULL
  ), error = function(e) e)
  if (is.null(z)) {
    mmsg <- if (NCOL(y) > 1L) " for matrix" else ""
    msg <- sprintf("method '%s' is not supported%s: %s", name, mmsg, fmsg)
    z <- stats::.lm.fit(x, y, tol = tol)
    return(structure(z, method = "qr", message = msg))
  }
  if (inherits(z, "error")) {
    emsg <- gsub("\n", " ", conditionMessage(z))
    msg <- sprintf("method '%s' failed (%s): %s", name, emsg, fmsg)
    z <- stats::.lm.fit(x, y, tol = tol)
    return(structure(z, method = "qr", message = msg))
  }
  return(structure(z, method = name))
}


# helpers for printing messages

verbose <- function(text, verbosity = 1L, level = 1L, timestamp = FALSE) {
  if (verbosity >= level) {
    ltag <- if (level >= 3L)
      "- [debug] " else if (level >= 2L) "[info] " else NULL
    stamp <- if (timestamp)
      format(Sys.time(), " (%Y-%m-%d %H:%M:%S)") else NULL
    text <- paste0(ltag, text, stamp)
    message(text, appendLF = getOption("midr.message.appendLF", TRUE))
  }
}


examples <- function(x, n = 3L, ...) {
  if (is.data.frame(x)) x <- as.matrix(x)
  dts <- if (length(x) > n) ", ..." else ""
  n <- min(length(x), n)
  paste0(paste(trimws(format(x[seq_len(n)], ...)), collapse = ", "), dts)
}


add_s <- function(n, noun, suffix = "s") {
  paste0(n, " ", noun, if (n > 1L) suffix)
}



# extract objects from fitted models

get_intercept <- function(object) {
  if (inherits(object, c("mid", "midrib"))) {
    object$intercept
  } else if (inherits(object, "mids")) {
    vapply(as.list(object), FUN = `[[`, FUN.VALUE = numeric(1L), "intercept")
  } else NULL
}


model_reframe <- function(object, data) {
  fm <- stats::formula(object)
  if (!is.data.frame(data) && !is.environment(data))
    data <- as.data.frame(data)
  args <- list(
    formula = fm,
    data = data,
    na.action = stats::na.pass,
    weights = stats::model.weights(data)
  )
  tryCatch(
    do.call(stats::model.frame.default, args),
    error = function(e) {
      args$formula[[2L]] <- NULL
      do.call(stats::model.frame.default, args)
    }
  )
}


model_data <- function(object, env = parent.frame()) {
  fcl <- object$call
  dat <- NULL
  if (!is.null(fcl$data)) {
    dat <- eval(fcl$data, envir = env)
  } else if (!is.null(fml <- fcl$formula)) {
    env <- environment(fml) %||% env
    dat <- stats::get_all_vars(fml, data = env)
  } else if (!is.null(fcl$x)) {
    x <- eval(fcl$x, envir = env)
    if (!is.null(ncol(x)) && is.null(colnames(x))) {
      colnames(x) <- paste0("x", seq_len(ncol(x)))
    }
    dat <- as.data.frame(x)
    if (!is.null(fcl$y)) {
      dat$y <- eval(fcl$y, envir = env)
    }
  }
  if (is.null(dat))
    return(NULL)
  if (!is.null(fcl$weights)) {
    wts <- eval(fcl$weights, envir = dat, enclos = env)
    if (!is.null(wts))
      dat[["(weigths)"]] <- wts
  }
  if (!is.null(fcl$subset)) {
    idx <- eval(fcl$subset, envir = dat, enclos = env)
    if (!is.null(idx))
      dat <- dat[idx, , drop = FALSE]
  }
  if (!is.null(object$na.action)) {
    dat <- dat[-object$na.action, , drop = FALSE]
  }
  dat
}

# other misc helpers

clean_names <- function(x, prefix = "X", sep = "") {
  m <- ncol(x)
  nms <- colnames(x) %||% rep.int(prefix, m)
  ng <- (nms == "" | is.na(nms))
  if (any(ng)) nms[ng] <- prefix
  nms <- stats::ave(
    nms, nms,
    FUN = function(v) if (length(v) > 1L) paste(v, seq_along(v), sep = sep) else v
  )
  make.unique(nms, sep = sep)
}


attract <- function(x, margin) {
  x[abs(x) <= margin] <- 0
  x
}


is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}


hcl_palette <- function(
    n, direction = 1L, alpha = NULL, chroma = 100, luminance = 65
) {
  if (n < 1L)
    return(character(0L))
  hues <- seq(0, 360, length.out = n + 1)[seq_len(n)] %% 360
  if (direction < 0L)
    hues <- rev(hues)
  grDevices::hcl(h = hues, c = chroma, l = luminance, alpha = alpha)
}
