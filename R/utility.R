# R/utillity.R

# utility functions

`%||%` <- function(x, y) if (is.null(x)) y else x


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

add_s <- function(n, noun, suffix = "s") {
  paste0(n, " ", noun, if (is.numeric(n) && n != 1L) suffix)
}

examples <- function(x, n = 3L, ...) {
  if (is.data.frame(x)) x <- as.matrix(x)
  dts <- if (length(x) > n) ", ..." else ""
  n <- min(length(x), n)
  paste0(paste(trimws(format(x[seq_len(n)], ...)), collapse = ", "), dts)
}


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


interaction_frame <- function(x, y) {
  if (!is.data.frame(x) && !is.matrix(x))
    x <- as.matrix(x)
  if (!is.data.frame(y) && !is.matrix(y))
    y <- as.matrix(y)
  nx <- nrow(x)
  ny <- nrow(y)
  z <- data.frame(
    x[rep(seq_len(nx), times = ny), , drop = FALSE],
    y[rep(seq_len(ny), each = nx), , drop = FALSE],
    row.names = NULL, check.names = FALSE
  )
  attributes(z) <- list(
    names = names(z), row.names = attr(z, "row.names"), class = "data.frame"
  )
  z
}

# extract objects from fitted models ----

get_intercept <- function(object) {
  if (inherits(object, c("mid", "midrib"))) {
    object$intercept
  } else if (inherits(object, "mids")) {
    vapply(as.list(object), FUN = `[[`, FUN.VALUE = numeric(1L), "intercept")
  } else NULL
}


model_reframe <- function(object, data, na.action = NULL) {
  fm <- stats::formula(object)
  naa <- stats::na.action(data) %||% stats::na.action(object)
  wts <- stats::model.weights(data)
  if (!is.data.frame(data) && !is.environment(data)) {
    data <- as.data.frame(data)
  }
  args <- list(
    formula = fm,
    data = data,
    na.action = stats::na.pass
  )
  out <- tryCatch(
    do.call(stats::model.frame.default, args),
    error = function(e) {
      args$formula[[2L]] <- NULL
      do.call(stats::model.frame.default, args)
    }
  )
  if (!is.null(wts)) {
    out[["(weights)"]] <- wts
  }
  if (!is.null(na.action)) {
    out <- do.call(na.action, list(out))
  } else if (!is.null(naa)) {
    attr(out, "na.action") <- naa
  }
  out
}


model_data <- function(object, env = parent.frame(), na.action = NULL) {
  fcl <- stats::getCall(object)
  dat <- NULL
  if (!is.null(fcl$data)) {
    dat <- eval(fcl$data, envir = env)
  } else if (!is.null(fcl$formula)) {
    fml <- stats::formula(object)
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
      dat[["(weights)"]] <- wts
  }
  if (!is.null(fcl$subset)) {
    idx <- eval(fcl$subset, envir = dat, enclos = env)
    if (!is.null(idx))
      dat <- dat[idx, , drop = FALSE]
  }
  if (!is.null(na.action)) {
    dat <- do.call(na.action, list(dat))
  } else if (!is.null(naa <- stats::na.action(object))) {
    dat <- structure(dat[-naa, , drop = FALSE], na.action = naa)
  }
  dat
}
