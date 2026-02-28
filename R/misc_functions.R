solveOLS <- function(x, y, tol = 1e-7, method = 0L, ...) {
  map <- c(
    qr = 0L, unpivoted.qr = 1L, llt = 2L, ldlt = 3L, svd = 4L, eigen = 5L
  )
  if (is.character(method)) {
    name <- try(match.arg(tolower(method), names(map)), silent = TRUE)
    if (inherits(name, "try-error")) name <- method
    method <- if (any(name == names(map))) map[[name]] else -1L
  } else if (is.numeric(method)) {
    method <- as.integer(method)
    name <- names(map)[match(method, map)]
    if (is.na(name)) name <- as.character(method)
  } else {
    name <- "qr"
    method = -1L
  }
  fmsg <- "falling back to 'qr' with stats::.lm.fit"
  custom_solver <- getOption(paste0("midr.solver.", name))
  if (!is.null(custom_solver) && is.function(custom_solver)) {
    z <- tryCatch(custom_solver(x, y), error = function(e) e)
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
    msg <- sprintf("method '%s' is not supported for matrix: %s", name, fmsg)
    z <- stats::.lm.fit(x, y, tol = tol)
    return(structure(z, method = "qr", message = msg))
  }
  if (inherits(z, "error")) {
    emsg <- gsub("\n", " ", conditionMessage(z))
    msg <- sprintf("method '%s' failed for matrix (%s): %s", name, emsg, fmsg)
    z <- stats::.lm.fit(x, y, tol = tol)
    return(structure(z, method = "qr", message = msg))
  }
  return(structure(z, method = name))
}

attract <- function(x, margin) {
  x[abs(x) <= margin] <- 0
  x
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

term.split <- function(x) {
  unlist(strsplit(x, split = ":"), use.names = FALSE)
}

term.check <- function(x, terms, stop = TRUE) {
  if (is.na(x)) {
    if (stop) stop("term can't be NA")
    return(NA_character_)
  }
  if (!any(x == terms)) {
    rx <- paste0(rev(term.split(x)), collapse = ":")
    if (!any(rx == terms)) {
      if (stop) stop("term '", x, "' does not exist")
      message("term '", x, "' does not exist")
      return(NA_character_)
    }
    return(rx)
  }
  return(x)
}

make.formula <- function(xlabels, ylabel = NULL, env = parent.frame()) {
  stats::as.formula(
    paste(if (!is.null(ylabel)) ylabel, "~", paste(xlabels, collapse = "+")),
    env = env
  )
}

model.reframe <- function(object, data) {
  if (is.null(object$call$formula)) {
    return(as.data.frame(data))
  }
  fm <- stats::formula(object)
  if (!is.data.frame(data) && !is.environment(data)) {
    data <- as.data.frame(data)
  }
  tryCatch(
    stats::model.frame.default(fm, data, na.action = "na.pass"),
    error = function(e) {
      fm[[2L]] <- NULL
      stats::model.frame.default(fm, data, na.action = "na.pass")
    }
  )
}

model.data <- function(object, env = parent.frame()) {
  fcl <- object$call
  if (!is.null(fcl$data)) {
    return(eval(fcl$data, envir = env))
  }
  if (!is.null(fml <- fcl$formula)) {
    env <- environment(fml) %||% env
    return(stats::get_all_vars(fml, data = env))
  }
  if (!is.null(fcl$x)) {
    x <- eval(fcl$x, envir = env)
    if (!is.null(ncol(x)) && is.null(colnames(x))) {
      colnames(x) <- paste0("x", seq_len(ncol(x)))
    }
    x <- as.data.frame(x)
    if (!is.null(fcl$y)) {
      x$y <- eval(fcl$y, envir = env)
    }
    return(x)
  }
  NULL
}

interaction.frame <- function(xfrm, yfrm) {
  nx <- nrow(xfrm)
  ny <- nrow(yfrm)
  data.frame(
    xfrm[rep(seq_len(nx), times = ny), , drop = FALSE],
    yfrm[rep(seq_len(ny), each = nx), , drop = FALSE],
    row.names = NULL, check.names = FALSE
  )
}

adjusted.mai <- function(labels, margin = 1/16) {
  mai <- graphics::par("mai")
  cex <- graphics::par("cex.lab")
  req <- max(graphics::strwidth(labels, "inch", cex = cex), na.rm = TRUE)
  req <- mai[4L] + req + margin
  if (mai[2L] < req)
    mai[2L] <- req
  mai
}

make_mat <- function(x, nrow, ncol) {
  if (length(x) == ncol && ncol > 1L) {
    matrix(x, nrow = nrow, ncol = ncol, byrow = TRUE)
  } else if (length(x) == nrow) {
    matrix(x, nrow = nrow, ncol = ncol, byrow = FALSE)
  } else if (length(x) == 1L) {
    matrix(x, nrow = nrow, ncol = ncol)
  } else {
    message("length of 'x' doesn't match 'nrow' nor 'ncol'")
    matrix(x, nrow = nrow, ncol = ncol)
  }
}

barplot2 <- function(
    to, from = 0, labels = NULL, horizontal = FALSE, limits = NULL, width = NULL,
    type = c("b", "d", "n"), col = NA, fill = "gray65", lty = NULL, lwd = NULL,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, cex = 1, pch = 16, ...
) {
  type <- match.arg(type)
  if (horizontal) {
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    graphics::par(mai = adjusted.mai(labels = labels), las = 1L)
  }
  to <- as.matrix(to)
  n <- nrow(to)
  m <- ncol(to)
  from <- make_mat(from, n, m)
  col <- make_mat(col, n, m)
  fill <- make_mat(fill, n, m)
  pch <- make_mat(pch, n, m)
  rng <- range(c(to, from), na.rm = TRUE)
  mgn <- if (diff(rng) == 0) 0.5 else abs(diff(rng)) / 100
  limits <- limits %||% c(rng[1L] - mgn, rng[2L] + mgn)
  at <- (if (horizontal) (n:1L) else (1L:n))
  graphics::plot.new()
  graphics::plot.window(xlim = if (horizontal) limits else c(0, n) + 0.5,
                        ylim = if (horizontal) c(0, n) + 0.5 else limits)
  graphics::box()
  graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  graphics::axis(side = if (horizontal) 1L else 2L)
  graphics::axis(side = if (horizontal) 2L else 1L, at = at, labels = labels)
  if (type == "b") {
    bw <- (width %||% 0.9) / m
    offsets <- (seq_len(m) - (m + 1) / 2) * bw
    hw <- bw / 2
    args <- as.list(numeric(8L))
    names(args) <- if (horizontal) {
      c("xleft", "xright", "ybottom", "ytop", "col", "border", "lty", "lwd")
    } else {
      c("ybottom", "ytop", "xleft", "xright", "col", "border", "lty", "lwd")
    }
    args[[7L]] <- lty %||% 1L
    args[[8L]] <- lwd %||% 1L
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        center <- at[i] + offsets[j]
        args[[1L]] <- from[i, j]
        args[[2L]] <- to[i, j]
        args[[3L]] <- center - hw
        args[[4L]] <- center + hw
        args[[5L]] <- fill[i, j]
        args[[6L]] <- col[i, j]
        do.call(graphics::rect, args)
      }
    }
  } else if (type == "d") {
    offsets <- (seq_len(m) - (m + 1) / 2) * (width %||% .6 / m)
    lcol <- graphics::par("fg") %||% "black"
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        center <- at[i] + offsets[j]
        graphics::lines.default(
          x = if (horizontal) c(from[i, j], to[i, j]) else c(center, center),
          y = if (horizontal) c(center, center) else c(from[i, j], to[i, j]),
          col = lcol, lty = lty %||% 3L, lwd = lwd %||% graphics::par("lwd")
        )
        graphics::points.default(
          x = if (horizontal) to[i, j] else center,
          y = if (horizontal) center else to[i, j],
          col = col[i, j], pch = pch[i, j], cex = cex
        )
      }
    }
  }
  invisible(NULL)
}

override <- function(args, dots,
    params = c("fill", "color", "colour", "col", "size", "cex", "shape", "pch",
               "linetype", "lty", "linewidth", "lwd",
               "title", "main", "subtitle", "sub", "xlab", "ylab",
               "limits", "width", "boxwex", "horizontal"),
    read.as = list(color = "col", colour = "col", size = "cex",
                   shape = "pch", linetype = "lty", linewidth = "lwd",
                   title = "main", subtitle = "sub", boxwex = "width")
  ) {
  for (param in params) {
    arg <- read.as[[param]] %||% param
    args[[arg]] <- dots[[param]] %||% args[[arg]]
  }
  args
}


verbose <- function(text, verbosity = 1L, level = 1L, timestamp = FALSE) {
  if (verbosity >= level) {
    ltag <- if (level >= 3L)
      "- [debug] " else if (level >= 2L) "[info] " else NULL
    stamp <- if (timestamp)
      format(Sys.time(), " (%Y-%m-%d %H:%M:%S)") else NULL
    text <- paste0(ltag, text, stamp)
    message(text, appendLF = getOption("message.appendLF", TRUE))
  }
}

examples <- function(x, n = 3L, ...) {
  if (is.data.frame(x)) x <- as.matrix(x)
  dts <- if (length(x) > n) ", ..." else ""
  n <- min(length(x), n)
  paste0(paste(trimws(format(x[seq_len(n)], ...)), collapse = ", "), dts)
}

mid.frames <- function(object, ...) {
  mfl <- lapply(object$encoders$main.effects, function(x) x$frame)
  ifl <- lapply(object$encoders$interactions, function(x) x$frame)
  tags <- unique(c(names(mfl), names(ifl)))
  res <- list()
  for (tag in tags) {
    if (identical(mfl[[tag]], ifl[[tag]])) {
      res[[tag]] <- mfl[[tag]]
    } else {
      res[[paste0("|", tag)]] <- mfl[[tag]]
      res[[paste0(":", tag)]] <- ifl[[tag]]
    }
  }
  res
}

#' @exportS3Method stats::formula
#'
formula.mid <- function(x, ...) {
  fm <- x$call$formula
  if (!is.null(fm)) {
    res <- stats::formula(stats::terms(x))
    environment(res) <- environment(fm)
    res
  } else {
    stats::formula(stats::terms(x))
  }
}

#' @exportS3Method stats::model.frame
#'
model.frame.mid <- function(object, ...) {
  model.reframe(object, data = model.data(object))
}


geom_dotchart <- function(
    mapping = NULL, data = NULL, position = "identity", ...
) {
  dots <- list(...)
  tlin <- ggplot2::theme_get()$line
  line_params  <- c(
    "alpha", "linetype", "lty", "linewidth", "lwd"
  )
  line_args <- c(
    list(mapping = mapping, data = data, position = position),
    override(list(col = tlin$colour, lty = 3L, lwd = tlin$linewidth),
             dots[names(dots) %in% line_params])
  )
  point_params <- c(
    "color", "colour", "alpha", "shape", "pch", "size", "cex", "fill"
  )
  mapping$xmin <- mapping$xmax <- mapping$ymin <- mapping$ymax <- NULL
  point_args <- c(
    list(mapping = mapping, data = data, position = position),
    dots[names(dots) %in% point_params]
  )
  list(
    do.call(ggplot2::geom_linerange, line_args),
    do.call(ggplot2::geom_point, point_args)
  )
}
