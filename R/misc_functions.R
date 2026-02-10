solveOLS <- function(x, y, method = 0L, solver = NULL, ...) {
  choices <- c(
    "fastLmMatrix",
    "RcppEigen::fastLmPure",
    "stats::lm.fit"
  )
  if (is.null(solver)) {
    solver <- if (any(method == 0L:6L)) {
      if (NCOL(y) == 1L) "RcppEigen::fastLmPure" else "fastLmMatrix"
    } else {
      "stats::lm.fit"
    }
  }
  solver <- match.arg(solver, choices = choices)
  if (solver == "fastLmMatrix") {
    fastLmMatrix(as.matrix(x), as.matrix(y), method)
  } else if (solver == "RcppEigen::fastLmPure") {
    RcppEigen::fastLmPure(x, y, method)
  } else {
    stats::lm.fit(x, y, method = "qr", ...)
  }
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
    if (stop)
      stop("term can't be NA")
    return(NA)
  }
  if (!any(x == terms)) {
    rx <- paste0(rev(term.split(x)), collapse = ":")
    if (!any(rx == terms)) {
      if (stop)
        stop("term '", x, "' does not exist")
      message("term '", x, "' does not exist")
      return(NA)
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
  if (!is.null(object$call$formula)) {
    fm <- stats::formula(object)
    if (!is.data.frame(data) && !is.environment(data))
      data <- as.data.frame(data)
    res <- try(stats::model.frame.default(fm, data, na.action = "na.pass"),
               silent = TRUE)
    if (inherits(res, "try-error")) {
      fm[[2L]] <- NULL
      res <- stats::model.frame.default(fm, data, na.action = "na.pass")
    }
    return(res)
  }
  as.data.frame(data)
}

model.data <- function(object, env = parent.frame()) {
  fcl <- object$call
  if (!is.null(fcl$data))
    return(eval(fcl$data, envir = env))
  if (!is.null(fml <- fcl$formula)) {
    env <- environment(fml) %||% env
    return(env)
  }
  if (!is.null(fcl$x)) {
    x <- eval(fcl$x, envir = env)
    if (!is.null(dim(x)[2L]) && is.null(colnames(x)))
      colnames(x) <- paste0("x", seq_len(dim(x)[2L]))
    if (!is.data.frame(x))
      x <- as.data.frame(x)
    if (!is.null(fcl$y)) {
      y <- eval(fcl$y, envir = env)
      x <- cbind.data.frame(x, y)
      colnames(x)[[ncol(x)]] <- "y"
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

barplot2 <- function(
    to, from = 0, labels = NULL, horizontal = FALSE, limits = NULL, width = .9,
    type = c("b", "d", "n"), col = NA, fill = "gray65", lty = NULL, lwd = 1L,
    main = NULL, sub = NULL, xlab = NULL, ylab = NULL, cex = 1, pch = 16, ...
) {
  type <- match.arg(type)
  if (horizontal) {
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    graphics::par(mai = adjusted.mai(labels = labels), las = 1L)
  }
  n <- max(length(to), length(from))
  to <- rep(to, length.out = n)
  from <- rep(from, length.out = n)
  col <- rep(col, length.out = n)
  fill <- rep(fill, length.out = n)
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
    hw <- width / 2
    args <- as.list(numeric(8L))
    names(args) <- if (horizontal) {
      c("xleft", "xright", "ybottom", "ytop", "col", "border", "lty", "lwd")
    } else {
      c("ybottom", "ytop", "xleft", "xright", "col", "border", "lty", "lwd")
    }
    args[[7L]] <- lty %||% 1L
    args[[8L]] <- lwd %||% 1L
    for (i in seq_len(n)) {
      args[[1L]] <- from[i]
      args[[2L]] <- to[i]
      args[[3L]] <- at[i] - hw
      args[[4L]] <- at[i] + hw
      args[[5L]] <- fill[i]
      args[[6L]] <- col[i]
      do.call(graphics::rect, args)
    }
  } else if (type == "d") {
    for (i in seq_len(n)) {
      graphics::lines.default(
        x = if (horizontal) c(from[i], to[i]) else c(at[i], at[i]),
        y = if (horizontal) c(at[i], at[i]) else c(from[i], to[i]),
        col = "black", lty = lty %||% 3L, lwd = lwd %||% 1L
      )
      graphics::points.default(
        x = if (horizontal) to[i] else at[i],
        y = if (horizontal) at[i] else to[i],
        col = col[i], pch = pch, cex = cex
      )
    }
  }
  invisible(NULL)
}

override <- function(args, dots,
    params = c("fill", "color", "colour", "col", "size", "cex", "shape", "pch",
               "linetype", "lty", "linewidth", "lwd",
               "title", "main", "subtitle", "sub", "xlab", "ylab"),
    read.as = list(color = "col", colour = "col", size = "cex",
                   shape = "pch", linetype = "lty", linewidth = "lwd",
                   title = "main", subtitle = "sub")
  ) {
  for (param in params) {
    arg <- read.as[[param]] %||% param
    args[[arg]] <- dots[[param]] %||% args[[arg]]
  }
  args
}

get.link <- function(link) {
  res <- switch(
    link,
    "transprobit" = list(
      linkfun = function(mu) stats::qnorm(mu, mean = .5, sd = sqrt(.5 / pi)),
      linkinv = function(eta) stats::pnorm(eta, mean = .5, sd = sqrt(.5 / pi))
    ),
    "identity-gaussian" = list(
      linkfun = function(mu) mu,
      linkinv = function(eta) stats::pnorm(eta, mean = .5, sd = sqrt(.5 / pi))
    ),
    "translogit" = list(
      linkfun = function(mu) .5 - .25 * log(1 / mu - 1),
      linkinv = function(eta) 1 / (1 + exp(2 - 4 * eta))
    ),
    "identity-logistic" = list(
      linkfun = function(mu) mu,
      linkinv = function(eta) 1 / (1 + exp(2 - 4 * eta))
    ),
    stats::make.link(link)
  )
  res$name <- as.character(link)
  if (!inherits(res, "link-glm"))
    class(res) <- "link-midr"
  res
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
  mfl <- lapply(object$encoders[["main.effects"]], function(enc) enc$frame)
  ifl <- lapply(object$encoders[["interactions"]], function(enc) enc$frame)
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
  }
  else stats::formula(stats::terms(x))
}


#' @exportS3Method stats::model.frame
#'
model.frame.mid <- function(object, ...) {
  model.reframe(object, data = model.data(object))
}
