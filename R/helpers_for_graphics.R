# base graphics

adjusted_margin <- function(labels, margin = 1/16) {
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

.barplot <- function(
    to, from = 0, labels = NULL, horizontal = FALSE, limits = NULL,
    width = NULL, type = c("b", "d", "n"), col = NULL, fill = NULL,
    lty = NULL, lwd = NULL, main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    cex = 1, pch = NULL, ...
) {
  type <- match.arg(type)
  if (horizontal) {
    opar <- graphics::par("mai", "mar", "las")
    on.exit(graphics::par(opar))
    graphics::par(mai = adjusted_margin(labels = labels), las = 1L)
  }
  to <- as.matrix(to)
  n <- nrow(to)
  m <- ncol(to)
  from <- make_mat(from %||% 0, n, m)
  col <- make_mat(col %||% NA, n, m)
  fill <- make_mat(fill %||% "gray65", n, m)
  pch <- make_mat(pch %||% 16L, n, m)
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

override <- function(args, dots) {
  rename = list(
    colour = "col", color = "col",
    size = "cex",
    shape = "pch",
    linetype = "lty",
    linewidth = "lwd",
    title = "main",
    subtitle = "sub"
  )
  dotnames <- names(dots)
  if (is.null(dotnames)) return(args)
  newnames <- vapply(
    X = dotnames, FUN = function(x) {rename[[x]] %||% x},
    FUN.VALUE = character(1L), USE.NAMES = FALSE
  )
  names(dots) <- newnames
  dots <- dots[!duplicated(names(dots), fromLast = TRUE)]
  utils::modifyList(args, dots, keep.null = TRUE)
}

set_alpha <- function(args, on = "col") {
  colors <- args[[on]]
  alpha <- args$alpha
  args$alpha <- NULL
  if (is.null(colors) || is.null(alpha) || !any(alpha < 1))
    return(args)
  alpha <- pmax(0, pmin(1, alpha))
  rgbs <- grDevices::col2rgb(col = colors)
  args[[on]] <- grDevices::rgb(
    rgbs[1L,], rgbs[2L,], rgbs[3L,], round(alpha * 255), maxColorValue = 255L
  )
  return(args)
}
