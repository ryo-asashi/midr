is.color <- function(x) {
  if (!is.character(x))
    return(FALSE)
  e <- try(grDevices::col2rgb(x, alpha = TRUE), silent = TRUE)
  !inherits(e, "try-error")
}

is.color.with.alpha <- function(x) {
  if (!is.character(x))
    return(FALSE)
  e <- try(grDevices::col2rgb(x, alpha = TRUE), silent = TRUE)
  !inherits(e, "try-error") && any(e[4L, ] != 255)
}

is.ramp <- function(fun, x.test = seq.int(0, 1, .2), ...) {
  if (!is.numeric(x.test) || any(x.test < 0, 1 < x.test))
    stop("'x.test' must be a numeric vector of values within [0, 1]")
  if (length(fun) != 1L || !is.function(fun))
    return(FALSE)
  e <- try(fun(x.test, ...), silent = TRUE)
  !inherits(e, "try-error") && length(e) == length(x.test) && is.color(e)
}

is.palette <- function(fun, n.test = 2L, ...) {
  if (!is.numeric(n.test) || length(n.test) != 1L)
    stop("'n.test' must be an integer")
  if (length(fun) != 1L || !is.function(fun))
    return(FALSE)
  e <- try(fun(n.test, ...), silent = TRUE)
  !inherits(e, "try-error") && length(e) == n.test && is.color(e)
}

get.kernel.size <- function(object, ok = 2L, ng = 256L, ...) {
  if (!is.kernel(object))
    stop("'object' must be a palette function, ramp function or color vector")
  if (is.ramp(object))
    return(Inf)
  if (is.null(object))
    return(0L)
  if (is.color(object))
    return(length(object))
  init.ng <- ng
  while (abs(ok - ng) > 1) {
    m <- (ok + ng) %/% 2
    if (is.palette(object, n.test = m, ...))
      ok <- m
    else
      ng <- m
  }
  if (ng < init.ng) as.integer(ok) else Inf
}

is.color.theme <- function(object) {
  inherits(object, "color.theme")
}

as.ramp <- function(colors) {
  if (!is.color(colors))
    stop("'colors' must be a character vector of color names")
  .Ramp <- if (is.color.with.alpha(colors)) {
    grDevices::colorRamp(colors, alpha = TRUE)
  } else {
    grDevices::colorRamp(colors, space = "Lab")
  }
  ramp <- function(x, direction = 1L, alpha = NULL, na.color = NA) {
    if (length(x) == 0L)
      return(character())
    res <- character(length(x))
    eps <- .Machine$double.eps
    ng <- is.na(x) | x < (0 - eps) | (1 + eps) < x
    res[ng] <- na.color
    if (all(ng)) return(res)
    x[!ng] <- pmin(1, pmax(0, x[!ng]))
    if (direction < 0) x[!ng] <- 1 - x[!ng]
    args <- as.data.frame(.Ramp(x[!ng]))
    if (!is.null(alpha)) args[[4L]] <- max(min(alpha, 1), 0) * 255
    names(args) <- c("red", "green", "blue", "alpha")[seq_len(length(args))]
    args$maxColorValue <- 255
    res[!ng] <- do.call(grDevices::rgb, args)
    res
  }
  environment(ramp) <- rlang::env(rlang::ns_env("midr"), .Ramp = .Ramp)
  structure(ramp, class = c("function", "ramp"))
}

kernel.class <- function(object) {
  if (is.palette(object))
    "palette"
  else if (is.ramp(object))
    "ramp"
  else if (is.color(object))
    "color"
  else if (is.null(object))
    "null"
  else
    "unknown"
}

is.kernel <- function(object) {
  kernel.class(object) != "unknown"
}

lazy.load.kernel <- function(object) {
  if (is.kernel(object))
    return(object)
  object <- eval(str2lang(object[[1L]]), envir = rlang::ns_env(object[[2L]]))
  if (!is.kernel(object))
    stop("'object' can't be loaded as a valid kernel")
  object
}

#' Color Themes for Graphics
#'
#' \code{color.theme()} returns an object of class "color.theme" that provides two types of color functions.
#'
#' "color.theme" object is a container of the two types of color functions: \code{palette(n)} returns a color name vector of length \code{n}, and \code{ramp(x)} returns color names for each value of \code{x} within [0, 1].
#' The color palettes implemented in the following packages are available: \code{grDevices}, \code{viridisLite}, \code{RColorBrewer} and \code{khroma}.
#'
#' @param object one of the following: a color theme name such as "Viridis" with the optional suffix: "_r" for color themes in reverse order ("Viridis_r"), "_q", "_d" and "_s" for color themes converted into another type, a character vector of color names, a palette function, or a ramp function to be used to create a color theme.
#' @param ... optional arguments to be passed to palette or ramp functions.
#' @examples
#' ct <- color.theme("Mako")
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, 1/4))
#' ct <- color.theme("RdBu")
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, 1/4))
#' ct <- color.theme("Tableau 10")
#' ct$palette(10L)
#' pals <- c("midr", "grayscale", "bluescale", "HCL", "shap")
#' pals <- unique(c(pals, hcl.pals(), palette.pals()))
#' pals <- lapply(pals, color.theme)
#' old.par <- par(no.readonly = TRUE)
#' par(mfrow = c(5L, 2L))
#' for (pal in pals) plot(pal, text = paste(pal$name, "-", pal$type))
#' par(old.par)
#' @returns
#' \code{color.theme()} returns a "color.theme" object containing following components:
#' \item{ramp}{the function that takes a numeric vector \code{x} of the values within [0, 1] and returns a color name vector.}
#' \item{palette}{the function that takes an integer \code{n} and returns a color name vector of length \code{n}.}
#' \item{type}{the type of the color theme; "sequential", "diverging" or "qualitative".}
#' \item{name}{the name of the color theme.}
#' @export color.theme
#'
color.theme <- function(object, ...) {
  if (is.null(object)) {
    NULL
  } else if (is.color.theme(object)) {
    object
  } else if (is.kernel(object)) {
    make.color.theme(object, ...)
  } else if (is.character(object) && length(object) == 1L) {
    get.color.theme(object, ...)
  } else {
    stop("passed object can't be converted to color theme")
  }
}

#' @rdname color.theme
#' @param kernel a palette function, ramp function or color vector, which is used in the palette and ramp methods of the color theme.
#' @param kernel.args a list containing the argument values to be passed to the kernel function.
#' @param options a list of option values to control the color theme's behavior.
#' @param name a character string for the color theme name.
#' @param source a character string for the source name of the color theme.
#' @param type a character string specifying the type of the color theme: One of "sequential", "qualitative" or "diverging".
#' @export
#'
make.color.theme <- function(
    kernel, kernel.args = list(), options = list(), name = NULL,
    source = NULL, type = c("sequential", "diverging", "qualitative")
  ) {
  # environment --------
  kernel <- lazy.load.kernel(kernel)
  if (!is.list(kernel.args))
    stop("'kernel.args' must be a list")
  if (!is.list(options))
    stop("'options' must be a list")
  if (!is.null(name) && (!is.character(name) || length(name) != 1L))
    stop("'name' must be a character string or NULL")
  if (!is.null(source) && (!is.character(source) || length(source) != 1L))
    stop("'source' must be a character string or NULL")
  type <- match.arg(type)
  kcl <- kernel.class(kernel)
  if (kcl == "color") {
    kernel.args$mode <- ifnot.null(kernel.args$mode, "palette")
    kernel.args$alpha <- ifnot.null(kernel.args$alpha, NA)
  }
  options$kernel.size <- ifnot.null(options$kernel.size, get.kernel.size(kernel))
  options$palette.formatter <- ifnot.null(
    options$palette.formatter,
    switch(type, qualitative = "recycle", "interpolate")
  )
  options$palette.reverse <- ifnot.null(options$palette.reverse, FALSE)
  options$ramp.rescaler <- ifnot.null(options$ramp.rescaler, c(0, 1))
  options$na.color <- ifnot.null(options$na.color, NA)
  options$reverse.method <- ifnot.null(options$reverse.method, NA)
  env <- rlang::env(rlang::ns_env("midr"),
    kernel = kernel, kernel.args = kernel.args, options = options,
    name = name, source = source, type = type
  )
  env$self <- env
  # palette --------
  env$palette <- function(n) {
    if (!is.numeric(n) || length(n) != 1L)
      stop("'n' must be an integer")
    if ((n <- as.integer(n)) <= 0L)
      return(character(0L))
    nks <- min(n, options$kernel.size)
    kcl <- kernel.class(kernel)
    if (kcl == "palette") {
      exec.args <- c(list(n = nks), kernel.args)
      ret <- do.call(kernel, exec.args)
    } else if (kcl == "ramp") {
      exec.args <- c(list(x = seq.int(0, 1, length.out = n)), kernel.args)
      ret <- do.call(kernel, exec.args)
    } else if (kcl == "color") {
      if (!is.null(kernel.args$alpha) && !is.na(kernel.args$alpha))
        kernel <- grDevices::adjustcolor(kernel, alpha.f = kernel.args$alpha)
      if (kernel.args$mode == "palette") {
        ret <- kernel[seq_len(nks)]
      } else if (kernel.args$mode == "ramp") {
        mks <- min(256L, options$kernel.size)
        ret <- as.ramp(kernel[seq_len(mks)])(x = seq.int(0, 1, length.out = n))
      }
    } else {
      ret <- rep_len(NA, length.out = n)
    }
    if (options$palette.reverse)
      ret <- rev(ret)
    if (length(ret) < n) {
      if (options$palette.formatter == "recycle") {
        ret <- rep_len(ret, length.out = n)
      } else if (options$palette.formatter == "interpolate") {
        ret <- as.ramp(ret)(x = seq.int(0, 1, length.out = n))
      } else if (any(options$palette.formatter == c("fillna", "fill.na"))) {
        ret <- ret[seq_len(n)]
      } else {
        stop("'n' must be equal to or smaller than the 'kernel.size' option")
      }
    }
    ret[is.na(ret)] <- options$na.color
    ret
  }
  environment(env$palette) <- env
  # ramp --------
  env$ramp <- function(x) {
    if (!is.numeric(x))
      stop("'x' must be a numeric vector")
    eps <- .Machine$double.eps
    ng <- is.na(x) | x < (0 - eps) | (1 + eps) < x
    ret <- character(length(x))
    ret[ng] <- NA
    x[!ng] <- pmin(1, pmax(0, x[!ng]))
    rsc <- ifnot.null(options$ramp.rescaler, c(0, 1))
    if (!is.numeric(rsc) || length(rsc) != 2L)
      stop("'ramp.rescaler' option must be a numeric vector of length 2")
    x[!ng] <- x[!ng] * diff(rsc) + rsc[1L]
    mks <- min(256L, options$kernel.size)
    kcl <- kernel.class(kernel)
    if (kcl == "palette") {
    exec.args <- c(list(n = mks), kernel.args)
    ret[!ng] <- as.ramp(do.call(kernel, exec.args))(x[!ng])
    } else if (kcl == "ramp") {
      exec.args <- c(list(x = x[!ng]), kernel.args)
      ret[!ng] <- do.call(kernel, exec.args)
    } else if (kcl == "color") {
      if (!is.null(kernel.args$alpha) && !is.na(kernel.args$alpha))
        kernel <- grDevices::adjustcolor(kernel, alpha.f = kernel.args$alpha)
      ret[!ng] <- as.ramp(kernel[seq_len(mks)])(x[!ng])
    } else {
      ret[!ng] <- NA
    }
    ret[is.na(ret)] <- options$na.color
    ret
  }
  environment(env$ramp) <- env
  # execute --------
  env$execute <- function(expr) {
    if (is.character(expr)) expr <- str2expression(expr)
    eval(expr, envir = eval(str2lang("self")), enclos = baseenv())
  }
  environment(env$execute) <- env
  # reverse --------
  env$reverse <- function() {
    expr <- ifnot.null(options$reverse.method, NA)
    if (is.na(expr)) {
      expr <- switch(
        kernel.class(kernel), color = "kernel <- rev(kernel)",
        c("options$palette.reverse <- !options$palette.reverse",
          "options$ramp.rescaler <- rev(options$ramp.rescaler)")
      )
    }
    do.call("execute", list(expr = expr))
  }
  environment(env$reverse) <- env
  # return --------
  structure(env, class = c("environment", "color.theme"))
}

#' @rdname color.theme
#' @export
#'
set.color.theme <- function(
    kernel, kernel.args = list(), options = list(), name = NULL,
    source = NULL, type = NULL
  ) {
  if (is.color.theme(kernel)) {
    args <- as.list(kernel)[c("kernel", "kernel.args", "options", "type")]
    args$name <- ifnot.null(name, kernel$name)
    args$source <- ifnot.null(source, kernel$source)
    return(do.call(set.color.theme, args))
  }
  lazy.load.kernel(kernel)
  if (!is.list(kernel.args))
    stop("'kernel.args' must be a list")
  if (!is.list(options))
    stop("'options' must be a list")
  if (!is.character(name) || length(name) != 1L)
    stop("'name' must be a character string of length 1")
  if (!is.character(source) || length(source) != 1L)
    stop("'source' must be a character string of length 1")
  type <- match.arg(type, c("sequential", "diverging", "qualitative"))
  new <- list(kernel = kernel, kernel.args = kernel.args,
              options = options, name = name, source = source, type = type)
  old <- kernel.env[[name]][[source]]
  kernel.env[[name]][[source]] <- new
  invisible(ifnot.null(old, new))
}

#' @rdname color.theme
#' @param reverse logical. If \code{TRUE}, the reversed color theme is returned.
#' @export
#'
get.color.theme <- function(
    name, source = NULL, type = NULL, reverse = FALSE
  ) {
  pre <- sub("#.*", "", name)
  if (pre != name) {
    name <- sub(".*#", "", name)
    source <- pre
  }
  suf <- sub(".*@", "", name)
  if (suf != name) {
    name <- sub("@.*", "", name)
    if (any(suf == c("s", "seq"))) type <- "sequential"
    if (any(suf == c("d", "div"))) type <- "diverging"
    if (any(suf == c("q", "qual"))) type <- "qualitative"
  }
  if (grepl("_r$", name)) {
    name <- sub("_r$", "", name)
    reverse <- TRUE
  }
  if (!exists(name, kernel.env))
    stop(sprintf("color theme '%s' can't be found", name))
  if (is.null(source))
    source <- utils::tail(names(kernel.env[[name]]), 1L)
  args <- kernel.env[[name]][[source]]
  if (is.null(args))
    stop(sprintf("color theme '%s#%s' can't be found", source, name))
  ret <- do.call(make.color.theme, args)
  if (!is.null(type)) ret$type <- type
  if (reverse) try(ret$reverse(), silent = TRUE)
  ret
}

#' @rdname color.theme
#' @param x a "color.theme" object to be displayed.
#' @param n integer. the number of colors.
#' @param text a character string to be displayed.
#' @exportS3Method base::plot
#'
plot.color.theme <- function(x, n = NULL, text = x$name, ...) {
  if (!is.null(n)) {
    colors <- x$palette(n)
  } else {
    if (x$type == "qualitative") {
      n <- min(48L, x$options$kernel.size, na.rm = TRUE)
      colors <- x$palette(n)
    } else {
      n <- 256L
      colors <- x$ramp(seq.int(0, 1, length.out = n))
    }
  }
  opar <- graphics::par("mai", "mar")
  on.exit(graphics::par(opar))
  graphics::par(mar = c(1, 1, 1, 1))
  graphics::plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
                 axes = FALSE, xlab = "", ylab = "")
  graphics::text(0.5, 0.7, text, pos = 3L)
  graphics::rect((seq_len(n) - 1L) / n, 0.3, seq_len(n) / n, 0.7,
                 col = colors, border = NA)
}

#' @rdname color.theme
#' @param x a "color.theme" object to be displayed.
#' @param display logical. If \code{TRUE}, colors are displayed in the plot area.
#' @exportS3Method base::print
#'
print.color.theme <- function(x, display = TRUE, ...) {
  type <- x$type
  substr(type, 1L, 1L) <- toupper(substr(type, 1L, 1L))
  text <- paste0(type, " Color Theme")
  if (!is.null(x$name)) text <- paste0(text, ' : "', x$name, '" ')
  cat(text)
  if (display)
    plot.color.theme(x, text = text)
}



#' Color Scales for ggplot2 Graphics based on Color Themes
#'
#' \code{scale_color_theme()} and family functions returns color scales for the "colour" and "fill" aesthetics of ggplot objects.
#'
#' @param theme one of the following: a color theme name such as "Viridis", a character vector of color names, a palette function, or a ramp function to be used to create a color theme.
#' @param ... optional arguments to be passed to \code{ggplot2::continuous_scale()} or \code{ggplot2::discrete_scale()}.
#' @param discrete logical. If \code{TRUE}, a discrete scale is returned.
#' @param middle a numeric value specifying the middle point for the diverging color themes.
#' @param aesthetics character string: "fill" or "color".
#' @examples
#' data(txhousing, package = "ggplot2")
#' cities <- c("Houston", "Fort Worth", "San Antonio", "Dallas", "Austin")
#' df <- subset(txhousing, city %in% cities)
#' d <- ggplot2::ggplot(data = df, ggplot2::aes(x = sales, y = median)) +
#'   ggplot2::geom_point(ggplot2::aes(colour = city))
#' d + scale_color_theme("Set 1")
#' d + scale_color_theme("R3")
#' d + scale_color_theme("Blues", discrete = TRUE)
#' d + scale_color_theme("SunsetDark", discrete = TRUE)
#' data(faithfuld, package = "ggplot2")
#' v <- ggplot2::ggplot(faithfuld) +
#'   ggplot2::geom_tile(ggplot2::aes(waiting, eruptions, fill = density))
#' v + scale_fill_theme("Plasma")
#' v + scale_fill_theme("Spectral")
#' v + scale_fill_theme("Spectral_r")
#' v + scale_fill_theme("midr", middle = 0.017)
#' @returns
#' \code{scale_color_theme()} returns a "ScaleContinuous" or "ScaleDiscrete" object that can be added to a "ggplot" object.
#' @export scale_color_theme
#'
scale_color_theme <- function(
    theme, ..., discrete = NULL, middle = 0, aesthetics = "colour") {
  theme <- color.theme(theme)
  if (is.null(discrete))
    discrete <- theme$type == "qualitative"
  if (discrete) {
    scale <- ggplot2::discrete_scale(
      aesthetics, palette = theme$palette,
      guide = "legend", ...)
  } else {
    if (theme$type == "qualitative")
      stop("qualitative color theme can't be used for continuous scales")
    rescale_fun <- if (theme$type == "sequential") {
      scales::rescale
    } else if (theme$type == "diverging") {
      function(x, to = c(0, 1), from = range(x, na.rm = TRUE))
        scales::rescale_mid(x, to, from, mid = middle)
    }
    scale <- ggplot2::continuous_scale(
      aesthetics, palette = theme$ramp,
      guide = "colorbar", rescaler = rescale_fun, ...)
  }
  scale
}

#' @rdname scale_color_theme
#' @export scale_colour_theme
#'
scale_colour_theme <- function(
    theme, ..., discrete = NULL, middle = 0, aesthetics = "colour") {
  scale_color_theme(theme = theme, ..., discrete = discrete,
                    middle = middle, aesthetics = aesthetics)
}

#' @rdname scale_color_theme
#' @export scale_fill_theme
#'
scale_fill_theme <- function(
    theme, ..., discrete = NULL, middle = 0, aesthetics = "fill") {
  scale_color_theme(theme = theme, ..., discrete = discrete,
                    middle = middle, aesthetics = aesthetics)
}


rescale <- function(x, middle = NULL) {
  if (is.character(x))
    x <- as.factor(x)
  if (is.factor(x) || is.logical(x))
    x <- as.numeric(x)
  from <- range(x, na.rm = TRUE, finite = TRUE)
  if (is.null(middle)) {
    d <- from[2L] - from[1L]
    if (d == 0)
      return(ifelse(is.na(x), NA, 0.5))
    res <- (x - from[1L]) / d
  } else {
    d <- 2 * max(abs(from - middle))
    if (d == 0)
      return(ifelse(is.na(x), NA, 0.5))
    res <- (x - middle) / d + 0.5
  }
  pmax(0, pmin(1, res))
}


to.colors <- function(x, theme, middle = 0, na.value = "gray50") {
  theme <- color.theme(theme)
  if (is.discrete(x)) {
    x <- as.integer(as.factor(x))
    cols <- theme$palette(max(x, na.rm = TRUE))[x]
  } else {
    if (theme$type == "qualitative") {
      stop("qualitative color theme can't be used for continuous variable")
    } else if (theme$type == "sequential") {
      cols <- theme$ramp(rescale(x))
    } else if (theme$type == "diverging") {
      cols <- theme$ramp(rescale(x, middle = middle))
    } else
      cols <- rep.int(1L, length(x))
  }
  cols[is.na(cols)] <- na.value
  cols
}

hcl.palette <- function(
    n, direction = 1L, alpha = NULL, chroma = 100, luminance = 65
  ) {
  if (n < 1L)
    return(character(0L))
  hues <- seq(0, 360, length.out = n + 1)[seq_len(n)] %% 360
  if (direction < 0L)
    hues <- rev(hues)
  grDevices::hcl(h = hues, c = chroma, l = luminance, alpha = alpha)
}
