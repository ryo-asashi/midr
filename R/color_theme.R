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

is.ramp <- function(fun, x.test = seq.int(0, 1, .2), args = list()) {
  if (!is.numeric(x.test) || any(x.test < 0, 1 < x.test))
    stop("'x.test' must be a numeric vector of values within [0, 1]")
  if (length(fun) != 1L || !is.function(fun))
    return(FALSE)
  e <- try(do.call(fun, c(list(x.test), args)), silent = TRUE)
  !inherits(e, "try-error") && length(e) == length(x.test) && is.color(e)
}

is.palette <- function(fun, n.test = 2L, args = list()) {
  if (!is.numeric(n.test) || length(n.test) != 1L)
    stop("'n.test' must be an integer")
  if (length(fun) != 1L || !is.function(fun))
    return(FALSE)
  e <- try(do.call(fun, c(list(n.test), args)), silent = TRUE)
  !inherits(e, "try-error") && length(e) == n.test && is.color(e)
}

get.kernel.size <- function(object, args = list(), ok = 1L, ng = 256L) {
  if (!is.kernel(object, args = args))
    stop("'object' must be a palette function, ramp function or color vector")
  if (is.ramp(object, args = args))
    return(Inf)
  if (is.null(object))
    return(0L)
  if (is.color(object))
    return(length(object))
  init.ng <- ng
  while (abs(ok - ng) > 1) {
    m <- (ok + ng) %/% 2
    if (is.palette(object, n.test = m, args = args))
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

kernel.class <- function(object, args = list()) {
  if (is.palette(object, args = args))
    "palette"
  else if (is.ramp(object, args = args))
    "ramp"
  else if (is.color(object))
    "color"
  else if (is.null(object))
    "null"
  else
    "unknown"
}

is.kernel <- function(object, args = list()) {
  kernel.class(object, args = args) != "unknown"
}

lazy.load.kernel <- function(object, args = list()) {
  if (is.kernel(object, args = args))
    return(object)
  if (!(is.character(object) || is.list(object)))
    stop("'object' must be a kernel or a character vector/list specifying expression and namespace")
  text <- object[[1L]]
  namespace <- if (length(object) == 1L) "base" else object[[2L]]
  object <- eval(str2expression(text), envir = rlang::ns_env(namespace))
  if (!is.kernel(object, args = args))
    stop("'object' can't be loaded as a valid kernel")
  object
}

get.color.theme.env <- function()
  getOption("midr.color.theme.env", kernel.env)



#' Color Themes for Graphics
#'
#' The \code{color.theme()} function is the main interface for working with "color.theme" objects. It acts as a dispatcher that, depending on the class of 'object', can retrieve a pre-defined theme by name (see the "Theme Name Syntax" section), create a new theme from a vector of colors or a color-generating function, and modify an existing "color.theme" object.
#'
#' The "color.theme" object is a special environment that provides two color-generating functions: \code{palette()} and \code{ramp()}.
#' \itemize{
#'   \item \code{palette()}: Takes an integer \code{n} and returns a vector of \code{n} discrete colors. It is primarily intended for qualitative themes, where distinct colors are used to represent categorical data.
#'   \item \code{ramp()}: Takes a numeric vector \code{x} with values in the [0, 1] interval, and returns a vector of corresponding colors. It maps numeric values onto a continuous color gradient, making it suitable for sequential and diverging themes.
#' }
#'
#' @section Functions:
#' The \code{color.theme()} family includes several functions:
#' \itemize{
#'   \item \code{color.theme()}: The main dispatcher function.
#'   \item \code{get.color.theme()}: Retrieves a pre-defined theme from the registry.
#'   \item \code{make.color.theme()}: Creates a new "color.theme" object from a kernel (a color vector or function).
#'   \item \code{set.color.theme()}: Registers a theme so it can be called by name.
#'   \item \code{modify.color.theme()}: Modifies the properties of an existing theme object.
#' }
#'
#' @section Theme Name Syntax:
#' When retrieving a theme using a character string, you can use a special syntax to specify the source and apply modifications:
#'
#' "\code{[(source)#](name)[_r][@(type)]}"
#'
#' \itemize{
#'   \item source: (Optional) The source package or collection of the theme
#'     (e.g., "grDevices").
#'   \item name: The name of the theme (e.g., "RdBu").
#'   \item "_r": (Optional) A suffix to reverse the color order.
#'   \item type: (Optional) A suffix to modify the theme type. Can be "@q" or
#'     "@qual" (qualitative), "@d" or "@div" (diverging), "@s" or "@seq" (sequential).
#' }
#'
#' @param object one of the following items: (i) a character string following the theme name syntax (e.g., "grDevices#RdBu@seq", see the "Theme Name Syntax" section), (ii) a character vector of color names, a palette function, or a ramp function to be used as a color kernel to create a color theme, or (iii) a "color.theme" object.
#' @param ... optional arguments to be passed on to \code{get.color.theme()}, \code{make.color.theme()} or \code{modify.color.theme()}.
#' @param theme a "color.theme" object to be modified.
#' @param kernel a color vector, a palette function, or a ramp function that serves as the basis for generating colors.
#' @param kernel.args a list containing the argument values to be passed to the color kernel.
#' @param options a list of option values to control the color theme's behavior (e.g., "palette.reverse", "na.color").
#' @param name a character string naming the color theme.
#' @param source a character string for the source of the color theme (e.g., a package name).
#' @param type a character string specifying the type of the color theme. One of "sequential", "diverging", or "qualitative".
#' @param reverse logical. If \code{TRUE}, the order of colors is reversed.
#'
#' @examples
#' # Retrieve a pre-defined theme
#' ct <- color.theme("Mako")
#' ct
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, 1/4))
#'
#' # Use special syntax to get a reversed, qualitative theme
#' ct <- color.theme("grDevices#Zissou 1_r@qual")
#' ct
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, 1/4))
#'
#' # Create a new theme from a vector of colors
#' ct <- color.theme(c("#003f5c", "#7a5195", "#ef5675", "#ffa600"))
#' ct
#' ct$palette(10L)
#' @returns
#' \code{color.theme()} returns a "color.theme" object containing the following two color-generating functions:
#' \item{ramp}{a function that accepts a numeric vector \code{x} of the values within [0, 1] and returns a color name vector corresponding the values in \code{x}.}
#' \item{palette}{a function that accepts an integer \code{n} and returns a color name vector of length \code{n}.}
#' @export color.theme
#'
color.theme <- function(object, ...) {
  if (is.null(object)) {
    NULL
  } else if (is.color.theme(object)) {
    if (nargs() == 1L)
      return(object)
    ret <- as.list(object)
    ret <- ret[c("kernel", "kernel.args", "options", "name", "source", "type")]
    ret <- do.call(make.color.theme, ret)
    modify.color.theme(ret, ...)
  } else if (is.kernel(object, args = list(...)$kernel.args)) {
    make.color.theme(object, ...)
  } else if (is.character(object) && length(object) == 1L) {
    get.color.theme(object, ...)
  } else {
    stop("'object' can't be converted to a color theme")
  }
}


#' @rdname color.theme
#' @export modify.color.theme
#'
modify.color.theme <- function(
    theme, kernel = NULL, kernel.args = NULL, options = NULL,
    name = NULL, source = NULL, type = NULL, reverse = FALSE
) {
  if (!is.color.theme(theme))
    stop("'theme' must be a color theme")
  if (!is.null(type)) {
    type <- match.arg(type, c("sequential", "diverging", "qualitative"))
    theme$type <- type
  }
  if (!is.null(kernel)) {
    if (!is.kernel(kernel))
      stop("'kernel' must be a palette function, ramp function or color vector")
    theme$kernel <- kernel
  }
  if (!is.null(kernel.args)) {
    for (item in names(kernel.args)) {
      theme$kernel.args[[item]] <- kernel.args[[item]]
    }
  }
  if (!is.null(options)) {
    for (item in names(options)) {
      theme$options[[item]] <- options[[item]]
    }
  }
  if (reverse) {
    method <- theme$options$reverse.method
    if (!is.null(method) && !is.na(method)) {
      eval(str2expression(method), envir = theme, enclos = baseenv())
    } else if (kernel.class(theme$kernel) == "color") {
      theme$kernel <- rev(theme$kernel)
    } else {
      theme$options$palette.reverse <- !theme$options$palette.reverse
      theme$options$ramp.rescaler <- rev(theme$options$ramp.rescaler)
    }
  }
  theme
}

#' @rdname color.theme
#' @export make.color.theme
#'
make.color.theme <- function(
    kernel, kernel.args = list(), options = list(), name = NULL,
    source = NULL, type = c("sequential", "diverging", "qualitative")
  ) {
  # environment --------
  kernel <- lazy.load.kernel(kernel, args = kernel.args)
  if (!is.list(kernel.args))
    stop("'kernel.args' must be a list")
  if (!is.list(options))
    stop("'options' must be a list")
  if (!is.null(name) && (!is.character(name) || length(name) != 1L))
    stop("'name' must be a character string or NULL")
  if (!is.null(source) && (!is.character(source) || length(source) != 1L))
    stop("'source' must be a character string or NULL")
  type <- match.arg(type)
  kcl <- kernel.class(kernel, args = kernel.args)
  if (kcl == "color") {
    kernel.args$mode <- ifnot.null(
      kernel.args$mode, switch(type, qualitative = "palette", "ramp")
    )
    kernel.args$alpha <- ifnot.null(kernel.args$alpha, NA)
  }
  options$kernel.size <- ifnot.null(options$kernel.size,
                                    get.kernel.size(kernel, args = kernel.args))
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
  # palette --------
  env$palette <- function(n) {
    if (!is.numeric(n) || length(n) != 1L)
      stop("'n' must be an integer")
    if ((n <- as.integer(n)) <= 0L)
      return(character(0L))
    nks <- min(n, options$kernel.size)
    kcl <- kernel.class(kernel, args = kernel.args)
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
    kcl <- kernel.class(kernel, args = kernel.args)
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
  # return --------
  structure(env, class = c("environment", "color.theme"))
}

#' @rdname color.theme
#' @export set.color.theme
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
  lazy.load.kernel(kernel, args = kernel.args)
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
  env <- get.color.theme.env()
  old <- env[[name]][[source]]
  env[[name]][[source]] <- new
  invisible(ifnot.null(old, new))
}

#' @rdname color.theme
#' @export get.color.theme
#'
get.color.theme <- function(
    name, source = NULL, kernel = NULL, kernel.args = NULL, options = NULL,
    type = NULL, reverse = FALSE
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
  env <- get.color.theme.env()
  if (!exists(name, env))
    stop(sprintf("'%s' is not found in the color theme environment", name))
  if (is.null(source))
    source <- utils::tail(names(env[[name]]), 1L)
  args <- env[[name]][[source]]
  if (is.null(args))
    stop(sprintf("'%s#%s' is not found in the color theme environment",
                 source, name))
  ret <- do.call(make.color.theme, args)
  modify.color.theme(ret, kernel = kernel, kernel.args = kernel.args,
                     options = options, type = type, reverse = reverse)
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
#' @param display logical. If \code{TRUE}, colors are displayed in the plot area.
#' @exportS3Method base::print
#'
print.color.theme <- function(x, display = TRUE, ...) {
  type <- x$type
  substr(type, 1L, 1L) <- toupper(substr(type, 1L, 1L))
  text <- paste0(type, " Color Theme")
  if (!is.null(x$name)) text <- paste0(text, ' : "', x$name, '" ')
  cat(text)
  if (display) plot.color.theme(x, text = text)
}


#' @rdname color.theme
#' @export color.theme.info
#'
color.theme.info <- function() {
  env <- get.color.theme.env()
  info.byname <- function(x) {
    data.frame(name = sapply(x, function(y) y$name),
               source = sapply(x, function(y) y$source),
               type = sapply(x, function(y) y$type))
  }
  info <- do.call(rbind, lapply(env, info.byname))
  info <- info[order(info$type, info$name, info$source), ]
  rownames(info) <- NULL
  info
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
  args <- list(...)
  args$aesthetics <- aesthetics
  theme <- color.theme(theme)
  if (is.null(discrete))
    discrete <- theme$type == "qualitative"
  if (discrete) {
    args$palette <- theme$palette
    args$guide <- ifnot.null(args$guide, "legend")
    args$na.value <- ifnot.null(args$na.value, theme$options$na.color)
    scale <- do.call(ggplot2::discrete_scale, args)
  } else {
    if (theme$type == "qualitative")
      stop("qualitative color theme can't be used for continuous scales")
    rescale_fun <- if (theme$type == "sequential") {
      scales::rescale
    } else if (theme$type == "diverging") {
      function(x, to = c(0, 1), from = range(x, na.rm = TRUE))
        scales::rescale_mid(x, to, from, mid = middle)
    }
    args$palette <- theme$ramp
    args$rescaler <- rescale_fun
    args$guide <- ifnot.null(args$guide, "colorbar")
    args$na.value <- ifnot.null(args$na.value, theme$options$na.color)
    scale <- do.call(ggplot2::continuous_scale, args)
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


to.colors <- function(x, theme, middle = 0, na.value = NULL) {
  theme <- color.theme(theme)
  if (is.null(na.value))
    na.value <- ifnot.null(theme$options$na.color, NA)
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
