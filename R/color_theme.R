#' Color Themes for Graphics
#'
#' @description
#' The \code{color.theme()} function is the main interface for working with "color.theme" objects. It acts as a dispatcher that, depending on the class of \code{object}, can retrieve a pre-defined theme by name (see the "Theme Name Syntax" section), create a new theme from a vector of colors or a color-generating function, and modify an existing "color.theme" object.
#'
#' @details
#' The "color.theme" object is a special environment that provides two color-generating functions: \code{palette()} and \code{ramp()}.
#'
#' \code{palette()} takes an integer \code{n} and returns a vector of \code{n} discrete colors. It is primarily intended for qualitative themes, where distinct colors are used to represent categorical data.
#'
#' \code{ramp()} takes a numeric vector \code{x} with values in the [0, 1] interval, and returns a vector of corresponding colors. It maps numeric values onto a continuous color gradient, making it suitable for sequential and diverging themes.
#'
#' @section Theme Name Syntax:
#' When retrieving a theme using a character string, you can use a special syntax to specify the source and apply modifications:
#'
#' "\code{[source/](name)[_r][@type][?query]}"
#'
#' \itemize{
#'   \item source: (optional) the source package or collection of the theme (e.g., "grDevices").
#'   \item name: the name of the theme (e.g., "RdBu").
#'   \item "_r": (optional) a suffix to reverse the color order.
#'   \item type: (optional) the desired theme type, which will be matched with "sequential", "diverging" or "qualitative" (i.e., "s", "d", and "q" are sufficient, but longer character strings such as "seq", "div", "qual" are also possible).
#'   \item query: (optional) a query string to overwrite the color theme's metadata including specific theme options or kernel arguments. Pairs are in \code{key=value} format and separated by \code{;} or \code{&} (e.g., "...?alpha=0.5;na.color='gray50'"). Possible keys include "name", "source", "type", "reverse" and any item of the theme's \code{options} and \code{kernel.args}.
#' }
#'0
#' @param object one of the following items: (i) a character string following the theme name syntax (e.g., "grDevices/RdBu@seq", see the "Theme Name Syntax" section), (ii) a character vector of color names, a palette function, or a ramp function to be used as a color kernel to create a color theme, or (iii) a "color.theme" object.
#' @param ... optional arguments to be passed on to \code{get.theme()}, \code{make.theme()} or \code{modify.theme()}.
#' @param kernel a color vector, a palette function, or a ramp function that serves as the basis for generating colors.
#' @param kernel.args a list of arguments to be passed to the color kernel.
#' @param options a list of option values to control the color theme's behavior.
#' @param name a character string for the color theme name.
#' @param source a character string for the source name of the color theme.
#' @param type a character string specifying the type of the color theme. One of "sequential", "diverging", or "qualitative".
#' @param reverse logical. If \code{TRUE}, the order of colors is reversed.
#'
#' @examples
#' # Retrieve a pre-defined theme
#' ct <- color.theme("Mako")
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, 1/4))
#'
#' # Use special syntax to get a reversed, qualitative theme with alpha value
#' ct <- color.theme("grDevices/Zissou 1_r@qual?alpha=0.75")
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, 1/4))
#'
#' # Create a new theme from a vector of colors
#' ct <- color.theme(c("#003f5c", "#7a5195", "#ef5675", "#ffa600"))
#' ct$palette(10L)
#'
#' # Create a new theme from a palette function
#' ct <- color.theme(grDevices::rainbow)
#' ct$palette(10L)
#' @returns
#' \code{color.theme()} returns a "color.theme" object, which is an environment with the special class attribute, containing the \code{palette()} and \code{ramp()} functions, along with other metadata about the theme.
#'
#' @seealso \code{\link{plot.mid}}, \code{\link{ggmid}}, \code{\link{scale_color_theme}}, \code{\link{set.color.theme}}, \code{\link{color.theme.info}}
#'
#' @export color.theme
#'
color.theme <- function(
    object, kernel.args = list(), options = list(),
    name = NULL, source = NULL, type = NULL, reverse = FALSE, ...
) {
  if (is.null(object)) {
    NULL
  } else if (is.color.theme(object)) {
    if (nargs() == 1L)
      return(object)
    args <- as.list(object)
    args <- args[c("kernel", "kernel.args", "options", "name", "source", "type")]
    theme <- do.call(make.theme, args)
    modify.theme(theme, kernel.args, options, name, source, type, reverse, ...)
  } else if (is.kernel(object, args = kernel.args)) {
    make.theme(object, kernel.args, options, name, source, type)
  } else if (is.character(object) && length(object) == 1L) {
    parsed <- try(parse.theme.name(object), silent = TRUE)
    if (inherits(parsed, "try-error"))
      stop(paste0(object, " can't be parsed"))
    args <- get.theme(parsed$name, ifnot.null(parsed$source, source))
    args <- list(theme = do.call(make.theme, args), kernel.args = kernel.args,
                 options = options, name = name, source = source, type = type,
                 reverse = reverse, ...)
    for (item in names(parsed$args))
      args[[item]] <- parsed$args[[item]]
    do.call(modify.theme, args)
  } else {
    stop("'object' can't be converted to a color theme")
  }
}


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

is.color.theme <- function(object) {
  inherits(object, "color.theme")
}

is.kernel <- function(object, args = list()) {
  kernel.class(object, args = args) != "unknown"
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

kernel.size <- function(object, args = list(), ok = 1L, ng = 256L) {
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

get.theme <- function(name, source = NULL) {
  env <- color.theme.env()
  if (!exists(name, env))
    stop(sprintf("'%s' is not found in the color theme environment", name))
  if (is.null(source))
    source <- utils::tail(names(env[[name]]), 1L)
  args <- env[[name]][[source]]
  if (is.null(args))
    stop(sprintf("'%s#%s' is not found in the color theme environment",
                 source, name))
  args
}

parse.theme.name <- function(name) {
  args <- new.env(parent = baseenv())
  source <- NULL
  # prefix: shortcut for source --------
  pre <- sub("/.*", "", name)
  if (pre != name) {
    name <- sub(".*/", "", name)
    source <- pre
  }
  # suffix: query --------
  query <- sub(".*\\?", "", name)
  if (query != name) {
    name <- sub("\\?.*", "", name)
    query <- unlist(strsplit(query, "&", fixed = TRUE))
    eval(str2expression(query), envir = args, enclos = baseenv())
  }
  args <- as.list(args)
  # suffix: shortcuts for type --------
  suf <- sub(".*@", "", name)
  if (suf != name) {
    name <- sub("@.*", "", name)
    args$type <- suf
  }
  # suffix: shortcut for reverse --------
  if (grepl("_r$", name)) {
    name <- sub("_r$", "", name)
    args$reverse <- TRUE
  }
  list(name = name, source = source, args = args)
}

make.theme <- function(
    kernel, kernel.args = list(), options = list(),
    name = NULL, source = NULL, type = NULL
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
  type <- match.arg(type, c("sequential", "diverging", "qualitative"))
  kcl <- kernel.class(kernel, args = kernel.args)
  if (kcl == "color") {
    kernel.args$mode <- ifnot.null(
      kernel.args$mode, switch(type, qualitative = "palette", "ramp")
    )
    kernel.args$alpha <- ifnot.null(kernel.args$alpha, NA)
  }
  options$kernel.size <- ifnot.null(options$kernel.size,
                                    kernel.size(kernel, args = kernel.args))
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

modify.theme <- function(
    theme, kernel.args = NULL, options = NULL,
    name = NULL, source = NULL, type = NULL, reverse = FALSE, ...
) {
  if (!is.color.theme(theme))
    stop("'theme' must be a color theme")
  if (!is.null(name))
    theme$name <- name
  if (!is.null(source))
    theme$source <- source
  if (!is.null(type)) {
    type <- match.arg(type, c("sequential", "diverging", "qualitative"))
    theme$type <- type
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
  if (!is.null(dots <- list(...))) {
    for (item in names(dots)) {
      if (item %in% names(theme$kernel.args)) {
        theme$kernel.args[[item]] <- dots[[item]]
      } else if (item %in% names(theme$options)) {
        theme$options[[item]] <- dots[[item]]
      }
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
