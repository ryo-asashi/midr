#' Color Themes for Graphics
#'
#' @description
#' The \code{color_theme()} function is the main interface for working with "color_theme" objects.
#' It acts as an generic dispatcher that can retrieve a pre-defined theme by name, create a new theme from a vector of colors or a color-generating function, and modify an existing "color_theme" object.
#'
#' @details
#' The "color_theme" object is a special environment that provides two color-generating functions: \code{...$palette()} and \code{...$ramp()}.
#'
#' \code{...$palette()} takes an integer \code{n} and returns a vector of \code{n} discrete colors. It is primarily intended for qualitative themes, where distinct colors are used to represent categorical data.
#'
#' \code{...$ramp()} takes a numeric vector \code{x} with values in the [0, 1] interval, and returns a vector of corresponding colors. It maps numeric values onto a continuous color gradient, making it suitable for sequential and diverging themes.
#'
#' This function, \code{color_theme()}, is a versatile dispatcher that behaves differently depending on the class of the \code{object} argument.
#' If \code{object} is a character string (e.g., "Viridis", "grDevices/RdBu_r@q?alpha=.5"), the string is parsed according to the theme name syntax, and the corresponding pre-defined theme is loaded (see the "Theme Name Syntax" section for details).
#' If \code{object} is a color kernel (i.e., a character vector of colors, a palette function, or a ramp function), a new color theme is created from the kernel.
#' If \code{object} is a "color_theme" object, the function returns a modified version of the theme, applying any other arguments to update its settings.
#'
#' @section Theme Name Syntax:
#' When retrieving a theme using a character string, you can use a special syntax to specify the source and apply modifications:
#'
#' "\code{[(source)/](name)[_r][@(type)][?(query)]}"
#'
#' \itemize{
#'   \item source: (optional) the source package or collection of the theme (e.g., "grDevices").
#'   \item name: the name of the theme (e.g., "RdBu").
#'   \item "_r": (optional) a suffix to reverse the color order.
#'   \item type: (optional) the desired theme type, which will be matched with "sequential", "diverging" or "qualitative" (i.e., "s", "d", and "q" are sufficient, but longer strings such as "seq", "div", "qual" are also possible).
#'   \item query: (optional) a query string to overwrite the color theme's metadata including specific theme options or kernel arguments. Pairs are in \code{key=value} format and separated by \code{;} or \code{&} (e.g., "...?alpha=0.5;na.color='gray50'"). Possible keys include "name", "source", "type", "reverse" and any item of the theme's \code{options} and \code{kernel.args}.
#' }
#'
#' @param object a character string to retrieve a pre-defined theme, a color kernel (i.e., a vector of colors or a color generating function) to create a new theme, or a "color_theme" object to be modified.
#' @param kernel.args a list of arguments to be passed to the color kernel.
#' @param options a list of option values to control the color theme's behavior.
#' @param name a character string for the color theme name.
#' @param source a character string for the source name of the color theme.
#' @param type a character string specifying the type of the color theme. One of "sequential", "diverging", or "qualitative".
#' @param reverse logical. If \code{TRUE}, the order of colors is reversed.
#' @param env an environment where the color themes are registered.
#' @param ... optional named arguments used to modify the color theme. Any argument passed here will override the corresponding settings in \code{kernel.args} or \code{options}.
#'
#' @examples
#' # Retrieve a pre-defined theme
#' ct <- color_theme("Mako")
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, length.out = 5))
#'
#' # Use special syntax to get a reversed, qualitative theme with alpha value
#' ct <- color_theme("grDevices/Zissou 1_r@qual?alpha=0.75")
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, length.out = 5))
#'
#' # Create a new theme from a vector of colors
#' ct <- color_theme(c("#003f5c", "#7a5195", "#ef5675", "#ffa600"))
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, length.out = 5))
#'
#' # Create a new theme from a palette function
#' ct <- color_theme(grDevices::rainbow)
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, length.out = 5))
#'
#' # Modify an existing theme
#' ct <- color_theme(ct, type = "qualitative", kernel.args = list(v = 0.5))
#' ct$palette(5L)
#' ct$ramp(seq.int(0, 1, length.out = 5))
#' @returns
#' \code{color_theme()} returns a "color_theme" object, which is an environment with the special class attribute, containing the \code{...$palette()} and \code{...$ramp} functions, along with other metadata about the theme.
#'
#' @seealso \code{\link{scale_color_theme}}, \code{\link{map_to_colors}}, \code{\link{register_color_theme}}, \code{\link{color_themes}}
#'
#' @export color_theme
#'
color_theme <- function(object, ...)
UseMethod("color_theme")


#' @rdname color_theme
#' @exportS3Method midr::color_theme
#'
color_theme.color_theme <- function(
    object, kernel.args = NULL, options = NULL, name = NULL, source = NULL,
    type = NULL, reverse = FALSE, ...
) {
  if (nargs() == 1L) return(object)
  modify_theme(
    theme = object, kernel.args = kernel.args, options = options,
    name = name, source = source, type = type, reverse = reverse, ...
  )
}


#' @rdname color_theme
#' @exportS3Method midr::color_theme
#'
color_theme.character <- function(
    object, kernel.args = list(), options = list(), name = NULL, source = NULL,
    type = NULL, reverse = FALSE, env = NULL, ...
) {
  if (length(object) > 1L || (length(object) == 1L && is_color(object) && !is_registered_theme(object, env))) {
    theme <- make_theme(object, kernel.args, options, name, source, type)
    if (reverse || length(list(...)) > 0L) {
      theme <- modify_theme(theme, reverse = reverse, ...)
    }
    return(theme)
  }
  if (length(object) != 1L) {
    stop("'object' must be a single theme name or a vector of valid colors.")
  }
  parsed <- tryCatch(parse_theme_name(object), error = function(e) {
    stop("'", object, "' can't be parsed: ", conditionMessage(e), call. = FALSE)
  })
  env <- env %||% color_theme_registry()
  reg_args <- get_theme(parsed$name, parsed$source %||% source, env)
  theme <- do.call(make_theme, reg_args)
  mod_args <- c(
    list(
      theme = theme, kernel.args = kernel.args, options = options,
      name = name, source = source, type = type, reverse = reverse
    ),
    list(...)
  )
  for (item in names(parsed$args)) {
    mod_args[[item]] <- parsed$args[[item]]
  }
  do.call(modify_theme, mod_args)
}


#' @rdname color_theme
#' @exportS3Method midr::color_theme
#'
color_theme.default <- function(
    object, kernel.args = list(), options = list(), name = NULL, source = NULL,
    type = NULL, reverse = FALSE, ...
) {
  if (is.null(object))
    return(NULL)
  if (is_kernel(object, args = kernel.args)) {
    theme <- make_theme(object, kernel.args, options, name, source, type)
    if (reverse || length(list(...)) > 0L) {
      theme <- modify_theme(theme, reverse = reverse, ...)
    }
    return(theme)
  }
  stop("'object' of class '", class(object)[1L], "' can't be converted to a color theme.")
}


# internal helpers

is_color_theme <- function(object) {
  inherits(object, "color_theme")
}

is_registered_theme <- function(name, env = NULL) {
  if (!is.character(name) || length(name) != 1L)
    return(FALSE)
  env <- env %||% tryCatch(color_theme_registry(), error = function(e) NULL)
  if (is.null(env))
    return(FALSE)
  parsed <- tryCatch(parse_theme_name(name), error = function(e) NULL)
  !is.null(parsed) && exists(parsed$name, envir = env, inherits = FALSE)
}

is_color <- function(x) {
  if (!is.character(x) || length(x) == 0L)
    return(FALSE)
  e <- try(grDevices::col2rgb(x, alpha = TRUE), silent = TRUE)
  !inherits(e, "try-error")
}

is_color_with_alpha <- function(x) {
  if (!is.character(x) || length(x) == 0L)
    return(FALSE)
  e <- try(grDevices::col2rgb(x, alpha = TRUE), silent = TRUE)
  !inherits(e, "try-error") && any(e[4L, ] != 255)
}

is_ramp <- function(fun, x.test = seq.int(0, 1, 0.2), args = list()) {
  if (!is.numeric(x.test) || any(x.test < 0 | 1 < x.test))
    stop("'x.test' must be a numeric vector of values within [0, 1]")
  if (length(fun) != 1L || !is.function(fun))
    return(FALSE)
  e <- try(do.call(fun, c(list(x.test), args)), silent = TRUE)
  !inherits(e, "try-error") && length(e) == length(x.test) && is_color(e)
}

is_palette <- function(fun, n.test = 2L, args = list()) {
  if (!is.numeric(n.test) || length(n.test) != 1L)
    stop("'n.test' must be an integer")
  if (length(fun) != 1L || !is.function(fun))
    return(FALSE)
  e <- try(do.call(fun, c(list(n.test), args)), silent = TRUE)
  !inherits(e, "try-error") && length(e) == n.test && is_color(e)
}

is_kernel <- function(object, args = list()) {
  kernel_class(object, args = args) != "unknown"
}

kernel_class <- function(object, args = list()) {
  if (is_palette(object, args = args)) {
    "palette"
  } else if (is_ramp(object, args = args)) {
    "ramp"
  } else if (is_color(object)) {
    "color"
  } else if (is.null(object)) {
    "null"
  } else {
    "unknown"
  }
}

kernel_size <- function(object, args = list(), kcl = NULL, ok = 1L, ng = 256L) {
  kcl <- kcl %||% kernel_class(object, args = args)
  if (kcl == "unknown")
    stop("'object' must be a palette function, ramp function or color vector")
  if (kcl == "ramp") return(Inf)
  if (kcl == "null") return(0L)
  if (kcl == "color") return(length(object))

  init.ng <- ng
  while (abs(ok - ng) > 1L) {
    m <- (ok + ng) %/% 2L
    if (is_palette(object, n.test = m, args = args))
      ok <- m
    else
      ng <- m
  }
  if (ng < init.ng) as.integer(ok) else Inf
}

lazy_load_kernel <- function(object, args = list()) {
  if (is_kernel(object, args = args))
    return(object)
  if (!(is.character(object) || is.list(object)))
    stop("'object' must be a kernel or a character vector/list specifying expression and namespace")
  text <- object[[1L]]
  namespace <- if (length(object) == 1L) "base" else object[[2L]]
  object <- eval(str2expression(text), envir = rlang::ns_env(namespace))
  if (!is_kernel(object, args = args))
    stop("'object' can't be loaded as a valid kernel")
  object
}

colors_to_ramp <- function(colors) {
  if (!is_color(colors))
    stop("'colors' must be a character vector of color names")
  .Ramp <- if (is_color_with_alpha(colors)) {
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
  structure(ramp, class = c("function", "ramp"))
}


# theme registry & parser

get_theme <- function(name, source = NULL, env = color_theme_registry()) {
  if (!exists(name, envir = env, inherits = FALSE))
    stop(sprintf("'%s' is not found in the color theme environment", name))
  if (is.null(source))
    source <- utils::tail(names(env[[name]]), 1L)
  args <- env[[name]][[source]]
  if (is.null(args))
    stop(sprintf("'%s/%s' is not found in the color theme environment", source, name))
  args
}

parse_theme_name <- function(name) {
  args <- list()
  source <- NULL
  # 1. query: ?key1=val1&key2=val2;key3=val3
  if (grepl("\\?", name)) {
    query <- sub(".*\\?", "", name)
    name <- sub("\\?.*", "", name)
    pairs <- unlist(strsplit(query, "[;&]"))
    pairs <- pairs[nzchar(pairs)]
    for (p in pairs) {
      kv <- unlist(strsplit(p, "=", fixed = TRUE))
      if (length(kv) != 2L) stop("Invalid query parameter: ", p)
      key <- trimws(kv[1L])
      val <- trimws(kv[2L])
      val <- gsub("^['\"]|['\"]$", "", val)
      args[[key]] <- utils::type.convert(val, as.is = TRUE)
    }
  }
  # 2. type: @qual, @seq, @div
  if (grepl("@", name)) {
    suf <- sub(".*@", "", name)
    name <- sub("@.*", "", name)
    args$type <- suf
  }
  # 3. reverse: _r
  if (grepl("_r$", name)) {
    name <- sub("_r$", "", name)
    args$reverse <- TRUE
  }
  # 4. source: package/name
  if (grepl("/", name)) {
    source <- sub("/.*", "", name)
    name <- sub(".*/", "", name)
  }
  list(name = name, source = source, args = args)
}


# theme Construction & modification

make_theme <- function(
    kernel, kernel.args = list(), options = list(),
    name = NULL, source = NULL, type = NULL
) {
  kernel <- lazy_load_kernel(kernel, args = kernel.args)
  if (!is.list(kernel.args))
    stop("'kernel.args' must be a list")
  if (!is.list(options))
    stop("'options' must be a list")
  if (!is.null(name) && (!is.character(name) || length(name) != 1L))
    stop("'name' must be a character string or NULL")
  if (!is.null(source) && (!is.character(source) || length(source) != 1L))
    stop("'source' must be a character string or NULL")
  type <- match.arg(type, c("sequential", "diverging", "qualitative"))
  kcl <- kernel_class(kernel, args = kernel.args)
  if (kcl == "color") {
    kernel.args$mode <- kernel.args$mode %||%
      switch(type, qualitative = "palette", "ramp")
    kernel.args$alpha <- kernel.args$alpha %||% NA
  }
  options$kernel.size <- options$kernel.size %||%
    kernel_size(kernel, args = kernel.args, kcl = kcl)
  options$palette.formatter <- options$palette.formatter %||%
    switch(type, qualitative = "recycle", "interpolate")
  options$palette.reverse <- options$palette.reverse %||% FALSE
  options$ramp.rescaler <- options$ramp.rescaler %||% c(0, 1)
  options$na.color <- options$na.color %||% NA
  options$reverse.method <- options$reverse.method %||% NA
  theme <- list(
    kernel = kernel,
    kernel.class = kcl,
    kernel.args = kernel.args,
    options = options,
    name = name,
    source = source,
    type = type
  )
  compile_theme(theme)
}


compile_theme <- function(theme) {
  kernel <- theme$kernel
  kcl <- theme$kernel.class
  kernel.args <- theme$kernel.args
  options <- theme$options
  theme$palette <- function(n) {
    if (!is.numeric(n) || length(n) != 1L)
      stop("'n' must be an integer")
    if ((n <- as.integer(n)) <= 0L)
      return(character(0L))
    nks <- min(n, options$kernel.size)
    if (kcl == "palette") {
      ret <- do.call(kernel, c(list(n = nks), kernel.args))
    } else if (kcl == "ramp") {
      ret <- do.call(kernel, c(list(x = seq.int(0, 1, length.out = n)), kernel.args))
    } else if (kcl == "color") {
      cols <- kernel
      if (!is.null(kernel.args$alpha) && !is.na(kernel.args$alpha))
        cols <- grDevices::adjustcolor(cols, alpha.f = kernel.args$alpha)
      if (identical(kernel.args$mode, "palette")) {
        ret <- cols[seq_len(nks)]
      } else {
        mks <- min(256L, options$kernel.size)
        ret <- colors_to_ramp(cols[seq_len(mks)])(x = seq.int(0, 1, length.out = n))
      }
    } else {
      ret <- rep_len(NA_character_, length.out = n)
    }
    if (isTRUE(options$palette.reverse))
      ret <- rev(ret)
    if (length(ret) < n) {
      if (options$palette.formatter == "recycle") {
        ret <- rep_len(ret, length.out = n)
      } else if (options$palette.formatter == "interpolate") {
        ret <- colors_to_ramp(ret)(x = seq.int(0, 1, length.out = n))
      } else if (options$palette.formatter %in% c("fillna", "fill.na")) {
        ret <- ret[seq_len(n)]
      } else {
        stop("'n' must be equal to or smaller than the 'kernel.size' option")
      }
    }
    ret[is.na(ret)] <- options$na.color
    ret
  }
  theme$ramp <- function(x) {
    if (!is.numeric(x))
      stop("'x' must be a numeric vector")
    eps <- .Machine$double.eps
    ng <- is.na(x) | x < (0 - eps) | (1 + eps) < x
    ret <- character(length(x))
    ret[ng] <- NA_character_
    if (all(ng)) {
      ret[is.na(ret)] <- options$na.color
      return(ret)
    }
    x_ok <- pmin(1, pmax(0, x[!ng]))
    rsc <- options$ramp.rescaler %||% c(0, 1)
    if (!is.numeric(rsc) || length(rsc) != 2L)
      stop("'ramp.rescaler' option must be a numeric vector of length 2")
    x_ok <- x_ok * diff(rsc) + rsc[1L]
    mks <- min(256L, options$kernel.size)
    if (kcl == "palette") {
      cols <- do.call(kernel, c(list(n = mks), kernel.args))
      ret[!ng] <- colors_to_ramp(cols)(x_ok)
    } else if (kcl == "ramp") {
      ret[!ng] <- do.call(kernel, c(list(x = x_ok), kernel.args))
    } else if (kcl == "color") {
      cols <- kernel
      if (!is.null(kernel.args$alpha) && !is.na(kernel.args$alpha))
        cols <- grDevices::adjustcolor(cols, alpha.f = kernel.args$alpha)
      ret[!ng] <- colors_to_ramp(cols[seq_len(mks)])(x_ok)
    } else {
      ret[!ng] <- NA_character_
    }
    ret[is.na(ret)] <- options$na.color
    ret
  }
  structure(theme, class = "color_theme")
}

modify_theme <- function(
    theme, kernel.args = NULL, options = NULL,
    name = NULL, source = NULL, type = NULL, reverse = FALSE, ...
) {
  if (!is_color_theme(theme))
    stop("'theme' must be a color theme")
  if (!is.null(name))
    theme$name <- name
  if (!is.null(source))
    theme$source <- source
  if (!is.null(type)) {
    old_type <- theme$type
    new_type <- match.arg(type, c("sequential", "diverging", "qualitative"))
    theme$type <- new_type
    if (new_type != old_type) {
      if (theme$kernel.class == "color") {
        default_old_mode <- switch(old_type, qualitative = "palette", "ramp")
        if (identical(theme$kernel.args$mode, default_old_mode)) {
          theme$kernel.args$mode <- switch(new_type, qualitative = "palette", "ramp")
        }
      }
      default_old_fmt <- switch(old_type, qualitative = "recycle", "interpolate")
      if (identical(theme$options$palette.formatter, default_old_fmt)) {
        theme$options$palette.formatter <- switch(new_type, qualitative = "recycle", "interpolate")
      }
    }
  }
  if (!is.null(kernel.args) && length(kernel.args) > 0L) {
    for (item in names(kernel.args)) {
      theme$kernel.args[[item]] <- kernel.args[[item]]
    }
  }
  if (!is.null(options) && length(options) > 0L) {
    for (item in names(options)) {
      theme$options[[item]] <- options[[item]]
    }
  }
  dots <- list(...)
  if (length(dots) > 0L) {
    for (item in names(dots)) {
      if (item %in% names(theme$options)) {
        theme$options[[item]] <- dots[[item]]
      } else if (item %in% names(theme$kernel.args)) {
        theme$kernel.args[[item]] <- dots[[item]]
      } else {
        theme$kernel.args[[item]] <- dots[[item]]
      }
    }
  }
  if (isTRUE(reverse)) {
    method <- theme$options$reverse.method
    if (!is.null(method) && !is.na(method)) {
      tmp_env <- list2env(theme, parent = baseenv())
      eval(str2expression(method), envir = tmp_env)
      for (nm in names(theme)) theme[[nm]] <- tmp_env[[nm]]
    } else if (theme$kernel.class == "color") {
      theme$kernel <- rev(theme$kernel)
    } else {
      theme$options$palette.reverse <- !theme$options$palette.reverse
      theme$options$ramp.rescaler <- rev(theme$options$ramp.rescaler)
    }
  }
  compile_theme(theme)
}


#' @export
#'
`$<-.color_theme` <- function(x, name, value) {
  x <- NextMethod()
  if (!name %in% c("palette", "ramp")) {
    x <- compile_theme(x)
  }
  x
}

#' @export
#'
`[[<-.color_theme` <- function(x, i, value) {
  x <- NextMethod()
  if (is.character(i) && !all(i %in% c("palette", "ramp"))) {
    x <- compile_theme(x)
  }
  x
}


# misc methods for color themes

#' @exportS3Method base::plot
#'
plot.color_theme <- function(x, n = NULL, text = x$name, ...) {
  if (!is.null(n)) {
    colors <- x$palette(n)
  } else if (x$type == "qualitative") {
    n <- min(48L, x$options$kernel.size, na.rm = TRUE)
    colors <- x$palette(n)
  } else {
    n <- 256L
    colors <- x$ramp(seq.int(0, 1, length.out = n))
  }
  opar <- graphics::par("mai", "mar")
  on.exit(graphics::par(opar))
  graphics::par(mar = c(1, 1, 1, 1))
  graphics::plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
                 axes = FALSE, xlab = "", ylab = "")
  graphics::text(0.5, 0.7, text, pos = 3L)
  graphics::rect((seq_len(n) - 1L) / n, 0.3, seq_len(n) / n, 0.7,
                 col = colors, border = NA)
  invisible(x)
}

#' @exportS3Method base::print
#'
print.color_theme <- function(x, display = TRUE, ...) {
  type <- x$type
  substr(type, 1L, 1L) <- toupper(substr(type, 1L, 1L))
  text <- paste0(type, " Color Theme")
  if (!is.null(x$name))
    text <- paste0(text, ' : "', x$name, '"')
  cat(text, "\n", sep = "")
  if (display)
    plot.color_theme(x, text = text)
  invisible(x)
}
