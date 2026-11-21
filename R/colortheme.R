# R/colortheme.R

#' Color Themes for Graphics
#'
#' @description
#' The \code{colortheme()} function is the main interface for working with \code{"colortheme"} objects.
#' It acts as a generic dispatcher that can retrieve a pre-defined theme by name, create a new theme from a vector of colors or a color-generating function, and modify an existing \code{"colortheme"} object.
#'
#' @details
#' A \code{"colortheme"} object is a specialized environment that provides two primary color-generating functions: \code{$palette()} and \code{$ramp()}.
#'
#' \code{$palette(n)} takes an integer \code{n} and returns a character vector of \code{n} discrete colors. It is primarily intended for qualitative themes, where distinct colors represent categorical data.
#'
#' \code{$ramp(x)} takes a numeric vector \code{x} with values in the \code{[0, 1]} interval and returns a character vector of corresponding colors. It maps numeric values onto a continuous color gradient, making it suitable for sequential and diverging themes.
#'
#' \code{colortheme()} dispatches based on the class of \code{object}:
#' \itemize{
#'   \item If \code{object} is a character string (e.g., \code{"Viridis"}, \code{"grDevices/RdBu_r@q?alpha=.5"}), it is parsed according to the theme name syntax and loaded from the registry.
#'   \item If \code{object} is a color kernel (a character vector of colors, a palette function, or a ramp function), a new color theme is constructed.
#'   \item If \code{object} is already a \code{"colortheme"} object, a modified copy is returned with updated settings.
#' }
#'
#' @section Theme Name Syntax:
#' When retrieving a registered theme by string, you can use a compact URL-like syntax to specify the source and apply on-the-fly modifications:
#'
#' \code{"[(source)/](name)[_r][@(type)][?(query)]"}
#'
#' \itemize{
#'   \item \code{source}: (optional) the source package or collection (e.g., \code{"grDevices"}).
#'   \item \code{name}: the name of the theme (e.g., \code{"RdBu"}).
#'   \item \code{"_r"}: (optional) suffix to reverse the color order.
#'   \item \code{type}: (optional) desired theme type, matched against \code{"sequential"}, \code{"diverging"}, or \code{"qualitative"} (abbreviations like \code{"s"}, \code{"d"}, \code{"q"} or \code{"seq"}, \code{"div"}, \code{"qual"} are supported).
#'   \item \code{query}: (optional) key-value pairs separated by \code{;} or \code{&} (e.g., \code{"?alpha=0.5;na.color='gray50'"}) to override theme metadata, \code{options}, or \code{kernel.args}.
#' }
#'
#' @param object a character string to retrieve a pre-defined theme, a color kernel (vector of colors or color-generating function) to create a new theme, or a \code{"colortheme"} object to modify.
#' @param kernel.args a list of arguments passed to the color kernel.
#' @param options a list of option values controlling the color theme's behavior.
#' @param name a character string for the color theme name.
#' @param source a character string for the source name of the color theme.
#' @param type a character string specifying the type of the color theme: \code{"sequential"}, \code{"diverging"}, or \code{"qualitative"}.
#' @param reverse logical. If \code{TRUE}, the order of colors is reversed.
#' @param env an environment where color themes are registered.
#' @param x a \code{"colortheme"} object for \code{print()} and \code{plot()}.
#' @param n optional integer specifying the number of colors to display in \code{plot()}.
#' @param text optional character string for the title displayed in \code{plot()}.
#' @param display logical. If \code{TRUE} (default), \code{print()} also plots the color swatch.
#' @param ... optional named arguments used to modify the color theme (overriding corresponding entries in \code{options} or \code{kernel.args}).
#'
#' @returns
#' \code{colortheme()} returns a \code{"colortheme"} object (an environment with class \code{c("colortheme", "environment")}) containing \code{$palette()} and \code{$ramp()} functions along with theme metadata.
#'
#' @seealso \code{\link{colorthemes}}, \code{\link{register_colortheme}}, \code{\link{as_palette}}, \code{\link{map_to_colors}}, \code{\link{scale_color_theme}}
#'
#' @keywords internal
#'
colortheme <- function(object, ...)
UseMethod("colortheme")


#' @rdname colortheme
#' @exportS3Method midr::colortheme
colortheme.colortheme <- function(
    object, kernel.args = NULL, options = NULL, name = NULL, source = NULL,
    type = NULL, reverse = FALSE, ...
) {
  if (nargs() == 1L) return(object)
  theme <- modify_theme(
    theme = object, kernel.args = kernel.args, options = options,
    name = name, source = source, type = type, reverse = reverse, ...
  )
  theme
}


#' @rdname colortheme
#' @exportS3Method midr::colortheme
colortheme.character <- function(
    object, kernel.args = list(), options = list(), name = NULL, source = NULL,
    type = NULL, reverse = FALSE, env = NULL, ...
) {
  if (
    length(object) > 1L ||
    (length(object) == 1L && is_color(object) && !is_registered_theme(object, env))
  ) {
    theme <- make_theme(object, kernel.args, options, name, source, type)
    if (reverse || length(list(...)) > 0L) {
      theme <- modify_theme(theme, reverse = reverse, ..., inplace = TRUE)
    }
    return(theme)
  }
  if (length(object) != 1L) {
    stop("'object' must be a single theme name or a vector of valid colors.", call. = FALSE)
  }
  parsed <- tryCatch(parse_theme_name(object), error = function(e) {
    stop("'", object, "' can't be parsed: ", conditionMessage(e), call. = FALSE)
  })
  env <- env %||% colortheme_registry()
  reg_args <- get_theme(parsed$name, parsed$source %||% source, env)
  theme <- do.call(make_theme, reg_args)
  mod_args <- c(
    list(
      theme = theme, kernel.args = kernel.args, options = options, name = name,
      source = source, type = type, reverse = reverse, inplace = TRUE
    ),
    list(...)
  )
  for (item in names(parsed$args)) {
    mod_args[[item]] <- parsed$args[[item]]
  }
  theme <- do.call(modify_theme, mod_args)
  theme
}


#' @rdname colortheme
#' @exportS3Method midr::colortheme
colortheme.default <- function(
    object, kernel.args = list(), options = list(), name = NULL, source = NULL,
    type = NULL, reverse = FALSE, ...
) {
  if (is.null(object)) return(NULL)
  if (is_kernel(object, args = kernel.args)) {
    theme <- make_theme(object, kernel.args, options, name, source, type)
    if (reverse || length(list(...)) > 0L) {
      theme <- modify_theme(theme, reverse = reverse, ...)
    }
    return(theme)
  }
  stop(
    "'object' of class '", class(object)[1L],
    "' can't be converted to a 'colortheme' object.",
    call. = FALSE
  )
}



# global helpers

`%||%` <- function(x, y) if (is.null(x)) y else x

is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}


# local helpers

is_colortheme <- function(object) {
  inherits(object, "colortheme")
}

is_registered_theme <- function(name, env = NULL) {
  if (!is.character(name) || length(name) != 1L) return(FALSE)
  env <- env %||% tryCatch(colortheme_registry(), error = function(e) NULL)
  if (is.null(env)) return(FALSE)
  parsed <- tryCatch(parse_theme_name(name), error = function(e) NULL)
  !is.null(parsed) && exists(parsed$name, envir = env, inherits = FALSE)
}

is_color <- function(x) {
  if (!is.character(x) || length(x) == 0L) return(FALSE)
  e <- try(grDevices::col2rgb(x, alpha = TRUE), silent = TRUE)
  !inherits(e, "try-error")
}

is_color_with_alpha <- function(x) {
  if (!is.character(x) || length(x) == 0L) return(FALSE)
  e <- try(grDevices::col2rgb(x, alpha = TRUE), silent = TRUE)
  !inherits(e, "try-error") && any(e[4L, ] != 255)
}

is_ramp <- function(fun, x.test = seq.int(0, 1, 0.2), args = list()) {
  if (!is.numeric(x.test) || any(x.test < 0 | 1 < x.test))
    stop("'x.test' must be a numeric vector of values within [0, 1]", call. = FALSE)
  if (length(fun) != 1L || !is.function(fun)) return(FALSE)
  e <- try(do.call(fun, c(list(x.test), args)), silent = TRUE)
  !inherits(e, "try-error") && length(e) == length(x.test) && is_color(e)
}

is_palette <- function(fun, n.test = 2L, args = list()) {
  if (!is.numeric(n.test) || length(n.test) != 1L)
    stop("'n.test' must be an integer", call. = FALSE)
  if (length(fun) != 1L || !is.function(fun)) return(FALSE)
  e <- try(do.call(fun, c(list(n.test), args)), silent = TRUE)
  !inherits(e, "try-error") && length(e) == n.test && is_color(e)
}

is_kernel <- function(object, args = list()) {
  get_kernel_class(object, args = args) != "unknown"
}

get_kernel_class <- function(object, args = list()) {
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

get_kernel_size <- function(object, args = list(), kernel.class = NULL, ok = 1L, ng = 256L) {
  kernel.class <- kernel.class %||% get_kernel_class(object, args = args)
  if (kernel.class == "unknown")
    stop("'object' must be a palette function, ramp function or color vector", call. = FALSE)
  if (kernel.class == "ramp") return(Inf)
  if (kernel.class == "null") return(0L)
  if (kernel.class == "color") return(length(object))

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
  if (is_kernel(object, args = args)) return(object)
  if (!(is.character(object) || is.list(object)))
    stop("'object' must be a kernel or a character vector/list specifying expression and namespace", call. = FALSE)
  text <- object[[1L]]
  namespace <- if (length(object) == 1L) "base" else object[[2L]]
  object <- eval(str2expression(text), envir = asNamespace(namespace))
  if (!is_kernel(object, args = args))
    stop("'object' can't be loaded as a valid kernel", call. = FALSE)
  object
}

colors_to_ramp <- function(colors) {
  if (!is_color(colors))
    stop("'colors' must be a character vector of color names", call. = FALSE)
  .Ramp <- if (is_color_with_alpha(colors)) {
    grDevices::colorRamp(colors, alpha = TRUE)
  } else {
    grDevices::colorRamp(colors, space = "Lab")
  }
  ramp <- function(x, direction = 1L, alpha = NULL, na.color = NA) {
    if (length(x) == 0L) return(character())
    res <- character(length(x))
    eps <- .Machine$double.eps
    ng <- is.na(x) | x < (0 - eps) | (1 + eps) < x
    res[ng] <- na.color
    if (all(ng)) return(res)
    x[!ng] <- pmin(1, pmax(0, x[!ng]))
    if (direction < 0) x[!ng] <- 1 - x[!ng]
    args <- as.data.frame(round(.Ramp(x[!ng])))
    if (!is.null(alpha)) args[[4L]] <- max(min(alpha, 1), 0) * 255
    names(args) <- c("red", "green", "blue", "alpha")[seq_len(length(args))]
    args$maxColorValue <- 255
    res[!ng] <- do.call(grDevices::rgb, args)
    res
  }
  environment(ramp) <- list2env(
    list(.Ramp = .Ramp), parent = parent.env(environment())
  )
  structure(ramp, class = c("function", "ramp"))
}


# theme registry lookup & parser

get_theme <- function(name, source = NULL, env = colortheme_registry()) {
  if (!exists(name, envir = env, inherits = FALSE))
    stop(sprintf("'%s' is not found in the color theme environment", name), call. = FALSE)
  if (is.null(source))
    source <- utils::tail(names(env[[name]]), 1L)
  args <- env[[name]][[source]]
  if (is.null(args))
    stop(sprintf("'%s/%s' is not found in the color theme environment", source, name), call. = FALSE)
  args
}

parse_theme_name <- function(name) {
  # prefix: shortcut for source --------
  if (grepl("/", name)) {
    source <- sub("/.*", "", name)
    name <- sub(".*/", "", name)
  } else {
    source <- NULL
  }
  # suffix: query --------
  query_env <- new.env(parent = baseenv())
  if (grepl("\\?", name)) {
    query <- unlist(strsplit(sub(".*\\?", "", name), "&", fixed = TRUE))
    eval(str2expression(query), envir = query_env, enclos = baseenv())
    name <- sub("\\?.*", "", name)
  }
  args <- as.list(query_env)
  # suffix: shortcuts for type --------
  if (grepl("@", name)) {
    args$type <- sub(".*@", "", name)
    name <- sub("@.*", "", name)
  }
  # suffix: shortcut for reverse --------
  if (grepl("_r$", name)) {
    args$reverse <- TRUE
    name <- sub("_r$", "", name)
  }
  list(name = name, source = source, args = args)
}


# theme construction & modification

make_theme <- function(
    kernel, kernel.args = list(), options = list(),
    name = NULL, source = NULL, type = NULL
) {
  kernel <- lazy_load_kernel(kernel, args = kernel.args)
  if (!is.list(kernel.args))
    stop("'kernel.args' must be a list", call. = FALSE)
  if (!is.list(options))
    stop("'options' must be a list", call. = FALSE)
  if (!is.null(name) && (!is.character(name) || length(name) != 1L))
    stop("'name' must be a character string or NULL", call. = FALSE)
  if (!is.null(source) && (!is.character(source) || length(source) != 1L))
    stop("'source' must be a character string or NULL", call. = FALSE)
  type <- match.arg(type, c("sequential", "diverging", "qualitative"))
  kernel.class <- get_kernel_class(kernel, args = kernel.args)
  if (kernel.class == "color") {
    kernel.args$mode <- kernel.args$mode %||%
      switch(type, qualitative = "palette", "ramp")
    kernel.args$alpha <- kernel.args$alpha %||% NA
  }
  options$kernel.size <- options$kernel.size %||%
    get_kernel_size(kernel, args = kernel.args, kernel.class = kernel.class)
  options$palette.formatter <- options$palette.formatter %||%
    switch(type, qualitative = "recycle", "interpolate")
  options$palette.reverse <- options$palette.reverse %||% FALSE
  options$ramp.rescaler <- options$ramp.rescaler %||% c(0, 1)
  options$ramp.steps <- options$ramp.steps %||%
    switch(type, qualitative = min(options$kernel.size, Inf), Inf)
  options$na.color <- options$na.color %||% NA
  options$reverse.method <- options$reverse.method %||% NA

  env <- list2env(
    list(
      kernel = kernel,
      kernel.class = kernel.class,
      kernel.args = kernel.args,
      options = options,
      name = name,
      source = source,
      type = type
    ),
    parent = parent.env(environment())
  )

  # palette --------
  env$palette <- function(n) {
    if (!is.numeric(n) || length(n) != 1L)
      stop("'n' must be an integer", call. = FALSE)
    if ((n <- as.integer(n)) <= 0L)
      return(character(0L))
    nks <- min(n, options$kernel.size)
    if (kernel.class == "palette") {
      ret <- do.call(kernel, c(list(n = nks), kernel.args))
    } else if (kernel.class == "ramp") {
      ret <- do.call(kernel, c(list(x = seq.int(0, 1, length.out = n)), kernel.args))
    } else if (kernel.class == "color") {
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
        stop("'n' must be equal to or smaller than the 'kernel.size' option", call. = FALSE)
      }
    }
    ret[is.na(ret)] <- options$na.color
    unname(ret)
  }
  environment(env$palette) <- env

  # ramp --------
  env$ramp <- function(x) {
    if (!is.numeric(x))
      stop("'x' must be a numeric vector", call. = FALSE)
    eps <- .Machine$double.eps
    ng <- is.na(x) | x < (0 - eps) | (1 + eps) < x
    ret <- character(length(x))
    ret[ng] <- NA_character_
    if (all(ng)) {
      ret[is.na(ret)] <- options$na.color
      return(ret)
    }
    x_ok <- pmin(1, pmax(0, x[!ng]))
    brks <- options$ramp.steps
    if (!is.null(brks) && !all(is.na(brks) | is.infinite(brks))) {
      if (!is.numeric(brks))
        stop("'ramp.steps' option must be an integer or a numeric vector of cutoffs", call. = FALSE)
      if (length(brks) == 1L && brks >= 1L && brks == as.integer(brks)) {
        k <- as.integer(brks)
        cuts <- seq.int(0, 1, length.out = k + 1L)
      } else {
        cuts <- c(-Inf, sort(unique(brks[0 < brks & brks < 1])), Inf)
        k <- length(cuts) - 1L
      }
      idx <- findInterval(x_ok, cuts, all.inside = TRUE)
      pts <- if (k == 1L) 0.5 else seq.int(0, 1, length.out = k)
      x_ok <- pts[idx]
    }
    rsc <- options$ramp.rescaler %||% c(0, 1)
    if (!is.numeric(rsc) || length(rsc) != 2L)
      stop("'ramp.rescaler' option must be a numeric vector of length 2", call. = FALSE)
    x_ok <- x_ok * diff(rsc) + rsc[1L]
    mks <- min(256L, options$kernel.size)
    if (kernel.class == "palette") {
      cols <- do.call(kernel, c(list(n = mks), kernel.args))
      ret[!ng] <- colors_to_ramp(cols)(x_ok)
    } else if (kernel.class == "ramp") {
      ret[!ng] <- do.call(kernel, c(list(x = x_ok), kernel.args))
    } else if (kernel.class == "color") {
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
  environment(env$ramp) <- env

  structure(env, class = c("colortheme", "environment"))
}

clone_theme <- function(theme) {
  env <- list2env(as.list(theme, all.names = TRUE), parent = parent.env(theme))
  environment(env$palette) <- env
  environment(env$ramp) <- env
  structure(env, class = class(theme))
}

modify_theme <- function(
    theme, kernel.args = NULL, options = NULL, name = NULL,
    source = NULL, type = NULL, reverse = FALSE, ..., inplace = FALSE
) {
  if (!inplace) {
    theme <- clone_theme(theme)
  }
  if (!is_colortheme(theme))
    stop("'theme' must be a 'colortheme' object", call. = FALSE)
  if (!is.null(name))
    theme$name <- name
  if (!is.null(source))
    theme$source <- source
  if (!is.null(type))
    theme$type <- match.arg(type, c("sequential", "diverging", "qualitative"))
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
      } else {
        theme$kernel.args[[item]] <- dots[[item]]
      }
    }
  }
  if (isTRUE(reverse)) {
    method <- theme$options$reverse.method
    if (!is.null(method) && !is.na(method)) {
      eval(str2expression(method), envir = theme, enclos = baseenv())
    } else if (theme$kernel.class == "color") {
      theme$kernel <- rev(theme$kernel)
    } else {
      theme$options$palette.reverse <- !theme$options$palette.reverse
      theme$options$ramp.rescaler <- rev(theme$options$ramp.rescaler)
    }
  }
  if (length(kernel.args) > 0L || length(dots) > 0L) {
    theme$kernel.class <- get_kernel_class(theme$kernel, args = theme$kernel.args)
  }
  theme
}


# S3 methods

#' @rdname colortheme
#' @exportS3Method base::plot
plot.colortheme <- function(x, n = NULL, text = x$name, ...) {
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


#' @rdname colortheme
#' @exportS3Method base::print
print.colortheme <- function(x, display = TRUE, ...) {
  type <- x$type
  substr(type, 1L, 1L) <- toupper(substr(type, 1L, 1L))
  text <- paste0(type, " Color Theme")
  if (!is.null(x$name)) {
    text <- paste0(text, ": '", x$name, "'")
  }
  header <- text
  if (!is.null(x$source)) {
    header <- paste0(header, " (from '", x$source, "')")
  }
  cat(header, "\n", sep = "")
  cat("\nKernel Class: ", x$kernel.class, "\n", sep = "")
  kernel <- x$kernel
  if (is.function(kernel)) {
    cat(
      "\nKernel Function:\n",
      sub("\\s*NULL$", "", deparse1(args(kernel))),
      "\n", sep = ""
    )
  } else if (is_color(kernel)) {
    cat(
      "\nKernel Colors:\n",
      paste(kernel[seq_len(min(length(kernel), 12L))], collapse = ", "),
      if (length(kernel) > 12L) ", ..." else "",
      "\n", sep = ""
    )
  }
  if (length(x$options) > 0L) {
    cat("\nOptions:\n")
    nms <- format(paste0("  ", names(x$options)), justify = "left")
    vals <- vapply(x$options, function(v) {
      if (is.function(v)) "<function>" else deparse1(v)
    }, character(1L))
    cat(paste0(nms, ": ", vals, collapse = "\n"), "\n", sep = "")
  }
  if (length(x$kernel.args) > 0L) {
    cat("\nKernel Arguments:\n")
    nms <- format(paste0("  ", names(x$kernel.args)), justify = "left")
    vals <- vapply(x$kernel.args, function(v) {
      if (is.function(v)) "<function>" else deparse1(v)
    }, character(1L))
    cat(paste0(nms, ": ", vals, collapse = "\n"), "\n", sep = "")
  }
  cat("\n")
  if (display) plot.colortheme(x, text = text)
  invisible(x)
}

