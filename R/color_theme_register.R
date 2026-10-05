#' Register Color Themes
#'
#' @description
#' \code{color.theme.register()} registers a custom color theme in the package's theme registry.
#'
#' @details
#' This function takes a color vector, a color-generating function, or an existing "color.theme" object and registers it under a specified \code{name} and \code{source} (default is "custom/newtheme").
#' The registered color theme can then be easily retrieved using the "Theme Name Syntax" (see \code{help(color.theme)}).
#'
#' To keep the registry environment size small, the \code{kernel} argument supports a form of lazy loading.
#' To use this feature, provide a vector or list containing two character strings.
#' The first is an R expression that returns a color kernel (e.g., "rainbow"), and the second is the namespace in which to evaluate the expression (e.g., "grDevices").
#' The expression is evaluated only when the color theme is loaded by \code{color.theme()}.
#'
#' @param kernel a color vector, a palette function, or a ramp function to be used as a color kernel. It can also be a character vector or a list (see the "Details" section). A "color.theme" object can also be passed.
#' @param kernel.args a list of arguments to be passed to the color kernel.
#' @param options a list of option values to control the color theme's behavior.
#' @param name a character string for the color theme name.
#' @param source a character string for the source name of the color theme.
#' @param type a character string specifying the type of the color theme. One of "sequential", "diverging", or "qualitative".
#' @param env an environment where the color themes are registered.
#'
#' @returns
#' \code{color.theme.register()} returns the metadata of the previous theme that was overwritten (or \code{NULL} if none existed) invisibly.
#'
#' @seealso \code{\link{color.theme}}, \code{\link{color.themes}}
#'
#' @export color.theme.register
#'
color.theme.register <- function(
    kernel, kernel.args = list(), options = list(), name = "newtheme",
    source = "custom", type = NULL, env = NULL
  ) {
  env <- env %||% color.theme.registry()
  if (is.color.theme(kernel)) {
    args <- as.list(kernel)[c("kernel", "kernel.args", "options", "type")]
    args$name <- name %||% kernel$name
    args$source <- source %||% kernel$source
    args$env = env
    return(do.call(color.theme.register, args))
  }
  lazy.load.kernel(kernel, args = kernel.args)
  if (!is.list(kernel.args))
    stop("'kernel.args' must be a list")
  if (!is.list(options))
    stop("'options' must be a list")
  if (!is.character(name) || length(name) != 1L)
    stop("'name' must be a character string of length 1")
  if (grepl("(/|_r$|@|\\?)", name))
    stop("'name' must not contain the reserved characters: '/', '_r', '@', or '?'")
  if (!is.character(source) || length(source) != 1L)
    stop("'source' must be a character string of length 1")
  type <- match.arg(type, c("sequential", "diverging", "qualitative"))
  new <- list(kernel = kernel, kernel.args = kernel.args,
              options = options, name = name, source = source, type = type)
  old <- env[[name]][[source]]
  env[[name]][[source]] <- new
  invisible(old)
}

#' @rdname color.theme.register
#'
#' @description
#' \code{set.color.theme()} is deprecated and will be removed in a future release.
#' Please use \code{color.theme.register()} instead.
#'
#' @export set.color.theme
#'
set.color.theme <- function(
    kernel, kernel.args = list(), options = list(), name = "newtheme",
    source = "custom", type = NULL, env = NULL
) {
  .Deprecated(new = "color.theme.register", package = "midr")
  color.theme.register(
    kernel = kernel, kernel.args = kernel.args, options = options,
    name = name, source = source, type = type, env = env
  )
}
