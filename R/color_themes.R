#' Retrieve Color Theme Information
#'
#' @description
#' \code{color.themes()} returns a data frame listing all available color themes.
#'
#' @details
#' These functions provide tools for inspecting the color themes available in the current R session.
#'
#' \code{color.themes()} is the primary user-facing function for discovering themes by name, source, and type.
#'
#' \code{color.theme.registry()} is an advanced function that returns the environment currently used as the theme registry.
#' It first checks for a user-specified environment via \code{getOption("midr.color.theme.registry")}.
#' If this option is \code{NULL} (the default), the function returns the package's internal environment where the default themes are stored.
#'
#' @param env an environment where the color themes are registered.
#'
#' @examples
#' # Get a data frame of all available themes
#' head(color.themes())
#' @returns
#' \code{color.themes()} returns a data frame with columns "name", "source", and "type".
#'
#' @seealso \code{\link{color.theme}}, \code{\link{color.theme.register}}
#'
#' @export color.themes
#'
color.themes <- function(env = NULL) {
  env <- env %||% color.theme.registry()
  info.collect <- function(x) {
    err <- try(
      data.frame(name = vapply(x, function(y) y$name),
                 source = vapply(x, function(y) y$source),
                 type = vapply(x, function(y) y$type)),
      silent = TRUE
    )
    if (inherits(err, "try-error")) NULL else err
  }
  info <- do.call(rbind, lapply(env, info.collect))
  if (is.null(info)) return(NULL)
  info <- info[order(info$type, info$name, info$source), ]
  rownames(info) <- NULL
  info
}
