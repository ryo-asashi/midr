#' Retrieve Color Theme Information
#'
#' @description
#' \code{color.theme.info()} returns a data frame listing all available color themes.
#'
#' @details
#' These functions provide tools for inspecting the color themes available in the current R session.
#'
#' \code{color.theme.info()} is the primary user-facing function for discovering themes by name, source, and type.
#'
#' \code{color.theme.env()} is a more advanced function that returns the actual environment used as the theme registry.
#' This can be useful for programmatic inspection or manipulation of the theme database.
#'
#' @examples
#' # Get a data frame of all available themes
#' head(color.theme.info())
#'
#' # Get the environment where color themes are stored
#' theme_env <- color.theme.env()
#' names(theme_env)[1:5]
#' @returns
#' \code{color.theme.info()} returns a data frame with columns "name", "source", and "type".
#'
#' @seealso \code{\link{color.theme}}, \code{\link{set.color.theme}}
#'
#' @export color.theme.info
#'
color.theme.info <- function() {
  env <- color.theme.env()
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

#' @rdname color.theme.info
#'
#' @description
#' \code{color.theme.env()} provides direct access to the environment where the color themes are registered.
#'
#' @returns
#' \code{color.theme.env()} returns the environment object used as the theme registry.
#'
#' @export color.theme.env
#'
color.theme.env <- function()
  getOption("midr.color.theme.env", kernel.env)
