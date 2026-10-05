#' Retrieve Color Theme Information
#'
#' @description
#' \code{color.themes()} returns a data frame listing all available color themes.
#'
#' @details
#' This function provides a convenient way to inspect the color themes currently available in the R session.
#' It extracts metadata from a specified registry environment and structures it into a clean data frame, allowing users to discover themes by their type, name, and source.
#'
#' @param env an environment where the color themes are registered. Defaults to the default color theme registry that can be accessed via \code{midr.options("color.theme.registry")}.
#'
#' @returns
#' A data frame with columns \code{"name"}, \code{"source"}, and \code{"type"} containing the metadata of the registered color themes.
#' If no color themes are found or the environment is empty, it returns an empty data frame with these exact columns.
#'
#' @seealso \code{\link{color.theme}}, \code{\link{color.theme.register}}
#'
#' @examples
#' # Get a data frame of all available themes
#' head(color.themes())
#' @export color.themes
#'
color.themes <- function(env = NULL) {
  env <- env %||% color.theme.registry()
  nst <- c("name", "source", "type")

  lenv <- as.list(env)
  if (length(lenv) == 0L) {
    res <- data.frame(
      name = character(),
      source = character(),
      type = character(),
      stringsAsFactors = FALSE
    )
    return(res)
  }
  collect <- function(x) {
    if (!is.list(x)) return(matrix(NA_character_, nrow = 3, ncol = 0))
    mat <- vapply(
      X = x,
      FUN = function(y) {
        if (is.list(y) && all(nst %in% names(y))) {
          z <- y[nst]
          if (all(vapply(z, is.character, logical(1L))) && all(lengths(z) == 1L))
            return(unlist(z, use.names = FALSE))
        }
        return(rep.int(NA_character_, 3L))
      },
      FUN.VALUE = character(3L)
    )
    return(mat)
  }
  res <- lapply(lenv, collect)
  mat <- do.call(cbind, res)
  if (is.null(mat) || NCOL(mat) == 0L) {
    res <- data.frame(
      name = character(),
      source = character(),
      type = character(),
      stringsAsFactors = FALSE
    )
    return(res)
  }
  res <- as.data.frame(t(mat), stringsAsFactors = FALSE)
  res <- setNames(res, nst)
  res <- res[rowSums(is.na(res)) < 3L, , drop = FALSE]
  if (nrow(res) == 0L) {
    res <- data.frame(
      name = character(),
      source = character(),
      type = character(),
      stringsAsFactors = FALSE
    )
    return(res)
  }
  res <- res[order(res$type, res$name, res$source), , drop = FALSE]
  rownames(res) <- NULL
  res
}
