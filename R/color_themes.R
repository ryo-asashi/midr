#' Retrieve Color Theme Information
#'
#' @description
#' \code{color_themes()} returns a data frame listing all available color themes.
#'
#' @details
#' This function provides a convenient way to inspect the color themes currently available in the R session.
#' It extracts metadata from a specified registry environment and structures it into a clean data frame, allowing users to discover themes by their type, name, and source.
#'
#' @param env an environment where the color themes are registered. Defaults to the default color theme registry that can be accessed via \code{midr_options("color_theme_registry")}.
#' @param name a character string or vector specifying the name of color themes to list. Case-insensitive partial prefix matching is supported.
#' @param source a character string or vector specifying the source of color themes to list. Case-insensitive partial prefix matching is supported.
#' @param type a character string or vector specifying the type of color themes to list.
#'
#' @returns
#' A data frame with columns \code{"name"}, \code{"source"}, and \code{"type"} containing the metadata of the registered color themes.
#' If no color themes are found or the environment is empty, it returns an empty data frame with these exact columns.
#'
#' @seealso \code{\link{color_theme}}, \code{\link{register_color_theme}}
#'
#' @examples
#' # Get a data frame of all available themes
#' head(color_themes())
#' # Case-insensitive prefix matching: matches "HCL", "hcl", etc.
#' head(color_themes(name = "hc"))
#' @export color_themes
#'
color_themes <- function(env = NULL, name = NULL, source = NULL, type = NULL) {
  env <- env %||% color_theme_registry()
  nst <- c("name", "source", "type")
  err <- data.frame(
    name = character(),
    source = character(),
    type = character(),
    stringsAsFactors = FALSE
  )
  lenv <- as.list(env)
  if (!is.null(name)) {
    orgs <- names(lenv)
    nms <- unique(unlist(lapply(name, function(nm) {
      orgs[startsWith(tolower(orgs), tolower(nm))]
    }), use.names = FALSE))
    lenv <- lenv[nms]
  }
  if (length(lenv) == 0L) return(err)
  collect <- function(x, source = NULL) {
    if (!is.null(source)) {
      nms <- names(x)
      srcs <- unique(unlist(lapply(source, function(src) {
        nms[startsWith(tolower(nms), tolower(src))]
      }), use.names = FALSE))
      x <- x[srcs]
    }
    if (!is.list(x) || length(x) == 0L)
      return(matrix(NA_character_, nrow = 3, ncol = 0))
    mat <- vapply(
      X = x,
      FUN = function(y) {
        if (is.list(y) && all(nst %in% names(y))) {
          z <- y[nst]
          if (all(vapply(z, is.character, logical(1L))) && all(lengths(z) == 1L))
            return(unlist(z, use.names = FALSE))
        }
        rep.int(NA_character_, 3L)
      },
      FUN.VALUE = character(3L)
    )
    return(mat)
  }
  res <- lapply(lenv, collect, source = source)
  mat <- do.call(cbind, res)
  if (is.null(mat) || NCOL(mat) == 0L) return(err)
  res <- as.data.frame(t(mat), stringsAsFactors = FALSE)
  res <- setNames(res, nst)
  res <- res[rowSums(is.na(res)) < 3L, , drop = FALSE]
  if (!is.null(type)) {
    type <- match.arg(
      type, c("sequential", "diverging", "qualitative"), several.ok = TRUE
    )
    res <- res[res$type %in% type, , drop = FALSE]
  }
  if (nrow(res) == 0L) return(err)
  res <- res[order(res$type, res$name, res$source), , drop = FALSE]
  rownames(res) <- NULL
  res
}
