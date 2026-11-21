# R/zzz.R

.onLoad <- function(libname, pkgname) {

  init_colortheme(overwrite.options = FALSE)

  ops <- list(
    midr.verbosity        = 1L,
    midr.message.appendLF = TRUE,
    dualplot.engine       = "graphics"
  )
  toset <- !(names(ops) %in% names(options()))
  if (any(toset)) {
    options(ops[toset])
  }

#  if (exists("s3register_shapviz.mid", mode = "function")) {
#    s3register_shapviz.mid()
#  }

  invisible(NULL)
}



#' Set or Get Global Options for midr
#'
#' @description
#' \code{midr_options()} provides a unified interface to R's global
#' \code{\link[base]{options}()} for \pkg{midr}, including verbosity,
#' plotting backend, default color themes, and custom solvers.
#'
#' @details
#' Supported option names (passed without package prefixes) include:
#' \itemize{
#'   \item \code{verbosity}: integer verbosity level for \code{\link{interpret}()} (default \code{1L}).
#'   \item \code{message.appendLF}: logical passed to \code{message()} (default \code{TRUE}).
#'   \item \code{engine}: default plotting backend, \code{"graphics"} or \code{"ggplot2"}.
#'   \item \code{gg.theme}: optional \strong{ggplot2} theme object applied by \code{ggmid()} / \code{autoplot()}.
#'   \item \code{sequential}, \code{diverging}, \code{qualitative}: default color theme names or objects.
#'   \item \code{solver.<name>}: custom OLS solver function \code{function(x, y)} for \code{interpret(method = "<name>")}.
#' }
#'
#' @param ... options specified as \code{name = value} pairs, a single named
#'   list of options, character strings to query specific options, or empty to
#'   retrieve all current \pkg{midr} options.
#'
#' @returns
#' When querying options (no arguments or character names), a named list of
#' current option values. When setting options, invisibly returns a named list
#' of the previous option values.
#'
#' @examples
#' midr_options()
#' old <- midr_options(verbosity = 0L, engine = "ggplot2")
#' midr_options("verbosity", "engine")
#' midr_options(old)
#'
#' @export
#'
midr_options <- function(...) {
  ct_keys <- c("sequential", "diverging", "qualitative", "registry")
  dp_keys <- c("engine", "gg.theme")
  mid_keys <- c("verbosity", "message.appendLF")

  to_full_name <- function(nm) {
    if (nm %in% ct_keys) return(paste0("colortheme.", nm))
    if (nm %in% dp_keys) return(paste0("dualplot.", nm))
    if (grepl("^(midr|colortheme|dualplot)\\.", nm)) return(nm)
    paste0("midr.", nm)
  }
  to_short_name <- function(nm) {
    sub("^(midr|colortheme|dualplot)\\.", "", nm)
  }

  args <- list(...)
  if (length(args) == 0L) {
    opts <- options()
    keep <- grep("^(midr|colortheme|dualplot)\\.", names(opts))
    if (length(keep) == 0L) return(list())
    res <- opts[keep]
    names(res) <- to_short_name(names(res))
    return(res)
  }

  if (length(args) == 1L && is.list(args[[1L]]) && !is.null(names(args[[1L]]))) {
    args <- args[[1L]]
  }

  if (is.null(names(args)) && all(vapply(args, is.character, logical(1L)))) {
    nms <- unlist(args, use.names = FALSE)
    full <- vapply(nms, to_full_name, character(1L), USE.NAMES = FALSE)
    res <- lapply(full, getOption)
    names(res) <- nms
    return(res)
  }

  if (is.null(names(args)) || any(names(args) == "")) {
    stop("options must be specified as name = value pairs", call. = FALSE)
  }

  valid_prefix <- "^solver\\."
  valid <- c(mid_keys, ct_keys, dp_keys)
  new <- list()
  for (nm in names(args)) {
    short <- to_short_name(nm)
    if (!short %in% valid && !grepl(valid_prefix, short)) {
      warning(
        sprintf("unknown midr option '%s'", short),
        call. = FALSE
      )
    }
    if (short == "engine" && !is.null(args[[nm]])) {
      args[[nm]] <- match.arg(args[[nm]], c("graphics", "ggplot2"))
    }
    new[[to_full_name(nm)]] <- args[[nm]]
  }

  old <- options(new)
  names(old) <- to_short_name(names(old))
  invisible(old)
}
