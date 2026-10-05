#' Set or Get Global Options for the midr Package
#'
#' @description
#' \code{midr.options()} manages global settings specific to the \strong{midr} package, such as computational solvers and color theme environments.
#'
#' @details
#' \code{midr.options()} provides an interface to R's global \code{options()} but safely scopes all parameters with a \code{midr.} prefix.
#'  To prevent typos, it validates option names against expected prefixes:
#' \itemize{
#'   \item \code{solver.*}: For registering custom least-squares solvers; e.g., \code{solver.TAG = FUN}, where \code{FUN(x, y)} returns a list containing at least \code{coefficients} and possibly \code{residuals} and \code{rank}. The custom solver can be called via \code{interpret(method = "TAG")}.
#'   \item \code{color.theme.*}: For configuring color theme settings and registries.
#'   \item \code{verbosity}: For controlling message verbosity. See \code{\link{interpret}()}.
#' }
#'
#' @param ... For \code{midr.options()}: options to be defined in \code{name = value} form.
#'   If no arguments are provided, it returns all current package options.
#'   If character strings are provided, it returns the values of those options.
#'   For \code{midr.par()}: graphical parameters in \code{name = value} form can be supplied as arguments.
#'
#' @returns
#' When called without arguments, \code{midr.options()} returns a named list of all current options with the \code{midr.} prefix.
#' When called with character strings, \code{midr.options()} returns a named list of the requested options.
#' When called with \code{name = value} pairs, \code{midr.options()} invisibly returns a named list of the previous values.
#'
#' @export midr.options
#'
midr.options <- function(...) {
  args <- list(...)
  if (length(args) == 0L) {
    opts <- options()
    opts <- opts[grep("^midr\\.", names(opts))]
    if (length(opts) == 0L) return(NULL)
    names(opts) <- sub("^midr\\.", "", names(opts))
    return(opts)
  }
  if (is.null(names(args)) && all(vapply(args, is.character, logical(1L)))) {
    req <- paste0("midr.", unlist(args))
    res <- lapply(req, getOption)
    names(res) <- unlist(args)
    return(res)
  }
  if (is.null(names(args)) || any(names(args) == "")) {
    stop("options must be specified as name = value")
  }
  new <- list()
  prefixes <- c("solver.", "color.theme.", "verbosity")
  for (nm in names(args)) {
    if (!any(startsWith(nm, prefixes))) {
      warning(
        sprintf("option '%s' might be a typo: expected prefixes: %s",
                nm, paste(paste0("'", prefixes, "'"), collapse = ", ")),
        call. = FALSE
      )
    }
    new[[paste0("midr.", nm)]] <- args[[nm]]
  }
  old <- options(new)
  names(old) <- sub("^midr\\.", "", names(old))
  invisible(old)
}

color.theme.registry <- function() {
  getOption("midr.color.theme.registry", kernel.env)
}

color.theme.defaults <- function(type = NULL) {
  gds <- c(sequential = "bluescale", qualitative = "HCL", diverging = "midr")
  if (is.null(type)) {
    return(c(
      sequential = getOption("midr.color.theme.sequential", gds[["sequential"]]),
      qualitative = getOption("midr.color.theme.qualitative", gds[["qualitative"]]),
      diverging = getOption("midr.color.theme.diverging", gds[["diverging"]])
    ))
  }
  type <- match.arg(type, names(gds))
  getOption(paste0("midr.color.theme.", type), gds[[type]])
}


#' @rdname midr.options
#'
#' @description
#' \code{midr.par()} manages graphical parameters for base R graphics.
#'
#' @details
#' \code{midr.par()} wraps \code{\link[graphics]{par}()} to enforce a themed aesthetic as the default, which can be further customized by the user.
#'
#' @returns
#' \code{midr.par()} returns the previous values of the changed parameters in an invisible named list.
#'
#' @export midr.par
#'
midr.par <- function(...) {
  dots <- list(...)
  dots <- dots[names(dots) %in% names(graphics::par(no.readonly = TRUE))]
  args <- list(
    bg = "white", bty = "o", mar = c(4.1, 4.1, 2.1, 1.1), family = "serif",
    font = 1L, font.axis = 1L, font.lab = 1L, font.main = 1L, font.sub = 1L,
    col = "black", col.axis = "black", col.lab = "black", col.main ="black",
    col.sub = "black", cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1.2,
    cex.sub = .9, las = 0L, lty = "solid", lwd = 1, pch = 16L
  )
  args <- utils::modifyList(args, dots)
  do.call(what = graphics::par, args = args)
}
