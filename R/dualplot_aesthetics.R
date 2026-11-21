# R/dualplot_aesthetics.R
# Bidirectional aesthetic parameter translation and color manipulation

dict_to_base_params <- c(
  colour    = "col",
  color     = "col",
  size      = "cex",
  shape     = "pch",
  linetype  = "lty",
  linewidth = "lwd",
  title     = "main",
  subtitle  = "sub"
)

dict_to_gg_params <- c(
  col       = "colour",
  color     = "colour",
  fg        = "colour",
  bg        = "fill",
  cex       = "size",
  pch       = "shape",
  lty       = "linetype",
  lwd       = "linewidth"
)

#' Translate and Merge Parameters for Base Graphics
#'
#' Converts ggplot2-style parameter names in \code{dots} to base graphics equivalents,
#' merges them with \code{defaults}, and optionally applies alpha transparency.
#'
#' @param dots a named list of user-supplied parameters (typically \code{list(...)}).
#' @param allowed optional character vector of allowed parameter names.
#' @param defaults a named list of default base graphics parameters.
#' @param alpha.on character vector of parameter names (e.g., \code{"col"}, \code{"fill"})
#'   to which the \code{alpha} parameter should be applied. Set to \code{NULL} to leave \code{alpha} as-is.
#'
#' @return a sanitized named list ready for \code{do.call(graphics::..., args)}.
#' @keywords internal
#'
cast_to_base_params <- function(
    dots = list(), allowed = NULL, defaults = list(), alpha.on = "col"
) {
  if (length(dots) > 0L && !is.null(names(dots))) {
    nms <- names(dots)
    mapped <- dict_to_base_params[nms]
    names(dots) <- ifelse(is.na(mapped), nms, unname(mapped))
    dots <- dots[!duplicated(names(dots), fromLast = TRUE)]
  }
  out <- utils::modifyList(defaults, dots, keep.null = TRUE)
  if (!is.null(alpha.on)) {
    alpha <- out$alpha
    out$alpha <- NULL
    if (!is.null(alpha)) {
      for (target in alpha.on) {
        if (!is.null(out[[target]])) {
          out[[target]] <- apply_alpha(out[[target]], alpha)
        }
      }
    }
  }
  if (!is.null(allowed)) {
    out <- out[names(out) %in% allowed]
  }
  out
}

#' Translate and Filter Parameters for ggplot2 Layers
#'
#' Converts base graphics parameter names in \code{dots} to standard ggplot2 aesthetics
#' without requiring ggplot2 to be loaded at package build time.
#'
#' @param dots a named list of user-supplied parameters (typically \code{list(...)}).
#' @param allowed optional character vector of allowed parameter names for a specific geom.
#' @param defaults optional named list of default parameters.
#'
#' @return a sanitized named list of parameters for a ggplot2 layer.
#' @keywords internal
#'
cast_to_gg_params <- function(
    dots = list(), allowed = NULL, defaults = list()
) {
  if (length(dots) > 0L && !is.null(names(dots))) {
    nms <- names(dots)
    mapped <- dict_to_gg_params[nms]
    nms <- ifelse(is.na(mapped), nms, unname(mapped))
    nms <- sub("color$", "colour", nms)
    names(dots) <- nms
    dots <- dots[!duplicated(names(dots), fromLast = TRUE)]
  }
  out <- if (length(defaults) > 0L) {
    utils::modifyList(defaults, dots, keep.null = TRUE)
  } else {
    dots
  }
  if (!is.null(allowed)) {
    out <- out[names(out) %in% allowed]
  }
  out
}

# Extract title/subtitle/xlab/ylab into a ggplot2::labs() object
extract_gg_labs <- function(
    dots = list(), xlab = NULL, ylab = NULL, main = NULL, sub = NULL
) {
  check_ggplot2()
  title <- dots$title %||% dots$main %||% main
  subtitle <- dots$subtitle %||% dots$sub %||% sub
  x <- dots$xlab %||% xlab
  y <- dots$ylab %||% ylab
  args <- list()
  if (!is.null(title)) args$title <- title
  if (!is.null(subtitle)) args$subtitle <- subtitle
  if (!is.null(x)) args$x <- x
  if (!is.null(y)) args$y <- y
  do.call(ggplot2::labs, args)
}

#' Apply Alpha Transparency to Colors (Vectorized & NA-safe)
#'
#' @param col a vector of R colors (names, hex strings, or integer indices).
#' @param alpha a numeric vector of alpha values in \code{[0, 1]}.
#'
#' @return a character vector of hex colors with alpha channel.
#' @keywords internal
#'
apply_alpha <- function(col, alpha = NULL) {
  if (is.null(col) || is.null(alpha) || all(is.na(col))) {
    return(col)
  }
  n <- max(length(col), length(alpha))
  col <- rep_len(col, n)
  alpha <- pmax(0, pmin(1, rep_len(alpha, n)))
  if (!any(alpha < 1, na.rm = TRUE)) {
    return(col)
  }
  is_na <- is.na(col) | is.na(alpha)
  out <- rep(NA_character_, n)
  if (any(!is_na)) {
    rgbs <- grDevices::col2rgb(col[!is_na], alpha = FALSE)
    out[!is_na] <- grDevices::rgb(
      red   = rgbs[1L, ],
      green = rgbs[2L, ],
      blue  = rgbs[3L, ],
      alpha = round(alpha[!is_na] * 255),
      maxColorValue = 255L
    )
  }
  out
}
