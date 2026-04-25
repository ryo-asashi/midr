#' Plot MID Breakdown with ggplot2
#'
#' @description
#' For "midbrk" objects, \code{ggmid()} visualizes the breakdown of a prediction by component functions.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that creates a breakdown plot from a "midbrk" object, visualizing the contribution of each component function to a single prediction.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "waterfall"}, creates a waterfall plot that shows how the prediction is built up from the intercept, with each term's contribution sequentially added or subtracted.
#' The \code{type = "barplot"} option creates a standard bar plot where the length of each bar represents the magnitude of the term's contribution.
#' The \code{type = "dotchart"} option creates a dot plot showing the contribution of each term as a point connected to a zero baseline.
#'
#' @param object a "midbrk" object to be visualized.
#' @param type the plotting style. One of "waterfall", "barplot" or "dotchart".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display.
#' @param max.nterms the maximum number of terms to display in the plot. Less important terms will be grouped into a "catchall" category.
#' @param vline logical. If \code{TRUE}, a vertical line is drawn at the zero or intercept line.
#' @param others a character string for the catchall label.
#' @param pattern a character vector of length one or two specifying the format of the axis labels. The first element is used for main effects (default \code{"\%t = \%v"}), and the second is for interactions (default \code{"\%t:\%t"}). Use \code{"\%t"} for the term name and \code{"\%v"} for its value.
#' @param format.args a named list of additional arguments passed to \code{\link[base]{format}} for formatting the values. Common arguments include \code{digits}, \code{nsmall}, and \code{big.mark}.
#' @param ... optional parameters passed on to the main layer.
#'
#' @examples
#' data(diamonds, package = "ggplot2")
#' set.seed(42)
#' idx <- sample(nrow(diamonds), 1e4)
#' mid <- interpret(price ~ (carat + cut + color + clarity)^2, diamonds[idx, ])
#' mbd <- mid.breakdown(mid, diamonds[1L, ])
#'
#' # Create a waterfall plot
#' ggmid(mbd, type = "waterfall")
#'
#' # Create a bar plot with a different theme
#' ggmid(mbd, type = "barplot", theme = "highlight")
#'
#' # Create a dot chart
#' ggmid(mbd, type = "dotchart", size = 3)
#' @returns
#' \code{ggmid.midbrk()} returns a "ggplot" object.
#'
#' @seealso \code{\link{mid.breakdown}}, \code{\link{ggmid}}, \code{\link{plot.midbrk}}
#'
#' @exportS3Method midr::ggmid
#'
ggmid.midbrk <- function(
    object, type = c("waterfall", "barplot", "dotchart"), theme = NULL,
    terms = NULL, max.nterms = 15L, vline = TRUE, others = "others",
    pattern = c("%t=%v", "%t:%t"), format.args = list(), ...) {
  type <- match.arg(type)
  if (missing(theme))
    theme <- getOption("midr.sequential", getOption("midr.qualitative", NULL))
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")
  bd <- object$breakdown
  bd$term <- as.character(bd$term)
  use.others <- FALSE
  if (!is.null(terms)) {
    idx <- match(terms, bd$term)
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0L) {
      resid_idx <- setdiff(seq_len(nrow(bd)), idx)
      resid_sum <- sum(bd$mid[resid_idx])
      bd <- bd[idx, , drop = FALSE]
      if (length(resid_idx) > 0L) {
        bd[nrow(bd) + 1L, "mid"] <- resid_sum
        use.others <- TRUE
      }
    }
  }
  nmax <- min(max.nterms, nrow(bd), na.rm = TRUE)
  if (nmax < nrow(bd)) {
    resid <- sum(bd[nmax:nrow(bd), "mid"])
    bd <- bd[1L:(nmax - 1L), ]
    bd[nmax, "mid"] <- resid
    use.others <- TRUE
  }
  # update labels
  format.args$x <- as.data.frame(object$data)
  values <- unlist(do.call(base::format, format.args))
  for (i in seq_len(nrow(bd) - as.numeric(use.others))) {
    term <- bd[i, "term"]
    tags <- term.split(term)
    vals <- values[tags]
    if (length(tags) == 1L) {
      label <- sub("%v", vals[1L], sub("%t", tags[1L], pattern[1L]))
    } else {
      if (length(pattern) == 1L)
        pattern <- c(pattern, "%t:%t")
      label <- sub("%v", vals[1L], sub("%t", tags[1L], pattern[2L]))
      label <- sub("%v", vals[2L], sub("%t", tags[2L], label))
    }
    bd[i, "term"] <- label
  }
  if (use.others)
    bd[nrow(bd), "term"] <- others
  bd$term <- factor(bd$term, levels = rev(bd$term))
  # barplot and dotchart
  if (type == "barplot" || type == "dotchart") {
    pl <- ggplot2::ggplot(
      bd, ggplot2::aes(x = .data[["mid"]], y = .data[["term"]])
    ) + ggplot2::labs(y = NULL)
    if (type == "barplot") {
      pl <- pl + .geom_col(...)
      if (use.theme) {
        pl <- if (theme$type == "qualitative") {
          pl + ggplot2::aes(fill = .data[["order"]])
        } else {
          pl + ggplot2::aes(fill = .data[["mid"]])
        }
        pl <- pl + scale_fill_theme(theme = theme, na.value = "gray50")
      }
    } else if (type == "dotchart") {
      pl <- pl +
        .geom_dotchart(ggplot2::aes(xmin = 0, xmax = .data[["mid"]]), ...)
      if (use.theme) {
        pl <- if (theme$type == "qualitative") {
          pl + ggplot2::aes(color = .data[["order"]])
        } else {
          pl + ggplot2::aes(color = .data[["mid"]])
        }
        pl <- pl + scale_color_theme(theme = theme, na.value = "gray50")
      }
    }
    if (vline) {
      tli <- ggplot2::theme_get()$line
      pl <- pl + ggplot2::geom_vline(
        xintercept = 0,
        linewidth = (tli$linewidth %||% 0.5) * 0.5,
        colour = tli$colour %||% "black"
      )
    }
    return(pl)
  # waterfall
  } else if (type == "waterfall") {
    dots <- standardize_param_names(list(...))
    width <- dots$width %||% .6
    hw <- width / 2
    bd$ymin <- as.integer(bd$term) - hw
    bd$ymax <- as.integer(bd$term) + hw
    cs <- cumsum(c(object$intercept, bd$mid))
    bd$xmin <- cs[1L:nrow(bd)]
    bd$xmax <- cs[2L:(nrow(bd) + 1L)]
    pl <- ggplot2::ggplot(
      bd, ggplot2::aes(x = .data[["xmax"]], y = .data[["term"]])
    ) + ggplot2::labs(x = "yhat", y = NULL) + ggplot2::geom_blank()
    tli <- ggplot2::theme_get()$line
    if (vline) {
      pl <- pl + ggplot2::geom_vline(
        xintercept = object$intercept,
        linewidth = (tli$linewidth %||% 0.5) * 0.5,
        colour = tli$colour %||% "black"
      )
    }
    linedefaults <- list(
      colour = tli$colour %||% "black",
      linetype = tli$linetype %||% 1L,
      linewidth = tli$linewidth %||% 0.5
    )
    lineargs <- c(
      list(mapping = ggplot2::aes(
        x = .data[["xmax"]], y = NULL,
        ymax = .data[["ymax"]], ymin = pmax(.data[["ymin"]] - 1, 1 - hw)
      )), utils::modifyList(linedefaults, dots)
    )
    pl <- pl + .geom_rect(
        mapping = ggplot2::aes(
          x = NULL, y = NULL,
          xmin = .data[["xmin"]], xmax = .data[["xmax"]],
          ymin = .data[["ymin"]], ymax = .data[["ymax"]]
        ), ...
    ) +
      do.call(.geom_linerange, lineargs)
    if (use.theme) {
      pl <- if (theme$type == "qualitative") {
        pl + ggplot2::aes(fill = ifelse(.data[["mid"]] > 0, "> 0", "< 0")) +
          ggplot2::labs(fill = "mid")
      } else {
        pl + ggplot2::aes(fill = .data[["mid"]])
      }
      pl <- pl + scale_fill_theme(theme = theme)
    }
    return(pl)
  }
}


#' @rdname ggmid.midbrk
#' @exportS3Method ggplot2::autoplot
#'
autoplot.midbrk <- function(object, ...) {
  mcall <- match.call(expand.dots = TRUE)
  mcall[[1L]] <- quote(ggmid.midbrk)
  mcall[["object"]] <- object
  eval(mcall, parent.frame())
}

