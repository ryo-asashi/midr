#' Compare MID Breakdowns with ggplot2
#'
#' @description
#' For "midbrks" collection objects, \code{ggmid()} visualizes and compares the breakdown of a prediction by component functions.
#'
#' @details
#' This is an S3 method for the \code{ggmid()} generic that creates a comparative importance plot from a "midbrks" collection object. It visualizes the contribution of each component function to a single prediction across multiple models, allowing for easy comparison across different models.
#'
#' The \code{type} argument controls the visualization style.
#' The default, \code{type = "barplot"}, creates a standard grouped bar plot where the length of each bar represents the overall importance of the term, positioned side-by-side (\code{position_dodge}) by model label.
#' The \code{type = "dotchart"} option creates a grouped dot plot, offering a clean alternative to the bar plot for visualizing and comparing term importance across models.
#'
#' @param object a "midbrks" collection object to be visualized.
#' @param type the plotting style. One of "barplot", "dotchart", or "series".
#' @param theme a character string or object defining the color theme. Defaults to "HCL". See \code{\link{color.theme}} for details.
#' @param terms an optional character vector specifying which terms to display. If \code{NULL}, terms are automatically extracted from the object.
#' @param max.nterms the maximum number of terms to display. Defaults to 15.
#' @param vline logical. If \code{TRUE}, a vertical line is drawn at the zero or intercept line.
#' @param others a character string for the catchall label. Defaults to \code{"others"}.
#' @param label.pattern a character vector of length one or two specifying the format of the axis labels. The first element is used for main effects (default \code{"\%t = \%v"}), and the second is for interactions (default \code{"\%t:\%t"}). Use \code{"\%t"} for the term name and \code{"\%v"} for its value.
#' @param format.args a named list of additional arguments passed to \code{\link[base]{format}} for formatting the values. Common arguments include \code{digits}, \code{nsmall}, and \code{big.mark}.
#' @param labels an optional numeric or character vector to specify the model labels or x-axis coordinates. Defaults to the labels found in the object.
#' @param ... optional parameters passed on to the main layer (e.g., \code{\link[ggplot2]{geom_col}}).
#'
#' @exportS3Method midr::ggmid
#'
ggmid.midbrks <- function(
    object, type = c("barplot", "dotchart", "series"), theme = NULL,
    terms = NULL, max.nterms = 15L, vline = TRUE, others = "others",
    label.pattern = c("%t=%v", "%t:%t"), format.args = list(), labels = NULL,
    ...
) {
  type <- match.arg(type)
  brk <- summary(object, shape = "long")
  brk$term <- as.character(brk$term)
  olabs <- unique(brk$label)
  labels <- labels %||% olabs
  if (length(labels) != length(olabs)) {
    stop("length of 'labels' must match the number of models in the collection")
  }
  brk$label <- labels[match(brk$label, olabs)]
  parsed <- suppressWarnings(as.numeric(labels))
  discrete <- anyNA(parsed)
  if (!discrete) {
    labels <- parsed
    brk$label <- as.numeric(brk$label)
  } else if (!is.factor(labels)) {
    labels <- factor(labels, levels = unique(labels))
    brk$label <- factor(brk$label, levels = levels(labels))
  }
  theme <- theme %||% (
    if (type == "series" || discrete) getOption("midr.qualitative", "HCL")
    else getOption("midr.sequential", "bluescale")
  )
  theme <- color.theme(theme)
  terms.all <- as.character(unique(brk$term))
  if (!is.null(terms)) {
    idx <- match(terms, terms.all)
    terms.kept <- terms.all[idx[!is.na(idx)]]
  } else {
    terms.kept <- terms.all
  }
  terms.drop <- setdiff(terms.all, terms.kept)
  nmax <- min(max.nterms, length(terms.kept), na.rm = TRUE)
  if (nmax < length(terms.kept)) {
    terms.drop <- c(terms.drop, terms.kept[nmax:length(terms.kept)])
    terms.kept <- terms.kept[seq_len(nmax - 1L)]
  }
  use.others <- length(terms.drop) > 0L
  if (use.others) {
    brk.drop <- brk[brk$term %in% terms.drop, c("label", "mid"), drop = FALSE]
    df.resid <- stats::aggregate(mid ~ label, data = brk.drop, FUN = sum)
    df.resid$term <- others
    brk <- brk[brk$term %in% terms.kept, c("term", "label", "mid"), drop = FALSE]
    brk <- rbind(brk, df.resid[, c("term", "label", "mid")])
  } else {
    brk <- brk[brk$term %in% terms.kept, c("term", "label", "mid"), drop = FALSE]
  }
  format.args$x <- as.data.frame(object[[1L]]$data)
  values <- unlist(do.call(base::format, format.args))
  formatted <- character(length(terms.kept))
  for (i in seq_along(terms.kept)) {
    term <- terms.kept[i]
    tags <- term.split(term)
    vals <- values[tags]
    if (length(tags) == 1L) {
      label <- sub("%v", vals[1L], sub("%t", tags[1L], label.pattern[1L]))
    } else {
      if (length(label.pattern) == 1L) {
        label.pattern <- c(label.pattern, "%t:%t")
      }
      label <- sub("%v", vals[1L], sub("%t", tags[1L], label.pattern[2L]))
      label <- sub("%v", vals[2L], sub("%t", tags[2L], label))
    }
    formatted[i] <- label
  }
  for (i in seq_along(terms.kept)) {
    brk$term[brk$term == terms.kept[i]] <- formatted[i]
  }
  terms.kept <- formatted
  terms <- if (use.others) c(terms.kept, others) else terms.kept
  grid <- expand.grid(term = terms, label = labels, stringsAsFactors = FALSE)
  brk <- merge(grid, brk, by = c("term", "label"), all.x = TRUE)
  brk$mid[is.na(brk$mid)] <- 0
  if (type == "series") {
    brk$term <- factor(brk$term, levels = terms)
    pl <- ggplot2::ggplot(
      brk, ggplot2::aes(x = .data[["label"]], y = .data[["mid"]],
                        color = .data[["term"]], group = .data[["term"]])
    )
    pl <- pl + if (discrete) geom_dotline(...) else ggplot2::geom_line(...)
    pl <- pl + scale_color_theme(theme, discrete = TRUE)
  } else if (type == "barplot") {
    brk$term <- factor(brk$term, levels = rev(terms))
    pl <- ggplot2::ggplot(
      brk, ggplot2::aes(y = .data[["term"]], x = .data[["mid"]])
    ) + ggplot2::geom_col(
      ggplot2::aes(fill = .data[["label"]], group = factor(.data[["label"]])),
      position = ggplot2::position_dodge(), ...
    ) + scale_fill_theme(theme, discrete = discrete)
  } else if (type == "dotchart") {
    args <- list(...)
    dodge_width <- args$width %||% 0.6
    brk$term <- factor(brk$term, levels = rev(terms))
    pl <- ggplot2::ggplot(
      brk, ggplot2::aes(y = .data[["term"]], x = .data[["mid"]])
    ) + geom_dotchart(
      ggplot2::aes(xmin = 0, xmax = .data[["mid"]],
                   color = .data[["label"]], group = factor(.data[["label"]])),
      position = ggplot2::position_dodge(width = dodge_width), ...
    ) + scale_color_theme(theme, discrete = discrete)
  }
  if (type != "series" && vline) {
    tli <- ggplot2::theme_get()$line
    pl <- pl + ggplot2::geom_vline(
      xintercept = 0, lwd = (tli$linewidth %||% .5) * .5)
  }
  pl
}

#' @rdname ggmid.midbrks
#'
#' @exportS3Method ggplot2::autoplot
#'
autoplot.midbrks <- function(object, ...) {
  ggmid.midbrks(object = object, ...)
}
