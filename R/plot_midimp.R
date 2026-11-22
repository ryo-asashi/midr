# R/plot_midimp.R

#' Plot Term or Variable Importance
#'
#' @description
#' Visualize term or variable importance for a \code{"midimp"} object.
#' \code{plot()} renders via base graphics, while \code{ggmid()} returns
#' \strong{ggplot2} objects via the \code{dualplot} engine.
#'
#' @param x,object a \code{"midimp"} object.
#' @param type visualization style: \code{"barplot"} (default), \code{"dotchart"},
#'   \code{"heatmap"} (when \code{by = "term"}), or \code{"boxplot"}.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param terms optional character vector specifying which terms/variables to display.
#' @param max.nterms maximum number of terms to display (ignored for \code{"heatmap"}).
#' @param ... additional graphical parameters passed to \code{dualplot}.
#'
#' @examples
#' data(airquality, package = "datasets")
#' aq <- na.omit(airquality)
#' mid <- interpret(Ozone ~ (Solar.R + Wind + Temp)^2, data = aq, lambda = 0.5)
#' imp_v <- eval_importance(mid, by = "variable")
#'
#' plot(imp_v, type = "boxplot")
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   imp <- eval_importance(mid)
#'   ggmid(imp, type = "heatmap") +
#'     ggplot2::geom_text(ggplot2::aes(label = round(importance, 1)), colour = "white")
#' }
#' @returns
#' \code{plot()} returns \code{NULL} invisibly.
#' \code{ggmid()} returns a \code{"ggplot"} object.
#'
#' @seealso \code{\link{eval_importance}}, \code{\link{plot.midimps}}
#'
#' @name plot.midimp
#' @rdname plot.midimp
#' @exportS3Method base::plot
#'
plot.midimp <- function(
    x, type = c("barplot", "dotchart", "heatmap", "boxplot"),
    theme = NULL, terms = NULL, max.nterms = 30L, ...
) {
  render_midimp(
    x = x, type = type, theme = theme, terms = terms,
    max.nterms = max.nterms, engine = "graphics", ...
  )
  invisible(NULL)
}


#' @rdname plot.midimp
#' @exportS3Method midr::ggmid
#'
ggmid.midimp <- function(
    object, type = c("barplot", "dotchart", "heatmap", "boxplot"),
    theme = NULL, terms = NULL, max.nterms = 30L, ...
) {
  render_midimp(
    x = object, type = type, theme = theme, terms = terms,
    max.nterms = max.nterms, engine = "ggplot2", ...
  )
}


#' @exportS3Method ggplot2::autoplot
#'
autoplot.midimp <- function(object, ...) {
  ggmid(object, ...)
}


#' Compare Term or Variable Importance Across Models
#'
#' @description
#' Compare term or variable importance across a \code{"midimps"} collection.
#' \code{plot()} renders via base graphics, while \code{ggmid()} returns
#' \strong{ggplot2} objects via the \code{dualplot} engine.
#'
#' @param x,object a \code{"midimps"} collection object.
#' @param type visualization style: \code{"barplot"} (default), \code{"dotchart"}, or \code{"series"}.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param terms optional character vector specifying which terms/variables to display.
#' @param max.nterms maximum number of terms to display.
#' @param labels optional numeric or character vector of model labels.
#' @param ... additional graphical parameters passed to \code{dualplot}.
#'
#' @examples
#' data(iris, package = "datasets")
#' mids <- interpret(cbind(Sepal.Length, Sepal.Width) ~ .^2, data = iris, lambda = 1)
#' imps <- eval_importance(mids, by = "variable")
#'
#' plot(imps, type = "dotchart", legend = "bottomright")
#' plot(imps, type = "series", legend = "bottom")
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggmid(imps, type = "dotchart")
#' }
#'
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   vet <- na.omit(survival::veteran)
#'   fit <- survival::coxph(survival::Surv(time, status) ~ trt + celltype + karno + age, data = vet)
#'   survmid <- interpret(~ trt + celltype + karno + age, data = vet, model = fit, lambda = .1)
#'   imp <- eval_importance(survmid)
#'   plot(imp, type = "series", legend = "topright")
#' }
#' @returns
#' \code{plot()} returns \code{NULL} invisibly.
#' \code{ggmid()} returns a \code{"ggplot"} object.
#'
#' @seealso \code{\link{eval_importance}}, \code{\link{plot.midimp}}, \code{\link{midlist}}
#'
#' @name plot.midimps
#' @rdname plot.midimps
#' @exportS3Method base::plot
#'
plot.midimps <- function(
    x, type = c("barplot", "dotchart", "series"), theme = NULL,
    terms = NULL, max.nterms = 30L, labels = NULL, ...
) {
  render_midimps(
    x = x, type = type, theme = theme, terms = terms,
    max.nterms = max.nterms, labels = labels, engine = "graphics", ...
  )
  invisible(NULL)
}


#' @rdname plot.midimps
#' @exportS3Method midr::ggmid
#'
ggmid.midimps <- function(
    object, type = c("barplot", "dotchart", "series"), theme = NULL,
    terms = NULL, max.nterms = 30L, labels = NULL, ...
) {
  render_midimps(
    x = object, type = type, theme = theme, terms = terms,
    max.nterms = max.nterms, labels = labels, engine = "ggplot2", ...
  )
}


#' @exportS3Method ggplot2::autoplot
#'
autoplot.midimps <- function(object, ...) {
  ggmid(object, ...)
}


# internal rendering engines ----

render_midimp <- function(
    x, type, theme, terms, max.nterms, engine, ...
) {
  dots <- list(...)
  type <- match.arg(type, c("barplot", "dotchart", "heatmap", "boxplot"))
  imp <- x$importance
  if (!is.null(terms)) {
    idx <- match(terms, as.character(imp$term), nomatch = 0L)
    imp <- imp[idx, , drop = FALSE]
  }
  if (nrow(imp) == 0L) {
    stop("none of the specified 'terms' are in 'x'", call. = FALSE)
  }
  if (type != "heatmap" && !is.null(max.nterms) && !is.na(max.nterms)) {
    imp <- utils::head(imp, max.nterms)
  }

  term_names <- as.character(imp$term)

  if (type %in% c("barplot", "dotchart")) {
    bar_type <- if (type == "dotchart") "dot" else "bar"
    tvals <- NULL
    ltitle <- "importance"
    if (!is.null(theme)) {
      ct <- colortheme(theme)
      if (ct$type == "qualitative" && "order" %in% names(imp)) {
        tvals <- imp$order
        ltitle <- "order"
      } else {
        tvals <- imp$importance
      }
    }
    return(call_dualplot(
      draw_bars,
      list(
        to = imp$importance, ticks = term_names, horizontal = TRUE,
        type = bar_type, theme = theme, theme.values = tvals,
        legend.title = ltitle, xlab = "importance", engine = engine
      ),
      dots
    ))
  }

  if (type == "heatmap") {
    if (identical(x$by, "variable")) {
      stop("type = 'heatmap' is only supported when 'by = \"term\"'", call. = FALSE)
    }
    rownames(imp) <- term_names
    tags <- unique(unlist(lapply(term_names, get_variables), use.names = FALSE))
    m <- length(tags)
    mat <- matrix(NA_real_, nrow = m, ncol = m, dimnames = list(tags, tags))
    for (i in seq_len(m)) {
      for (j in seq_len(m)) {
        tlab <- if (tags[i] == tags[j]) {
          tags[i]
        } else {
          cand <- paste0(tags[i], ":", tags[j])
          if (cand %in% term_names) cand else paste0(tags[j], ":", tags[i])
        }
        if (tlab %in% term_names) {
          mat[i, j] <- imp[tlab, "importance"]
        }
      }
    }
    use_theme <- theme %||% colortheme_default("sequential")
    return(call_dualplot(
      draw_matrix,
      list(
        z = mat, ticks = tags, theme = use_theme,
        legend.title = "importance", engine = engine
      ),
      dots
    ))
  }

  # boxplot
  plist <- lapply(term_names, function(tlab) x$predictions[, tlab])
  names(plist) <- term_names
  tvals <- NULL
  ltitle <- "importance"
  if (!is.null(theme)) {
    ct <- colortheme(theme)
    if (ct$type == "qualitative" && "order" %in% names(imp)) {
      tvals <- imp$order
      ltitle <- "order"
    } else {
      tvals <- imp$importance
    }
  }
  call_dualplot(
    draw_boxes,
    list(
      x = plist, horizontal = TRUE, theme = theme, theme.values = tvals,
      legend.title = ltitle, xlab = "effect", engine = engine
    ),
    dots
  )
}


render_midimps <- function(
    x, type, theme, terms, max.nterms, labels, engine, ...
) {
  dots <- list(...)
  type <- match.arg(type, c("barplot", "dotchart", "series"))
  imp <- summarize_midlist(
    object = x, shape = "long",
    dfname = "importance", keycol = "term", valcol = "importance", keys = "term"
  )
  olabs <- as.character(unique(imp$label))
  labels <- labels %||% olabs
  if (length(labels) != length(olabs)) {
    stop("length of 'labels' must match the number of models in 'x'", call. = FALSE)
  }
  imp$label <- labels[match(as.character(imp$label), olabs)]
  nums <- suppressWarnings(as.numeric(labels))
  labels_parsed <- if (!anyNA(nums)) nums else factor(labels, levels = unique(labels))
  discrete_labels <- is_discrete(labels_parsed)

  all_terms <- as.character(unique(imp$term))
  terms <- if (is.null(terms)) {
    all_terms
  } else {
    all_terms[match(terms, all_terms, nomatch = 0L)]
  }
  if (!is.null(max.nterms) && !is.na(max.nterms)) {
    terms <- utils::head(terms, max.nterms)
  }
  if (length(terms) == 0L) {
    stop("none of the specified 'terms' are in 'x'", call. = FALSE)
  }

  n <- length(terms)
  m <- length(labels)
  mat <- matrix(NA_real_, nrow = n, ncol = m, dimnames = list(terms, as.character(labels)))
  for (j in seq_len(m)) {
    subimp <- imp[as.character(imp$label) == as.character(labels[j]), , drop = FALSE]
    mat[, j] <- subimp$importance[match(terms, as.character(subimp$term))]
  }

  if (type %in% c("barplot", "dotchart")) {
    th <- theme %||% colortheme_default(if (discrete_labels) "qualitative" else "sequential")
    bar_type <- if (type == "dotchart") "dot" else "bar"
    return(call_dualplot(
      draw_bars,
      list(
        to = mat, ticks = terms, horizontal = TRUE, type = bar_type,
        theme = th, theme.values = rep(labels_parsed, each = n),
        legend.title = NULL, xlab = "importance", engine = engine
      ),
      dots
    ))
  }

  # series
  th <- theme %||% colortheme_default("qualitative")
  ymat <- t(mat)
  colnames(ymat) <- terms
  call_dualplot(
    draw_lines,
    list(
      x = labels_parsed, y = ymat, discrete = discrete_labels,
      theme = th, theme.values = factor(terms, levels = terms),
      legend.title = NULL, xlab = "", ylab = "importance", engine = engine
    ),
    dots
  )
}
