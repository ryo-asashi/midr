# R/plot_midbrk.R

#' Plot and Compare MID Breakdown
#'
#' @description
#' Visualize the prediction breakdown for a \code{"midbrk"} object, or compare
#' breakdowns across a \code{"midbrks"} collection.
#' \code{plot()} renders via base graphics, while \code{ggmid()} and \code{autoplot()}
#' return \strong{ggplot2} objects via the \code{dualplot} engine.
#'
#' @param x,object a \code{"midbrk"} or \code{"midbrks"} object.
#' @param type visualization style:
#'   for \code{"midbrk"}, one of \code{"waterfall"} (default), \code{"barplot"}, or \code{"dotchart"};
#'   for \code{"midbrks"}, one of \code{"barplot"} (default), \code{"dotchart"}, or \code{"series"}.
#' @param theme optional color theme name or \code{"colortheme"} object.
#' @param terms optional character vector specifying which terms/variables to display.
#' @param max.nterms maximum number of terms to display; remaining terms are aggregated into \code{others}.
#' @param vline logical. If \code{TRUE} (default), draws a reference line at zero (or at the intercept for waterfall plots).
#' @param others character label for the aggregated residual category (default \code{"others"}).
#' @param pattern character vector of length 1 or 2 controlling axis label formatting
#'   (\code{"\%t"} for variable/term name, \code{"\%v"} for observed value).
#' @param format.args named list of additional arguments passed to \code{\link[base]{format}()}.
#' @param labels optional numeric or character vector of model labels for \code{"midbrks"}.
#' @param ... additional graphical parameters passed to \code{dualplot}.
#'
#' @returns
#' \code{plot()} returns \code{NULL} invisibly.
#' \code{ggmid()} and \code{autoplot()} return a \code{"ggplot"} object.
#'
#' @seealso \code{\link{eval_breakdown}}
#'
#' @name plot.midbrk
#' @rdname plot.midbrk
#' @exportS3Method base::plot
#'
plot.midbrk <- function(
    x, type = c("waterfall", "barplot", "dotchart"), theme = NULL,
    terms = NULL, max.nterms = 15L, vline = TRUE, others = "others",
    pattern = c("%t=%v", "%t:%t"), format.args = list(), ...
) {
  render_midbrk(
    x = x, type = type, theme = theme, terms = terms,
    max.nterms = max.nterms, vline = vline, others = others,
    pattern = pattern, format.args = format.args, engine = "graphics", ...
  )
  invisible(NULL)
}


#' @rdname plot.midbrk
#' @exportS3Method midr::ggmid
#'
ggmid.midbrk <- function(
    object, type = c("waterfall", "barplot", "dotchart"), theme = NULL,
    terms = NULL, max.nterms = 15L, vline = TRUE, others = "others",
    pattern = c("%t=%v", "%t:%t"), format.args = list(), ...
) {
  render_midbrk(
    x = object, type = type, theme = theme, terms = terms,
    max.nterms = max.nterms, vline = vline, others = others,
    pattern = pattern, format.args = format.args, engine = "ggplot2", ...
  )
}


#' @rdname plot.midbrk
#' @exportS3Method ggplot2::autoplot
#'
autoplot.midbrk <- function(object, ...) {
  ggmid(object, ...)
}


#' @rdname plot.midbrk
#' @exportS3Method base::plot
#'
plot.midbrks <- function(
    x, type = c("barplot", "dotchart", "series"), theme = NULL,
    terms = NULL, max.nterms = 15L, vline = TRUE, others = "others",
    pattern = c("%t=%v", "%t:%t"), format.args = list(), labels = NULL, ...
) {
  render_midbrks(
    x = x, type = type, theme = theme, terms = terms,
    max.nterms = max.nterms, vline = vline, others = others,
    pattern = pattern, missing_pattern = missing(pattern),
    format.args = format.args, labels = labels,
    engine = "graphics", ...
  )
  invisible(NULL)
}


#' @rdname plot.midbrk
#' @exportS3Method midr::ggmid
#'
ggmid.midbrks <- function(
    object, type = c("barplot", "dotchart", "series"), theme = NULL,
    terms = NULL, max.nterms = 15L, vline = TRUE, others = "others",
    pattern = c("%t=%v", "%t:%t"), format.args = list(), labels = NULL, ...
) {
  render_midbrks(
    x = object, type = type, theme = theme, terms = terms,
    max.nterms = max.nterms, vline = vline, others = others,
    pattern = pattern, missing_pattern = missing(pattern),
    format.args = format.args, labels = labels,
    engine = "ggplot2", ...
  )
}


#' @rdname plot.midbrk
#' @exportS3Method ggplot2::autoplot
#'
autoplot.midbrks <- function(object, ...) {
  ggmid(object, ...)
}


# internal rendering engines ----

render_midbrk <- function(
    x, type, theme, terms, max.nterms, vline, others, pattern, format.args, engine, ...
) {
  dots <- list(...)
  type <- match.arg(type, c("waterfall", "barplot", "dotchart"))
  bd <- x$breakdown
  bd$term <- as.character(bd$term)

  use_others <- FALSE
  if (!is.null(terms)) {
    idx <- match(terms, bd$term)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0L) {
      stop("none of the specified 'terms' are in 'x'", call. = FALSE)
    }
    resid_idx <- setdiff(seq_len(nrow(bd)), idx)
    bd <- bd[idx, , drop = FALSE]
    if (length(resid_idx) > 0L) {
      resid_sum <- sum(x$breakdown$mid[resid_idx])
      extra <- bd[1L, , drop = FALSE]
      extra$term <- others
      extra$mid <- resid_sum
      if ("order" %in% names(extra)) extra$order <- NA
      bd <- rbind(bd, extra)
      use_others <- TRUE
    }
  }

  nmax <- min(max.nterms, nrow(bd), na.rm = TRUE)
  if (!is.na(nmax) && nmax < nrow(bd)) {
    resid_sum <- sum(bd$mid[seq.int(nmax, nrow(bd))])
    bd <- bd[seq_len(nmax - 1L), , drop = FALSE]
    extra <- bd[1L, , drop = FALSE]
    extra$term <- others
    extra$mid <- resid_sum
    if ("order" %in% names(extra)) extra$order <- NA
    bd <- rbind(bd, extra)
    use_others <- TRUE
  }

  k_fmt <- nrow(bd) - as.integer(use_others)
  tick_labs <- bd$term
  if (k_fmt > 0L) {
    tick_labs[seq_len(k_fmt)] <- format_breakdown_labels(
      terms = bd$term[seq_len(k_fmt)],
      data = x$data,
      pattern = pattern,
      format.args = format.args
    )
  }

  tvals <- NULL
  ltitle <- "effect"
  if (!is.null(theme)) {
    ct <- colortheme(theme)
    if (ct$type == "qualitative") {
      tvals <- if (type == "waterfall") {
        factor(ifelse(bd$mid > 0, "positive", "negative"), levels = c("negative", "positive"))
      } else if ("order" %in% names(bd)) {
        bd$order
      } else {
        factor(ifelse(bd$mid > 0, "positive", "negative"), levels = c("negative", "positive"))
      }
      ltitle <- if (type != "waterfall" && "order" %in% names(bd)) "order" else "sign"
    } else {
      tvals <- bd$mid
    }
  }

  if (type %in% c("barplot", "dotchart")) {
    bar_type <- if (type == "dotchart") "dot" else "bar"
    return(call_dualplot(
      draw_bars,
      list(
        to = bd$mid, ticks = tick_labs, horizontal = TRUE, type = bar_type,
        vline = if (isTRUE(vline)) 0 else NULL,
        theme = theme, theme.values = tvals,
        legend.title = ltitle, xlab = "effect", engine = engine
      ),
      dots
    ))
  }

  # waterfall
  cs <- cumsum(c(x$intercept, bd$mid))
  from_vals <- cs[seq_len(nrow(bd))]
  to_vals <- cs[seq_len(nrow(bd)) + 1L]
  call_dualplot(
    draw_bars,
    list(
      to = to_vals, from = from_vals, ticks = tick_labs, horizontal = TRUE,
      type = "bar", connect = TRUE,
      vline = if (isTRUE(vline)) x$intercept else NULL,
      theme = theme, theme.values = tvals,
      legend.title = ltitle, xlab = "prediction", engine = engine
    ),
    dots
  )
}


render_midbrks <- function(
    x, type, theme, terms, max.nterms, vline, others, pattern, missing_pattern = FALSE,
    format.args, labels, engine, ...
) {
  dots <- list(...)
  type <- match.arg(type, c("barplot", "dotchart", "series"))
  brk <- summary(x, shape = "long", fill = 0)
  brk$term <- as.character(brk$term)
  olabs <- as.character(unique(brk$label))
  labels <- labels %||% olabs
  if (length(labels) != length(olabs)) {
    stop("length of 'labels' must match the number of models in 'x'", call. = FALSE)
  }
  brk$label <- labels[match(as.character(brk$label), olabs)]
  nums <- suppressWarnings(as.numeric(labels))
  labels_parsed <- if (!anyNA(nums)) nums else factor(labels, levels = unique(labels))
  discrete_labels <- is_discrete(labels_parsed)

  terms_all <- as.character(unique(brk$term))
  terms_kept <- if (!is.null(terms)) {
    idx <- match(terms, terms_all)
    terms_all[idx[!is.na(idx)]]
  } else {
    terms_all
  }
  if (length(terms_kept) == 0L) {
    stop("none of the specified 'terms' are in 'x'", call. = FALSE)
  }

  terms_drop <- setdiff(terms_all, terms_kept)
  nmax <- min(max.nterms, length(terms_kept), na.rm = TRUE)
  if (!is.na(nmax) && nmax < length(terms_kept)) {
    terms_drop <- c(terms_drop, terms_kept[seq.int(nmax, length(terms_kept))])
    terms_kept <- terms_kept[seq_len(nmax - 1L)]
  }
  use_others <- length(terms_drop) > 0L
  if (use_others) {
    brk_drop <- brk[brk$term %in% terms_drop, c("label", "mid"), drop = FALSE]
    df_resid <- stats::aggregate(mid ~ label, data = brk_drop, FUN = sum)
    df_resid$term <- others
    brk <- brk[brk$term %in% terms_kept, c("term", "label", "mid"), drop = FALSE]
    brk <- rbind(brk, df_resid[, c("term", "label", "mid")])
  } else {
    brk <- brk[brk$term %in% terms_kept, c("term", "label", "mid"), drop = FALSE]
  }

  objs <- as.list(x)
  same_data <- length(objs) <= 1L || all(vapply(
    objs[-1L],
    function(o) isTRUE(all.equal(o$data, objs[[1L]]$data, check.attributes = FALSE)),
    logical(1L)
  ))
  formatted <- if (!same_data && missing_pattern) {
    terms_kept
  } else {
    format_breakdown_labels(
      terms = terms_kept,
      data = objs[[1L]]$data,
      pattern = pattern,
      format.args = format.args
    )
  }
  for (i in seq_along(terms_kept)) {
    brk$term[brk$term == terms_kept[i]] <- formatted[i]
  }
  tick_labs <- if (use_others) c(formatted, others) else formatted

  n <- length(tick_labs)
  m <- length(labels)
  mat <- matrix(0, nrow = n, ncol = m, dimnames = list(tick_labs, as.character(labels)))
  for (j in seq_len(m)) {
    subbrk <- brk[as.character(brk$label) == as.character(labels[j]), , drop = FALSE]
    vals <- subbrk$mid[match(tick_labs, subbrk$term)]
    vals[is.na(vals)] <- 0
    mat[, j] <- vals
  }

  if (type %in% c("barplot", "dotchart")) {
    th <- theme %||% colortheme_default(if (discrete_labels) "qualitative" else "sequential")
    bar_type <- if (type == "dotchart") "dot" else "bar"
    return(call_dualplot(
      draw_bars,
      list(
        to = mat, ticks = tick_labs, horizontal = TRUE, type = bar_type,
        vline = if (isTRUE(vline)) 0 else NULL,
        theme = th, theme.values = rep(labels_parsed, each = n),
        legend.title = NULL, xlab = "effect", engine = engine
      ),
      dots
    ))
  }

  # series
  th <- theme %||% colortheme_default("qualitative")
  ymat <- t(mat)
  colnames(ymat) <- tick_labs
  call_dualplot(
    draw_lines,
    list(
      x = labels_parsed, y = ymat, discrete = discrete_labels,
      theme = th, theme.values = factor(tick_labs, levels = tick_labs),
      legend.title = NULL, xlab = "", ylab = "effect", engine = engine
    ),
    dots
  )
}


format_breakdown_labels <- function(terms, data, pattern = c("%t=%v", "%t:%t"), format.args = list()) {
  if (length(pattern) == 1L) pattern <- c(pattern, "%t:%t")
  format.args$x <- as.data.frame(data)
  values <- unlist(do.call(base::format, format.args))
  vapply(terms, function(term) {
    tags <- get_variables(term)
    vals <- values[tags]
    if (length(tags) == 1L) {
      sub("%v", vals[1L], sub("%t", tags[1L], pattern[1L]))
    } else {
      lbl <- sub("%v", vals[1L], sub("%t", tags[1L], pattern[2L]))
      sub("%v", vals[2L], sub("%t", tags[2L], lbl))
    }
  }, character(1L), USE.NAMES = FALSE)
}
