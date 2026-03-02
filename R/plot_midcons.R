#' Plot MID Conditional Expectations for Collections
#'
#' @description
#' For "midcons" collection objects, \code{plot()} visualizes and compares Individual Conditional Expectation (ICE) curves derived from multiple fitted MID models using base R graphics.
#'
#' @details
#' This is an S3 method for the \code{plot()} generic that produces comparative ICE curves from a "midcons" object.
#' It plots one line for each observation in the data per model. For \code{type = "iceplot"} and \code{"centered"}, lines are colored by the model label. For \code{type = "series"}, lines are colored by the feature value and plotted across models.
#'
#' The \code{var.alpha}, \code{var.linetype}, and \code{var.linewidth} arguments allow you to map aesthetics to other variables in your data using (possibly) unquoted expressions.
#'
#' @param x a "midcons" collection object to be visualized.
#' @param type the plotting style. One of "iceplot", "centered", or "series".
#' @param theme a character string or object defining the color theme. See \code{\link{color.theme}} for details.
#' @param var.alpha a variable name or expression to map to the alpha aesthetic.
#' @param var.linetype a variable name or expression to map to the linetype aesthetic.
#' @param var.linewidth a variable name or expression to map to the linewidth aesthetic.
#' @param reference an integer specifying the index of the evaluation point to use as the reference for centering the c-ICE plot.
#' @param sample an optional vector specifying the names of observations to be plotted.
#' @param labels an optional numeric or character vector to specify the model labels. Defaults to the labels found in the object.
#' @param ... optional parameters passed on to the graphing functions (e.g., \code{col}, \code{lty}, \code{lwd}).
#'
#' @returns
#' \code{plot.midcons()} produces a plot as a side effect and returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{ggmid.midcons}}, \code{\link{plot.midcon}}
#'
#' @exportS3Method base::plot
#'
plot.midcons <- function(
    x, type = c("iceplot", "centered", "series"), theme = NULL,
    var.alpha = NULL, var.linetype = NULL, var.linewidth = NULL,
    reference = 1L, sample = NULL, labels = NULL, ...
) {
  dt <- list(...)
  type <- match.arg(type)

  # --- 1. Data Preparation (from ggmid.midcons) ---
  variable <- x[[1L]]$variable
  obs <- x[[1L]]$observed
  con <- summary(x, shape = "long")
  values <- x[[1L]]$values

  olabs <- unique(con$label)
  labels <- labels %||% olabs
  if (length(labels) != length(olabs)) {
    stop("length of 'labels' must match the number of models in the collection")
  }
  con$label <- labels[match(con$label, olabs)]

  parsed <- suppressWarnings(as.numeric(labels))
  discrete <- anyNA(parsed)
  if (!discrete) {
    labels <- parsed
    con$label <- as.numeric(con$label)
  } else if (!is.factor(labels)) {
    labels <- factor(labels, levels = unique(labels))
    con$label <- factor(con$label, levels = levels(labels))
  }

  theme <- theme %||% (
    if (discrete) getOption("midr.qualitative", "HCL")
    else getOption("midr.sequential", "bluescale")
  )
  theme <- color.theme(theme)
  use.theme <- inherits(theme, "color.theme")

  # --- 2. Centering Logic (from ggmid.midcons) ---
  yvar <- "yhat"
  if (type == "centered") {
    if (reference < 0) reference <- length(values)
    refval <- values[min(length(values), max(1L, reference))]
    ref <- con[con[[variable]] == refval, , drop = FALSE]
    key1 <- sprintf("%s(%s)", con$.id, con$label)
    key2 <- sprintf("%s(%s)", ref$.id, ref$label)
    ynew <- paste0("centered ", yvar)
    con[, ynew] <- con[, yvar] - ref[match(key1, key2), yvar]
    yvar <- ynew
  }

  # --- 3. Sampling ---
  if (!is.null(sample)) {
    obs <- obs[obs$.id %in% sample, ]
    con <- con[con$.id %in% sample, ]
  }
  n <- nrow(obs)
  if (n == 0L) {
    message("no observations found")
    return(invisible(NULL))
  }

  # --- 4. Aesthetics Mapping (from plot.midcon) ---
  aes <- list(alpha = rep.int(1, n), lty = rep.int(1L, n), lwd = rep.int(1L, n))

  if (!is.null(alp <- substitute(var.alpha))) {
    if (is.character(alp)) alp <- str2lang(alp)
    ref <- rescale(eval(alp, envir = obs))
    aes$alpha <- ref * .75 + .25
  }
  if (!is.null(lty <- substitute(var.linetype))) {
    if (is.character(lty)) lty <- str2lang(lty)
    ref <- rescale(eval(lty, envir = obs))
    aes$lty <- pmin(ref * 6 + 1, 6L)
  }
  if (!is.null(lwd <- substitute(var.linewidth))) {
    if (is.character(lwd)) lwd <- str2lang(lwd)
    ref <- rescale(eval(lwd, envir = obs))
    aes$lwd <- ref * 3
  }

  # Allow direct override via `...` for base line aesthetics
  for (arg in c("lty", "lwd", "alpha")) {
    if (!is.null(dt[[arg]])) aes[[arg]] <- rep_len(dt[[arg]], length.out = n)
  }

  ylim <- range(con[[yvar]], na.rm = TRUE)

  # --- 5. Plotting ---
  if (type %in% c("iceplot", "centered")) {
    fv <- is.factor(values) || is.character(values)
    if (fv) {
      flvs <- levels(factor(values))
      x_pos <- seq_along(flvs)
    } else {
      x_pos <- values
    }

    args <- list(x = x_pos, ylim = ylim, xlab = variable, ylab = yvar,
                 type = "n", xaxt = if (fv) "n")
    args <- override(args, dt)
    do.call(graphics::plot.default, args)
    if (fv) graphics::axis(side = 1L, at = x_pos, labels = flvs)

    m_cols <- length(labels)
    base_cols <- if (use.theme) theme$palette(m_cols) else rep.int(1L, m_cols)

    # Loop over models
    for (i in seq_along(labels)) {
      lab <- labels[i]
      col_i <- base_cols[i]
      subcon <- con[con$label == lab, ]

      # Loop over observations
      for (j in seq_len(n)) {
        id <- obs$.id[j]
        y_vals <- subcon[subcon$.id == id, yvar]

        # Apply alpha blending
        col_with_alpha <- col_i
        alpha_val <- aes$alpha[j]
        if (alpha_val < 1 && is.null(dt$col)) {
          clr <- grDevices::col2rgb(col_i)
          col_with_alpha <- grDevices::rgb(clr[1L,], clr[2L,], clr[3L,],
                                           round(alpha_val * 255), maxColorValue = 255L)
        }

        lines_args <- list(x = x_pos, y = y_vals, col = col_with_alpha,
                           lty = aes$lty[j], lwd = aes$lwd[j])
        do.call(graphics::lines.default, override(lines_args, dt))
      }
    }

  } else if (type == "series") {
    if (discrete) {
      x_pos <- seq_along(labels)
    } else {
      x_pos <- labels
    }

    args <- list(x = x_pos, ylim = ylim, xlab = "model", ylab = yvar,
                 type = "n", xaxt = if (discrete) "n")
    args <- override(args, dt)
    do.call(graphics::plot.default, args)
    if (discrete) graphics::axis(side = 1L, at = x_pos, labels = as.character(labels))

    n_vals <- length(values)
    base_cols <- if (use.theme) theme$palette(n_vals) else rep.int(1L, n_vals)

    # Loop over feature grid values
    for (v_idx in seq_along(values)) {
      v <- values[v_idx]
      col_v <- base_cols[v_idx]
      subcon <- con[con[[variable]] == v, ]

      # Loop over observations
      for (j in seq_len(n)) {
        id <- obs$.id[j]
        subcon_id <- subcon[subcon$.id == id, ]
        subcon_id <- subcon_id[match(labels, subcon_id$label), ] # Sort by model order
        y_vals <- subcon_id[[yvar]]

        # Apply alpha blending
        col_with_alpha <- col_v
        alpha_val <- aes$alpha[j]
        if (alpha_val < 1 && is.null(dt$col)) {
          clr <- grDevices::col2rgb(col_v)
          col_with_alpha <- grDevices::rgb(clr[1L,], clr[2L,], clr[3L,],
                                           round(alpha_val * 255), maxColorValue = 255L)
        }

        lines_args <- list(x = x_pos, y = y_vals, col = col_with_alpha,
                           lty = aes$lty[j], lwd = aes$lwd[j],
                           type = if(discrete) "b" else "l", pch = 16L)
        do.call(graphics::lines.default, override(lines_args, dt))
      }
    }
  }

  invisible(NULL)
}
