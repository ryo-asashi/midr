#' Summarize MID Models
#'
#' @description
#' \code{summary()} methods for a fitted MID model ("mid") or a collection of models ("mids").
#' It prints a comprehensive summary of the model structure and fit quality.
#'
#' @details
#' The S3 method \code{summary.mid()} generates a comprehensive overview of the fitted MID model.
#' The output includes:
#' \itemize{
#'   \item \strong{Call}: the function call used to fit the MID model.
#'   \item \strong{Link}: name of the link function used to fit the MID model, if applicable.
#'   \item \strong{Uninterpreted Variation Ratio}: proportion of target model variance not explained by MID model.
#'   \item \strong{Residuals}: five-number summary of (working) residuals.
#'   \item \strong{Encoding}: summary of encoding schemes per variable.
#'   \item \strong{Diagnosis}: residuals vs fitted values plot (displayed only when \code{diagnose = TRUE}).
#' }
#'
#' @param object a "mid" or "mids" object to be summarized.
#' @param diagnose logical. If \code{TRUE}, the diagnosis plot is displayed. Defaults to \code{FALSE}.
#' @param digits the number of significant digits for printing numeric values.
#' @param max.nmodels an integer specifying the maximum number of models to summarize for a "mids" collection.
#' @param ... arguments to be passed to \code{graphics::panel.smooth()} for the diagnosis plot.
#'
#' @examples
#' # Summarize a fitted MID model
#' data(cars, package = "datasets")
#' mid <- interpret(dist ~ speed, cars)
#' summary(mid)
#' @returns
#' \code{summary.mid()} returns the original "mid" object invisibly.
#'
#' \code{summary.mids()} returns the original "mids" object invisibly.
#'
#' @seealso \code{\link{interpret}}, \code{\link{print.mid}}
#'
#' @exportS3Method base::summary
#'
summary.mid <- function(
    object, diagnose = FALSE, digits = max(3L, getOption("digits") - 2L), ...
  ) {
  # call
  cl <- paste0(trimws(deparse(object$call)), sep = "", collapse = "\n ")
  cat(paste0("\nCall:\n", cl, "\n", collapse = ""))
  # link
  if(use.link <- !is.null(object$link)) {
    ln <- object$link$name %||% attr(object$link, "name", exact = TRUE)
    cat(paste0("\nLink: ", ln %||% "custom", "\n", collapse = ""))
  }
  # ratio
  cat(paste0("\nUninterpreted Variation Ratio:\n"))
  print.default(object$ratio, digits = digits)
  # residuals
  rsd <- object$residuals
  cat(paste0("\n", if (use.link) "Working ", "Residuals:\n"))
  if (NROW(rsd) > 5L) {
    nm <- c("Min", "1Q", "Median", "3Q", "Max")
    if (is.matrix(rsd)) {
      rq <- apply(rsd, 2L, stats::quantile)
      rownames(rq) <- nm
    } else {
      rq <- stats::quantile(rsd)
      names(rq) <- nm
    }
    rq <- zapsmall(rq, digits + 1L)
    print(rq, digits = digits)
  } else {
    print(rsd, digits = digits)
  }
  # encoding
  cat("\nEncoding:\n")
  print.data.frame(mid.encoding.scheme(object))
  # diagnosis
  if (diagnose) {
    plot.diagnosis <- function(.obj) {
      yhat <- if (use.link) .obj$linear.predictors else .obj$fitted.values
      .rsd <- .obj$residuals
      graphics::plot.default(
        yhat, .rsd,
        xlab = ifelse(use.link, "Linear Predictors", "Fitted Values"),
        ylab = paste0(if (use.link) "Working ", "Residuals"),
        main = "Residuals vs Fitted Values", font.main = 1L, type = "n"
      )
      args <- list(x = yhat, y = .rsd, iter = 3L)
      args <- utils::modifyList(args, list(...))
      do.call(graphics::panel.smooth, args)
      graphics::abline(h = 0L, lty = 3L, col = "gray65")
    }
    if (inherits(object, "mids")) {
      invisible(lapply(X = as.list(object), FUN = plot.diagnosis))
    } else {
      plot.diagnosis(.obj = object)
    }
  }
  invisible(object)
}

mid.encoding.scheme <- function(object, ...) {
  fun <- function(enc) paste0(enc$type, "(", enc$n, ")")
  mencs <- sapply(object$encoders$main.effects, fun)
  iencs <- sapply(object$encoders$interactions, fun)
  tags <- unique(c(mtags <- names(mencs), itags <- names(iencs)))
  df <- data.frame(row.names = tags)
  if (length(mtags) > 0L) {
    df[["main.effect"]] <- character(1L)
    for (tag in mtags)
      df[tag, "main.effect"] <- mencs[tag]
  }
  if (length(itags) > 0L) {
    df[["interaction"]] <- character(1L)
    for (tag in itags)
      df[tag, "interaction"] <- iencs[tag]
  }
  df
}


#' @rdname summary.mid
#' @exportS3Method base::summary
#'
summary.mids <- function(object, max.nmodels = 1L, ...) {
  if (inherits(object, "midrib"))
    return(summary.mid(object, ...))
  nms <- labels(object)
  nmodels <- length(nms)
  n <- min(nmodels, max.nmodels)
  Map(
    function(obj, nm) {
      cat("\n$", nm, "\n", sep = "")
      summary.mid(obj, ...)
    }, object[seq_len(n), drop = FALSE], nms[seq_len(n)]
  )
  if (n < nmodels)
    cat(sprintf("\n... and %d more models\n", nmodels - max.nmodels))
  invisible(object)
}



.summarize.collection <- function(
    object, shape, dfname, keycol, valcol, keys, targets
) {
  shape <- match.arg(shape, c("wide", "long"))
  if (shape == "wide") {
    fun <- function(x) {
      inner <- x[[dfname]]
      res <- inner[[valcol]][match(targets, inner[[keycol]])]
      replace(res, is.na(res), 0)
    }
    info <- object[[1L]][[dfname]]
    info <- info[match(targets, info[[keycol]]), keys, drop = FALSE]
    if (is.null(names(object))) names(object) <- labels(object)
    out <- data.frame(
      info, as.data.frame(lapply(object, fun)), check.names = FALSE
    )
  } else {
    nms <- labels(object)
    fun_long <- function(x, nm) {
      inner <- x[[dfname]]
      res <- inner[inner[[keycol]] %in% targets, c(keys, valcol), drop = FALSE]
      res$label <- rep.int(nm, nrow(res))
      res
    }
    out <- do.call(rbind, Map(fun_long, object, nms))
  }
  rownames(out) <- NULL
  return(out)
}

#' @exportS3Method base::summary
#'
summary.midimps <- function(
    object, shape = c("wide", "long"), terms = NULL, ...
) {
  shape <- match.arg(shape)
  terms <- terms %||% mid.terms(object[[1L]])
  .summarize.collection(
    object = object, shape = shape,
    dfname = "importance", keycol = "term", valcol = "importance",
    keys = "term", targets = terms
  )
}

#' @exportS3Method base::summary
#'
summary.midbrks <- function(
    object, shape = c("wide", "long"), terms = NULL, ...
) {
  shape <- match.arg(shape)
  terms <- terms %||% mid.terms(object[[1L]])
  .summarize.collection(
    object = object, shape = shape,
    dfname = "breakdown", keycol = "term", valcol = "mid",
    keys = "term", targets = terms
  )
}

#' @exportS3Method base::summary
#'
summary.midcons <- function(
    object, shape = c("long", "wide"), ids = NULL, ...
) {
  shape <- match.arg(shape)
  ids <- ids %||% object[[1L]]$ids
  variable <- object[[1L]]$variable
  .summarize.collection(
    object = object, shape = shape,
    dfname = "conditional", keycol = ".id", valcol = "yhat",
    keys = c(".id", variable), targets = ids
  )
}

#' @exportS3Method base::summary
#'
summary.midimp <- function(object, ...) {
  object$importance
}

#' @exportS3Method base::summary
#'
summary.midbrk <- function(object, ...) {
  object$breakdown
}

#' @exportS3Method base::summary
#'
summary.midcon <- function(object, ...) {
  object$conditional
}
