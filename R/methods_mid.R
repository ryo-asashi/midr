# R/methods_mid.R

#' Print MID Models
#'
#' @description
#' \code{print()} methods for a fitted MID model ("mid") or a collection of models ("mids").
#'
#' @details
#' By default, the \code{print()} method for "mid" objects provides a quick overview of the model structure by listing the number of main effect and interaction terms.
#' If \code{main.effects = TRUE} is specified, the method will also print the contribution of each main effect at its sample points, providing a more detailed look at the model's components.
#'
#' For a collection of models in the structure-of-array format ("midrib"), the method prints a summarized overview. For array-of-structures collections ("midlist"), it prints the first few models up to \code{max.nmodels}.
#'
#' @param x a "mid" or "mids" object to be printed.
#' @param digits an integer specifying the number of significant digits for printing.
#' @param main.effects logical. If \code{TRUE}, the MID values of each main effect are also printed (only applicable for single "mid" objects).
#' @param max.nmodels an integer specifying the maximum number of models to print for a "midlist" collection.
#' @param ... arguments to be passed to other methods.
#'
#' @examples
#' data(cars, package = "datasets")
#' mid <- interpret(dist ~ speed, cars)
#'
#' # Default print provides a concise summary
#' print(mid)
#'
#' # Setting main.effects = TRUE prints the contributions of each main effect
#' print(mid, main.effects = TRUE)
#' @returns
#' \code{print.mid()} returns the original "mid" object invisibly.
#'
#' \code{print.mids()} returns the original "mids" object invisibly.
#'
#' @seealso \code{\link{interpret}}, \code{\link{summary.mid}}
#'
#' @exportS3Method base::print
#'
print.mid <- function(
    x, digits = max(3L, getOption("digits") - 2L), main.effects = FALSE, ...
) {
  cl <- paste0(trimws(deparse(x$call)), sep = "", collapse = "\n ")
  cat("\nCall:\n", cl, "\n", sep = "")
  if (!is.null(x$model.class)) {
    cat("\nModel Class: ", paste0(x$model.class, collapse = ", "), "\n", sep = "")
  }
  cat("\nIntercept: ", examples(x$intercept, digits = digits), "\n", sep = "")
  m <- length(x$main.effects)
  if (m > 0L) {
    cat("\nMain Effects:\n")
    if (main.effects) {
      for (i in seq_len(m)) {
        cat("---\n$", names(x$main.effects)[i], "\n", sep = "")
        vl <- x$main.effects[[i]]$.effect
        nm <- x$main.effects[[i]][[1L]]
        if (is.numeric(nm)) nm <- format(nm, digits = digits)
        if (is.matrix(vl)) {
          rownames(vl) <- nm
        } else {
          names(vl) <- nm
        }
        print.default(vl, digits = digits)
      }
    } else {
      cat(add_s(m, "main effect term"), "\n", sep = "")
    }
  }
  m <- length(x$interactions)
  if (m > 0L) {
    cat("\nInteractions:\n", add_s(m, "interaction term"), "\n", sep = "")
  }
  ur <- x$ratio
  ur <- if (!inherits(x, "midrib")) ur[1L] else if (is.matrix(ur)) ur[1L, ] else ur
  cat("\nUninterpreted Variation Ratio: ", examples(ur, digits = digits), "\n",
      sep = "")
  invisible(x)
}


#' @rdname print.mid
#' @exportS3Method base::print
#'
print.mids <- function(x, max.nmodels = 1L, ...) {
  if (inherits(x, "midrib")) {
    args <- list(...)
    args$main.effects <- FALSE
    return(do.call(print.mid, c(list(x = x), args)))
  }
  nms <- labels(x)
  nmodels <- length(nms)
  n <- min(nmodels, max.nmodels)
  Map(
    function(obj, nm) {
      cat("\n$", nm, "\n", sep = "")
      print.mid(obj, ...)
    },
    x[seq_len(n), drop = FALSE], nms[seq_len(n)]
  )
  if (n < nmodels && n > 0L) {
    cat(sprintf("\n... and %d more models\n", nmodels - n))
  }
  invisible(x)
}


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
  cl <- paste0(trimws(deparse(object$call)), sep = "", collapse = "\n ")
  cat("\nCall:\n", cl, "\n", sep = "")
  use.link <- !is.null(object$link)
  if (use.link) {
    ln <- object$link$name %||% attr(object$link, "name", exact = TRUE)
    cat("\nLink: ", ln %||% "custom", "\n", sep = "")
  }
  cat("\nUninterpreted Variation Ratio:\n")
  print.default(object$ratio, digits = digits)
  rsd <- object$residuals
  cat("\n", if (use.link) "Working ", "Residuals:\n", sep = "")
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
  cat("\nEncoding:\n")
  print.data.frame(encoding_scheme(object))
  if (diagnose) {
    plot_diagnosis <- function(.obj) {
      yhat <- if (use.link) .obj$linear.predictors else .obj$fitted.values
      .rsd <- .obj$residuals
      graphics::plot.default(
        yhat, .rsd,
        xlab = if (use.link) "Linear Predictors" else "Fitted Values",
        ylab = paste0(if (use.link) "Working ", "Residuals"),
        main = "Residuals vs Fitted Values", font.main = 1L, type = "n"
      )
      args <- utils::modifyList(list(x = yhat, y = .rsd, iter = 3L), list(...))
      do.call(graphics::panel.smooth, args)
      graphics::abline(h = 0L, lty = 3L, col = "gray65")
    }
    if (inherits(object, "midrib")) {
      invisible(lapply(X = as.list(object), FUN = plot_diagnosis))
    } else {
      plot_diagnosis(.obj = object)
    }
  }
  invisible(object)
}


#' @rdname summary.mid
#' @exportS3Method base::summary
#'
summary.mids <- function(object, max.nmodels = 1L, ...) {
  if (inherits(object, "midrib")) {
    return(summary.mid(object, ...))
  }
  nms <- labels(object)
  nmodels <- length(nms)
  n <- min(nmodels, max.nmodels)
  Map(
    function(obj, nm) {
      cat("\n$", nm, "\n", sep = "")
      summary.mid(obj, ...)
    },
    object[seq_len(n), drop = FALSE], nms[seq_len(n)]
  )
  if (n < nmodels) {
    cat(sprintf("\n... and %d more models\n", nmodels - n))
  }
  invisible(object)
}


#' Extract Submodels and Coefficients from MID Models
#'
#' @description
#' \code{subset()} and \code{coef()} methods for extracting a subset of component
#' functions from a fitted MID model (\code{"mid"} / \code{"midrib"}) or a
#' collection of models (\code{"mids"}).
#' \code{subset()} returns a lightweight submodel object, while \code{coef()}
#' returns a named list of the fitted intercept and component effect tables.
#'
#' @param x,object a fitted model object (\code{"mid"}, \code{"midrib"}, or \code{"mids"} for \code{subset()}).
#' @param terms an optional character vector, formula, or \code{\link[stats]{terms}} object specifying the subset of terms (and intercept) to retain or extract. Defaults to all terms in the model.
#' @param intercept logical. If \code{TRUE}, the intercept is preserved or included; if \code{FALSE}, it is zeroed out or omitted. If \code{NULL} (default), inferred from \code{terms}.
#' @param ... further arguments passed to methods.
#'
#' @examples
#' data(airquality, package = "datasets")
#' mid <- interpret(Ozone ~ Wind * Temp, airquality, k = c(5, 3))
#'
#' # Extract a submodel containing only Wind and Temp
#' submid <- subset(mid, ~ Wind + Temp)
#' predict(submid, head(airquality), type = "terms")
#'
#' # Extract component effect tables without the intercept
#' coef(mid, terms = c("Wind", "Wind:Temp"), intercept = FALSE)
#' @returns
#' \code{subset()} returns a \code{"mid"} (or \code{"mids"}) object containing only the selected component functions.
#' \code{coef()} returns a named list containing the intercept (if included) and data frames of component effects (excluding \code{.density}) for each matched term.
#'
#' @seealso \code{\link{interpret}}, \code{\link{predict.mid}}
#'
#' @name subset.mid
#' @rdname subset.mid
#' @exportS3Method base::subset
#'
subset.mid <- function(
    x, terms = NULL, intercept = NULL, ...
) {
  if (is.null(terms) && is.null(intercept)) {
    return(x)
  }
  full <- stats::terms(x)
  env <- environment(full) %||% parent.frame()
  sub <- if (is.null(terms)) full else resolve_terms_arg(x, terms, env = env)
  mlabs <- unique(match_labels(sub, full, nomatch = "fail"))
  intercept <- intercept %||% has_intercept(sub)
  if (length(mlabs) == 0L && !intercept) {
    stop("none of the specified 'terms' are in 'x'", call. = FALSE)
  }
  obj <- x
  if (!intercept) {
    obj$intercept[] <- 0
  }
  if (!is.null(obj$main.effects)) {
    keep <- intersect(names(obj$main.effects), mlabs)
    obj$main.effects <- obj$main.effects[keep]
  }
  if (!is.null(obj$interactions)) {
    keep <- intersect(names(obj$interactions), mlabs)
    obj$interactions <- obj$interactions[keep]
  }
  obj$terms <- as_terms(mlabs, intercept = intercept, env = env)
  obj$ratio <- NA_real_
  obj
}


#' @rdname subset.mid
#' @exportS3Method base::subset
#'
subset.mids <- function(x, ...) {
  if (inherits(x, "midrib")) {
    return(subset.mid(x, ...))
  }
  out <- lapply(X = as.list(x), FUN = subset.mid, ...)
  structure(out, class = class(x))
}


encoding_scheme <- function(object, ...) {
  fun <- function(enc) paste0(enc$type, "(", enc$n, ")")
  mencs <- sapply(object$encoders$main.effects, fun)
  iencs <- sapply(object$encoders$interactions, fun)
  mtags <- names(mencs)
  itags <- names(iencs)
  tags <- unique(c(mtags, itags))
  df <- data.frame(row.names = tags)
  if (length(mtags) > 0L) {
    df[["main.effect"]] <- character(nrow(df))
    for (tag in mtags)
      df[tag, "main.effect"] <- mencs[tag]
  }
  if (length(itags) > 0L) {
    df[["interaction"]] <- character(nrow(df))
    for (tag in itags)
      df[tag, "interaction"] <- iencs[tag]
  }
  df
}


# standard stats methods for mid / midrib ----

#' @exportS3Method stats::formula
#'
formula.mid <- function(x, ...) {
  fm <- x$call$formula
  if (!is.null(fm)) {
    res <- stats::formula(stats::terms(x))
    environment(res) <- environment(fm)
    res
  } else {
    stats::formula(stats::terms(x))
  }
}

#' @exportS3Method stats::formula
#'
formula.midrib <- function(x, ...) {
  formula.mid(x, ...)
}


#' @exportS3Method stats::model.frame
#'
model.frame.mid <- function(object, ...) {
  dat <- model_data(object, env = parent.frame())
  if (is.null(dat)) {
    stop("model data could not be extracted from 'object'", call. = FALSE)
  }
  model_reframe(object, data = dat)
}

#' @exportS3Method stats::model.frame
#'
model.frame.midrib <- function(object, ...) {
  model.frame.mid(object, ...)
}


#' @exportS3Method stats::nobs
#'
nobs.mid <- function(object, ...) {
  NROW(object$fitted.values)
}

#' @exportS3Method stats::nobs
#'
nobs.midrib <- function(object, ...) {
  nobs.mid(object, ...)
}


#' @exportS3Method stats::variable.names
#'
variable.names.mid <- function(object, ...) {
  get_variables(object)
}

#' @exportS3Method stats::variable.names
#'
variable.names.midrib <- function(object, ...) {
  variable.names.mid(object, ...)
}


#' @rdname subset.mid
#' @exportS3Method stats::coef
#'
coef.mid <- function(object, terms = NULL, intercept = NULL, ...) {
  full <- stats::terms(object)
  env <- environment(full) %||% parent.frame()
  sub <- if (is.null(terms)) full else resolve_terms_arg(object, terms, env = env)
  intercept <- intercept %||% has_intercept(sub)
  labs <- unique(match_labels(sub, object, nomatch = "omit"))
  res <- lapply(X = labs, FUN = function(lab) {
    vars <- get_variables(lab)
    dat <- if (length(vars) == 1L) {
      object$main.effects[[lab]]
    } else if (length(vars) == 2L) {
      object$interactions[[lab]]
    }
    dat$.density <- NULL
    dat
  })
  names(res) <- labs
  if (intercept && !is.null(object$intercept)) {
    res <- c(list("(Intercept)" = object$intercept), res)
  }
  res
}

#' @rdname subset.mid
#' @exportS3Method stats::coef
#'
coef.midrib <- function(object, ...) {
  coef.mid(object, ...)
}


#' @exportS3Method stats::getCall
#'
getCall.midrib <- function(object, ...) {
  object$call
}


#' @exportS3Method base::labels
#'
labels.mid <- function(object, ...) {
  get_labels(filter_terms(object, ...))
}


resolve_terms_arg <- function(x, terms, env = parent.frame()) {
  dat <- NULL
  if (any(grepl("\\.", as.character(terms)))) {
    vars <- unique(c(
      get_response(stats::terms(x)),
      get_variables(stats::terms(x), raw = TRUE)
    ))
    vars <- vars[nzchar(vars)]
    if (length(vars) > 0L) {
      dat <- as.data.frame(stats::setNames(rep(list(numeric(0L)), length(vars)), vars))
    }
  }
  as_terms(x = terms, data = dat, env = env)
}
