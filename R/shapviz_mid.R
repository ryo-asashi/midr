#' Calculate MID-Derived Shapley Values
#'
#' @description
#' \code{shapviz.mid()} is an S3 method for the \code{shapviz::shapviz()} generic, which calculates MID-derived Shapley values from a fitted MID model.
#'
#' @details
#' The function calculates MID-derived Shapley values by attributing the contribution of each component function to its respective variables as follows:
#' first, each main effect is fully attributed to its corresponding variable; and
#' then, each second-order interaction effect is split equally between the two variables involved.
#'
#' @param object a "mid" object.
#' @param data a data frame containing the observations for which to calculate MID-derived Shapley values. If not passed, data is automatically extracted based on the function call.
#'
#' @returns
#' \code{shapviz.mid()} returns an object of class "shapviz".
#'
#' @exportS3Method shapviz::shapviz
#'
shapviz.mid <- function(object, data = NULL) {
  if (missing(data))
    data <- model.data(object, env = parent.frame())
  preds <- predict.mid(object, data, type = "term", na.action = "na.pass")
  xvars <- unique(term.split(colnames(preds)))
  shaps <- matrix(0, nrow = nrow(preds), ncol = length(xvars))
  colnames(shaps) <- xvars
  for(i in seq_len(ncol(preds))){
    term <- colnames(preds)[i]
    tags <- term.split(term)
    if(length(tags) == 1L){
      shaps[, term] <- shaps[, term] + preds[, term]
    } else {
      shaps[, tags[1L]] <- shaps[, tags[1L]] + preds[, term] / 2
      shaps[, tags[2L]] <- shaps[, tags[2L]] + preds[, term] / 2
    }
  }
  if (is.null(data)) {
    message("'data' not passed")
    data <- matrix(nrow = nrow(shaps), ncol = ncol(shaps))
    colnames(data) <- xvars
    data <- as.data.frame(data)
  } else {
    data <- model.reframe(object, data)
  }
  shapviz::shapviz(object = shaps, X = data, baseline = object$intercept)
}
