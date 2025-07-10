#' Calculate SHAP of MID Predictions
#'
#' \code{shapviz.mid()} is a S3 method of \code{shapviz::shapviz()} for the fitted MID models.
#'
#' The S3 method of \code{shapviz()} for the "mid" objects returns an object of class "shapviz" to be used to create SHAP plots with the functions of the shapviz package such as \code{sv_waterfall()} and \code{sv_importance()}.
#'
#' @param object a "mid" object.
#' @param data a data frame containing observations for which SHAP values are calculated. If not passed, data is extracted from \code{parent.env()} based on the function call of the "mid" object.
#' @returns
#' \code{shapviz.mid()} returns an object of class "shapviz".
#' @exportS3Method shapviz::shapviz
#'
shapviz.mid <- function(object, data = NULL) {
  if (missing(data))
    data <- model.data(object)
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
