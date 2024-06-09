#' Obtain the TreeSHAP embedding of observations
#'
#' This function embeds the observations of a dataset into the TreeSHAP space.
#'
#' @param model A unified model from the `treeshap` package.
#' @param data A `data.frame` with the same features as the model.
#'
#' @return A `data.frame`` with the TreePath embeddings of the observations.
#'
#'
#' @examples
#' \donttest{
#' library(xgboost)
#' data <- fifa20$data[colnames(fifa20$data) != "work_rate"]
#' target <- fifa20$target
#'
#' # calculating simple SHAP values
#' param <- list(objective = "reg:squarederror", max_depth = 3)
#' xgb_model <- xgboost::xgboost(as.matrix(data),
#'     params = param, label = target,
#'     nrounds = 20, verbose = FALSE
#' )
#' unified_model <- xgboost.unify(xgb_model, as.matrix(data))
#' embeddings <- embed_treeshap(unified_model, data)
#' }
#'
#' @export
embed_treeshap <- function(model, data) {
    if (!inherits(model, "model_unified")) {
        stop("`model` needs to be a unified model from the `treeshap` package")
    }

    if (!inherits(data, "data.frame")) {
        stop("`data` needs to be a 'data.frame'")
    }

    result <- treeshap::treeshap(model, data)
    result$shaps
}
