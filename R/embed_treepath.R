#' Obtain the TreePath embedding of observations
#'
#' This function embeds the observations of a dataset into the TreePath space.
#'
#' @param model A unified model from the `treeshap` package.
#' @param data A `data.frame` with the same features as the model.
#' @param depth The maximum depth of the TreePath embedding.
#'
#' @return A `data.frame` with the TreePath embeddings of the observations.
#'
#' @importFrom Rcpp sourceCpp
#' @useDynLib treeshap
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
#' embeddings <- embed_treepath(unified_model, data)
#' }
#'
#' @export
embed_treepath <- function(model, data, depth = 8) {
    if (!inherits(model, "model_unified")) {
        stop("`model` needs to be a unified model from the `treeshap` package")
    }

    if (!inherits(data, "data.frame")) {
        stop("`data` needs to be a 'data.frame'")
    }

    data <- data[, colnames(data) %in% c(model$feature_names)]

    model <- model$model
    roots <- which(model$Node == 0) - 1
    yes <- model$Yes - 1
    no <- model$No - 1
    missing <- model$Missing - 1
    is_leaf <- is.na(model$Feature)
    feature <- match(model$Feature, colnames(data)) - 1
    split <- model$Split
    decision_type <- unclass(model$Decision.type)
    trees <- unique(model$Tree)

    n <- nrow(data)
    data <- as.data.frame(sapply(data, as.numeric))
    if (n > 1) data <- t(data)

    is_na <- is.na(data)
    embed_treepath_cpp(
        data, is_na, roots, yes, no, missing, is_leaf,
        feature, split, decision_type, trees, depth
    )
}
