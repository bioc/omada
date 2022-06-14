#' Get the optimal features
#'
#' @param object An object of class "clusterAnalysis"
#' @return The list of optimal features
#' @export
get_feature_selection_optimal_features <- function(object) {
    UseMethod("get_feature_selection_optimal_features")
}
#' @export
get_feature_selection_optimal_features.clusterAnalysis <-
    function(object) {
        object$feature.selection.optimal.features
    }

