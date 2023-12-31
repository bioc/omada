#' Get the optimal number of features
#'
#' @param object An object of class "clusterAnalysis"
#' @return The optimal number of features
#' @export
#'
#' @examples
#' oa.object <- omada(toy_genes, method.upper.k = 6)
#' get_feature_selection_optimal_number_of_features(oa.object)
get_feature_selection_optimal_number_of_features <- function(object) {
    UseMethod("get_feature_selection_optimal_number_of_features")
}

#' @export
get_feature_selection_optimal_number_of_features.clusterAnalysis <-
    function(object) {
        object$feature.selection.optimal.number.of.features
}
