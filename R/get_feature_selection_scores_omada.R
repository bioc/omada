#' Get a dataframe of average bootstrap stabilities
#'
#' @param object An object of class "clusterAnalysis"
#' @return A dataframe of average bootstrap stabilities
#' @export
#'
#' @examples
#' oa.object <- omada(toy_genes, method.upper.k = 6)
#' get_feature_selection_scores(oa.object)
get_feature_selection_scores <- function(object) {
    UseMethod("get_feature_selection_scores")
}
#' @export
get_feature_selection_scores.clusterAnalysis <- function(object) {
    object$feature.selection.scores
}
