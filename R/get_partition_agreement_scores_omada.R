#' Get a dataframe of partition agreement scores
#'
#' @param object An object of class "clusterAnalysis"
#' @return A dataframe of partition agreement scores
#' parameters clustering runs across different methods
#' @export
#'
#' @examples
#' oa.object <- omada(toy_genes, method.upper.k = 4)
#' get_partition_agreement_scores(oa.object)
get_partition_agreement_scores <- function(object) {
    UseMethod("get_partition_agreement_scores")
}
#' @export
get_partition_agreement_scores.clusterAnalysis <- function(object) {
    object$partition.agreement.scores
}
