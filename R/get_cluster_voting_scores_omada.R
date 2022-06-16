#' Get a matrix with metric scores for every k and internal index
#'
#' @param object An object of class "clusterAnalysis"
#' @return A matrix with metric scores for every k and internal index
#' @export
#'
#' @examples
#' oa.object <- omada(toy_genes, method.upper.k = 6)
#' get_cluster_voting_scores(oa.object)
get_cluster_voting_scores <- function(object) {
    UseMethod("get_cluster_voting_scores")
}
#' @export
get_cluster_voting_scores.clusterAnalysis <- function(object) {
    object$cluster.voting.scores
}
