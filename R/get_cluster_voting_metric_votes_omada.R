#' Get a dataframe with the k votes for every index
#'
#' @param object An object of class "clusterAnalysis"
#' @return Dataframe with the k votes for every index
#' @export
#'
#' @examples
#' oa.object <- omada(toy_genes, method.upper.k = 6)
#' get_cluster_voting_metric_votes(oa.object)
get_cluster_voting_metric_votes <- function(object) {
    UseMethod("get_cluster_voting_metric_votes")
}
#' @export
get_cluster_voting_metric_votes.clusterAnalysis <- function(object) {
    object$cluster.voting.metric.votes
}
