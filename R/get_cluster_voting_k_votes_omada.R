#' Get k vote frequencies
#'
#' @param object An object of class "clusterAnalysis"
#' @return Matrix with k vote frequencies
#' @export
get_cluster_voting_k_votes <- function(object) {
    UseMethod("get_cluster_voting_k_votes")
}
#' @export
get_cluster_voting_k_votes.clusterAnalysis <- function(object) {
    object$cluster.voting.k.votes
}
