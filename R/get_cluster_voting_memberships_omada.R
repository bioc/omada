#' Get cluster memberships for every k
#'
#' @param object An object of class "clusterAnalysis"
#' @return Cluster memberships for every k
#' @export
get_cluster_voting_memberships <- function(object) {
    UseMethod("get_cluster_voting_memberships")
}
#' @export
get_cluster_voting_memberships.clusterAnalysis <- function(object) {
    object$cluster.voting.memberships
}
