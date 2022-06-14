#' Get cluster memberships for every k
#'
#' @param object An object of class "clusterVoting"
#' @return Cluster memberships for every k
#' @export
get_cluster_memberships_k <- function(object) {
    UseMethod("get_cluster_memberships_k")
}
#' @export
get_cluster_memberships_k.clusterVoting <- function(object) {
    object$cluster.memberships.k
}
