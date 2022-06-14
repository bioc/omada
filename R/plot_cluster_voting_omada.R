#' Plot k vote frequencies
#'
#' @param object An object of class "clusterAnalysis"
#' @return Plot k vote frequencies
#' @export
plot_cluster_voting <- function(object) {
    UseMethod("plot_cluster_voting")
}
#' @export
plot_cluster_voting.clusterAnalysis <- function(object) {
    object$cluster.voting.plot
}
