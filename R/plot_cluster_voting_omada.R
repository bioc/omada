#' Plot k vote frequencies
#'
#' @param object An object of class "clusterAnalysis"
#' @return Plot k vote frequencies
#' @export
#'
#' @examples
#' oa.object <- omada(toy_genes, method.upper.k = 6)
#' plot_cluster_voting(oa.object)
plot_cluster_voting <- function(object) {
    UseMethod("plot_cluster_voting")
}
#' @export
plot_cluster_voting.clusterAnalysis <- function(object) {
    object$cluster.voting.plot
}
