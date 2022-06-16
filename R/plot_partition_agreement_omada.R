#' Plot of partition agreement scores
#'
#' @param object An object of class "clusterAnalysis"
#' @return Plot of partition agreement scores
#' @export
#'
#' @examples
#' oa.object <- omada(toy_genes, method.upper.k = 4)
#' plot_partition_agreement(oa.object)
plot_partition_agreement <- function(object) {
    UseMethod("plot_partition_agreement")
}
#' @export
plot_partition_agreement.clusterAnalysis <- function(object) {
    object$partition.agreement.plot
}
