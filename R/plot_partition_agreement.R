#' Plot of partition agreement scores
#'
#' @param object An object of class "methodSelection"
#' @return Plot of partition agreement scores
#' @export
#'
#' @examples
#' ms.object <- clusteringMethodSelection(toy_genes, method.upper.k = 3,
#' number.of.comparisons = 2)
#' plot_partition_agreement(ms.object)
plot_partition_agreement <- function(object) {
    UseMethod("plot_partition_agreement")
}
#' @export
plot_partition_agreement.methodSelection <- function(object) {
    object$partition.agreement.plot
}
