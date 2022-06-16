#' Plot the average bootstrap stabilities
#'
#' @param object An object of class "clusterAnalysis"
#' @return Line plot of average bootstrap stabilities
#' @export
#'
#' @examples
#' oa.object <- omada(toy_genes, method.upper.k = 6)
#' plot_feature_selection(oa.object)
plot_feature_selection <- function(object) {
    UseMethod("plot_feature_selection")
}
#' @export
plot_feature_selection.clusterAnalysis <- function(object) {
    object$feature.selection.plot
}
