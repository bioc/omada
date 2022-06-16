#' Plot of the highest 30 percentage of coefficients per cluster
#'
#' @param object An object of class "clusterAnalysis"
#' @return A plot of the highest 30 percentage of coefficients per cluster
#' @export
#'
#' @examples
#' oa.object <- omada(toy_genes, method.upper.k = 4)
#' plot_signature_feature(oa.object)
plot_signature_feature <- function(object) {
    UseMethod("plot_signature_feature")
}
#' @export
plot_signature_feature.clusterAnalysis <- function(object) {
    object$signature.feature.plot
}
