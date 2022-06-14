#' Plot of the highest 30 percentage of coefficients per cluster
#'
#' @param object An object of class "clusterAnalysis"
#' @return A plot of the highest 30 percentage of coefficients per cluster
#' @export
plot_signature_feature <- function(object) {
    UseMethod("plot_signature_feature")
}
#' @export
plot_signature_feature.clusterAnalysis <- function(object) {
    object$signature.feature.plot
}
