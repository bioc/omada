#' Plot of the highest 30 percentage of coefficients per cluster
#'
#' @param object An object of class "geneSignature"
#' @return A plot of the highest 30 percentage of coefficients per cluster
#' @export
get_top30percent_coefficients <- function(object) {
    UseMethod("get_top30percent_coefficients")
}
#' @export
get_top30percent_coefficients.geneSignature <- function(object) {
    object$top30percent.coefficients
}
