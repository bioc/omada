#' Get a list of LASSO (regression analysis) coefficients of each gene
#'
#' @param object An object of class "clusterAnalysis"
#' @return A list of LASSO (regression analysis) coefficients of each gene
#' @export
get_signature_feature_coefs <- function(object) {
    UseMethod("get_signature_feature_coefs")
}
#' @export
get_signature_feature_coefs.clusterAnalysis <- function(object) {
    object$signature.feature.coefs
}
