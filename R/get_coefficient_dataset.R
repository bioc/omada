#' Get a list of LASSO (regression analysis) coefficients of each gene
#'
#' @param object An object of class "geneSignature"
#' @return A list of LASSO (regression analysis) coefficients of each gene
#' @export
get_coefficient_dataset <- function(object) {
    UseMethod("get_coefficient_dataset")
}
#' @export
get_coefficient_dataset.geneSignature <- function(object) {
    object$coefficient.dataset
}
