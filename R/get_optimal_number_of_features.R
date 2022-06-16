#' Get the optimal number of features
#'
#' @param object An object of class "featureSelection"
#' @return The optimal number of features
#' @export
#'
#' @examples
#' fs.object <- featureSelection(toy_genes, min.k = 3, max.k = 6, step = 10)
#' get_optimal_number_of_features(fs.object)
get_optimal_number_of_features <- function(object) {
    UseMethod("get_optimal_number_of_features")
}
#' @export
get_optimal_number_of_features.featureSelection <- function(object) {
    object$optimal.number.of.features
}
