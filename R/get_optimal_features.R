#' Get the optimal features
#'
#' @param object An object of class "featureSelection"
#' @return The list of optimal features
#' @export
#'
#' @examples
#' fs.object <- featureSelection(toy_genes, min.k = 3, max.k = 6, step = 10)
#' get_optimal_features(fs.object)
get_optimal_features <- function(object) {
    UseMethod("get_optimal_features")
}
#' @export
get_optimal_features.featureSelection <- function(object) {
    object$optimal.features
}
