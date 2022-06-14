#' Get the optimal features
#'
#' @param object An object of class "featureSelection"
#' @return The list of optimal features
#' @export
get_optimal_features <- function(object) {
    UseMethod("get_optimal_features")
}
#' @export
get_optimal_features.featureSelection <- function(object) {
    object$optimal.features
}
