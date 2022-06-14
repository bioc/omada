#' Get the optimal parameter used
#'
#' @param object An object of class "optimalClustering"
#' @return The optimal parameter used
#' @export
get_optimal_parameter_used <- function(object) {
    UseMethod("get_optimal_parameter_used")
}
#' @export
get_optimal_parameter_used.optimalClustering <- function(object) {
    object$optimal.parameter.used
}
