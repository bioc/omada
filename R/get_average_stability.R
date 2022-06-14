#' Get the average stability(over all k)
#'
#' @param object An object of class "feasibilityAnalysis"
#' @return The average stability(over all k)
#' @export
get_average_stability <- function(object) {
    UseMethod("get_average_stability")
}

#' @export
get_average_stability.feasibilityAnalysis <- function(object) {
    object$avg_stability
}
