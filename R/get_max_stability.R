#' Get the maximum stability
#'
#' @param object An object of class "feasibilityAnalysis"
#' @return The maximum stability
#' @export
get_max_stability <- function(object) {
    UseMethod("get_max_stability")
}

#' @export
get_max_stability.feasibilityAnalysis <- function(object) {
    object$max_stability
}
