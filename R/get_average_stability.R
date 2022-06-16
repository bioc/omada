#' Get the average stability(over all k)
#'
#' @param object An object of class "feasibilityAnalysis"
#' @return The average stability(over all k)
#' @export
#'
#' @examples
#' fa.object <- feasibilityAnalysis(classes = 4, samples = 50, features = 15)
#' average.st <- get_average_stability(fa.object)
get_average_stability <- function(object) {
    UseMethod("get_average_stability")
}

#' @export
get_average_stability.feasibilityAnalysis <- function(object) {
    object$avg_stability
}
