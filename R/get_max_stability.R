#' Get the maximum stability
#'
#' @param object An object of class "feasibilityAnalysis"
#' @return The maximum stability
#' @export
#'
#' @examples
#' fa.object <- feasibilityAnalysis(classes = 4, samples = 50, features = 15)
#' maximum.st <- get_max_stability(fa.object)
get_max_stability <- function(object) {
    UseMethod("get_max_stability")
}

#' @export
get_max_stability.feasibilityAnalysis <- function(object) {
    object$max_stability
}
