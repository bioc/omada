#' Get average stabilities for all numbers of clusters(k)
#'
#' @param object An object of class "feasibilityAnalysis"
#' @return Average stabilities for all numbers of clusters(k)
#' @export
get_average_stabilities_per_k <- function(object) {
    UseMethod("get_average_stabilities_per_k")
}

#' @export
get_average_stabilities_per_k.feasibilityAnalysis <- function(object) {
    object$avg_stabilities_per_k
}
