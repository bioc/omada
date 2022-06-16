#' Get the optimal stability score
#'
#' @param object An object of class "optimalClustering"
#' @return The optimal stability score
#' @export
#'
#' @examples
#' oc.object <- optimalClustering(toy_genes, 4, "spectral")
#' get_optimal_stability_score(oc.object)
get_optimal_stability_score <- function(object) {
    UseMethod("get_optimal_stability_score")
}
#' @export
get_optimal_stability_score.optimalClustering <- function(object) {
    object$optimal.stability.score
}
