#' Get a dataframe with the memberships of the samples found in the input data
#'
#' @param object An object of class "optimalClustering"
#' @return A dataframe with the memberships of the samples found in the
#' input data
#' @export
#'
#' @examples
#' oc.object <- optimalClustering(toy_genes, 4, "spectral")
#' get_optimal_memberships(oc.object)
get_optimal_memberships <- function(object) {
    UseMethod("get_optimal_memberships")
}
#' @export
get_optimal_memberships.optimalClustering <- function(object) {
    object$optimal.memberships
}
