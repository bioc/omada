#' Get a dataframe with the memberships of the samples found in the input data
#'
#' @param object An object of class "clusterAnalysis"
#' @return A dataframe with the memberships of the samples found in the
#' input data
#' @export
#'
#' @examples
#' oa.object <- omada(toy_genes, method.upper.k = 4)
#' get_sample_memberships(oa.object)
get_sample_memberships <- function(object) {
    UseMethod("get_sample_memberships")
}
#' @export
get_sample_memberships.clusterAnalysis <- function(object) {
    object$sample.memberships
}
