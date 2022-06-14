#' Get a dataframe with the memberships of the samples found in the input data
#'
#' @param object An object of class "clusterAnalysis"
#' @return A dataframe with the memberships of the samples found in the
#' input data
#' @export
get_sample_memberships <- function(object) {
    UseMethod("get_sample_memberships")
}
#' @export
get_sample_memberships.clusterAnalysis <- function(object) {
    object$sample.memberships
}
