#' Get a dataframe of partition agreement scores for a set of random parameters
#' clustering runs across different methods
#'
#' @param object An object of class "methodSelection"
#' @return A dataframe of partition agreement scores for a set of random
#' parameters clustering runs across different methods
#' @export
get_partition_agreement_scores <- function(object) {
    UseMethod("get_partition_agreement_scores")
}
#' @export
get_partition_agreement_scores.methodSelection <- function(object) {
    object$partition.agreement.scores
}
