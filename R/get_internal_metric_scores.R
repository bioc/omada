#' Get a matrix with metric scores for every k and internal index
#'
#' @param object An object of class "clusterVoting"
#' @return A matrix with metric scores for every k and internal index
#' @export
#'
#' @examples
#' cv.object <- clusterVoting(toy_genes, 4,8,"sc")
#' get_internal_metric_scores(cv.object)
get_internal_metric_scores <- function(object) {
    UseMethod("get_internal_metric_scores")
}
#' @export
get_internal_metric_scores.clusterVoting <- function(object) {
    object$internal.metric.scores
}
