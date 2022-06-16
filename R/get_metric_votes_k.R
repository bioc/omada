#' Get a dataframe with the k votes for every index
#'
#' @param object An object of class "clusterVoting"
#' @return Dataframe with the k votes for every index
#' @export
#'
#' @examples
#' cv.object <- clusterVoting(toy_genes, 4,8,"sc")
#' get_metric_votes_k(cv.object)
get_metric_votes_k <- function(object) {
    UseMethod("get_metric_votes_k")
}
#' @export
get_metric_votes_k.clusterVoting <- function(object) {
    object$metric.votes.k
}

