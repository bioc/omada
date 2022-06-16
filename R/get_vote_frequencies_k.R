#' Get k vote frequencies
#'
#' @param object An object of class "clusterVoting"
#' @return Matrix with k vote frequencies
#' @export
#'
#' @examples
#' cv.object <- clusterVoting(toy_genes, 4,8,"sc")
#' get_vote_frequencies_k(cv.object)
get_vote_frequencies_k <- function(object) {
    UseMethod("get_vote_frequencies_k")
}
#' @export
get_vote_frequencies_k.clusterVoting <- function(object) {
    object$vote.frequencies.k
}

