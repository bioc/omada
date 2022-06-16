#' Plot k vote frequencies
#'
#' @param object An object of class "clusterVoting"
#' @return Plot k vote frequencies
#' @export
#'
#' @examples
#' cv.object <- clusterVoting(toy_genes, 4,6,"sc")
#' plot_vote_frequencies(cv.object)
plot_vote_frequencies <- function(object) {
    UseMethod("plot_vote_frequencies")
}
#' @export
plot_vote_frequencies.clusterVoting <- function(object) {
    object$vote.frequencies.plot
}
