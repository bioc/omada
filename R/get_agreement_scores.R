#' Get a dataframe of partition agreement scores for a set of random parameters
#' clustering runs across different methods
#'
#' @param object An object of class "partitionAgreement"
#' @return A dataframe of partition agreement scores for a set of random
#' parameters clustering runs across different methods
#' @export
#'
#' @examples
#' pa.object <- partitionAgreement(toy_genes, algorithm.1 = "spectral",
#' measure.1 = "rbfdot", algorithm.2 = "kmeans",measure.2 = "Lloyd",
#' number.of.clusters = 3)
#' get_agreement_scores(pa.object)

#' @export
get_agreement_scores <- function(object) {
    UseMethod("get_agreement_scores")
}
#' @export
get_agreement_scores.partitionAgreement <- function(object) {
    object$ari.scores
}
