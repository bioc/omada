#' Get a dataframe of average bootstrap stabilities
#'
#' @param object An object of class "featureSelection"
#' @return A dataframe of average bootstrap stabilities
#' @export
#'
#' @examples
#' fs.object <- featureSelection(toy_genes, min.k = 3, max.k = 4, step = 10)
#'get_average_feature_k_stabilities(fs.object)
get_average_feature_k_stabilities <- function(object) {
  UseMethod("get_average_feature_k_stabilities")
}
#' @export
get_average_feature_k_stabilities.featureSelection <- function(object) {
  object$average.feature.k.stabilities
}

