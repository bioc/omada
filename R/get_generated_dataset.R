#' Get the simulated dataset
#'
#' @param object An object of class "feasibilityAnalysis"
#' @return Simulated dataset
#' @export
#'
#' @examples
#' fa.object <- feasibilityAnalysis(classes = 4, samples = 50, features = 15)
#' generated.ds <- get_generated_dataset(fa.object)
get_generated_dataset <- function(object) {
    UseMethod("get_generated_dataset")
}

#' @export
get_generated_dataset.feasibilityAnalysis <- function(object) {
    object$generated.dataset
}
