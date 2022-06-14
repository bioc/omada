#' Get the simulated dataset
#'
#' @param object An object of class "feasibilityAnalysis"
#' @return Simulated dataset
#' @export
get_generated_dataset <- function(object) {
    UseMethod("get_generated_dataset")
}

#' @export
get_generated_dataset.feasibilityAnalysis <- function(object) {
    object$generated.dataset
}
