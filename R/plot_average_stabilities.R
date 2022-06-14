#' Plot the average bootstrap stabilities
#'
#' @param object An object of class "featureSelection"
#' @return Line plot of average bootstrap stabilities
#' @export
plot_average_stabilities <- function(object) {
    UseMethod("plot_average_stabilities")
}
#' @export
plot_average_stabilities.featureSelection <- function(object) {
    object$average.stabilities.plot
}
