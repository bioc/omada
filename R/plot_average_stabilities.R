#' Plot the average bootstrap stabilities
#'
#' @param object An object of class "featureSelection"
#' @return Line plot of average bootstrap stabilities
#' @export
#'
#' @examples
#' fs.object <- featureSelection(toy_genes, min.k = 3, max.k = 6, step = 3)
#' plot_average_stabilities(fs.object)
plot_average_stabilities <- function(object) {
    UseMethod("plot_average_stabilities")
}
#' @export
plot_average_stabilities.featureSelection <- function(object) {
    object$average.stabilities.plot
}
