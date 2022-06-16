#' Plot of the highest 30 percentage of coefficients per cluster
#'
#' @param object An object of class "geneSignature"
#' @return A plot of the highest 30 percentage of coefficients per cluster
#' @export
#'
#' @examples
#' gs.object <- geneSignatures(toy_genes, toy_gene_memberships)
#' plot_top30percent_coefficients(gs.object)
plot_top30percent_coefficients <- function(object) {
    UseMethod("plot_top30percent_coefficients")
}
#' @export
plot_top30percent_coefficients.geneSignature <- function(object) {
    object$top30percent.coefficients
}
