% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_signature_feature_coefs_omada.R
\name{get_signature_feature_coefs}
\alias{get_signature_feature_coefs}
\title{Get a list of LASSO (regression analysis) coefficients of each gene}
\usage{
get_signature_feature_coefs(object)
}
\arguments{
\item{object}{An object of class "clusterAnalysis"}
}
\value{
A list of LASSO (regression analysis) coefficients of each gene
}
\description{
Get a list of LASSO (regression analysis) coefficients of each gene
}
\examples{
oa.object <- omada(toy_genes, method.upper.k = 4)
lasso.coefs <- get_signature_feature_coefs(oa.object)
}
