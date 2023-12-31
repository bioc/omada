% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vote_frequencies_k.R
\name{get_vote_frequencies_k}
\alias{get_vote_frequencies_k}
\title{Get k vote frequencies}
\usage{
get_vote_frequencies_k(object)
}
\arguments{
\item{object}{An object of class "clusterVoting"}
}
\value{
Matrix with k vote frequencies
}
\description{
Get k vote frequencies
}
\examples{
cv.object <- clusterVoting(toy_genes, 4,6,"sc")
get_vote_frequencies_k(cv.object)
}
