% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_vote_frequencies.R
\name{plot_vote_frequencies}
\alias{plot_vote_frequencies}
\title{Plot k vote frequencies}
\usage{
plot_vote_frequencies(object)
}
\arguments{
\item{object}{An object of class "clusterVoting"}
}
\value{
Plot k vote frequencies
}
\description{
Plot k vote frequencies
}
\examples{
cv.object <- clusterVoting(toy_genes, 4,6,"sc")
plot_vote_frequencies(cv.object)
}
