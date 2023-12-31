% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering_method_selection.R
\name{clusteringMethodSelection}
\alias{clusteringMethodSelection}
\title{Method Selection through intra-method Consensus Partition Consistency}
\usage{
clusteringMethodSelection(data, method.upper.k = 5, number.of.comparisons = 3)
}
\arguments{
\item{data}{A dataframe, where columns are features and rows are data points}

\item{method.upper.k}{The number of clusters, k, up to which the average
agreements will be calculated}

\item{number.of.comparisons}{The number of comparisons to average over per k}
}
\value{
An object of class "methodSelection" containing a dataframe of
partition agreement scores for a set of random parameters
clustering runs across different methods and the corresponding plot
}
\description{
Method Selection through intra-method Consensus Partition Consistency
}
\examples{
clusteringMethodSelection(toy_genes, method.upper.k = 3,
number.of.comparisons = 2)

}
