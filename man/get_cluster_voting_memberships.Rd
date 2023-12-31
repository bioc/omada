% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cluster_voting_memberships_omada.R
\name{get_cluster_voting_memberships}
\alias{get_cluster_voting_memberships}
\title{Get cluster memberships for every k}
\usage{
get_cluster_voting_memberships(object)
}
\arguments{
\item{object}{An object of class "clusterAnalysis"}
}
\value{
Cluster memberships for every k
}
\description{
Get cluster memberships for every k
}
\examples{
oa.object <- omada(toy_genes, method.upper.k = 4)
get_cluster_voting_memberships(oa.object)
}
