% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_generated_dataset.R
\name{get_generated_dataset}
\alias{get_generated_dataset}
\title{Get the simulated dataset}
\usage{
get_generated_dataset(object)
}
\arguments{
\item{object}{An object of class "feasibilityAnalysis"}
}
\value{
Simulated dataset
}
\description{
Get the simulated dataset
}
\examples{
fa.object <- feasibilityAnalysis(classes = 4, samples = 50, features = 15)
generated.ds <- get_generated_dataset(fa.object)
}
