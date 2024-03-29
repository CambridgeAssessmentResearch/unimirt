% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MirtToEstimatedClassical.R
\name{MirtToEstimatedClassical}
\alias{MirtToEstimatedClassical}
\title{Estimate classical item statistics for data in a "mirt" object.}
\usage{
MirtToEstimatedClassical(mirtobj, theta = NULL, qwts = NULL)
}
\arguments{
\item{mirtobj}{An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.}

\item{theta}{A matrix giving the ability values used in estimation. By default this is extracted directly from mirtobj.}

\item{qwts}{A vector giving the weight to assign to each ability in thetas. By default this is extracted directly from mirtobj.}
}
\description{
This function estimates classical item statistics from the fitted IRT model estimated using "mirt".
This is useful as it will produce these estimates even if different students have taken different items.
}
\examples{
\dontrun{
mirt1=unimirt(mathsdata,"2")
MirtToEstimatedClassical(mirt1)
}
}
