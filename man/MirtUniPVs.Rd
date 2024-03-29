% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MirtLogLikMatrix.R
\name{MirtUniPVs}
\alias{MirtUniPVs}
\title{Function to create plausible values from a mirt object}
\usage{
MirtUniPVs(mirtobj, excludefake = TRUE)
}
\arguments{
\item{mirtobj}{An estimated IRT model (of class SingleGroupClass) estimated either using the function "unimirt"
or by applying the function "mirt" directly.}

\item{excludefake}{A logical value dnoting whether any added "fake" cases should be excluded
from the returned ability estimates (default=TRUE).}
}
\value{
A vector of plausible ability values. The number of plausible values is always equal to the number of 
cases used to fit the IRT model in the first place.
}
\description{
Similar results can be achieved by using the "fscores" function from the package "mirt".
However, this version may be more effective in (unidimensional only) cases where the fitted
ability distribution is non-normal (e.g. fitted using a Davidian polynomial).
}
\examples{
\dontrun{
mirt1=unimirt(mathsdata,"2")
MirtUniPVs(mirt1)
}
}
