% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MirtTidyCoef.R
\name{MirtTidyCoef}
\alias{MirtTidyCoef}
\title{Get table of item parameters}
\usage{
MirtTidyCoef(mirtobj)
}
\arguments{
\item{mirtobj}{An estimated IRT model (of class SingleGroupClass) estimated using \link[mirt]{mirt} or \link[unimirt]{unimirt}.}
}
\value{
A data frame of item parameters.
}
\description{
This function produces a slightly more condensed table of IRT item parameters than using the "coef" function.
directly on a mirt object. IRT parameters are prefixed by 'a', 'g' or 'b'. Slope parameters are prefixed with an 'a',
difficulty parameters with a 'b' and guessing parameters are labelled 'g'.
}
\examples{
\dontrun{
mirt1=unimirt(mathsdata,"2")
MirtTidyCoef(mirt1)
}
}
