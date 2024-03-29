% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itemplots.R
\name{itemplotdata}
\alias{itemplotdata}
\title{Create list of data sets used in typical IRT plots}
\usage{
itemplotdata(mirtobj, which.items = NULL, Theta = NULL)
}
\arguments{
\item{mirtobj}{An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.}

\item{which.items}{an integer vector indicating which items to include. By default all items are included.}

\item{Theta}{a vector of ability values where calculations should focus. By default this is extracted from mirtobj.}
}
\value{
A list of data frames covering the required information at item level, test level and for individual item categories.
}
\description{
For unidimensional IRT models this function creates
a full set of the data that would be used in item information plots, item characteristic curves, and item category probability curves.
}
\examples{
\dontrun{
mirt1=unimirt(mathsdata)
itemplotdata(mirt1,which.items=1:3,Theta=-2:2)
}
}
