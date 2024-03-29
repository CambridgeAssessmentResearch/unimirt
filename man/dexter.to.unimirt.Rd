% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dexter.to.unimirt.R
\name{dexter.to.unimirt}
\alias{dexter.to.unimirt}
\title{Convert IRT model fitted using fit_enorm (from dexter package) to a mirt object}
\usage{
dexter.to.unimirt(dexter.obj = NULL, dexter.dat = NULL)
}
\arguments{
\item{dexter.obj}{An object  derived using the function fit_enorm from the R package dexter.}

\item{dexter.dat}{The data matrix used to fit the model held in dexter.obj.}
}
\description{
This function is designed to allow users to fit a Rasch model using the
conditional maximum likelihood (CML) procedure but to still have access to all of the interactive
graphical displays and other functions that are available in unimirt.
}
\details{
Note that model objects derived using the fit_enorm function do not retain the full data set.
As a result, the original data needs to be supplied as a second parameter to the conversion function.
}
\examples{
\dontrun{
raschcml=dexter::fit_enorm(as.matrix(mathsdata[,1:5]))
coef(raschcml)
mirtfromcml=dexter.to.unimirt(raschcml,as.matrix(mathsdata[,1:5]))
MirtTidyCoef(mirtfromcml)
}
}
