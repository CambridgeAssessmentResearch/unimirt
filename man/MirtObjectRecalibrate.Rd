% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MirtStockingLordRecalibrate.R
\name{MirtObjectRecalibrate}
\alias{MirtObjectRecalibrate}
\title{Use the Stocking-Lord procedure to recalibrate ome mirt object to ability scale of another}
\usage{
MirtObjectRecalibrate(mirtobj1, mirtobj2, fixSLA = FALSE, SE = FALSE)
}
\arguments{
\item{mirtobj1}{An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
This object defines the ability scale that everything is being transformed to.}

\item{mirtobj2}{An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}. This object is the one that will be re-created on the revised ability scale.}

\item{fixSLA}{Logical value denoting that the Stocking-Lord slope should be fixed at 1. 
By default this parameter is FALSE but will be set to TRUE if any items of itemtype "Rasch" are 
detected in the data sets.}

\item{SE}{Logical value denoting whether SEs should be re-estimated on the new scale.
By default this parameter is FALSE meaning that SEs will not be available
in the rescaled object.}
}
\value{
A estimated "mirt" object that can be used as a replacement for mirtobj2 but on the desired abiltiy scale.
}
\description{
This functions takes two fitted unidimensional IRT models (fitted using mirt) 
as an input, and finds the Stocking-Lord transformation (based on common items) from one ability scale to the other.
Finally it recalibrates the second object to produce a new "mirt" object calibrated to the desired ability scale.
This function uses the item names within each fitted object to identify common items automatically.
At present, this function will only work correctly with the itemtypes implemented within \link[unimirt]{unimirt}.
}
\examples{
\dontrun{
mirt1=unimirt(mathsdata[1:250,1:10])
mirt2=unimirt(mathsdata[251:500,6:15])
ExpectedScoreCompare(mirt1,mirt2)

mirt2.rescale=MirtObjectRecalibrate(mirt1,mirt2)
ExpectedScoreCompare(mirt1,mirt2.rescale)
}
}
