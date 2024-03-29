% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SlopePlot.R
\name{DifficultyPlot}
\alias{DifficultyPlot}
\title{Compare difficulty parameters across items}
\usage{
DifficultyPlot(mirtobj, which.items = NULL)
}
\arguments{
\item{mirtobj}{An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.}

\item{which.items}{an integer vector indicating which items to include in the plot. By default all items are included.
Note that the reference median line is always based on all items in the object.}
}
\value{
A ggplot.
}
\description{
This function shows the difficulty parameters graphically
with each of them compared against a dotted reference line
relating to the median parameter value within the model.
If the model was estimated with "SE=TRUE" then 95 per cent confidence
intervals for the slope parameters are displayed within the chart.
}
\examples{
\dontrun{
mirt1=unimirt(mathsdata,SE=TRUE)
DifficultyPlot(mirt1)
DifficultyPlot(mirt1,1:10)#plot for first 10 items only
}
}
