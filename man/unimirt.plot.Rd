% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itemplots.R
\name{unimirt.plot}
\alias{unimirt.plot}
\title{IRT plots}
\usage{
unimirt.plot(
  mirtobj,
  type,
  which.items = NULL,
  thetamin = NULL,
  thetamax = NULL
)
}
\arguments{
\item{mirtobj}{An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.}

\item{type}{The type of plot to produce. "trace" produces category probability tracce plots,
 "cumtrace" produces cumulative probability curve (the chance of achieving each category or above),
 "infotrace" produces item information
curves for each item, "itemscore" produces item characteristic curves for each item, "info" produces total test information
(based on selected items), "SE" produces an idea of the precision of ability estimates based on the selected items,
and "score" gives the test characteristic curve (based on the selected items).}

\item{which.items}{an integer vector indicating which items to include. By default all items are included.}

\item{thetamin}{The minimum ability to include in plots. By default this is extracted from the mirtobj object.}

\item{thetamax}{The maximum ability to include in plots. By default this is extracted from the mirtobj object.}
}
\value{
A ggplot.
}
\description{
For unidimensional IRT models this function
produces the standard types of plot associated with IRT
including: item and test information functions,
item and test characteristic curves 
and item category probability trace plots. 
In contrast to the function "itemplot" in the mirt package
this function is based around ggplots which makes it easier
for users to reformat and customise the look of these plots.
}
\examples{
\dontrun{
mirt1=unimirt(mathsdata)
unimirt.plot(mirt1,"trace",which.items=34,thetamin=-4,thetamax=4)
unimirt.plot(mirt1,"trace",which.items=c(1,9),thetamin=-4,thetamax=4)
unimirt.plot(mirt1,"trace",which.items=c(1,9,12,34),thetamin=-4,thetamax=4)
unimirt.plot(mirt1,"cumtrace",which.items=34,thetamin=-4,thetamax=4)
unimirt.plot(mirt1,"infotrace",which.items=c(1,9,12,34),thetamin=-4,thetamax=4)
unimirt.plot(mirt1,"itemscore",which.items=c(1,9,12,34),thetamin=-4,thetamax=4)
unimirt.plot(mirt1,"info",which.items=c(1,9,12,34),thetamin=-4,thetamax=4)
unimirt.plot(mirt1,"SE",which.items=c(1,9,12,34),thetamin=-4,thetamax=4)
unimirt.plot(mirt1,"score",which.items=c(1,9,12,34),thetamin=-2,thetamax=3)
}
}
