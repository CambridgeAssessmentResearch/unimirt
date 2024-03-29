% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TCClookup.R
\name{TCClookup}
\alias{TCClookup}
\title{Lookup ability values on the test characteristic curve}
\usage{
TCClookup(mirtobj, fulliterative = FALSE, which.items = NULL)
}
\arguments{
\item{mirtobj}{An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.}

\item{fulliterative}{A logical value denoting whether a full iterative 
method should be used to identify the appropriate abilities 
for each score or whether approximation 
using interpolation between (a thousand) fixed ability points will suffice.
The default value is FALSE. Setting this to TRUE makes estimation noticeably slower
and tends to make very little difference to the resulting abilities.}

\item{which.items}{an integer vector indicating which items to include in the expected test score. 
Default uses all possible items.}
}
\value{
A data frame showing the mapping between each possible raw score 
and the corresponding ability value in the test characteristics curve.
}
\description{
For unidimensional IRT models this function looks up the ability value
associated with each whole number raw score within the test characteristic curve.
Where no equivalent ability can be found (e.g. for all item answered correctly)
or for raw scores below the total expected by pure guessing
values of NA will be returned.
}
\examples{
mirt1=unimirt(mathsdata[,1:20],"3")
#method based on interpolation
TCC1=TCClookup(mirt1)
#method based on full iterative identification of ability points
TCC2=TCClookup(mirt1,fulliterative=TRUE)
#demonstrate that differences are trivial (at least in this example)
plot(TCC1$score,TCC1$TCCabil-TCC2$TCCabil,type='l')
#apply to first five items in test
TCC3=TCClookup(mirt1,which.items=1:5)
}
\seealso{
\code{\link[mirt]{expected.test}}
}
