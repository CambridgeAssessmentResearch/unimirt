% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbind_all.columns.R
\name{rbind_all.columns}
\alias{rbind_all.columns}
\title{Function to stack two data frames and keep all the columns}
\usage{
rbind_all.columns(x, y)
}
\arguments{
\item{x}{A data frame.}

\item{y}{A data frame.}
}
\description{
This function is essentially a wrapper for the base function rbind. It takes two data.frames as an input.
Before attempting to stack them using rbind it (internally) adds any column names that occur in the other data frame.
This avoids errors. This function is included in this package as it may be useful as a precursor to concurrent calibration
of two data sets of items using different data.
}
\examples{
dat1=mathsdata[1:100,1:10]#a sample who did items 1:10
dat2=mathsdata[101:200,6:15]#a different group who did 6:15
alldat=rbind_all.columns(dat1,dat2)#combine the data
summary(alldat)
allmirt=unimirt(alldat,"2")#fit an IRT model on combined data
}
