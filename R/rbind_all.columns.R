#' Function to stack two data frames and keep all the columns
#' 
#' This function is essentially a wrapper for the base function rbind. It takes two data.frames as an input.
#' Before attempting to stack them using rbind it (internally) adds any column names that occur in the other data frame.
#' This avoids errors. This function is included in this package as it may be useful as a precursor to concurrent calibration
#' of two data sets of items using different data.
#' 
#' @param x A data frame.
#' @param y A data frame.
#' @examples
#' dat1=mathsdata[1:100,1:10]#a sample who did items 1:10
#' dat2=mathsdata[101:200,6:15]#a different group who did 6:15
#' alldat=rbind_all.columns(dat1,dat2)#combine the data
#' summary(alldat)
#' allmirt=unimirt(alldat,"2")#fit an IRT model on combined data
#' @export
rbind_all.columns <- function(x, y) {
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    return(rbind(x, y))
}