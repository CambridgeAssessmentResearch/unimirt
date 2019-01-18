#' Augment existing item data
#' 
#' This function first checks if there are any missing categories between 0 and the max available for each item in a data set.
#' For each missing category, an additional row is added to the data with missing values for all items
#' except the one with the missing category and with the value for this item set equal to the missing category.
#' Using augmented data provides an alternative to the default behaviour of "mirt" where items are entirely rescored
#' to remove missing categories. The augmented data approach can help facilitate later analyses such as selecting
#' a number of items with an overall total number of marks fixed at some level (see runItemSelectionApp). Provided
#' that a reasonably large amount of data is being analysed and very little data is being added to augment the data set,
#' this step should have minimal impact upon estimated item parameters.
#' 
#' @param data A data frame containing information about all the items.
#' 
#' @return A data frame with additional rows of augmented data (if necessary) and an attribute "fakedata" indicating the rows in the data frame that have been added in.
#'
#' @examples
#' aug1=augmentdata(mathsdata[1:10,15:20])
#' aug1
#' attributes(aug1)
#' @export
augmentdata=function(data){

nunique=function(x){length(unique(x[!is.na(x)]))}

data=data.frame(data)
nc=ncol(data)
maxes=apply(data,2,max,na.rm=TRUE)
nuniques=apply(data,2,nunique)
misscols=(1:ncol(data))[(maxes+1)>nuniques]
fakedata=rep(FALSE,nrow(data))

if(length(misscols)>0){print(paste("Items with missing categories:"
                                   ,paste(names(data)[misscols],collapse=" ")))}

nnewrows=0
for (col in misscols){
  newrows=data[1:(maxes[col]+1-nuniques[col]),]
  newrows[,1:nc]=NA
  missvals=(0:maxes[col])[!(0:maxes[col])%in%unique(data[,col])]
  newrows[,col]=missvals
  data=rbind(data,newrows)
  nnewrows=nnewrows+nrow(newrows)
  fakedata=c(fakedata,rep(TRUE,nrow(newrows)))
}

if(nnewrows==1){print("1 new row of data has been added to the data frame.")}
if(nnewrows>1){print(paste(nnewrows,"new rows of data have been added to the data frame."))}

attr(data,"fakedata")=fakedata

return(data)}
