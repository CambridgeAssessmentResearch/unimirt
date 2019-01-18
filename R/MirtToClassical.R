#' Get table of classical item statistics for data in a "mirt" object.
#' 
#' This function extracts the data from a IRT model estimated using "mirt".
#' Then, if there are no missing item scores, it produces a data frame containing the usual classical test statistics.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' 
#' @examples
#' mirt1<-unimirt(mathsdata[1:100,1:10],"Rasch")
#' MirtToClassical(mirt1)
#' @export
MirtToClassical=function(mirtobj){
  data=data.frame(GetDataFromMirt(mirtobj))
  if(sum(is.na(data))>0){return(NULL)}
  itenames=names(data)
  means=apply(data,2,mean,na.rm=TRUE)
  maxes=apply(data,2,max,na.rm=TRUE)
  facils=100*means/maxes
  covtot=stats::cov(data,rowSums(data,na.rm=TRUE),use="pairwise")
  itevars=diag(stats::cov(data,use="pairwise"))
  covrest=covtot-itevars
  itesd=sqrt(itevars)
  restsd=sqrt(stats::var(rowSums(data,na.rm=TRUE))+itevars-2*covtot)
  rrest=covrest/(itesd*restsd)
  ClassStats=data.frame(Item=itenames,Max=maxes,Mean=means
                      ,Facility=facils,SD=itesd,R_rest=rrest)
  return(ClassStats)
  }

#' Cronbach's alpha.
#' 
#' This function estimates Cronbach's alpha from a data set of item score. For the purposes of this function only
#' any missing items are rescored to zero.
#'
#' 
#' @param data A data frame of item data.
#' 
#' @export
alfunc<-function(data){
    data[is.na(data)]=0
    if(sum(is.na(data))>0){return(NULL)}
    (ncol(data)/(ncol(data)-1))*(1-(sum(apply(data,2,stats::var)/stats::var(rowSums(data)))))
    }

