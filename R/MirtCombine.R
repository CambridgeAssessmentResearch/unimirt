#' Combine two estimated IRT models into a single object
#' 
#' This function takes two fitted unidimensional IRT models (fitted using mirt or unimirt) 
#' as an input, and combines them into a single object.
#' The function assumes that all parameters are calibrated to a common scale
#' to begin with (e.g. using the function 'MirtStockingLordRecalibrate').
#' Group parameters are defined by the first object and
#' if common items occur in both objects then parameters from the first
#' object are taken in preference over parameters from the second.
#' 
#' Note that this function will not generally give the same results
#' as concurrent parameter estimation. It's intention is to be used
#' as a pre-cursor for other activities (such as item selection using
#' 'runItemSelectionApp') where it is convenient to store item information
#' in a single object.
#' 
#' @param mirtobj1 An estimated IRT model (of class SingleGroupClass) estimated either using the function \link[unimirt]{unimirt}
#' or by applying the function \link[mirt]{mirt} directly. This object defines the
#' ability scale and is given priority in specifying item parameters.
#' @param mirtobj2 An estimated IRT model (of class SingleGroupClass) estimated either using the function \link[unimirt]{unimirt}
#' or by applying the function \link[mirt]{mirt} directly. 
#' 
#' @return A estimated "mirt" object.
#' @examples
#' \dontrun{
#' dat1=mathsdata[1:300,1:8]
#' dat2=mathsdata[301:400,5:10]
#' mirt1=unimirt(dat1,"Rasch")
#' MirtTidyCoef(mirt1)
#' mirt2=unimirt(dat2,"Rasch")
#' MirtTidyCoef(mirt2)
#' mirtcomb=MirtCombine(mirt1,mirt2)
#' MirtTidyCoef(mirtcomb)
#' }
#' @export
MirtCombine=function(mirtobj1,mirtobj2){

dat1=data.frame(mirtobj1@Data$data)
dat2=data.frame(mirtobj2@Data$data)
names1=colnames(dat1)
names2=colnames(dat2)

itetypes1=extract.mirt(mirtobj1,"itemtype")
itetypes2=extract.mirt(mirtobj2,"itemtype")[!names2%in%names1]
itetypes=c(itetypes1,itetypes2)

alldat=rbind_all.columns(dat1,dat2)

nextmirt2v=mirt(alldat,1,pars="values",itemtype=itetypes)
rowids=paste(nextmirt2v$item,"_",nextmirt2v$name,sep="")

pars1=mod2values(mirtobj1)
pars2=mod2values(mirtobj2)
rowids1=paste(pars1$item,"_",pars1$name,sep="")
rowids2=paste(pars2$item,"_",pars2$name,sep="")

for(rowid in rowids){if(rowid%in%rowids2 & !rowid%in%rowids1){
  nextmirt2v$value[rowids==rowid]=pars2$value[rowids2==rowid]
  }}

for(rowid in rowids){if(rowid%in%rowids1){
  nextmirt2v$value[rowids==rowid]=pars1$value[rowids1==rowid]
}}

nextmirt2v$est=FALSE
nextmirt2=mirt(alldat,1,pars=nextmirt2v,itemtype=itetypes,TOL = NaN)
return(nextmirt2)
}

