#' Create a mirt object based on a subset of items in another one
#' 
#' This function takes a fitted unidimensional IRT model (fitted using mirt or unimirt) 
#' as an input, and creates a smaller one based on a particular subset of items.
#' This may be useful to enable the shiny apps to focus upon a particular subsection of items.
#' Note that the item parameters are NOT re-estimated. This function is simply intended
#' to allow users to zoom in on results for particular sets of items.
#' For example this might be useful to help produce a Wright Map based
#' on a subset of the items.
#' 
#' Note that although the item parameters are unchanged in this process.
#' Fit statistics (or anything else dependent upon producing 
#' ability estimates as an intermediate step) 
#' will be affected
#' by focussing on a subset of items.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' @param which.items an integer vector indicating which items to include in the reduced object. 
#' 
#' @return A estimated "mirt" object.
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata,"Rasch")
#' MirtTidyCoef(mirt1)
#' mirt2=MirtSubset(mirt1,c(1,3,5,10))
#' MirtTidyCoef(mirt2)
#' }
#' @export
MirtSubset=function(mirtobj,which.items){

dat1=data.frame(mirtobj@Data$data[,which.items])
names(dat1)=colnames(mirtobj@Data$data)[which.items]

names1=colnames(dat1)
itetypes=extract.mirt(mirtobj,"itemtype")[which.items]

nextmirt2v=mirt(dat1,1,pars="values",itemtype=itetypes)
rowids=paste(nextmirt2v$item,"_",nextmirt2v$name,sep="")

pars1=mod2values(mirtobj)
rowids1=paste(pars1$item,"_",pars1$name,sep="")

for(rowid in rowids){if(rowid%in%rowids1){
  nextmirt2v$value[rowids==rowid]=pars1$value[rowids1==rowid]
}}

#nextmirt2v$est=FALSE
nextmirt2=mirt(dat1,1,pars=nextmirt2v,itemtype=itetypes,TOL = NaN)

return(nextmirt2)
}
