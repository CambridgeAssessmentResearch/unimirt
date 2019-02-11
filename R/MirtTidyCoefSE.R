#' Get table of item parameters with standard errors
#' 
#' This function produces a slightly more condensed table of IRT item parameters than using the "coef" function.
#' directly on a mirt object. IRT parameters are prefixed by 'a', 'g' or 'b'. Slope parameters are prefixed with an 'a',
#' difficulty parameters with a 'b' and guessing parameters are labelled 'g'. Standard errors are included in the same
#' table. Note that if the original IRT model was estimated without the option "SE=TRUE" then this function will give an
#' error.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' 
#' @return A data frame of item parameters and associated standard errors.
#'
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata,"2",SE=TRUE)
#' MirtTidyCoefSE(mirt1)
#' }
#' @export
MirtTidyCoefSE=function(mirtobj){

coef1=coef(mirtobj,printSE=TRUE,IRTpars=TRUE)
coef1=coef1[names(coef1)!="GroupPars"]
allcoef=data.frame(a=NA)
allcoefSE=data.frame(a=NA)

for (itenum in 1:length(coef1)){
  pari=data.frame(coef1[[itenum]])
  allcoef=rbind_all.columns(allcoef,pari)
  allcoefSE=rbind_all.columns(allcoefSE,pari)
  allcoef=allcoef[rownames(allcoef)!="SE",]
  allcoefSE=allcoefSE[rownames(allcoefSE)!="par",]
  rownames(allcoef)[itenum+1]=names(coef1)[itenum]
  rownames(allcoefSE)[itenum+1]=names(coef1)[itenum]}

names(allcoefSE)=paste(names(allcoefSE),".SE",sep="")
allcoef=cbind(allcoef,allcoefSE)
#put columns in alphabetical order
colord=order(names(allcoef))
allcoef=allcoef[,colord]
#remove dummy starter row
allcoef=allcoef[-1,]

#if we have both "b1" and "b" amongst the parameters then use b1
if(("b1"%in%names(allcoef)) & ("b"%in%names(allcoef))){
bcol=(1:ncol(allcoef))[names(allcoef)=="b"]
allcoef$b1[is.na(allcoef$b1)]=allcoef$b[is.na(allcoef$b1)]
allcoef=allcoef[,-bcol]
if("b1.SE"%in%names(allcoef)){
	allcoef$b1.SE[is.na(allcoef$b1.SE)]=allcoef$b.SE[is.na(allcoef$b1.SE)]
	bSEcol=(1:ncol(allcoef))[names(allcoef)=="b.SE"]
	allcoef=allcoef[,-bSEcol]
	}
}

#if "b" but not "b1" amongst the parameters then rename to "b1"
if(!("b1"%in%names(allcoef)) & ("b"%in%names(allcoef))){
bcol=(1:ncol(allcoef))[names(allcoef)=="b"]
names(allcoef)[bcol]="b1"
if("b.SE"%in%names(allcoef)){
	bSEcol=(1:ncol(allcoef))[names(allcoef)=="b.SE"]
	names(allcoef)[bSEcol]="b1.SE"
	}
}

return(allcoef)
}
