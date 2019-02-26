#' Get table of item parameters
#' 
#' This function produces a slightly more condensed table of IRT item parameters than using the "coef" function.
#' directly on a mirt object. IRT parameters are prefixed by 'a', 'g' or 'b'. Slope parameters are prefixed with an 'a',
#' difficulty parameters with a 'b' and guessing parameters are labelled 'g'.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' 
#' @return A data frame of item parameters.
#'
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata,"2")
#' MirtTidyCoef(mirt1)
#' }
#' @export
MirtTidyCoef=function(mirtobj){
coef1=data.frame(coef(mirtobj,simplify=TRUE,IRTpar=TRUE)$items)
coef1=coef1[,order(names(coef1))]#put names in alphabetical order (helps later)
itenums=1:nrow(coef1)
itemnames=rownames(coef1)
a=coef1$a
g=rep(0,nrow(coef1))
if("g"%in%names(coef1)){g=coef1$g}
bcols=names(coef1)[grep("b",names(coef1))]
outcoef=data.frame(Item=itemnames,a=a,g=g,b1=coef1[,bcols[1]])
if("b1"%in%names(coef1)){outcoef$b1[is.na(outcoef$b1)]=coef1$b1[is.na(outcoef$b1)]}

bcols=bcols[!bcols%in%c("b","b1")]
bnums=substr(bcols,2,9999)
if(length(bnums)>0){
  for(bname in bcols){
    outcoef[,bname]=coef1[,bname]
  }
}
rownames(outcoef)=1:nrow(outcoef)
return(outcoef)
}
