#' Estimate the score distribution for an individual item in a "mirt" object.
#' 
#' This function uses the fitted model estimates the percentage of persons that will get each score.
#' If there is no missing data for the item (i.e. all persons have atctually taken the item)
#' then the function will also provide the actual percentages.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' @param which.item An integer denoting which item to investigate.
#' @param theta A matrix giving the ability values used in estimation. By default this is extracted directly from mirtobj.
#' @param qwts A vector giving the weight to assign to each ability in thetas. By default this is extracted directly from mirtobj.
#' 
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata,"2")
#' EstimatedItemDistribution(mirt1,1)
#' EstimatedItemDistribution(mirt1,19)
#' }
#' @export
EstimatedItemDistribution=function(mirtobj,which.item,theta=NULL,qwts=NULL){
  if(is.null(theta)){theta=mirtobj@Model$Theta}
  if(is.null(qwts)){qwts=extract.mirt(mirtobj,"Prior")[[1]]}
pt=probtrace(extract.item(mirtobj,which.item),theta)
expected.distribution=apply(pt,2,stats::weighted.mean,w=qwts)
max1=(extract.mirt(mirtobj,"K")-extract.mirt(mirtobj,"mins")-1)[which.item]

actual.distribution=tabulate(mirtobj@Data$data[,which.item]+1,max1+1)
actual.distribution=actual.distribution/sum(actual.distribution)

out1=data.frame(Item=extract.mirt(mirtobj,"itemnames")[[which.item]]
	,Score=0:max1,Expected_per_cent=round(100*expected.distribution,1))
if(!any(is.na(mirtobj@Data$data[,which.item]))){
out1$Actual_per_cent=round(100*actual.distribution,1)
}
rownames(out1)=NULL
out1
}
