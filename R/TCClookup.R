#' Lookup ability values on the test characteristic curve
#' 
#' For unidimensional IRT models this function looks up the ability value
#' associated with each whole number raw score within the test characteristic curve.
#' Where no equivalent ability can be found (e.g. for all item answered correctly)
#' or for raw scores below the total expected by pure guessing
#' values of NA will be returned.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' @param fulliterative A logical value denoting whether a full iterative 
#' method should be used to identify the appropriate abilities 
#' for each score or whether approximation 
#' using interpolation between (a thousand) fixed ability points will suffice.
#' The default value is FALSE. Setting this to TRUE makes estimation noticeably slower
#' and tends to make very little difference to the resulting abilities. 
#' @param which.items an integer vector indicating which items to include in the expected test score. 
#' Default uses all possible items.
#' 
#' @return A data frame showing the mapping between each possible raw score 
#' and the corresponding ability value in the test characteristics curve.
#'
#' @seealso \code{\link[mirt]{expected.test}}
#' @examples
#' mirt1=unimirt(mathsdata[,1:20],"3")
#' #method based on interpolation
#' TCC1=TCClookup(mirt1)
#' #method based on full iterative identification of ability points
#' TCC2=TCClookup(mirt1,fulliterative=TRUE)
#' #demonstrate that differences are trivial (at least in this example)
#' plot(TCC1$score,TCC1$TCCabil-TCC2$TCCabil,type='l')
#' #apply to first five items in test
#' TCC3=TCClookup(mirt1,which.items=1:5)
#' @export
TCClookup=function(mirtobj,fulliterative=FALSE,which.items = NULL){
#use group parameters to set limits to consider for abilities
coef1=data.frame(coef(mirtobj)$GroupPars)
mean1=coef1[1,1]
sd1=sqrt(coef1[1,2])
lowextreme=mean1-12*sd1
highextreme=mean1+12*sd1

#work out maximum paper score we need to consider
maxes=apply(mirt::extract.mirt(mirtobj,"data"),2,max,na.rm=TRUE)
if(is.null(which.items)){papermax=sum(maxes)}
if(!is.null(which.items)){papermax=sum(maxes[which.items])}

#the (quick) approxfun route using linear interpolation
if(fulliterative==FALSE){
aseq=seq(lowextreme,highextreme,length=1000)
ET=mirt::expected.test(mirtobj,as.matrix(aseq),which.items = which.items)
TCCdata=data.frame(score=0:papermax,TCCabil=stats::approx(ET,aseq,0:papermax)$y)
}

#the (slow) route based on specifically finding the correct ability each time
if(fulliterative==TRUE){
TCCdata=data.frame(score=0:papermax,TCCabil=sapply(0:papermax,
	function(score1){
		tryCatch(
			stats::uniroot(function(theta) 
			mirt::expected.test(mirtobj,as.matrix(theta)
					,which.items = which.items)-score1
			,interval=c(lowextreme,highextreme))$root
		,error=function(e) NA)
		}
	))
}

return(TCCdata)
}




