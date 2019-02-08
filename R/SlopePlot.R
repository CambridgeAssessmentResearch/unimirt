#' Compare slope parameters across items
#' 
#' This function shows the slope parameters graphically
#' with each of them compared against a dotted reference line
#' relating to the median parameter value within the model.
#' If the model was estimated with "SE=TRUE" then 95 per cent confidence
#' intervals for the slope parameters are displayed within the chart.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' @param which.items an integer vector indicating which items to include in the plot. By default all items are included.
#' Note that the reference median line is always based on all items in the object.
#' 
#' @return A ggplot.
#'
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata,"gpcm",SE=TRUE)
#' SlopePlot(mirt1)
#' SlopePlot(mirt1,1:10)#plot for first 10 items only
#' }
#' @export
SlopePlot=function(mirtobj,which.items=NULL){
tidy=tryCatch(MirtTidyCoefSE(mirtobj),error = function(e) MirtTidyCoef(mirtobj))
if(!"Item"%in%names(tidy)){tidy$Item=rownames(tidy)}
tidy$itenum=1:nrow(tidy)
tidy$nmarks=factor(extract.mirt(mirtobj,"K")-1-extract.mirt(mirtobj,"mins"))
if("a.SE"%in%names(tidy)){
   tidy$ahigh=tidy$a+1.96*tidy$a.SE
   tidy$alow=tidy$a-1.96*tidy$a.SE
}

medianslope=stats::median(tidy$a)

if(is.null(which.items)){which.items=1:nrow(tidy)}

gg1=ggplot(data=tidy[which.items,],aes(x=itenum,y=a,label=Item))+geom_line()+geom_text(aes(col=nmarks))
gg1=gg1+geom_hline(yintercept=medianslope,lty=2)
gg1=gg1+scale_x_continuous(trans="reverse",breaks = tidy$itenum,  labels = tidy$Item)
gg1=gg1+labs(x=NULL,y="IRT Slope",col="Item Maximum")

if("a.SE"%in%names(tidy)){
  gg1=gg1+geom_errorbar(aes(ymax=ahigh,ymin=alow),alpha=0.5)
}

gg1=gg1+coord_flip()+theme_minimal()
return(gg1)
}
