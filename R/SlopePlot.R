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

#if(is.null(which.items)){which.items=1:nrow(tidy)}
if(!is.null(which.items)){tidy=tidy[which.items,]}
tidy$itenum=1:nrow(tidy)

gg1=ggplot(data=tidy
           ,aes_string(x="itenum",y="a",label="Item"))+
  geom_text(aes_string(col="nmarks"))
gg1=gg1+geom_hline(yintercept=medianslope,lty=2)
gg1=gg1+scale_x_continuous(trans="reverse",breaks = tidy$itenum,  labels = tidy$Item)
gg1=gg1+labs(x=NULL,y="IRT Slope",col="Item Maximum")

if("a.SE"%in%names(tidy)){
  gg1=gg1+geom_errorbar(aes_string(ymax="ahigh",ymin="alow"),alpha=0.5)
}

gg1=gg1+coord_flip()+theme_minimal()
return(gg1)
}

#' Compare difficulty parameters across items
#' 
#' This function shows the difficulty parameters graphically
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
#' mirt1=unimirt(mathsdata,SE=TRUE)
#' DifficultyPlot(mirt1)
#' DifficultyPlot(mirt1,1:10)#plot for first 10 items only
#' }
#' @export
DifficultyPlot=function(mirtobj,which.items=NULL){
  
  tidy=tryCatch(MirtTidyCoefSE(mirtobj),error = function(e) MirtTidyCoef(mirtobj))
  if(!"Item"%in%names(tidy)){tidy$Item=rownames(tidy)}
  tidy$itenum=1:nrow(tidy)
  tidy$nmarks=factor(extract.mirt(mirtobj,"K")-1-extract.mirt(mirtobj,"mins"))
  if("a.SE"%in%names(tidy)){
    tidy$ahigh=tidy$a+1.96*tidy$a.SE
    tidy$alow=tidy$a-1.96*tidy$a.SE
  }
  
  #get the same plot for difficulties (just need to melt)
  tnams=names(tidy)
  bnams=tnams[tnams%in%paste0("b",1:100)]
  bSEnams=tnams[tnams%in%paste0("b",1:100,".SE")]
  tb=reshape2::melt(tidy,id.vars=c("itenum","Item","nmarks")
                    ,measure.vars=bnams,value.name="b",na.rm = TRUE)
  tb$variable=as.character(tb$variable)
  tb$mark=as.numeric(substr(tb$variable,2,99))
  tb$bpar=as.character(tb$variable)
  tb$variable=paste0(tb$variable,".SE")

  if("a.SE"%in%names(tidy)){
    tbSE=reshape2::melt(tidy,id.vars=c("itenum","Item","nmarks")
                      ,measure.vars=bSEnams,value.name="b.SE",na.rm = TRUE)
  tbSE$variable=as.character(tbSE$variable)
  
  tb=merge(tb,tbSE)
  }

    tb$OrigItem=tb$Item
  tb$Item=paste0(tb$Item,"_",tb$mark)
  if("b.SE"%in%names(tb)){
    tb$bhigh=tb$b+1.96*tb$b.SE
    tb$blow=tb$b-1.96*tb$b.SE
  }

  mediandiff=stats::median(tb$b)
  if(!is.null(which.items)){tb=tb[tb$itenum%in%which.items,]}
  tb$itenum=as.numeric(as.factor(tb$itenum))
  breaksdat=unique(tb[,c("itenum","OrigItem")])
    
  gg1=ggplot(data=tb,aes_string(x="itenum",y="b",label="Item"))+
    geom_text(aes_string(col="as.factor(mark)"))
  gg1=gg1+geom_hline(yintercept=mediandiff,lty=2)
  gg1=gg1+scale_x_continuous(trans="reverse",breaks = breaksdat$itenum,  labels = breaksdat$OrigItem)
  gg1=gg1+labs(x=NULL,y="IRT Difficulty",col="Mark")
  
  if("a.SE"%in%names(tidy)){
    gg1=gg1+geom_errorbar(aes_string(ymax="bhigh",ymin="blow"),alpha=0.5)
  }
  
  gg1=gg1+coord_flip()+theme_minimal()
  return(gg1)
}

