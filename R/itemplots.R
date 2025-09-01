#' Create list of data sets used in typical IRT plots
#' 
#' For unidimensional IRT models this function creates
#' a full set of the data that would be used in item information plots, item characteristic curves, and item category probability curves.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' @param which.items an integer vector indicating which items to include. By default all items are included.
#' @param Theta a vector of ability values where calculations should focus. By default this is extracted from mirtobj.
#' 
#' @return A list of data frames covering the required information at item level, test level and for individual item categories.
#'
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata)
#' itemplotdata(mirt1,which.items=1:3,Theta=-2:2)
#' }
#' @export
itemplotdata=function(mirtobj,which.items=NULL,Theta=NULL){

nitems=extract.mirt(mirtobj,"nitems")
if(is.null(which.items)){which.items=1:nitems}
itemnames=extract.mirt(mirtobj,"itemnames")[which.items]

if(is.null(Theta)){Theta=mirtobj@Model$Theta}
if(!is.null(Theta)){Theta=matrix(Theta,ncol=1)}
ntheta=length(Theta)

#item results
itenums=rep(which.items,each=ntheta)
itenames=rep(itemnames,each=ntheta)

infs=unlist(lapply(which.items
	,function(i) 
	iteminfo(extract.item(mirtobj, i),Theta)))

scores=unlist(lapply(which.items
	,function(i) 
	expected.item(extract.item(mirtobj, i),Theta)))

SEs=sqrt(1/infs)

itemdata=data.frame(item.number=itenums,item.name=itenames
	,theta=rep(as.vector(Theta),length(which.items))
	,item.information=infs
	,item.score=scores
	,item.se.theta=SEs)

#test results
testdata=stats::aggregate(itemdata[,c("item.information","item.score")]
	,by=list(theta=itemdata$theta),sum)
names(testdata)=c("theta","test.information","test.score")
testdata$test.se.theta=sqrt(1/testdata$test.information)

#trace data
tracelist0=lapply(which.items
                  ,function(i) probtrace(extract.item(mirtobj, i), Theta))
tracelist=lapply(1:length(tracelist0)
                 ,function(i) as.vector(tracelist0[[i]]))
traces=unlist(tracelist)
maxes=extract.mirt(mirtobj,"K")-1
maxes=maxes[which.items]
ncats=maxes+1

categories=unlist(lapply(1:length(which.items),function(i) rep(0:maxes[i],each=ntheta)))

traceitenums=unlist(lapply(1:length(which.items),function(i) rep(i,ntheta*ncats[i])))
traceitenames=unlist(lapply(1:length(which.items),function(i) rep(itemnames[i],ntheta*ncats[i])))
tracetheta=unlist(lapply(1:length(which.items),function(i) rep(as.vector(Theta),ncats[i])))

tracedata=data.frame(item.number=traceitenums,item.name=traceitenames
	,theta=tracetheta
	,category=as.character(categories)
	,category.prob=traces)

#cumulative trace
csfunc=function(mat){mat[,1]=0
  mat=mat[,ncol(mat):1]
  mat=t(apply(mat,1,cumsum))
  mat=mat[,ncol(mat):1]
  mat[,1]=0
  return(mat)
}
cumtraces=unlist(
  lapply(1:length(tracelist0),function(i) csfunc(tracelist0[[i]]))
)
tracedata$cum.prob=cumtraces
tracedata$cum.prob[tracedata$category=="0"]=NA

return(list(itemdata=itemdata,testdata=testdata,tracedata=tracedata))
}

#' IRT plots
#' 
#' For unidimensional IRT models this function
#' produces the standard types of plot associated with IRT
#' including: item and test information functions,
#' item and test characteristic curves 
#' and item category probability trace plots. 
#' In contrast to the function "itemplot" in the mirt package
#' this function is based around ggplots which makes it easier
#' for users to reformat and customise the look of these plots.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' @param type The type of plot to produce. "trace" produces category probability tracce plots,
#'  "cumtrace" produces cumulative probability curve (the chance of achieving each category or above),
#'  "infotrace" produces item information
#' curves for each item, "itemscore" produces item characteristic curves for each item, "info" produces total test information
#' (based on selected items), "SE" produces an idea of the precision of ability estimates based on the selected items,
#' and "score" gives the test characteristic curve (based on the selected items).
#' @param which.items an integer vector indicating which items to include. By default all items are included.
#' @param thetamin The minimum ability to include in plots. By default this is extracted from the mirtobj object.
#' @param thetamax The maximum ability to include in plots. By default this is extracted from the mirtobj object.
#' 
#' @return A ggplot.
#'
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata)
#' unimirt.plot(mirt1,"trace",which.items=34,thetamin=-4,thetamax=4)
#' unimirt.plot(mirt1,"trace",which.items=c(1,9),thetamin=-4,thetamax=4)
#' unimirt.plot(mirt1,"trace",which.items=c(1,9,12,34),thetamin=-4,thetamax=4)
#' unimirt.plot(mirt1,"cumtrace",which.items=34,thetamin=-4,thetamax=4)
#' unimirt.plot(mirt1,"infotrace",which.items=c(1,9,12,34),thetamin=-4,thetamax=4)
#' unimirt.plot(mirt1,"itemscore",which.items=c(1,9,12,34),thetamin=-4,thetamax=4)
#' unimirt.plot(mirt1,"info",which.items=c(1,9,12,34),thetamin=-4,thetamax=4)
#' unimirt.plot(mirt1,"SE",which.items=c(1,9,12,34),thetamin=-4,thetamax=4)
#' unimirt.plot(mirt1,"score",which.items=c(1,9,12,34),thetamin=-2,thetamax=3)
#' }
#' @export
unimirt.plot=function(mirtobj,type,which.items=NULL,thetamin=NULL,thetamax=NULL){

nitems=extract.mirt(mirtobj,"nitems")
if(is.null(which.items)){which.items=1:nitems}

if(!(type %in% c("trace","cumtrace","infotrace","itemscore","info","SE","score"))){
	stop('type supplied is not supported')
	}

if(is.null(thetamin)){thetamin=min(mirtobj@Model$Theta)}
if(is.null(thetamax)){thetamax=max(mirtobj@Model$Theta)}
theta=seq(thetamin,thetamax,length=50)

plotdat=itemplotdata(mirtobj,which.items=which.items,Theta=theta)

#trace
if(type=="trace"){
ggp=ggplot(data=plotdat$tracedata
	,aes(x=.data[["theta"]],y=.data[["category.prob"]],col=.data[["item.name"]],lty=.data[["category"]]))+geom_line()+ylim(0,1)
}

#cumulative trace
if(type=="cumtrace"){
  ggp=ggplot(data=plotdat$tracedata[!plotdat$tracedata$category=="0",]
            ,aes(x=.data[["theta"]],y=.data[["cum.prob"]],col=.data[["item.name"]],lty=.data[["category"]]))+geom_line()+ylim(0,1)
}

#infotrace
if(type=="infotrace"){
ggp=ggplot(data=plotdat$itemdata
           ,aes(x=.data[["theta"]],y=.data[["item.information"]],col=.data[["item.name"]]))+geom_line()+ylim(0,NA)
}

#itemscore
if(type=="itemscore"){
ggp=ggplot(data=plotdat$itemdata
           ,aes(x=.data[["theta"]],y=.data[["item.score"]],col=.data[["item.name"]]))+
  geom_line()+ylim(0,ceiling(max(plotdat$itemdata$item.score)))
}

#info
if(type=="info"){
ggp=ggplot(data=plotdat$testdata
           ,aes(x=.data[["theta"]],y=.data[["test.information"]]))+
	geom_line()+ylim(0,NA)
}

#SE
if(type=="SE"){
ggp=ggplot(data=plotdat$testdata
           ,aes(x=.data[["theta"]],y=.data[["test.se.theta"]]))+
  geom_line()+ylim(0,NA)
}

#score
if(type=="score"){
maxes=extract.mirt(mirtobj,"K")-1
testmax=sum(maxes[which.items])
ggp=ggplot(data=plotdat$testdata
           ,aes(x=.data[["theta"]],y=.data[["test.score"]]))+geom_line()+ylim(0,testmax)
}

return(ggp)
}

