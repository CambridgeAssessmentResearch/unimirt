#' Use the Stocking-Lord procedure to recalibrate ome mirt object to ability scale of another
#' 
#' This functions takes two fitted unidimensional IRT models (fitted using mirt) 
#' as an input, and finds the Stocking-Lord transformation (based on common items) from one ability scale to the other.
#' Finally it recalibrates the second object to produce a new "mirt" object calibrated to the desired ability scale.
#' This function uses the item names within each fitted object to identify common items automatically.
#' 
#' @param mirtobj1 An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}. This object defines the ability scale that everything is being transformed
#' to.
#' @param mirtobj2 An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}. This object is the one that will be re-created on the revised ability scale.
#' @param fixSLA Logical value denoting that the Stocking-Lord slope should be fixed at 1. 
#' By default this parameter is FALSE but will be set to TRUE if any items of itemtype "Rasch" are 
#' detected in the data sets.
#' 
#' @return A estimated "mirt" object that can be used as a replacement for mirtobj2 but on the desired abiltiy scale.
#' 
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata[1:250,1:10])
#' mirt2=unimirt(mathsdata[251:500,6:15])
#' ExpectedScoreCompare(mirt1,mirt2)
#' 
#' mirt2.rescale=MirtObjectRecalibrate(mirt1,mirt2)
#' ExpectedScoreCompare(mirt1,mirt2.rescale)
#' }
#' @export
MirtObjectRecalibrate=function(mirtobj1,mirtobj2,fixSLA=FALSE){

itetypes=extract.mirt(mirtobj2,"itemtype")
israsch=(max(itetypes%in%"Rasch")==1)#check whether we are recalibrating Rasch items

#If not rasch use usual Stocking-Lord
if(israsch==FALSE){SL=MirtStockingLord(mirtobj2,mirtobj1,fixSLA=fixSLA)}
#If any rasch then just use a mean adjustment
if(israsch==TRUE){SL=MirtStockingLord(mirtobj2,mirtobj1,fixSLA=TRUE)}

SLpars=SL$StockingLord
print(SLpars)
pars2=mod2values(mirtobj2)

nextmirt2v=mirt(mirtobj2@Data$data,1,pars="values",itemtype=itetypes)
nextmirt2v$value=pars2$value#start with existing estimates
nextmirt2v$value[nextmirt2v$name=="MEAN_1"]=nextmirt2v$value[nextmirt2v$name=="MEAN_1"]*SLpars$A+SLpars$B
nextmirt2v$value[nextmirt2v$name=="COV_11"]=nextmirt2v$value[nextmirt2v$name=="COV_11"]*(SLpars$A^2)
#can easily adjust slopes
nextmirt2v$value[substr(nextmirt2v$name,1,1)=="a"]=nextmirt2v$value[substr(nextmirt2v$name,1,1)=="a"]/SLpars$A
#adjusting intercepts is complicated
ditems=data.frame(item=nextmirt2v$item[substr(nextmirt2v$name,1,1)=="d"]
                  ,d=nextmirt2v$value[substr(nextmirt2v$name,1,1)=="d"]
                  ,name=nextmirt2v$name[substr(nextmirt2v$name,1,1)=="d"]
                  ,class=nextmirt2v$class[substr(nextmirt2v$name,1,1)=="d"])
ditems$mark=as.numeric(substr(ditems$name,2,20))
ditems$mark[is.na(ditems$mark)]=1
aitems=data.frame(item=pars2$item[substr(pars2$name,1,1)=="a"
                                       & substr(pars2$name,1,2)!="ak"]
                  ,a=pars2$value[substr(pars2$name,1,1)=="a"
                                      & substr(pars2$name,1,2)!="ak"])
ditems=merge(ditems,aitems,sort=FALSE)
#usual adjustment
nextmirt2v$value[substr(nextmirt2v$name,1,1)=="d"]=ditems$d-ditems$a*SLpars$B/SLpars$A
#adjustment for pcm and gpcm items
nextmirt2v$value[substr(nextmirt2v$name,1,1)=="d" & nextmirt2v$class=="gpcm"]=ditems$d[ditems$class=="gpcm"]-ditems$a[ditems$class=="gpcm"]*ditems$mark[ditems$class=="gpcm"]*SLpars$B

nextmirt2v$est=FALSE
nextmirt2=mirt(mirtobj2@Data$data,1,pars=nextmirt2v,itemtype=itetypes,TOL = NaN)

#copy across "fakedata" attribute if there is one
if("fakedata"%in%names(attributes(mirtobj2))){
	  attr(nextmirt2,"fakedata")=attr(mirtobj2,"fakedata")
  }

return(nextmirt2)
}


#' Plot comparing expected total scores on common items between mirt objects
#' 
#' Common items are identified automatically on the basis of shared variable names.
#' 
#' @param mirtobj1 An estimated IRT model (of class SingleGroupClass) estimated either using the function "unimirt"
#' or by applying the function "mirt" directly.
#' @param mirtobj2 An estimated IRT model (of class SingleGroupClass) estimated either using the function "unimirt"
#' or by applying the function "mirt" directly.
#' @param lab1 Optional character string that can be added to change labelling of object 1 in the chart.
#' @param lab2 Optional character string that can be added to change labelling of object 2 in the chart.
#' 
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata[1:250,1:10])
#' mirt2=unimirt(mathsdata[251:500,6:15])
#' ExpectedScoreCompare(mirt1,mirt2)
#' 
#' mirt2.rescale=MirtObjectRecalibrate(mirt1,mirt2)
#' ExpectedScoreCompare(mirt1,mirt2.rescale)
#' }
#' @export
ExpectedScoreCompare=function(mirtobj1,mirtobj2,lab1=NULL,lab2=NULL){
names1=colnames(mirtobj1@Data$data)
names2=colnames(mirtobj2@Data$data)
which1=which(names1%in%names2)
which2=which(names2%in%names1)
thetas=mirtobj1@Model$Theta
#deal with cases where internal Theta is very short
#(can happen if mirtobj1 is extracted from a multiple group object)
if(length(thetas)<3){
	GPars=data.frame(coef(mirtobj1)$GroupPars)
	thetas=seq(GPars[1,1]-6*GPars[1,2],GPars[1,1]+6*GPars[1,2],length=61)
	thetas=as.matrix(thetas)
}
nquad=length(thetas)
etest1=expected.test(mirtobj1,thetas,which.items = which1)
etest2=expected.test(mirtobj2,thetas,which.items = which2)

obj1lab=deparse(substitute(mirtobj1))
obj2lab=deparse(substitute(mirtobj2))
#deal with cases within shiny app (get better naming labels from parent environment)
if(!is.null(lab1)){obj1lab=lab1}
if(!is.null(lab2)){obj2lab=lab2}

cdata=data.frame(Ability=rep(as.numeric(thetas),2)
                  ,Expected_Score=c(etest1,etest2)
                 ,Object=c(rep(obj1lab,nquad),rep(obj2lab,nquad)))
eplot=ggplot(data=cdata,aes(x=Ability,y=Expected_Score,col=Object))+geom_line()
return(eplot)
}

#' Plot comparing item parameters for common items
#' 
#' Common items are identified automatically on the basis of shared variable names.
#' 
#' @param mirtobj1 An estimated IRT model (of class SingleGroupClass) estimated either using the function "unimirt"
#' or by applying the function "mirt" directly.
#' @param mirtobj2 An estimated IRT model (of class SingleGroupClass) estimated either using the function "unimirt"
#' or by applying the function "mirt" directly.
#' @param compare Character string equal to "Difficulties" (the default) or "Slopes" defining which item parameters should be compared.
#' 
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata[1:250,1:10])
#' mirt2=unimirt(mathsdata[251:500,6:15])
#' 
#' ItemParameterCompare(mirt1,mirt2, compare = "Difficulties")
#' ItemParameterCompare(mirt1,mirt2, compare = "Slopes")
#' }
#' @export
ItemParameterCompare=function(mirtobj1,mirtobj2,compare="Difficulties"){

coef1=MirtTidyCoef(mirtobj1)
coef2=MirtTidyCoef(mirtobj2)
  
if(compare=="Difficulties"){
difslong1=reshape2::melt(coef1[,c("Item",names(coef1)[substr(names(coef1),1,1)=="b"])]
               ,id.vars="Item",na.rm=TRUE)
difslong1$names=paste(difslong1$Item,"_",difslong1$variable,sep="")
names(difslong1)[3]="C1_dif"

difslong2=melt(coef2[,c("Item",names(coef2)[substr(names(coef2),1,1)=="b"])]
               ,id.vars="Item",na.rm=TRUE)
difslong2$names=paste(difslong2$Item,"_",difslong2$variable,sep="")
names(difslong2)[3]="C2_dif"
bcoef=merge(difslong1,difslong2,sort=FALSE)
mindif=min(c(bcoef$C1_dif,bcoef$C2_dif))
maxdif=max(c(bcoef$C1_dif,bcoef$C2_dif))
range1=c(floor(mindif),ceiling(maxdif))
iteplot=ggplot(data=bcoef,aes(x=C1_dif,y=C2_dif,label=names))+
  geom_text()+geom_abline()+geom_point(alpha=0.3)+
  scale_x_continuous(limits=range1)+
  scale_y_continuous(limits=range1)+
  labs(x="Item difficulties in object 1",y="Item difficulties in object 2")
#print(iteplot)
}

if(compare=="Slopes"){
names(coef1)[-1]=paste("C1_",names(coef1)[-1],sep="")
names(coef2)[-1]=paste("C2_",names(coef2)[-1],sep="")
acoef=merge(coef1,coef2,sort=FALSE)
mina=min(c(acoef$C1_a,acoef$C2_a))
maxa=max(c(acoef$C1_a,acoef$C2_a))
rangea1=c(floor(mina),ceiling(maxa))
iteplot=ggplot(data=acoef,aes(x=C1_a,y=C2_a,label=Item))+
  geom_text()+geom_abline()+
  scale_x_continuous(limits=rangea1)+
  scale_y_continuous(limits=rangea1)+
  labs(x="Item slopes in object 1",y="Item slopes in object 2")
#print(iteplot)
}
return(iteplot)
}

