#' Plot comparing item parameters
#' 
#' Common items are identified automatically on the basis of shared variable names.
#' This function differs from "ItemParameterCompare" in that
#' more than two IRT objects can be compared.
#' In addition, if all models have been estimated with 'SE=TRUE'
#' then this function include 95 per cent confidence intervals for
#' item parameters making it easier to assess whether differences
#' between item parameters are statistically significant.
#' 
#' @param mirtobjlist A list of estimated IRT models. For this function to make
#' any sense at all these should all be of the same type.
#' @param mirtnames A character vector of the same length as "mirtobjlist"
#' use to add colour labels to the chart. Dy default they are just labelled
#' obj1, obj2, ...
#' @param commononly Logical value denoting whether only items that occur in more
#' than one object should be included in the chart. Default TRUE.
#' @param compare Character string equal to "Difficulties" (the default), "Slopes" or "Guessing" defining
#' which item parameters should be compared.
#' 
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata[1:250,1:10],SE=TRUE)
#' mirt2=unimirt(mathsdata[251:500,6:15],SE=TRUE)
#' 
#' ParComparePlot(list(mirt1,mirt2),c("first250","next250"), compare = "Difficulties")
#' ParComparePlot(list(mirt1,mirt2),c("first250","next250"), compare = "Slopes")
#' }
#' @export
ParComparePlot=function(mirtobjlist,mirtnames=NULL
                        ,commononly=TRUE,compare = "Difficulties"){

if(is.null(mirtnames)){mirtnames=paste0("obj",1:length(mirtobjlist))}
  
tidy1=data.frame(temp1=1)#initialise the object to store results

for(iiz in 1:length(mirtobjlist)){
  mirtobj=mirtobjlist[[iiz]]
tidy=tryCatch(MirtTidyCoefSE(mirtobj),error = function(e) MirtTidyCoef(mirtobj))
if(!"Item"%in%names(tidy)){tidy$Item=rownames(tidy)}
tidy$itenum=1:nrow(tidy)
tidy$nmarks=factor(extract.mirt(mirtobj,"K")-1-extract.mirt(mirtobj,"mins"))
if("a.SE"%in%names(tidy)){
  tidy$ahigh=tidy$a+1.96*tidy$a.SE
  tidy$alow=tidy$a-1.96*tidy$a.SE
}
if("g.SE"%in%names(tidy)){
  tidy$ghigh=pmin(1,tidy$g+1.96*tidy$g.SE)
  tidy$glow=pmax(0,tidy$g-1.96*tidy$g.SE)
}
tidy$object=mirtnames[iiz]
tidy1=rbind_all.columns(tidy1,tidy)
}
tidy1=tidy1[-1,]

#add g if not there
if(!"g"%in%names(tidy1)){tidy1$g=0}
tidy1$g[is.na(tidy1$g)]=0

itecounts=table(tidy1$Item)
if(commononly==TRUE){
  tidy1=tidy1[tidy1$Item%in%names(itecounts)[itecounts>1],]
}

ites=unique(tidy1$Item)
ites=ites[length(ites):1]
tidy1$Item=factor(tidy1$Item,levels=ites)

if(compare=="Slopes"){
gg1=ggplot(data=tidy1,aes_string(x="Item",y="a",label="Item",col="object"))+geom_text(position=position_dodge(width=1))
gg1=gg1+labs(x=NULL,y="IRT Slope",col="Object")

if("a.SE"%in%names(tidy)){
  gg1=gg1+geom_errorbar(aes_string(ymax="ahigh",ymin="alow"),alpha=0.5,position=position_dodge(width=1))
}

gg1=gg1+coord_flip()+theme_minimal()
return(gg1)
}

if(compare=="Guessing"){
gg1=ggplot(data=tidy1,aes_string(x="Item",y="g",label="Item",col="object"))+geom_text(position=position_dodge(width=1))
gg1=gg1+labs(x=NULL,y="IRT guessing parameter",col="Object")

if("g.SE"%in%names(tidy)){
  gg1=gg1+geom_errorbar(aes_string(ymax="ghigh",ymin="glow"),alpha=0.5,position=position_dodge(width=1))
}

gg1=gg1+coord_flip()+theme_minimal()
return(gg1)
}

#get the same plot for difficulties (just need to melt)
tnams=names(tidy1)
bnams=tnams[tnams%in%paste0("b",1:100)]
bSEnams=tnams[tnams%in%paste0("b",1:100,".SE")]
tb=reshape2::melt(tidy1,id.vars=c("Item","object","nmarks")
        ,measure.vars=bnams,value.name="b",na.rm = TRUE)
tb$variable=as.character(tb$variable)
tb$mark=as.numeric(substr(tb$variable,2,99))
tb$bpar=as.character(tb$variable)
tb$variable=paste0(tb$variable,".SE")

if("a.SE"%in%names(tidy)){
 tbSE=reshape2::melt(tidy1,id.vars=c("Item","object","nmarks")
         ,measure.vars=bSEnams,value.name="b.SE",na.rm = TRUE)
 tbSE$variable=as.character(tbSE$variable)
 tb=merge(tb,tbSE,all.x=TRUE)
}

tb$Item=paste0(tb$Item,"_",tb$mark)
if("b.SE"%in%names(tb)){
  tb$bhigh=tb$b+1.96*tb$b.SE
  tb$blow=tb$b-1.96*tb$b.SE
}

ites=unique(tb$Item)
ites=ites[length(ites):1]
tb$Item=factor(tb$Item,levels=ites)

gg1=ggplot(data=tb,aes_string(x="Item",y="b",label="Item",col="object"))+geom_text(position=position_dodge(width=1))
gg1=gg1+labs(x=NULL,y="IRT Difficulty",col="Object")

if("b.SE"%in%names(tb)){
  gg1=gg1+geom_errorbar(aes_string(ymax="bhigh",ymin="blow"),alpha=0.5,position=position_dodge(width=1))
}

gg1=gg1+coord_flip()+theme_minimal()
gg1

return(gg1)
}
