#' Estimate classical item statistics for data in a "mirt" object.
#' 
#' This function estimates classical item statistics from the fitted IRT model estimated using "mirt".
#' This is useful as it will produce these estimates even if different students have taken different items.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' @param theta A matrix giving the ability values used in estimation. By default this is extracted directly from mirtobj.
#' @param qwts A vector giving the weight to assign to each ability in thetas. By default this is extracted directly from mirtobj.
#' 
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata,"2")
#' MirtToEstimatedClassical(mirt1)
#' }
#' @export
MirtToEstimatedClassical=function(mirtobj,theta=NULL,qwts=NULL){
  nitems=extract.mirt(mirtobj,"nitems")
  itenames=extract.mirt(mirtobj,"itemnames")
  maxes=extract.mirt(mirtobj,"K")-1
  if(is.null(theta)){theta=mirtobj@Model$Theta}
  if(is.null(qwts)){qwts=extract.mirt(mirtobj,"Prior")[[1]]}
  #multiple group compliant version	
  #if(is.null(qwts)){qwts=rowMeans(sapply(1:extract.mirt(mg1,"ngroups"),function(i) extract.mirt(mg1,"Prior")[[i]]))}

  qwts=qwts/sum(qwts)
  
  ptfunc=function(i,mirtobj,thetas){probtrace(extract.item(mirtobj,i),thetas)}
  ptlist=lapply(1:nitems,ptfunc,mirtobj=mirtobj,thetas=theta)
  
  eifunc=function(i,ptlist,maxes){ptlist[[i]]%*%as.matrix(0:maxes[i])}
  eilist=lapply(1:nitems,eifunc,ptlist=ptlist,maxes=maxes)
  
  means=sapply(1:nitems,function(i) sum(eilist[[i]]*qwts))
  facils=100*means/maxes

  ei2func=function(i,ptlist,maxes){ptlist[[i]]%*%as.matrix((0:maxes[i])^2)}
  ei2list=lapply(1:nitems,ei2func,ptlist=ptlist,maxes=maxes)
  
  vilist=lapply(1:nitems,function(i){ei2list[[i]]-eilist[[i]]^2})
  itesd=sapply(1:nitems,function(i) sqrt(sum(vilist[[i]]*qwts)))

  itesd=sqrt(sapply(1:nitems,function(i) sum(ei2list[[i]]*qwts))-means^2)
  
  r2func=function(i,qwts){(1+(sum(vilist[[i]]*qwts))/(sum((eilist[[i]]^2)*qwts)-sum(eilist[[i]]*qwts)^2))^-1}
  r2s=sapply(1:nitems,r2func,qwts=qwts)
  rabils=sqrt(r2s)#note that this correlation accounts for the non-linear relationship between ability and item scores
  rabilsbest=FindBestRabils(mirtobj)
  
  ClassStats=data.frame(Item=itenames,Max=maxes,Mean=means
                        ,Facility=facils,SD=itesd,R_abil=rabils
                        ,R_abil_best=rabilsbest)
  
  return(ClassStats)
}

