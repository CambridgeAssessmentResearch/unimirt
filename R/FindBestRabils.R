#' Function to estimate how big the R_rest values for a set of items might get
#' 
#' For each item in an IRT model in turn, this function estimates the largest possible theoretical correlation between
#' true ability and item scores that might occur if the only thing that changed was the mean ability of the population
#' (i.e. item parameters as well as standard deviation and shape of ability distribution are fixed). This allows an
#' idea of how high an R_rest value might get if the item was used in a population suitable to its difficulty
#' and if the scores on the remainder of the test were perfectly reliable.
#' 
#' For the purposes of this function only the underlying ability distribution is assumed to be normally distributed.
#' Also note that R_abil is actually estimated as the square root of the percentage of the variance in item scores
#' that can be explained by ability. As such, it accounts for nonlinear relationships between item scores and ability.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using the function "unimirt"
#' or by applying the function "mirt" directly.
#' 
#' @return A vector of highest possible correlations between true ability and item scores.
#'
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata,"2")
#' FindBestRabils(mirt1)
#' }
#' @export
FindBestRabils=function(mirtobj){

#for the purposes of calculations use a bespoke list of thetas
gcoef=data.frame(coef(mirtobj)$GroupPars)
mean1=gcoef$MEAN_1[1]
sd1=sqrt(gcoef$COV_11[1])

thetas=as.matrix(seq(mean1-10*sd1,mean1+10*sd1,length=301))
nitems=extract.mirt(mirtobj,"nitems")
maxes=extract.mirt(mirtobj,"K")-1

ptfunc=function(i,mirtobj,thetas){probtrace(extract.item(mirtobj,i),thetas)}
ptlist=lapply(1:nitems,ptfunc,mirtobj=mirtobj,thetas=thetas)

eifunc=function(i,ptlist,maxes){ptlist[[i]]%*%as.matrix(0:maxes[i])}
eilist=lapply(1:nitems,eifunc,ptlist=ptlist,maxes=maxes)

ei2func=function(i,ptlist,maxes){ptlist[[i]]%*%as.matrix((0:maxes[i])^2)}
ei2list=lapply(1:nitems,ei2func,ptlist=ptlist,maxes=maxes)

vilist=lapply(1:nitems,function(i){ei2list[[i]]-eilist[[i]]^2})

#now attempt to find best pop for each item and loot at correl within that
r2func=function(i,qwts){(1+(sum(vilist[[i]]*qwts))/(sum((eilist[[i]]^2)*qwts)-sum(eilist[[i]]*qwts)^2))^-1}
rfunc2=function(amean,i){
    qwts1=stats::dnorm(thetas,amean,sd1)
    qwts1=qwts1/sum(qwts1)
    sqrt(r2func(i,qwts=qwts1))        
}

#find objective for all items
rabilsbest=sapply(1:nitems
                    ,function(i) 
                        stats::optimize(f=rfunc2
                                 ,interval=c(-8*sd1,8*sd1)
                                 ,maximum=TRUE,i=i)$objective)

#set to negative if negative slope
acoef=data.frame(coef(mirtobj,simplify=TRUE,IRTpar=TRUE)$items)$a
rabilsbest[acoef<0]=-1*rabilsbest[acoef<0]

#could get area under information curves and area under information curve per mark as well
#aucs=sapply(1:nitems,function(i) areainfo(mirtobj,c(-10*sd1,10*sd1),i)$Info)
#aucspermark=aucs/maxes

return(rabilsbest)
}
