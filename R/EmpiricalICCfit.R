#' Create a ggplot of empirical item characteristic curves compared to theoretical relationship based on the model
#' 
#' The empirical item characteristic curve plots the mean scores on each item for groups with different total scores
#' on the whole test. 
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' @param itenum A numeric input denoting which item the plot should be produced for.
#' @param which.items A numeric vector denoting which items should be used to create the total test score. By default all available items are included.
#' @param ngroups The number of groups to split the cohort into in order to produce the empirical points on the curve (the mean total score in each group is plotted against the mean item score). Setting ngroups=1 (the default and the exception to the usual pattern) will create one group for each possible total test score.
#' 
#' @return A list with the following elements.
#' \describe{
#'   \item{plot1}{A function that translates any vector of scores on form X into equivalent scores on form Y.}
#'   \item{modelchartdat}{A data frame giving the expected relationship between total score and mean item score based on the IRT model}
#'   \item{empiricalchartdat}{A data frame giving the empirical results within each group}
#' }#'
#' @import ggplot2
#' @examples
#' \dontrun{
#' mirt1=mirt(mathsdata,1)
#' EICC=EmpiricalICCfit(mirt1,1)
#' EICC
#' EICC$plot1
#' EmpiricalICCfit(mirt1,1,ngroups=10)$plot1
#' }
#' @export
EmpiricalICCfit=function(mirtobj,itenum,which.items=NULL,ngroups=1){

  itedata=GetDataFromMirt(mirtobj)
  if(is.null(which.items) & sum(is.na(itedata))>0){return(NULL)}
  thetas = mirtobj@Model$Theta
  qwts = mirtobj@Internals$Prior[[1]]
  if (length(qwts) > length(thetas)) {
    qwts = mirtobj@Internals$Prior[[1]][1, ]
  }
  nqpts = dim(thetas)[1]
  coefs = coef(mirtobj, simplify = TRUE)$items
  coefsd = coefs[, substr(colnames(coefs), 1, 1) == "d" & !colnames(coefs) == 
                   "d0"]
  if (!is.matrix(coefsd)) {
    coefsd = as.matrix(coefsd)
  }
  nites = dim(coefs)[1]
  if (is.null(which.items)) {
    which.items = 1:nites
  }
  
  #if itenum is not in which.items can just use original EmpiricalICCfit and stop there
  if(!itenum%in%which.items){return(EmpiricalICCfitV1(mirtobj=mirtobj,itenum=itenum
                                                          ,which.items=which.items,ngroups=ngroups))}
  #else remove the item itself from which.items and carry on
  which.items=which.items[which.items!=itenum]
  
  papermax = sum(!is.na(coefsd[which.items, ]))
  paperps = matrix(rep(0, (papermax + 1) * nqpts), ncol = nqpts)
  paperps[1, ] = 1
  for (iiz in which.items) {
    probs = t(mirt::probtrace(mirt::extract.item(mirtobj, 
                                                 iiz), thetas))
    nrowp = dim(probs)[1]
    temp1 = 0 * paperps
    for (prow in 1:nrowp) {
      pscore = prow - 1
      addmat = paperps
      if (pscore > 0) {
        extrarows = matrix(rep(0, pscore * nqpts), nrow = pscore)
        addmat = rbind(extrarows, paperps[1:(papermax + 
                                               1 - pscore), ])
      }
      temp1 = temp1 + t(t(addmat) * probs[prow, ])
    }
    paperps = temp1
  }
  
  #probability of scores on rest given theta
  paperps=t(paperps)
  #probability of individual item scores for each thera
  iprobs = mirt::probtrace(mirt::extract.item(mirtobj,itenum), thetas)
  #item maximum
  imax=ncol(iprobs)-1
  #matrix of associated item scores of same shape
  iscoresmat=matrix(rep(0:imax,each=length(thetas)),nrow=length(thetas))
  #matrix of ability priors of same shape
  qwtsmat=matrix(rep(qwts,(imax+1)),nrow=length(thetas))
  #maximum on TOTAL test (including item)
  totmax=imax+ncol(paperps)-1
  #paperps with extra columns of zeros for where X exceeds total
  paperpsaug=cbind(matrix(rep(0,imax*length(thetas)),nrow=length(thetas))
                    ,paperps
                   ,matrix(rep(0,imax*length(thetas)),nrow=length(thetas)))  
    #function to work out E(x|total) for a fixed total
  ExFunc=function(total){
    #Pick up relevant column of paperps=P(Rest|x) to give us P(Tot|x)=P(Tot-x|x)
    PxGivenTot=paperpsaug[,total+imax+1-(0:imax)]*iprobs*qwtsmat
    #note that PxGivenTot isn't actually P(x|Total) as various standardising constants
    #have been left out of calculations
    #will divide by a standardising factor sum(PxGivenTot) instead
    sum(PxGivenTot*iscoresmat)/sum(PxGivenTot)
    }
  expectedite=sapply(0:totmax,ExFunc)
  modelchartdat=data.frame(raw.score=0:totmax,item.score=expectedite)
  
  scoredata=as.matrix(itedata[,sort(c(itenum,which.items))])
  scoretot=rowSums(scoredata)
  itescore=itedata[,itenum]
  keep=(!is.na(scoretot) & !is.na(itescore))
  
  itescore=itescore[keep]
  scoretot=scoretot[keep]
  scoregroups=scoretot
  if(ngroups>1){
    cuts=seq(0,1,length=ngroups+1)[-c(1,ngroups+1)]
    scoregroups=findInterval(scoretot,stats::quantile(scoretot,cuts))
  }
  empiricalchartdat=data.frame(raw.score=tapply(scoretot,scoregroups,mean)
                               ,item.score=tapply(itescore,scoregroups,mean)
                               ,N=table(scoregroups)
  )
  
  plot1=ggplot(data=modelchartdat,aes(x=.data[["raw.score"]],y=.data[["item.score"]]))+geom_line()+
    geom_point(data=empiricalchartdat,alpha=0.5,aes(size=.data[["N.Freq"]]))+
    scale_size_area()+ylim(0,imax)+xlim(0,totmax)

  return(list(plot1=plot1,modelchartdat=modelchartdat,empiricalchartdat=empiricalchartdat))
  
}


#' Subroutine for empirical item characteristic curves
#' 
#' NOTE: THIS IS A SIMPLIFIED VERSION OF THE FUNCTION FOR EmpiricalICCfit
#' 
#' The empirical item characteristic curve plots the mean scores on each item for groups with different total scores
#' on the whole test. If the total test scores include the item itself then technically this subroutine
#' calculates the expected relationship against total scores on a parallel test so (for example) a raw total score of zero
#' will not necessarily imply a definite score of zero on the item. 
#' 
#' Currently just used as a subroutine within the function EmpiricalICCfit
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using the function "unimirt"
#' or by applying the function "mirt" directly.
#' @param itenum A numeric input denoting which item the plot should be produced for.
#' @param which.items A numeric vector denoting which items should be used to create the total test score. By default all available items are included.
#' @param ngroups The number of groups to split the cohort into in order to produce the empirical points on the curve (the mean total score in each group is plotted against the mean item score). Setting ngroups=1 (the default and the exception to the usual pattern) will create one group for each possible total test score.
#' 
#' @return A list with the following elements.
#' \describe{
#'   \item{plot1}{A function that translates any vector of scores on form X into equivalent scores on form Y.}
#'   \item{modelchartdat}{A data frame giving the expected relationship between total score and mean item score based on the IRT model}
#'   \item{empiricalchartdat}{A data frame giving the empirical results within each group}
#' }
#' @import ggplot2
EmpiricalICCfitV1=function(mirtobj,itenum,which.items=NULL,ngroups=1){

  itedata=GetDataFromMirt(mirtobj)

  if(is.null(which.items) & sum(is.na(itedata))>0){return(NULL)}
  thetas = mirtobj@Model$Theta
  qwts = mirtobj@Internals$Prior[[1]]
  if (length(qwts) > length(thetas)) {
    qwts = mirtobj@Internals$Prior[[1]][1, ]
  }
  nqpts = dim(thetas)[1]
  coefs = coef(mirtobj, simplify = TRUE)$items
  coefsd = coefs[, substr(colnames(coefs), 1, 1) == "d" & !colnames(coefs) == 
                   "d0"]
  if (!is.matrix(coefsd)) {
    coefsd = as.matrix(coefsd)
  }
  nites = dim(coefs)[1]
  if (is.null(which.items)) {
    which.items = 1:nites
  }
  papermax = sum(!is.na(coefsd[which.items, ]))
  paperps = matrix(rep(0, (papermax + 1) * nqpts), ncol = nqpts)
  paperps[1, ] = 1
  for (iiz in which.items) {
    probs = t(mirt::probtrace(mirt::extract.item(mirtobj, 
                                                 iiz), thetas))
    nrowp = dim(probs)[1]
    temp1 = 0 * paperps
    for (prow in 1:nrowp) {
      pscore = prow - 1
      addmat = paperps
      if (pscore > 0) {
        extrarows = matrix(rep(0, pscore * nqpts), nrow = pscore)
        addmat = rbind(extrarows, paperps[1:(papermax + 
                                               1 - pscore), ])
      }
      temp1 = temp1 + t(t(addmat) * probs[prow, ])
    }
    paperps = temp1
  }
  
  #probability of each theta for each raw score
  thetaps=t(paperps)*qwts
  thetaps=t(thetaps)/colSums(thetaps)
  
  #expected theta for each given raw score
  expectedtheta=colSums(t(thetaps)*thetas[,1])
  sdtheta=sqrt(colSums(t(thetaps)*thetas[,1]*thetas[,1])-expectedtheta^2)
  
  #expected item score for each given theta
  expectedite1 <- expected.item(extract.item(mirtobj,itenum), thetas)
  expectedite=colSums(t(thetaps)*expectedite1)
  #really this shows expected score "if each pupil did ANOTHER item like this one" 
  modelchartdat=data.frame(raw.score=0:papermax,item.score=expectedite)
  
  scoredata=as.matrix(itedata[,which.items])
  scoretot=rowSums(scoredata)
  itescore=itedata[,itenum]
  keep=(!is.na(scoretot) & !is.na(itescore))
  
  itescore=itescore[keep]
  scoretot=scoretot[keep]
  scoregroups=scoretot
  #ngroups=10
  if(ngroups>1){
    cuts=seq(0,1,length=ngroups+1)[-c(1,ngroups+1)]
    scoregroups=findInterval(scoretot,stats::quantile(scoretot,cuts))
  }
  empiricalchartdat=data.frame(raw.score=tapply(scoretot,scoregroups,mean)
                               ,item.score=tapply(itescore,scoregroups,mean)
                               ,N=table(scoregroups)
  )
  
  maxes=extract.mirt(mirtobj,"K")-1

  plot1=ggplot(data=modelchartdat,aes(x=.data[["raw.score"]],y=.data[["item.score"]]))+geom_line()+
    geom_point(data=empiricalchartdat,alpha=0.5,aes(size=.data[["N.Freq"]]))+
    scale_size_area()+ylim(0,maxes[itenum])

  return(list(plot1=plot1,modelchartdat=modelchartdat,empiricalchartdat=empiricalchartdat))
  
}
