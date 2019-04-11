#' Thurstonian thresholds
#' 
#' The Thurstonian threshold for a score category is defined as the ability at which the probability of achieving 
#' that score or higher reaches the user-defined "prob" (0.5 by default).
#' If prob=0.5, for items analysed using the graded response model or for dichotomous items analysed
#' as part of a model using the (generalised) partial credit model (or Rasch) these will be equal to the usual difficulty parameters. 
#' However, Thurstonian thresholds may be a useful way of understanding the difficulty of marks within
#' polytomous items under the (generalised) partial credit model. This function calculates these.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' @param which.items an integer vector indicating which items to include. By default all items are included.
#' @param prob Probability of success for which thresholds are required.
#' 
#' @return A data frame of Thurstonian thresholds.
#'
#' @examples
#' \dontrun{
#' #model using "Science" data from mirt package
#' #for graded response model Thurstonian thresholds (for prob=0.5)
#' #and item difficulty parameters are the same
#' scimod=unimirt(Science[,1:4]-1)
#' unimirt::MirtTidyCoef(scimod)
#' ThurstonianThresh(scimod)
#' 
#' #will vary for other probs
#' ThurstonianThresh(scimod,prob=0.1)
#' 
#' #for gpcm (or Rasch) model they needn't be
#' mirtobj=unimirt(Science[,1:4]-1,"gpcmfixed")
#' unimirt::MirtTidyCoef(mirtobj)
#' ThurstonianThresh(mirtobj)
#' }
#' @export
ThurstonianThresh <- function (mirtobj, which.items = NULL, prob = 0.5) 
{
  nitems = extract.mirt(mirtobj, "nitems")
  itenames = extract.mirt(mirtobj, "itemnames")
  ncats = extract.mirt(mirtobj, "K")
  maxes = ncats - 1
  itypes = extract.mirt(mirtobj, "itemtype")
  coef1a = unimirt::MirtTidyCoef(mirtobj)
  coef1 = coef1a[, !names(coef1a) %in% c("a", "g", "u")]
  #use mathematical formula for GRM to give starting value (can adjust later)
  #thursthresh=b+qlogis(prob)/a
  for(col in 2:ncol(coef1)){coef1[,col]=coef1[,col]+stats::qlogis(prob)/coef1a$a}

  if (is.null(which.items)) {
    which.items = 1:nitems
  }
  itenames = itenames[which.items]
  nitems = length(which.items)
  maxes = maxes[which.items]
  itypes = itypes[which.items]
  coef1 = coef1[which.items, ]
  thetas = range(mirtobj@Model$Theta)
  range2 = thetas[2] - thetas[1]
  thetas[1] = thetas[1] - 2 * range2
  thetas[2] = thetas[2] + 2 * range2
  threshmat = matrix(NA, nrow = nitems, ncol = max(maxes))
  coef1 = coef1[, 1:(max(maxes) + 1)]
  for (row in 1:nrow(threshmat)) {
    threshmat[row, ] = as.numeric(coef1[row, -1])
  }
  thursfunc = function(ite, score, prob) {
    pgreater = function(theta) {
      sum(mirt::probtrace(mirt::extract.item(mirtobj, ite), 
                          theta)[(1:ncats[ite]) > score])
    }
    p2 = function(theta, prob) {
      pgreater(theta) - prob
    }
    stats::uniroot(p2, c(min(thetas) - 12, max(thetas) + 
                           12), prob, extendInt = "yes")$root
  }
  whichrows = (1:nrow(threshmat))
  notrows = (1:nrow(threshmat))[itypes %in% c("2PL", "graded") 
                                  |(maxes == 1 & itypes %in% c("Rasch", "gpcm"))]
  if (length(notrows) > 0) {
    whichrows = (1:nrow(threshmat))[-notrows]
  }
  for (row in whichrows) {
    for (col in 1:ncol(threshmat)) {
      if (!is.na(threshmat[row, col])) {
        threshmat[row, col] = thursfunc(which.items[row], 
                                        col, prob)
      }
    }
  }
  colnames(threshmat) = paste0("thur.b", 1:ncol(threshmat))
  threshmat = cbind(data.frame(Item = itenames), data.frame(threshmat))
  return(threshmat)
}

#' Location of each item's maximum information
#' 
#' This function calculates the ability value at which each item has the highest value for its information function.
#' For dichotomous items this will usually be the same as the item difficulty parameter.
#' For polytomous items it may provide an idea of the point in the ability scale where the item is most useful.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated using \link[mirt]{mirt} or \link[unimirt]{unimirt}.
#' @param which.items an integer vector indicating which items to include. By default all items are included.
#' 
#' @return A data frame of item names, item maxima and theta locations of maximum information.
#'
#' @examples
#' \dontrun{
#' #model using "Science" data from mirt package
#' scimod=unimirt(Science[,1:4]-1,"gpcm")
#' InformationLocation(scimod)
#' #visual verification
#' unimirt.plot(scimod,"infotrace")
#' }
#' @export
InformationLocation=function(mirtobj,which.items=NULL){

nitems=extract.mirt(mirtobj,"nitems")
itenames=extract.mirt(mirtobj,"itemnames")
ncats=extract.mirt(mirtobj,"K")
maxes=ncats-1
itypes=extract.mirt(mirtobj,"itemtype")
difs1=unimirt::MirtTidyCoef(mirtobj)$b1

if(is.null(which.items)){which.items=1:nitems}

itenames=itenames[which.items]
nitems=length(which.items)
maxes=maxes[which.items]
itypes=itypes[which.items]
difs1=difs1[which.items]

thetas=mirtobj@Model$Theta
range1=c(2*min(thetas)-max(thetas),2*max(thetas)-min(thetas))
#set up difficulties
infdifs=difs1

#function to find threshold for particular item and score
maxinffunc=function(ite){
#mixed approach to optimization
inf2=function(theta){mirt::iteminfo(mirt::extract.item(mirtobj,ite),theta)}
#first test all the usual values
inf2a=inf2(thetas)
inf2b=thetas[which.max(inf2a)]
#use in general optimization
opt1=stats::optim(inf2b,inf2,lower=range1[1],upper=range1[2]
	,method="Brent",control=list(fnscale=-1))
#if worse than initial search then search again close to initial value
if(opt1$value<max(inf2a)){
	opt1=stats::optim(inf2b,inf2,lower=inf2b-0.05,upper=inf2b+0.05
		,method="Brent",control=list(fnscale=-1))
	}
opt1$par
}

#identify which rows needs search for theta that maximises information
#(i.e. not equal to existing parameter)
whichdifs=(1:nitems)
notdifs=(1:nitems)[(maxes==1 & itypes%in%c("Rasch","gpcm","2PL","3PL","graded"))]
if(length(notdifs)>0){whichdifs=(1:nitems)[-notdifs]}

infdifs[whichdifs]=unlist(sapply(which.items[whichdifs],maxinffunc))
data.frame(Item=itenames,max.marks=maxes,theta.max.inf=infdifs)
}
