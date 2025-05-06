#' Estimate Stocking-Lord transformation between two mirt objects and equate
#' 
#' This functions takes two fitted unidimensional IRT models (fitted using mirt or unimirt) 
#' as an input, finds the Stocking-Lord transformation from one ability scale to the other,
#' and then provides both true score and observed score equating between the two tests.
#' 
#' @param mirtobj1 An object from  a fitted unidimensional IRT model using \link[mirt]{mirt} or \link[unimirt]{unimirt}. This will be for the test which equating is being done from (i.e. form X in equating terminology).
#' @param mirtobj2 An object from  a fitted unidimensional IRT model using \link[mirt]{mirt} or \link[unimirt]{unimirt}. This will be for the test which equating is being done to (i.e. form Y in equating terminology).
#' @param which1 A vector denoting which items in mirtobj1 are common with those in mirtobj2. Note that either both which 1 and which2 must be specified (and be of the same length) or neither. If neither are specified the function will identify common based on the item names stored within the mirt objects.
#' @param which2 A vector denoting which items in mirtobj2 are common with those in mirtobj1. See above and examples for further details.
#' @param fixSLA Logical value denoting that the Stocking-Lord slope should be fixed at 1. This may be useful for linking two Rasch models. By default this parameter is FALSE.
#' 
#' @return The function returns a list of two data frames. 
#' The first labelled StockingLord just contains the Stocking-Lord transformation parameters.
#' The second labelled eqtable gives the possible scores from the test within mirtobj1
#' and the equated scores (using both true and observed score equating) from the test analysed in mirtobj2.
#'
#' @examples
#' \dontrun{
#' mirt1=mirt(mathsdata[,1:10],1)
#' mirt2=mirt(mathsdata[,6:15],1)
#' MirtStockingLord(mirt1,mirt2)

#' #same equate but using two groups (one that got Q30 wrong and one got it right)
#' newd1=mathsdata[mathsdata$q30==0,1:10]
#' newd2=mathsdata[mathsdata$q30==1,6:15]
#' newmirt1=mirt(newd1,1)
#' newmirt2=mirt(newd2,1)
#' MirtStockingLord(newmirt1,newmirt2)
#' 
#' #what if we only include Q6 and Q7 as common items
#' MirtStockingLord(newmirt1,newmirt2,which1=c(6,7),which2=c(1,2))
#' 
#' #what if we only include Q9 as a common item
#' MirtStockingLord(newmirt1,newmirt2,which1=9,which2=4)
#' 
#' #same idea but using a Rasch (partial credit) model
#' newmirt1R=mirt(newd1,1,itemtype="Rasch")
#' newmirt2R=mirt(newd2,1,itemtype="Rasch")
#' MirtStockingLord(newmirt1R,newmirt2R,fixSLA=TRUE,which1=9,which2=4)
#' }
#' @export
MirtStockingLord=function(mirtobj1,mirtobj2,which1=NULL,which2=NULL,fixSLA=FALSE){

if(is.null(which1) & !is.null(which2)){print("Either specify both which1 and which2 or neither")
	return(NULL)}
if(!is.null(which2) & is.null(which1)){print("Either specify both which1 and which2 or neither")
	return(NULL)}
if(length(which2)!=length(which1)){print("which1 and which2 should have the same length")
	return(NULL)}

#if not specified just find all shared item names across the two tests with the same names
if(is.null(which1) & is.null(which2)){
	#find common items (if not supplied already)
	names1=colnames(mirtobj1@Data$data)
	names2=colnames(mirtobj2@Data$data)
	which1=which(names1%in%names2)
	which2=which(names2%in%names1)
	}

if(length(which1)==0){print("No common items found in the items names. Please specify some common items manually.")
	return(NULL)}

#maxes1=apply(mirtobj1@Data$data,2,max,na.rm=TRUE)
#maxes2=apply(mirtobj2@Data$data,2,max,na.rm=TRUE)
maxes1=extract.mirt(mirtobj1,"K")-1
maxes2=extract.mirt(mirtobj2,"K")-1
thetas=mirtobj1@Model$Theta
qwts=mirtobj1@Internals$Prior[[1]]
expected1=mirt::expected.test(mirtobj1, thetas,which.items = which1)

if(fixSLA==FALSE){
#weighted difference between curves for given linear transformation of ability
StockLordFunc=function(ABvec){
	sum(qwts*((mirt::expected.test(mirtobj2,ABvec[1]*thetas+ABvec[2],which.items = which2)-expected1)^2))}
#StockLordFunc(c(1,0))
SLopt=stats::optim(c(1,0),StockLordFunc)
StockingLordA=SLopt$par[1]
StockingLordB=SLopt$par[2]
}

if(fixSLA==TRUE){
  #weighted difference between curves for given linear transformation of ability
  StockLordFuncB=function(B){
    sum(qwts*((mirt::expected.test(mirtobj2,thetas+B,which.items = which2)-expected1)^2))}
  #StockLordFunc(c(1,0))
  SLopt=stats::optim(0,StockLordFuncB,method="Brent",lower=-100,upper=100)
  StockingLordA=1
  StockingLordB=SLopt$par[1]
}


#True score equate
#First get abilities for each score on A except 0 and max
thetas1a=seq(-20,20,len=1000)
expected1a=mirt::expected.test(mirtobj1, Theta=as.matrix(thetas1a))
scores1=1:(sum(maxes1)-1)
thetascores=stats::approx(x=expected1a,y=seq(-20,20,len=1000),xout=scores1)$y
scores2=mirt::expected.test(mirtobj2, Theta=as.matrix(thetascores*StockingLordA+StockingLordB))
#add in zero and max at each end
trueeq2=c(0,scores2,sum(maxes2))
scores1=c(0,scores1,sum(maxes1))
#use interpolation to fill in any remaining missing values
trueeq2=stats::approx(scores1[!is.na(trueeq2)],trueeq2[!is.na(trueeq2)],scores1)$y


#Observed score equate (just use traditional equipercentile)

meansd1=as.numeric(coef(mirtobj1)$GroupPars[1,])
#meansd2=as.numeric(coef(mirtobj2)$GroupPars[1,])
#want to estimate the score distribution of the first population if they did the second test
meansd2=c(meansd1[1]*StockingLordA+StockingLordB,meansd1[2]*StockingLordA)
dist1=ScoreDistFromMirt(mirtobj1,theta.mean.sd=meansd1)
dist2=ScoreDistFromMirt(mirtobj2,theta.mean.sd=meansd2)

#my own version of the equate function rather tha having package dependencies
x2=c(-0.5,0.5+dist2$score)
p2=c(0,cumsum(dist2$prob))
p1=cumsum(dist1$prob)
p1=0.5*(p1+c(0,p1[1:length(p1)-1]))
obseq2=stats::approx(p2,x2,p1,rule=2)$y
obseq2

#check using the equate package
#equate::equate(equate::as.freqtab(dist1),equate::as.freqtab(dist2),type="equipercentile")$concordance$yx
#ok

eqtable=data.frame(x=dist1$score,trueyx=trueeq2,obsyx=obseq2)
StockingLord=data.frame(A=StockingLordA,B=StockingLordB)

RESULTS=list(StockingLord=StockingLord,eqtable=eqtable)
return(RESULTS)
}

