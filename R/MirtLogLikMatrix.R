#FUNCTION TO GET A MATRIX OF LOGLIKELIHOODS FOR ALL ABILITIES IN A VECTOR
#USED RATHER THAN fscores AS WORKS BETTER IF DAVIDIAN PRIOR HAS BEEN USED
#' @import mirt
MirtLogLikMatrix=function(mirtobj){
  thetas =as.numeric(mirtobj@Model$Theta)
  nites=extract.mirt(mirtobj,"nitems")
  nquad=length(thetas)
  nperson=nrow(mirtobj@Data$data)
  #first get a list of probtraces for all items
  probtraces=lapply(1:nites
                    ,function(itenum) probtrace(extract.item(mirtobj,itenum)
                                                ,Theta=thetas))

  #function to quickly get sum of log likelihoods for an individual
  #only counting the items they actually did
  indliktot=function(row){
    rowSums(sapply(which(!is.na(mirtobj@Data$data[row,]))
       ,function(i) log(probtraces[[i]][,1+mirtobj@Data$data[row,i]])
       ,simplify=TRUE))
    }
  iteliktot=t(sapply(1:nrow(mirtobj@Data$data),function(i) indliktot(i),simplify=TRUE))

  #now use to manually get posterior probabilities
  qwts=extract.mirt(mirtobj,"Prior")[[1]]
  Prior=rep(1,nrow(iteliktot))%*%t(qwts)
  Post=exp(log(Prior)+iteliktot)
  Post=Post/rowSums(Post)
  return(list(loglikmat=iteliktot,Posterior=Post))
  }

#' Function to create plausible values from a mirt object
#' 
#' Similar results can be achieved by using the "fscores" function from the package "mirt".
#' However, this version may be more effective in (unidimensional only) cases where the fitted
#' ability distribution is non-normal (e.g. fitted using a Davidian polynomial).
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using the function "unimirt"
#' or by applying the function "mirt" directly.
#' 
#' @return A vector of plausible ability values. The number of plausible values is always equal to the number of 
#' cases used to fit the IRT model in the first place.
#'
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata,"2")
#' MirtUniPVs(mirt1)
#' }
#' @export
MirtUniPVs=function(mirtobj){
  LogLik1=MirtLogLikMatrix(mirtobj)
  pvs=sapply(1:nrow(LogLik1$Posterior)
             ,function(i) sample(mirtobj@Model$Theta,1,prob=LogLik1$Posterior[i,]))
  #add a little uniform error around the PVs so they're not all on top of each other
  plusminus=mean(stats::filter(mirtobj@Model$Theta,c(1,-1)),na.rm=TRUE)/2
  pvs=pvs+stats::runif(length(pvs),-plusminus,+plusminus)
  return(pvs)
}

