#' Function to calculate conditional standard error of measurement at each true score
#' 
#' Calculates the conditional standard error of measurement at each unit true score
#' on the test (totalled across a given selection of items).
#' This is defined as the standard deviation of observed scores given the true score.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}. 
#' Must be fitted using the (default) "EM" method. Must be unidimensional.
#' @param which.items A vector of denoting which items should be included in calculating the score. 
#'
#' @return The function returns a data.frame with columns: true.score (a vector of possible integer true scores) 
#' , and csem (the standard deviation of obsereved scores for candidates with the given true score).
#'
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata,"2")
#' CSEMfromMirt(mirt1)
#' }
#' @export
CSEMfromMirt=
  function (mirtobj, which.items = NULL) 
  {

#mirtobj=unimirt(mathsdata,TOL=0.1);which.items = 11:30

coefs = coef(mirtobj, simplify = TRUE)$items
    nites = dim(coefs)[1]
    if (is.null(which.items)) {
      which.items = 1:nites
    }

thetas=TCClookup(mirtobj,fulliterative=TRUE,which.items=which.items)
thetas=thetas$TCCabil
thetas[1]=-Inf#Theta for zero score is minus infinity
thetas[length(thetas)]=Inf#Theta for full score is infinity
thetas=as.matrix(thetas)
nqpts = dim(thetas)[1]

maxes=extract.mirt(mirtobj,"K")-1
papermax=sum(maxes[which.items])

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

csemout=data.frame(true.score=round(colSums(paperps*(0:papermax)),2)
	,csem=sqrt(abs(colSums(paperps*((0:papermax)^2))-colSums(paperps*(0:papermax))^2))
	)
#csemout

#ggplot(data=csemout,aes(x=true.score,y=csem))+geom_line()

return(csemout)

  }


#library(unimirt)
#mirt1=unimirt(mathsdata,"2")
#csemtab=CSEMfromMirt(mirt1,11:30)
#ggplot(data=csemtab,aes(x=true.score,y=csem))+geom_line()
