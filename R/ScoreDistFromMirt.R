#' Function to estimate sum score distribution for a selection of items in a mirt object
#' 
#' This function can be used to help facilitate observed score IRT equating.
#' Early trials suggest this function works best with unidimensional models.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}. 
#' Must be fitted using the (default) "EM" method. Will work with any form of model (graded response, Rasch,...). Will even work
#' with multidimensional models but may be innaccurate unless the object incorporates a large number of quadrature points.
#' @param which.items A vector of denoting which items should be included in calculating the score. 
#' @param theta.mean.sd A vector of length 2 giving the mean and standard deviation of the ability distribution. 
#' If not supplied then these are derived directly from the mirt object. This parameter may only be used for unidimensional models.
#'
#' @return The function returns a data.frame with columns: score (a vector of possible scores) 
#' ,prob (the proportion of candidates expected to achieve each scores), expectedtheta (expected ability given the
#' total score), and sd theta the standard deviation of abilities for candidates with the given total score.
#'
#' @examples
#' \dontrun{
#' mirt1=unimirt(mathsdata,"2")
#' ScoreDistFromMirt(mirt1)
#' }
#' @export
ScoreDistFromMirt=
  function (mirtobj, which.items = NULL, theta.mean.sd = NULL) 
  {
    #mirtobj=mirtGRM;which.items = NULL;theta.mean.sd = NULL
    
    if (is.null(theta.mean.sd)) {
      thetas = mirtobj@Model$Theta
      qwts = mirtobj@Internals$Prior[[1]]
      if (length(qwts) > length(thetas)) {
        qwts = mirtobj@Internals$Prior[[1]][1, ]
      }
    }
    if (!is.null(theta.mean.sd)) {
      thetas = seq(theta.mean.sd[1] - 6 * theta.mean.sd[2], 
                   theta.mean.sd[1] + 6 * theta.mean.sd[2], len = 201)
      qwts = stats::dnorm(thetas, theta.mean.sd[1], theta.mean.sd[2])
      qwts = qwts/sum(qwts)
      thetas = as.matrix(thetas)
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
    ovdist = data.frame(score = 0:papermax, prob = rowSums(t(t(paperps) * 
                                                               qwts)))
    
    #add in expected theta and sd of theta for each raw score
    thetaps=t(paperps)*qwts
    thetaps=t(thetaps)/colSums(thetaps)
    expectedtheta=colSums(t(thetaps)*thetas[,1])
    sdtheta=sqrt(colSums(t(thetaps)*thetas[,1]*thetas[,1])-expectedtheta^2)
    
    ovdist$expectedtheta=expectedtheta
    ovdist$sdtheta=sdtheta
    return(ovdist)
  }