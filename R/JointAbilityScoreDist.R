#' Create joint distribution of true scores (i.e., theta) and observed scores
#' 
#' Calculate the probability of each combination of true and observed scores.
#' Note that, since we are dealing with unidimensional models, there is a one-to-one
#' correspondence between ability values and true scores.
#' 
#' As well as the joint distribution, the output also provides the marginal distribution
#' of true scores and raw scores derived from on the IRT model.
#' Marginal distributions of true scores are purely based on the weights provided for each 
#' quadrature point in model fitting.
#' 
#' Conditional distributions of true scores (i.e., the quadrature points) based on raw scores
#' and raw score based on true scores are also provided
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using \link[mirt]{mirt} or \link[unimirt]{unimirt}. 
#' Must be fitted using the (default) "EM" method. Must be unidimensional.
#' @param which.items A vector of denoting which items should be included in calculating the total raw score. (All items included by default). 
#' @param theta.mean.sd An optional 2-value vector giving a mean and standard deviation (SD) of ability.  
#' If supplied, this will be used to create 200 quadrature points relevant to the defined mean and SD.
#' These will be used instead of the quadrature points in the original IRT model.
#'
#' @return The function returns a data.frame with the following columns
#' \describe{
#'   \item{theta}{Value of abilit}
#'   \item{true_score}{True score associated with value of ability}
#'   \item{raw_score}{Raw (observed) score}
#'   \item{p}{Joint probability of true score (or theta) and raw observed score occuring together}
#'   \item{p_theta}{Marginal distribution of theta (will be duplicated for every occurrence of same theta in data)}
#'   \item{p_raw}{Marginal distribution of raw scores (will be duplicated for every occurrence of same raw score in data)}
#'   \item{p_raw_given_theta}{Conditional distribution of raw score given theta}
#'   \item{p_theta_given_raw}{Conditional distribution of theta given raw score}
#'   \item{e_true}{Expected value of true score given raw score}
#'   }
#'
#' @examples
#' \dontrun{
#' #library(unimirt)
#' mirtRasch=unimirt(mathsdata,"Rasch")
#' jdist=JointAbilityScoreDist(mirtRasch)
#' head(jdist)
#' plot(jdist$raw_score,jdist$p_raw)
#' plot(jdist$true_score,jdist$p_theta)
#' plot(jdist$theta,jdist$true_score)
#' plot(jdist$raw_score,jdist$e_true)+xlim(0,NA)+ylim(0,NA)+geom_abline()
#' }
#' @export
JointAbilityScoreDist=function (mirtobj, which.items = NULL, theta.mean.sd = NULL) {
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
    nites = dim(coefs)[1]
    if (is.null(which.items)) {
        which.items = 1:nites
    }
    maxes = extract.mirt(mirtobj, "K") - 1
    papermax = sum(maxes[which.items])
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

  ntheta=length(qwts)
  jointdist=data.frame(raw_score=rep(0:papermax,each=ntheta)
	        ,theta=rep(as.numeric(thetas),(papermax+1))
	       ,p_theta=rep(qwts,(papermax+1))
       	,p_raw_given_theta=as.numeric(t(paperps))
	)
  jointdist$p=jointdist$p_raw_given_theta*jointdist$p_theta
  true_score=data.frame(theta=sort(unique(jointdist$theta))
	,true_score=sapply(sort(unique(jointdist$theta))
		,function(i) sum(jointdist$raw_score[jointdist$theta==i]*jointdist$p_raw_given_theta[jointdist$theta==i]))
	)
  jointdist=merge(jointdist,true_score,by="theta")

  jointdist$p_raw=sapply(jointdist$raw_score,function(i) sum(jointdist$p[jointdist$raw_score==i]))
  jointdist$p_theta_given_raw=jointdist$p/jointdist$p_raw

  jointdist$e_true=sapply(jointdist$raw_score,function(i) sum(jointdist$p_theta_given_raw[jointdist$raw_score==i]*jointdist$true_score[jointdist$raw_score==i]))
  jointdist=jointdist[,c("theta","true_score","raw_score","p","p_theta","p_raw","p_raw_given_theta","p_theta_given_raw","e_true")]
  return(jointdist)
}

#library(unimirt)
#mirtRasch=unimirt(mathsdata,"Rasch")
#jdist=JointAbilityScoreDist(mirtRasch)
#head(jdist)
#plot(jdist$raw_score,jdist$p_raw)
#plot(jdist$true_score,jdist$p_theta)
#plot(jdist$theta,jdist$true_score)
#qplot(jdist$raw_score,jdist$e_true)+xlim(0,NA)+ylim(0,NA)+geom_abline()
#runResultsApp()
#
#jdist=JointAbilityScoreDist(mirtRasch,1:5)
#head(jdist)
#plot(jdist$raw_score,jdist$p_raw)
#summary(jdist)
#