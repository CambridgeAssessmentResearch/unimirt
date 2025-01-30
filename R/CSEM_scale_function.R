#' Calculate the Conditional Standard Error of Measurement (CSEM) of scale scores
#' 
#' The function is used to calculate the CSEM of some conversion of raw scores to a different reporting scale (e.g., coarse grades).
#' 
#' @param jointdist A data.frame giving the joint distribution of true and observed raw scores into the format created by 'JointAbilityScoreDist'.
#' @param my_scale_func A function that can taken any valid vector of raw scores and return the transformed scores on the reporting scale.
#'
#' @return The function returns a data.frame with the following columns
#' \describe{
#'   \item{theta}{Value of abilit}
#'   \item{p_theta}{Marginal distribution of theta}
#'   \item{true_raw}{True score associated with value of ability on raw score scale}
#'   \item{true_scale}{True score associated with value of ability on transformed reporting scale}
#'   \item{csem_raw}{CSEM on scale of raw scores}
#'   \item{csem_scale}{CSEM on scale of transformed scores on the reporting scale}
#'   }
#' @examples
#' \dontrun{
#' #library(unimirt)
#' mirtRasch=unimirt(mathsdata,"Rasch")
#' jdist=JointAbilityScoreDist(mirtRasch)
#' csem1=CSEM_scale_function(jdist,I)#if transformation is just the identity function
#' head(csem1)
#' ggplot(data=csem1,aes(x=true_raw,y=csem_raw))+geom_line()
#' ggplot(data=csem1,aes(x=true_scale,y=csem_scale))+geom_line()
#' 
#' #a more interesting example
#' #imagine raw scores are converted to grades from 0 to 5 according to some cut-scores
#' scale_func=stepfun(x=c(17,33,47,60,72),y=0:5)
#' csem2=CSEM_scale_function(jdist,scale_func)
#' ggplot(data=csem2,aes(x=true_raw,y=csem_raw))+geom_line()
#' ggplot(data=csem2,aes(x=true_raw,y=csem_scale))+geom_line()
#' ggplot(data=csem2,aes(x=true_scale,y=csem_scale))+geom_line()
#' }
#' @export
CSEM_scale_function=function(jointdist,my_scale_func){
  
  jointdist$scale_score=my_scale_func(jointdist$raw_score)
  
  thetas=sort(unique(jointdist$theta))
  true_scale=data.frame(theta=thetas
    ,true_raw=sapply(thetas
        		,function(i) sum(jointdist$raw_score[jointdist$theta==i]*jointdist$p_raw_given_theta[jointdist$theta==i]))
    ,e2_raw=sapply(thetas
        		,function(i) sum((jointdist$raw_score[jointdist$theta==i]^2)*jointdist$p_raw_given_theta[jointdist$theta==i]))
    ,true_scale=sapply(thetas
        		,function(i) sum(jointdist$scale_score[jointdist$theta==i]*jointdist$p_raw_given_theta[jointdist$theta==i]))
    ,e2=sapply(thetas
        		,function(i) sum((jointdist$scale_score[jointdist$theta==i]^2)*jointdist$p_raw_given_theta[jointdist$theta==i]))
    ,p_theta=sapply(thetas
        		,function(i) mean(jointdist$p_theta[jointdist$theta==i]))
    )
  true_scale$csem_raw=sqrt(pmax(0,true_scale$e2_raw-true_scale$true_raw^2))
  true_scale$csem_scale=sqrt(pmax(0,true_scale$e2-true_scale$true_scale^2))
  true_scale=true_scale[,c("theta","p_theta","true_raw","true_scale","csem_raw","csem_scale")]
  return(true_scale)
  }
 
#####################################################################
#####################################################################
######################################################################

#library(unimirt)
#mirtRasch=unimirt(mathsdata,"Rasch")
#jdist=JointAbilityScoreDist(mirtRasch)
#csem1=CSEM_scale_function(jdist,I)
#head(csem1)
#ggplot(data=csem1,aes(x=true_raw,y=csem_raw))+geom_line()
#ggplot(data=csem1,aes(x=true_scale,y=csem_scale))+geom_line()
#
##a more interesting example
##imagine raw scores are converted to grades from 0 to 5 
#scale_func=stepfun(x=c(17,33,47,60,72),y=0:5)
#csem2=CSEM_scale_function(jdist,scale_func)
#ggplot(data=csem2,aes(x=true_raw,y=csem_raw))+geom_line()
#ggplot(data=csem2,aes(x=true_raw,y=csem_scale))+geom_line()
#ggplot(data=csem2,aes(x=true_scale,y=csem_scale))+geom_line()
