#' Convert Rasch difficulties of many dichotomous items to Partial Credit Model (PCM) thresholds
#' 
#' Given a set of Rasch difficulties from n dichomtomous items, calculate the equivalent thresholds for a single polytomous item
#' (with n+1 categories) that fits the partial credit model (PCM).
#' 
#' @param dich_diffs A vector of Rasch difficulties. The length of the vector represents the number of dichotomous items considered.
#' 
#' @return A vector of thresholds for the partial credit model (PCM).
#'
#' @examples
#' \dontrun{
#' dichotomous.to.pcm(c(0,0))
#' pcm.to.dichotomous(c(-0.6931472,0.6931472))
#' 
#' dichotomous.to.pcm(c(-2,-4,1))
#' pcm.to.dichotomous(c(-4.132845,-1.922140,1.054985))
#' 
#' dichotomous.to.pcm(c(-1,-1,1,1))
#' pcm.to.dichotomous(c(-1.8200752,-0.6243906,0.6243906,1.8200752))
#' }
#' @export
dichotomous.to.pcm <- function (dich_diffs) 
{
  nite=length(dich_diffs)
  Cs=sapply(1:nite,function(i) 
	-log(sum(exp(colSums(-utils::combn(dich_diffs, i)))))
	)
  #now calculate thresholds
  taus=rep(NA,nite)
  for(thresh in 1:nite){
  	if(thresh==1){taus[thresh]=Cs[thresh]}
  	if(thresh>1){taus[thresh]=Cs[thresh]-sum(taus[1:(thresh-1)])}
  }
 return(taus)  
}

#' Convert Partial Credit Model (PCM) thresholds to equivalent set of n Rasch difficulties from dichotomous items
#' 
#' Given a single polytomous item with n thresholds, calculate an equivalent set of Rasch difficulties from n dichomtomous items. 
#' The aim is to provide a different way of interpreting the thresholds from a partial credit model by seeing 
#' what set of difficulties from dichotomous items would lead to the same result.
#' 
#' Note that, in practice, this function rarely works in the way we might hope.
#' Except in special circumstances (effectively when thresholds are correctly ordered and widely spaced), 
#' it is likely that some (or all) of the identified Rasch difficulties will be imaginary numbers.
#' If a mix of imaginary and real numbers are returned in may be that the 
#' imaginary ones can be combined to create PCM thresholds for an item with fewer categories than started with.
#' I have not investigated this fully.
#' 
#' Method here is based on numerically solving the equation for Fn(x) on page 115 of 
#' Huynh, H. (1994). On equivalence between a partial credit item and a set of independent Rasch binary items. Psychometrika, 59(1), 111-119.
#' (see https://doi.org/10.1007/BF02294270). We use the expressions directly in terms of Si (above equation 6
#' in the paper) rather than those in terms of ai.
#'
#' If the solution contains any non-zero imaginary parts it implies that an exactly equivalent 
#' set of dichotomoues difficulties cannot be found.
#' In other words, the polytomous item cannot be considered as being the sum of multiple independent dichotomous items.
#' 
#' @param pcm_thresh A vector of thresholds from the partial credit model.
#' 
#' @return A data.frame with the real and imaginary parts of the equivalent dichotomous Rasch difficulties.
#' Very small imaginary numbers are rounded down to zero as they are considered to likely be part of estimation error.
#'
#' @examples
#' \dontrun{
#' 
#' dichotomous.to.pcm(c(0,0))
#' pcm.to.dichotomous(c(-0.6931472,0.6931472))
#' 
#' dichotomous.to.pcm(c(-2,-4,1))
#' pcm.to.dichotomous(c(-4.132845,-1.922140,1.054985))
#' 
#' dichotomous.to.pcm(c(-1,-1,1,1))
#' pcm.to.dichotomous(c(-1.8200752,-0.6243906,0.6243906,1.8200752))
#' 
#' dich_diffs=pcm.to.dichotomous(c(-2,0,2,3.4))
#' dich_diffs
#' dichotomous.to.pcm(dich_diffs$real_part)
#' 
#' #and one that doesn't work (disordered thresholds)
#' pcm.to.dichotomous(c(1,0,3))
#' #investigate PCM thresholds for the imaginary bit
#' dichotomous.to.pcm(c(0.5228831-1.301217i,0.5228831+1.301217i))
#' #so equivalent to a 2-category PCM with disordered thresholds and a dichotomous item
#' 
#' }
#' @export
pcm.to.dichotomous<- function (pcm_thresh) 
{
  Ss=exp(-cumsum(pcm_thresh))
  n=length(pcm_thresh)
  polysigns=(-1)^(1:n)
  polycoefs=polysigns*Ss  

  #solutions found numerically
  #use rounding to ignore tiny imaginary elements of numbers
  roots=log(polyroot(c(1,polycoefs)))
  real_roots=Re(roots)
  im_roots=Im(roots)
  is_real=(abs(im_roots)<0.00001)+0
  im_roots[is_real==1]=0
  n_real=sum(is_real)
  dich_difficulties=data.frame(real_part=real_roots,imaginary_part=im_roots)
  dich_difficulties=dich_difficulties[order(dich_difficulties$real_part),]
  return(dich_difficulties)  
}

