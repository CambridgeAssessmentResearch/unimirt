#' Convert IRT model fitted using fit_enorm (from dexter package) to a mirt object
#' 
#' This function is designed to allow users to fit a Rasch model using the
#' conditional maximum likelihood (CML) procedure but to still have access to all of the interactive
#' graphical displays and other functions that are available in unimirt.
#' 
#' Note that model objects derived using the fit_enorm function do not retain the full data set.
#' As a result, the original data needs to be supplied as a second parameter to the conversion function.
#' 
#' @param dexter.obj An object  derived using the function fit_enorm from the R package dexter.
#' @param dexter.dat The data matrix used to fit the model held in dexter.obj.
#' 
#' @examples
#' \dontrun{
#' raschcml=dexter::fit_enorm(as.matrix(mathsdata[,1:5]))
#' coef(raschcml)
#' mirtfromcml=dexter.to.unimirt(raschcml,as.matrix(mathsdata[,1:5]))
#' MirtTidyCoef(mirtfromcml)
#' }
#' @export
dexter.to.unimirt <- function (dexter.obj=NULL, dexter.dat=NULL) 
{
  # general catch for misspecified argument
  if(is.null(dexter.obj)||!"prms" %in% class(dexter.obj)){
    stop("An object of class prms must be provided")
  }
  
  # if data supplied as dexter database, convert to response matrix
  if(class(dexter.dat)=="SQLiteConnection"){
    dexter.dat <- dexter::get_resp_matrix(dexter.dat)
  }
  
  # check that response data corresponds to model data
  if(!all(colnames(dexter.dat) %in% as.character(dexter.obj$inputs$design$item_id))){
    stop("dexter.obj and dexter.dat must have the same items")
  }
  
  # rearrange to ensure match between model and data
  dex.coefs <- coef(dexter.obj)
  dex.coefs <- dex.coefs[order(dex.coefs$item_id, dex.coefs$item_score),]
  
  dexter.dat <- dexter.dat[,order(colnames(dexter.dat))]
  
  if(all(c("item_id", "item_score", "beta") %in% names(dex.coefs))){
    item_scores <- paste0("Cat", dex.coefs$item_score)
    dexlabs = paste(dex.coefs$item_id, item_scores, sep="_")
    betapars = dex.coefs$beta
  } else {
    # error if key values not available
    stop("coefs(dexter.obj) must provide item_id, item_score and beta")
  } 
  
  
  stop1 = regexpr("_Cat", dexlabs)
  dexites = dexlabs
  if (max(stop1) > 0) {
    dexites = substr(dexlabs, 1, stop1 - 1)
  }
  dexds = NA + betapars
  for (x in unique(dexites)) {
    dexds[dexites == x] = -cumsum(betapars[dexites == x])
  }
  
  mA = mirt::mirt(dexter.dat, 1, "Rasch", pars = "values")
  mA$value[mA$name %in% c("d", paste0("d", 1:1000))] = dexds
  mA$est = FALSE
  mA$est[mA$class == "GroupPars"] = TRUE
  mB = mirt::mirt(dexter.dat, 1, "Rasch", pars = mA)
  return(mB)
}