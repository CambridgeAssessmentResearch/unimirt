#' Convert IRT model fitted using tam.jml (from TAM package) to a mirt object
#' 
#' This function is designed to allow users to fit a Rasch model using the
#' joint maximum likelihood (JML) procedure but to still have access to all of the interactive
#' graphical displays and other functions that are available in unimirt.
#' 
#' @param tamjml .
#' 
#' @examples
#' \dontrun{
#' tamjml=TAM::tam.jml(mathsdata[,1:5])
#' tamjml$item
#' mirtfromjml=tam.jml.to.unimirt(tamjml)
#' MirtTidyCoef(mirtfromjml)
#' }
#' @export
tam.jml.to.unimirt <- function (tamjml=NULL) 
{
  # general catch for misspecified argument
  if(is.null(tamjml)||class(tamjml)!="tam.jml"){
    stop("An object of class tam.jml must be provided")
  }
  
  # if statement used to catch change in tam.jml structure from TAM v3.5 to v3.6
  if(all(c("xsi.label", "xsi") %in% names(tamjml$item))){
  tamlabs = as.character(tamjml$item$xsi.label)
  xsi = tamjml$item$xsi
  } else {
    if(all(c("xsi.label", "xsi") %in% names(tamjml$item1))){
      tamlabs = as.character(tamjml$item1$xsi.label)
      xsi = tamjml$item1$xsi
    } else {
      # error just in case TAM changes structure again
      stop("tam.jml object must provide xsi and xsi.label")
    } 
  }
  
  stop1 = regexpr("_Cat", tamlabs)
  tamites = tamlabs
  if (max(stop1) > 0) {
    tamites = substr(tamlabs, 1, stop1 - 1)
  }
  tamds = NA + xsi
  for (i in unique(tamites)) {
    tamds[tamites == i] = -cumsum(xsi[tamites == i])
  }
  tamdata = tamjml$resp
  tamdata[tamjml$resp.ind == 0] = NA
  mA = mirt::mirt(tamdata, 1, "Rasch", pars = "values")
  mA$value[mA$name %in% c("d", paste0("d", 1:1000))] = tamds
  mA$est = FALSE
  mA$est[mA$class == "GroupPars"] = TRUE
  mB = mirt::mirt(tamdata, 1, "Rasch", pars = mA)
  return(mB)
}