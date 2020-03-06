#' Launch shiny app to compare estimated IRT objects
#' 
#' This function launches a shiny app where allowing a comparison of item parameters and expected total scores on the common items between different estimatyed mirt objects.
#' 
#' @param interactive.upload If TRUE launch a version of the App that allows loading previously fitted IRT models interactively.
#' Otherwise (default) the Shiny App is purely concerned with viewing results for IRT models
#' that have already been fitted using R code.
#' 
#' @examples
#' \dontrun{
#' runMirtCompareApp()
#' }
#' @import shiny
#' @export
runMirtCompareApp <- function(interactive.upload=FALSE) {
  appDir <- system.file("shiny-examples", package = "unimirt")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing package.", call. = FALSE)
  }

  if(interactive.upload==FALSE){
  shiny::runApp(paste(appDir,"/MirtCompareAppV2.R",sep=""), display.mode = "normal")
  }

  if(interactive.upload==TRUE){
  shiny::runApp(paste(appDir,"/MirtCompareAppV4.R",sep=""), display.mode = "normal")
  }

}