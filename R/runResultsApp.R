#' Launch shiny app to view IRT results
#' 
#' This function launches a shiny app where IRT objects estimated using "mirt" can be selected and results viewed.
#' 
#' @param interactive.data If TRUE launch a version of the App that allows loading data and model fitting also to be interactive.
#' Otherwise (default) the Shiny App is purely concerned with viewing results for IRT models
#' that have already been fitted using R code.Only the WLE=FALSE version is available.
#' 
#' @param WLE If TRUE launch a version of the App (with interactive.data=FALSE)
#' where all calculations using ability estimates (e.g. fit statistics) are based upon
#' WLE estimates rather than plausible values (the default). The WLE version tends to run a little more
#' slowly but may be of interest to some users.
#' 
#' @examples
#' \dontrun{
#' runResultsApp()
#' }
#' @import shiny
#' @export
runResultsApp <- function(interactive.data=FALSE,WLE=FALSE) {
  appDir <- system.file("shiny-examples", package = "unimirt")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing package.", call. = FALSE)
  }

  if(interactive.data==TRUE){
    shiny::runApp(paste(appDir,"/ItemFitAppV4.R",sep=""), display.mode = "normal")
  }
  if(interactive.data==FALSE & WLE==FALSE){
    shiny::runApp(paste(appDir,"/ItemFitAppV2.R",sep=""), display.mode = "normal")
  }  
  if(interactive.data==FALSE & WLE==TRUE){
    shiny::runApp(paste(appDir,"/ItemFitAppV3.R",sep=""), display.mode = "normal")
  }
}
