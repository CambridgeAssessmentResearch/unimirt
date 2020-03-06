#' Launch shiny app to facilitate item selection
#' 
#' This function launches a shiny app where allowing item selection based upon information estimated using IRT and stored in "mirt" objects. Supplementary information may also be uploaded to the app to support item selection.
#' 
#' @param interactive.upload If TRUE launch a version of the App that allows loading previously fitted IRT models interactively.
#' Otherwise (default) the Shiny App is purely concerned with viewing results for IRT models
#' that have already been fitted using R code.
#' 
#' @examples
#' \dontrun{
#' runItemSelectionApp()
#' }
#' @import shiny
#' @export
runItemSelectionApp <- function(interactive.upload=FALSE) {
  appDir <- system.file("shiny-examples", package = "unimirt")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing package.", call. = FALSE)
  }

  if(interactive.upload==TRUE){
    shiny::runApp(paste(appDir,"/IRTitemSelectionAppV4.R",sep=""), display.mode = "normal")
  }
  if(interactive.upload==FALSE){
    shiny::runApp(paste(appDir,"/IRTitemSelectionApp.R",sep=""), display.mode = "normal")
  }  

  }