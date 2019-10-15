#' Launch interactive shiny app to perform some simple classical item analyses
#' 
#' This function launches a shiny app where ".csv" files of data can be uploaded and analysed.
#' It assumes that all pupils have had a chance to attempt all items.
#' 
#' @param MC If TRUE launch a version of the App designed for analysis of multiple choice tests.
#' The only difference is that converting item responses (e.g. "A", "B", "C", "D") to scores
#' is incorporated in the App and that additional item characteristic curves (ICCs) exploring
#' the percentage of pupils choosing each category in each score group are included.
#' 
#' @examples
#' \dontrun{
#' runClassicalApp()
#' 
#' #To launch the version designed for multiple choice tests
#' runClassicalApp(TRUE)
#' }
#' @import shiny
#' @export
runClassicalApp <- function(MC=FALSE) {
  appDir <- system.file("shiny-examples", package = "unimirt")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing package.", call. = FALSE)
  }

  if(MC==TRUE){
    shiny::runApp(paste(appDir,"/ClassicalAppV4MC.R",sep=""), display.mode = "normal")
  }
  if(MC==FALSE){
    shiny::runApp(paste(appDir,"/ClassicalAppV4.R",sep=""), display.mode = "normal")
  }
}
