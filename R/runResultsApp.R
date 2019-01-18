#' Launch shiny app to view IRT results
#' 
#' This function launches a shiny app where IRT objects estimated using "mirt" can be selected and results viewed.
#' 
#' @examples
#' \dontrun{
#' runResultsApp()
#' }
#' @import shiny
#' @export
runResultsApp <- function() {
  appDir <- system.file("shiny-examples", package = "unimirt")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing package.", call. = FALSE)
  }

  shiny::runApp(paste(appDir,"/ItemFitAppV2.R",sep=""), display.mode = "normal")
}