#' Launch shiny app to facilitate item selection
#' 
#' This function launches a shiny app where allowing item selection based upon information estimated using IRT and stored in "mirt" objects. Supplementary information may also be uploaded to the app to support item selection.
#' 
#' @examples
#' \dontrun{
#' runItemSelectionApp()
#' }
#' @import shiny
#' @export
runItemSelectionApp <- function() {
  appDir <- system.file("shiny-examples", package = "unimirt")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing package.", call. = FALSE)
  }

  shiny::runApp(paste(appDir,"/IRTitemSelectionApp.R",sep=""), display.mode = "normal")
}