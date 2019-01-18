#' Launch shiny app to compare estimated IRT objects
#' 
#' This function launches a shiny app where allowing a comparison of item parameters and expected total scores on the common items between different estimatyed mirt objects.
#' 
#' @examples
#' \dontrun{
#' runMirtCompareApp()
#' }
#' @import shiny
#' @export
runMirtCompareApp <- function() {
  appDir <- system.file("shiny-examples", package = "unimirt")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing package.", call. = FALSE)
  }

  shiny::runApp(paste(appDir,"/MirtCompareAppV2.R",sep=""), display.mode = "normal")
}