#' Launch interactive shiny app to perform some simple classical equating
#' 
#' This function launches a shiny app where ".csv" files of data can be uploaded and analysed interactively.
#' It is nothing to do with IRT and is only including to support a wider training course.
#' 
#' @examples
#' \dontrun{
#' runClassicalEquatingApp()
#' }
#' @import shiny
#' @export
runClassicalEquatingApp <- function() {
  appDir <- system.file("shiny-examples", package = "unimirt")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing package.", call. = FALSE)
  }

    shiny::runApp(paste(appDir,"/ClassicalEquatingAppV4.R",sep=""), display.mode = "normal")
}
