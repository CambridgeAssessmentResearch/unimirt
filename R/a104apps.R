#' Pick an interative shiny app to launch
#' 
#' Running this function produces a simple menu. Enter the number corresponding to the intercative app you want to launch.
#' 
#' @examples
#' \dontrun{
#' a104apps()
#' }
#' @import shiny
#' @export
a104apps <- function() {
  choices=c("Classical Item Analysis"
            ,"Classical Equating"
            ,"IRT model fitting"
            ,"IRT model comparison and scaling"
            ,"IRT test construction")
  
  choice=utils::menu(choices,title="Enter a number corresponding to the App you want to launch.")
  if(choice==1){runClassicalApp()}
  if(choice==2){runClassicalEquatingApp()}
  if(choice==3){runResultsApp(TRUE)}
  if(choice==4){runMirtCompareApp(TRUE)}
  if(choice==5){runItemSelectionApp(TRUE)}
  return(choice)
  }
