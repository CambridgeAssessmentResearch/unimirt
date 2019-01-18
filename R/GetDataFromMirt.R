#' Function to extract the (non-fake) item data set from a mirt object
#' 
#' Extracts the data frame used for model fitting from within a 'mirt' object.
#' This function will automatically remove any rows of fake data added by the augmentdata function.
#' 
#' @param mirtobj An estimated IRT model (of class SingleGroupClass) estimated either using the function "unimirt"
#' or by applying the function "mirt" directly.
#' 
#' @return A matrix of item data.
#'
#' @examples
#' \dontrun{
#' smallmirt=unimirt(mathsdata[1:10,15:20],"Rasch")
#' smallmirt@Data$data
#' GetDataFromMirt(smallmirt)
#' }
#' @export
GetDataFromMirt=function(mirtobj){
  if(!"fakedata"%in%names(attributes(mirtobj))){return(mirtobj@Data$data)}
  return(mirtobj@Data$data[attr(mirtobj,"fakedata")==FALSE,])
}
