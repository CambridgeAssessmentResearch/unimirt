#' Wrapper for "mirt" to facilitate fitting unidimensional models of different types.
#' 
#' Function to fit a unidimensional IRT model. This function is actually just a wrapper for \link[mirt]{mirt}.
#' The main differences with mirt are a change to the default behaviour for handling missing categories within
#' polytomous items and a slightly more convenient way of fitting a mix of 3 parameter and graded response models
#' for datasets including a mix of dichotomous and polytomous items.
#' 
#' @param data A data frame of item data.
#' @param short.type A character string indicating the type of model to fit. "2" (the default) 
#' means that a two-parameter graded response IRT model will be used for al items. "1" also leads to a graded response
#' model but with the restriction that all items are constrained to have the same slope.
#' "3" will lead to a 3-parameter IRT model being fitted to all dichotmous items and a graded response model
#' to all polytomous items. "Rasch" will fit a Rasch/Partial Credit Model to all items.
#' @param augment.data Should missing categories in items be handled by adding cases to the data (default TRUE). Provided
#' the overall data set is reasonably large this should have minimal impact on estimated item parameters.
#' @param ... Other parameters to be fed to the function \link[mirt]{mirt}. For example including the option "SE=TRUE" will allow
#' standard errors to be calculated. Similarly including (for example) the option 'dentype="Davidian-5"' will allow
#' the model to be estimated with a non-normal ability distribution.
#'
#' @examples
#' \dontrun{
#' #two-parameter graded response model
#' mirt1=unimirt(mathsdata,"2")
#' coef(mirt1)
#' MirtTidyCoef(mirt1)
#' 
#' #two-parameter graded response model for polytomous items
#' #and three-parameter model for dichotmous items
#' mirt3=unimirt(mathsdata,"3")
#' coef(mirt3)
#' MirtTidyCoef(mirt3)
#' 
#' #Rasch/partial credit model
#' mirtRasch=unimirt(mathsdata,"Rasch")
#' coef(mirtRasch)
#' MirtTidyCoef(mirtRasch)
#' 
#' }
#' @import mirt
#' @export
unimirt=function(data,short.type="2",augment.data=TRUE,...){

  if(augment.data==TRUE){data=augmentdata(data)}
  if(augment.data==FALSE){
    data=data.frame(data)
    attr(data,"fakedata")=rep(FALSE,nrow(data))
  }

  nite=ncol(data)
  if(short.type=="Rasch"){itemtype="Rasch"}
  if(short.type%in%c("1","2")){itemtype="graded"}
  if(short.type=="3"){itemtype=rep("graded",nite)
  maxes=apply(data,2,max,na.rm=TRUE)
  itemtype[maxes=="1"]="3PL"
  }
  
  if(short.type!="1"){mirt1=mirt(data,1,itemtype=itemtype,...)}
  
  if(short.type=="1"){
    mirt0=mirt(data,1,itemtype=itemtype,pars="values")
    slopepars=mirt0$parnum[mirt0$name=="a1"]
    mirt1=mirt(data,1,itemtype=itemtype,constrain=list(slopepars),...)
  }

  attr(mirt1,"fakedata")=attr(data,"fakedata")  
  return(mirt1)
}
