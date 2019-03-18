#' Select items to maximise the sum of sum particular variable whilst meeting other constraints
#' 
#' This functions provides the basis on which the item selection App works.
#' 
#' @param idata A data frame containing information about all the items.
#' @param maxvar A character string defining the variable for which the sum will be maximised by the choice of items.
#' @param constrvars A character vector denoting the variable on which other contraints may be placed.
#' @param constrmins A numeric vector of the minima that the sums of the "constrvars" must add up to. Must be same length as "constrvars".
#' @param constrmaxs A numeric vector of the maxima that the sums of the "constrvars" must add up to. Must be same length as "constrvars".
#' 
#' @return A vector denoting the indices of the rows of the data frame that were selected.
#'
#' @import ompr ompr.roi ROI ROI.plugin.lpsolve reshape2 dplyr
#' @examples
#' #summary information for 11 items worth 20 marks
#' item.summary.data=data.frame(
#' 	item=1:11
#' 	,Max=c(1,1,1,1,1,2,2,2,2,2,5)
#' 	,Mean=c(0.02,0.23,0.61,0.65,0.56
#' 		,1.34,1.44,1.2,1.07,0.32,3.67)
#' 	,R_rest=c(0.14,0.46,0.59,0.63,0.42
#' 		,0.55,0.53,0.68,0.72,0.49,0.49)
#' 	)
#' #select an optimal 8 mark test (max R_rest per mark)
#' #with mean score between 5 and 6 marks)
#' 
#' choice=ChooseItems(item.summary.data,"R_rest"
#' 	,constrvars = c("Max","Mean")
#' 	,constrmins = c(8,5)
#' 	,constrmaxs = c(8,6))
#' item.summary.data[choice,]
#' #check choices satisfy constraints
#' apply(item.summary.data[choice,],2,sum)
#' @export
ChooseItems=function(idata,maxvar,
                     constrvars=NULL,
                     constrmins=NULL,
                     constrmaxs=NULL){
  
  nite=nrow(idata)
  
  MIPbit=MIPModel()
  MIPbit=add_variable(MIPbit,x[i], i = 1:nite, type = "binary")
  MIPbit=set_objective(MIPbit,sum_expr(idata[i,maxvar] * x[i], i = 1:nite), "max")
  
  if(length(constrvars)>0){
    for (constr in 1:length(constrvars)){
      MIPbit=add_constraint(MIPbit,sum_expr(idata[i,constrvars[constr]] * x[i], i = 1:nite) <= constrmaxs[constr])
      MIPbit=add_constraint(MIPbit,sum_expr(idata[i,constrvars[constr]] * x[i], i = 1:nite) >= constrmins[constr])
    }
  }
  
  MIPbit=solve_model(MIPbit,with_ROI(solver = "lpsolve"))
  MIPbit=get_solution(MIPbit,x[i]) 
  MIPbit=MIPbit[MIPbit$value>0,]
  
  choose=MIPbit$i
  return(choose)
}


