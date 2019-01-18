#' Extract single group mirt object from a multiple group one
#' 
#' This is function is essentially a wrapper for the function \link[mirt]{extract.group} from the mirt package but
#' ensures that extracted object retains a few pieces of information that are need by most of the interactive apps.
#' It may be useful when IRT models have been fitted using multiple group methods
#' (perhaps to help impose various contraints on parameters across groups)
#' but we wish to be able to interactively view the results within particular groups.
#' 
#' @param multgroupobj An object of returned from \link[mirt]{multipleGroup}.
#' @param group a number signifying which group should be extracted.
#' 
#' @examples
#' \dontrun{
#' groups=c(rep("1",200),rep("2",200))
#' mg1=multipleGroup(mathsdata[1:400,1:10],1,groups)
#' mirt1=extractGroup(mg1,1)
#' mirt2=extractGroup(mg1,2)
#' runResultsApp()
#' }
#' @export
extractGroup=function(multgroupobj,group){
	outmirt=extract.group(multgroupobj,group)
	outmirt@Model$Theta=multgroupobj@Model$Theta
	outmirt@Internals$Prior[[1]]=multgroupobj@Internals$Prior[[group]]
	return(outmirt)
	}
