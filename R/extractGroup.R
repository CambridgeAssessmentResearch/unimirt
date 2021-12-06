#' Extract single group mirt object from a multiple group one
#' 
#' This is function is essentially a wrapper for the function \link[mirt]{extract.group} from the mirt package but
#' ensures that extracted object retains a few pieces of information that are need by most of the interactive apps.
#' It may be useful when IRT models have been fitted using multiple group methods
#' (perhaps to help impose various constraints on parameters across groups)
#' but we wish to be able to interactively view the results within particular groups.
#' 
#' @param multgroupobj An object of returned from \link[mirt]{multipleGroup}.
#' @param group A character string specifying the name of the group to be
#'   extracted (e.g., "group1"), or a numeric value specifying the index of the
#'   group to be extracted (i.e., 1 will extract the first group in the object).
#' 
#' @examples
#' \dontrun{
#' groups=c(rep("grp1",200),rep("grp2",200))
#' mg1=multipleGroup(mathsdata[1:400,1:10],1,groups)
#'
#' # Extract group 1 using character string naming group
#' mirt1=extractGroup(mg1,"grp1")
#' 
#' # Extract group 2 using numeric index
#' mirt2=extractGroup(mg1,2)
#' 
#' runResultsApp()
#' }
#' @export
extractGroup=function(multgroupobj,group){
  
  if (missing(group)){
    stop("Must specify group number or name")
  }
  
  stopifnot("only one group can be extracted"=length(group) == 1L)
  
  groupNames <- extract.mirt(multgroupobj, "groupNames")
  if (is.character(group)) {
    stopifnot("specified group is not present in mirt object supplied"=any(group == groupNames))
    group <- which(group == groupNames)
  }
  
	outmirt=extract.group(multgroupobj,group)
	outmirt@Model$Theta=multgroupobj@Model$Theta
	outmirt@Internals$Prior[[1]]=multgroupobj@Internals$Prior[[group]]
	return(outmirt)
	}
