NULL

#'
#' It reads the CRS metadata utilzed in a GEOtop Simulation
#' 
#' @param x name and full path of the file containimg CRS information 
#' @param cond logical value. If \code{FALSE} the function returns \code{NA}. Default is \code{TRUE}. 
#' @param ... futher arguments
#' 
#' @export
#' @return A string corresponding the projection and CRS if the argument \code{cond} is \code{TRUE}. 
#' @examples 
#' library(geotopbricks)
#' wpath <- "http://www.boussinesq.org/geotopbricks/simulations/idroclim_test1"
#' x <- paste(wpath,"geotop.proj",sep="/")
#' 
#' 
#' crs <- getProjection(x)
#' 

getProjection <- function(x,cond=TRUE,...) {
	
	out <- NA
	
	open <- FALSE
	if (cond) {
		
#		if (str_sub(x,1,3)=='ssh' | str_sub(x,1,5)=='plink') {
#			
#			file <- pipe(x) # added line according to http://stackoverflow.com/posts/2226880/edit
#			open <- TRUE
#		}	else {
#			
#			file <- file(x)
#			ospen <- FALSE
#		}
		file <- file(x)
		out <- as.character(scan(file,what="list",sep="\n",n=1))
		
#		if (open) close(file)
		
	}
	
	
	return(out)
}
