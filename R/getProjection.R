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
#' 
#' wpath <- 'https://raw.githubusercontent.com/ecor/geotopbricks_doc/master/simulations/idroclim_test1'
#' ## URL path  (RAW VERSION) of 
#' ## https://github.com/ecor/geotopbricks_doc/tree/master/simulations/idroclim_test1
#' \dontrun{
#' 
#' x <- paste(wpath,"geotop.proj",sep="/")
#' 
#' 
#' crs <- getProjection(x)
#' }

getProjection <- function(x,cond=TRUE,...) {
	
	out <- NA
	
	open <- FALSE
	if (cond) {
		
	
		out <- as.character(scan(x,what="list",sep="\n",n=1))
		

		
	}
	
	
	return(out)
}
