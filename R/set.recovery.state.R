# TODO: Add comment
# 
# Author: ecor
###############################################################################


NULL

#'
#' This function re-writes the recovery ascii raster maps in a given folder 
#' 
#' 
#' 
#' 
#' 
#' 
#' @author Emanuele Cordano
#' 
#' @param rec a \code{list} object returened by \code{\link{get.geotop.recovery.state}}
#' @param newRecFolder directory where to write all recovery raster asccii maps
#' @param ... further arguments 
#' 
#' @seealso \code{\link{get.geotop.recovery.state}},\code{\link{writeRasterxGEOtop}}
#' @export
#' @examples
#' # See the examples of the 'get.geotop.recovery.state' function


set.geotop.recovery.state <- function(rec,newRecFolder,...) {
		out <- 0 
		
		if (file.exists(newRecFolder)!=TRUE) {
			
			msg <- sprintf("Directory %s does not exists, it must be created before running this function!!",newRecFolder)
			stop(msg)
		}
		if (is.null(rec$glacLayers)) {
			
			###warning("No GlacLayers found, rec created with previous version of geotopbricks: ")
			rec$glacLayers <- array(FALSE,length(rec$names))
		}
		
		names <- rec$names
		ii <- which(names %in% names(rec))
		names <- names[ii]
		NoLayers <- rec$noLayers[ii] 
		
		LayersWithoutZero <-  rec$soilLayers | rec$snowLayers | rec$glacLayers
		LayersWithoutZero <- LayersWithoutZero[ii]
		
		LayersWithZero <- rec$soilLayersWithZero[ii]
		
		
		files_w <- paste(newRecFolder,rec$files,sep="/")

		for (it in names[NoLayers]) {
			
			file  <- files_w[names==it]

			
			geotopbricks::writeRasterxGEOtop(x=rec[[it]],filename=file,use.decimal.formatter=FALSE,start.from.zero=FALSE,...)								
			
		}
		
		
		for (it in names[LayersWithoutZero]) {
			
			file  <- files_w[names==it]
			## print(it) 
			## print(file)
			geotopbricks::writeRasterxGEOtop(x=rec[[it]],filename=file,use.decimal.formatter=TRUE,start.from.zero=FALSE,...)								
			
		}
		
		for (it in names[LayersWithZero]) {
			
			file  <- files_w[names==it]	
			### print(it)
			## print(file)
			
			geotopbricks::writeRasterxGEOtop(x=rec[[it]],filename=file,use.decimal.formatter=TRUE,start.from.zero=TRUE,...)
			
		}		
#	out$names <- names
#	out$files <- files
	
#	out$noLayers <- noLayers
#	out$soilLayersWithZero <- soilLayersWithZero
#	out$soilLayers <- soilLayers
#	out$snowLayers <- snowLayers
	
	
	
	return(out)
	
}
