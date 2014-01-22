NULL 

#'
#' Graphic Representation of a Color legend of a Raster or GeotopbrickRaster object as a Color bar, inspired by the function written by John Colby
#' 
#' @param x a Rster or GeotopRasterBrick object
#' @param col the color palette used 
#' @param ...  arguments to be passed to \code{\link{color.bar}}
#' 
#' @seealso \code{\link{color.bar}}
#' @export 
#' 
color.bar.raster <- function(x,col,...) {
	
	if (class(x)=="GeotopRasterBrick") {
	
		min <- min_value(x)
		max <- max_value(x)
	
	} else {
				
		min <- min(minValue(x),na.rm=TRUE)	
		max <- max(maxValue(x),na.rm=TRUE)
		
	}
	
	color.bar(lut=col,min=min,max=max,...)
	
	# TO DO 
}