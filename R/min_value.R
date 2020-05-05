NULL
#'
#' Gets the minimum (scalar) values of a \code{\link{GeotopRasterBrick}} object 
#' 
#' @param x a \code{\link{GeotopRasterBrick}} object. 
#' @param na.rm,... further arguments foe \code{\link{min}}.
#' 
#' 
#' @return the minimum (scalar) values of a \code{\link{GeotopRasterBrick}} object
#' 
#' 
#' 
#' @title min_value
#' @name min_value
#' @rdname min_value
#' @export 
#'

min_value <- function(x,na.rm=TRUE,...) {
	
	out <- min(minValue(brick(x)),na.rm=na.rm,...)
	
	return (out) 
	
}


