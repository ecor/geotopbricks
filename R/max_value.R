NULL
#'
#' Gets the maximum (scalar) values of a \code{\link{GeotopRasterBrick}} object 
#' 
#' @param x a \code{\link{GeotopRasterBrick}} object. 
#' @param na.rm,... further arguments for \code{\link{max}}.
#' 
#' 
#' @return the maximum (scalar) values of a \code{\link{GeotopRasterBrick}} object
#' 
#' 
#' 
#' @title max_value
#' @name max_value
#' @rdname max_value
#' @export 
#'

max_value <- function(x,na.rm=TRUE,...) {
	
	out <- max(maxValue(brick(x)),na.rm=na.rm,...)
	
	return (out) 
	
}


