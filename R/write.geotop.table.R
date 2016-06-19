NULL
#' 
#' Writes an R object (\code{data.frame} or \code{zoo}) into a CSV file readible by GEOtop.
#' 
#' @param x R object (\code{data.frame} or \code{zoo}) to be exported and written.
#' @param file filename
#' @param wpath working path to the GEOtop simlation. If \code{wpath} is not \code{NULL} , \code{filename} will be put in \code{wpath}.
#' @param tz time zone. Default is \code{"Etc/GMT-1"}.  See \code{\link{get.geotop.inpts.keyword.value}} fur further details.
#' @param date_field  string used for date-time field. Deafult is \code{"Date12.DDMMYYYYhhmm."}. See \code{\link{get.geotop.inpts.keyword.value}} fur further details.
#' @param file_end  suffix of the file name (\code{file}) (optional). Default is \code{""}.
#' @param format date time format. Default is \code{"\%d/\%m/\%Y \%H:\%M"}. See \code{\link{get.geotop.inpts.keyword.value}} fur further details.
#' @param sep separator character. Default is \code{","}. See \code{\link{write.table}} fur further details.
#' @param na string for unassigned values. Defaults is \code{"-9999"}.  See \code{\link{write.table}} fur further details.
#' @param ... further arguments for \code{\link{write.table}}.
#' 
#' @importFrom utils write.table
#' 
#' @export
#' 

write.geotop.table <- function(x,file,wpath=NULL,tz = "Etc/GMT-1",date_field="Date12.DDMMYYYYhhmm.",
		file_end = "",sep=",",format="%d/%m/%Y %H:%M", na = "-9999",...) {
	
	if (class(x)=="zoo") {
	    time <- as.POSIXct(index(x))
	    time <- as.POSIXlt(time,tz=tz)
		x <- as.data.frame(x)
		nn <- names(x)
		x[,date_field] <- as.character(time,format=format)
		x <- x[,c(date_field,nn)]
	}
	
	if (!is.null(wpath)) file <- paste(wpath,file,sep="/")
	file <- paste(file,file_end,sep="")
	
	
	out <- write.table(x=x,file=file,row.names=FALSE,sep=sep,quote=FALSE,na=na,...)
	
	
	return(out)
	## TO DO ON MONDAY!!!!
	
	
}
