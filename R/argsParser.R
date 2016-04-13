NULL

#'Parser of an argument string
#' 
#' This command parses ...DESCRITION TO DO !!!
#' 
#' 
#' @param option character strings containg options (or flag) whose values 
#' @param args String containing all the arguments of an R script
#' @param sep separator character. Default is \code{" "}. If it is of length 2, the first is seperator among different options, the second is betwwen option name and its value.
#' @param novalue_response value used in case the option is missing. Default is \code{NULL}. 
#' 
#' 
#' 
#' 
#' 
#' @export
#' @importFrom stringr str_split 
#' @importFrom stringr str_trim  
#' 
#' @examples
#' 
#' args <- "--value  6  --fruit apple"
#' 
#' option <- "--fruit"
#' 
#' value <- argsParser(option=option,args=args)
#' 
#' option2 <- "--jobs"
#' 
#' value2 <- argsParser(option=option2,args=args)
#' value22 <- argsParser(option=option2,args=args,novalue_response="./")
#'  args_b <- "value=6 , fruit=apple"
#' 
#' value <- argsParser(option=option,args=args_b,sep=c(",","="))
#' 


argsParser <- function(option,args,sep=" ",novalue_response=NULL) {
	
	
	if (is.null(sep)) sep=""
	if (any(is.na(sep))) sep=""
	
	if (length(sep)==1) sep <- rep(sep,2)
	
	if (length(args)>1) args <- paste(args,collapse=sep[1])
	
	args <- str_split(args,sep[1])[[1]]
	args <- args[args!=""]
	args <- str_trim(args)
	
	
	if (is.logical(novalue_response)) {
	
		
        out <- (option %in% args) | novalue_response
		
	} else if (sep[2]!=sep[1]) {
		
		args_s <- str_split(args,sep[2],n=1)
		
		args <- lapply(X=args_s,FUN=function(x){x[2]})
		names(args) <- lapply(X=args_s,FUN=function(x){x[1]})
		args <- str_trim(args)
		names(args) <- str_trim(names(args))
		option <- str_trim(option)
		
		out <- args[option]
		
		if (is.na(out)) out <- novalue_response
	} else {
		
		args_s <- args
		
		index <- which(args_s %in% option) ###unique(ceiling(1:length(args_s)/2)*2-1)
		
		if (length(index)>0) {
			args <- args_s[index+1]
			names(args) <- args_s[index]
			out <- args
		} else {
			
			out <- novalue_response
		}
		
	}
	
	
	
	
	
	
	if (length(out)<1) out <- novalue_response
	
	
#	print(out)
#	
#	if (!is.null(out) & !is.null(novalue_response)) {
#		out[is.na(out)] <- novalue_response
#		
#	} else if (is.na(out)) {
#		
#		out <- novalue_response
#		
#	}
	
	
	
	
	return(out)
}