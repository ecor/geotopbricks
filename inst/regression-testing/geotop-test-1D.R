#! /usr/bin/Rscript
# file geotop.test-1D.R
# 
# This file make a regression testin of GEOtop Distributed Hydrological Model source code (1D mode)
# This programs is written in R and make use of "geotopbricks","hydroGOF" and "ggplot2" tools. (www.geotop.org)
#
# author: Emanuele Cordano on 12-04-2016
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################







rm(list=ls())

library(geotopbricks) 
library(hydroGOF)
library(ggplot2)
library(reshape2)

external=FALSE
## Set TRUE the following line if it is launched in a R console
Rconsole=FALSE

if (external==TRUE) {
	
	args<- commandArgs(TRUE)

} else {
	
	Rconsole=TRUE
}

if (length(args)==0) args <- "--help empty -npoints 2"

help_flag <- "--help"
flag_file <- system.file("regression-testing/test-1D.txt",package="geotopbricks") 




needHelp <- argsParser(option=help_flag,args=args,novalue_response=FALSE)


helpco <- readLines(flag_file)


if (Rconsole==TRUE) needHelp=!Rconsole

if (needHelp==TRUE) {
	
	
	
	
	
	vvout <- lapply(X=helpco,FUN=message)
	
	message("Mandatory Running Arguments missing!! ")
	
	
} else {
	
	
	### READ THE ARGS
	
	ai <- which(str_detect(helpco,"Options:"))+1
	bi <- which(str_detect(helpco,"Notes:"))-1
	
	helpco <- helpco[ai:bi]
	
	ii <- which(str_sub(helpco,1,1)=="-")
	
	helpco <- str_split(helpco[ii],pattern=" ",n=2)
	names(helpco) <- sapply(X=helpco,FUN=function(x){x[1]})
	helpco <- sapply(X=helpco,FUN=function(x){x[2]})
	value <- NULL
	for (it in names(helpco)) {
		
		value[it] <- argsParser(option=it,args=args,novalue_response=helpco[it],sep=" ")
		value[it] <- str_trim(value[it])
		
	}
	
	### END ARGS 
	
	####
	
	## Uncomment and update the following linse if it is launched in a R console
	if (Rconsole==TRUE) {
	  value["-wpath"] <- '/home/ecor/local/geotop_dev/geotop/tests/1D/Calabria' 
	  value["-output-dir"] <-  '/home/ecor/' 
	  value["-keyws"] <- "SoilTempProfileFile"###,SoilLiqContentProfileFile,SoilLiqWaterPressProfileFile"
	}
	###
	
	# Example Usage:  ./geotop-test-1D.R -wpath /home/ecor/local/geotop_dev/geotop/tests/1D/Calabria -output-dir /home/ecor/local/geotop_dev/geotop_tests_1D_Calabria 
	
	###
	####
	
	wpath <- value["-wpath"]
	inpts.file <- value["-inpts.file"]
	npoint <- as.numeric(value["-npoint"])
	outdir <- value["-output-dir"]
	suffix_n <- str_split(value["-suffix_version"],",")[[1]]
	help(merge)
	suffix <- paste("-",suffix_n,sep="")
	names(suffix) <- suffix_n
	
	
	####keyws <- c("SoilTempProfileFile","SoilLiqContentProfileFile","SoilLiqWaterPressProfileFile")
	keyws <- str_split(value["-keyws"],",")[[1]]
	date_field <- "Date12.DDMMYYYYhhmm."
	zlayer.formatter <- "z%04d"
	outv <- list()
	
	### TEST KEYWS 
	
	keyws_value <- lapply(X=keyws,FUN=get.geotop.inpts.keyword.value,wpath=wpath,inpts.file=inpts.file)
	names(keyws_value) <- keyws
	cond <- sapply(X=keyws_value,FUN=is.null)
	icond <- which(cond==TRUE)
	if (length(icond)>0) {
		
		msg <- sprintf("The keywords %s are not present in the %s file!",paste(keyws[icond],collapse=","),inpts.file)
		warning(msg)
		
		keyws <- keyws[-icond]
		
	}
	
	if (length(keyws)==0) {
		
		msg <- sprintf("None of the set keywords are present in the %s file!",inpts.file)
		stop(msg)
		
	}
	
	
	
	### END CHECK KEYWS
	
	
	
	it_this <- "this"
	outv[[it_this]] <- lapply(X=keyws,FUN=get.geotop.inpts.keyword.value,wpath=wpath,inpts.file=inpts.file,data.frame=TRUE,date_field=date_field,zlayer.formatter=zlayer.formatter,level=npoint) 
	
	names(outv[[it_this]]) <- keyws
	
	outv[[it_this]] <- do.call(merge,outv[[it_this]])
	if (length(keyws)==1) {
		
		###
		
		nn <- names(outv[[it_this]])
		nn <- paste(nn,keyws[1],sep=".")
		names(outv[[it_this]]) <- nn
		
		###
		
	}
	
	
	####print("ba::")
	####str(outv)
	
	
	###stop()
	for (it_ in names(suffix)) {
		
		outv[[it_]] <- lapply(X=keyws,FUN=get.geotop.inpts.keyword.value,wpath=wpath,inpts.file=inpts.file,data.frame=TRUE,add_suffix_dir=suffix[[it_]],date_field=date_field,zlayer.formatter=zlayer.formatter) 
		
		
		names(outv[[it_]]) <- keyws
		
		outv[[it_]] <- do.call(merge,outv[[it_]])
		
		
		if (length(keyws)==1) {
			
			###
			
			nn <- names(outv[[it_]])
			nn <- paste(nn,keyws[1],sep=".")
			names(outv[[it_]]) <- nn
			
			###
			
		}
		
		
		
		
		
		
	}
	
	outv <- do.call(merge,outv)	
	

	
	
	
	
	
	time <- index(outv)
	fields <- names(outv)
	
	metaoutv <- as.data.frame(do.call(rbind,str_split(fields,"[.]")),stringsAsFactors=FALSE)
	names(metaoutv) <- c("zdepth","keyv","model")
	metaoutv$field <- fields
	metaoutv$label <- paste(metaoutv$keyv,metaoutv$zdepth,sep="_")
	
	labels <- unique(metaoutv$label)
	check <- list()
	gpattern <- list()
	gerror <- list()
	for (il in labels) {
		
		metadaf <- metaoutv[metaoutv$label==il,]
		daf <- outv[,metadaf$field]		
		
		names(daf) <- metadaf$model
		
		
		#### GOF CHECK LOG #### 
		
		it_this_c <- rep(it_this,ncol(daf))
		
		
		sim <- daf
		
		
		obs <- daf[,it_this_c]
		
		
		
		check[[il]] <- try(gof(sim,obs),silent=TRUE)
		
		if (class(check[[il]])=="try-error") {
			
			oo <- apply(X=abs(sim-obs),FUN=max,MARGIN=2,na.rm=TRUE)<.Machine$double.eps*100
			
			sim[,] <- 1:length(sim)
			obs[,] <- sim[,]
			
			check[[il]] <- try(gof(sim,obs),silent=TRUE)
			
			if (all(oo==TRUE,na.rm=TRUE)) {
				
				
				
			} else {
				
				msq <- sprintf('GOF not working for %s',il)
				warning(msg)
				check[[il]][,] <- NA
				
			}
			
			
		}
		
		colnames(check[[il]])[colnames(check[[il]])==it_this_c] <- "optimum"
		
		if (external==TRUE) { 
		
			file <- sprintf("%s/gof_%s.log",outdir,il)
			collapse=";"
			lines <- paste(colnames(check[[il]]),collapse=collapse)
			ind <- c("",rownames(check[[il]]))
			lines <- c(lines,apply(check[[il]],FUN=paste,MARGIN=1,collapse=collapse))
			lines <- paste(ind,lines,sep=collapse)
			writeLines(lines,con=file)
		#write.table(check[[il]],file=file,sep="  ",quote=TRUE)
		
		}	
		
		
		
		#### END GOF CHECK LOG #### 
		
		### BEHAVIOR PLOT ####
		
		time <- index(daf)
		dv <- as.data.frame(daf)
		dv$time <- as.numeric(time-time[1],units="secs")
		
		rownames(dv) <- NULL
		mdf <- melt(dv,id="time")
		mdf$time <- mdf$time+time[1]
		
		mdf <- mdf[!is.na(mdf$value),]
		title <- sprintf("Variable Pattern: %s",il)
		gpattern[[il]] <- ggplot(data=mdf,aes(x=time,y=value)) + geom_line(aes(color=variable),size=1.25)+ggtitle(title)
		
		if (external==TRUE) {
			
			file <- sprintf("%s/gpattern_%s.png",outdir,il)
			ggsave(filename=file,plot=gpattern[[il]])
		}
		
		#### END BEHAVIOR PLOT ####
		
		### ERROR BEHAVIOR PLOT ####
		
		time <- index(daf)
		dv <- as.data.frame(daf-daf$this)
		dv$time <- as.numeric(time-time[1],units="secs")
		
		
		
		rownames(dv) <- NULL
		mdf <- melt(dv,id="time")
		mdf <- mdf[!is.na(mdf$value),]
		mdf$time <- mdf$time+time[1]
		title <- sprintf("Variable Error: %s",il)
		gerror[[il]] <- ggplot(data=mdf,aes(x=time,y=value)) + geom_line(aes(color=variable),size=1.25)+ggtitle(title)
		
		if (external==TRUE) {
			
			file <- sprintf("%s/gerror_%s.png",outdir,il)
			ggsave(filename=file,plot=gerror[[il]])
		}
		
		#### END BEHAVIOR PLOT ####
		
		
		
		
		
	}
	
}	
