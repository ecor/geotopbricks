# TODO: Add comment
# 
# Author: ecor
###############################################################################


NULL

#'
#' This function saves all spatially distrubuted information contained in the recovery folder into a comprehensive \code{list} object.
#' 
#' 
#' 
#' 
#' 
#' 
#' @author Emanuele Cordano
#' 
#' @param recFolder directory when recvery maps are set. In GEOtop it is ...
#' @param xx charcter String. Default is \code{"0000"}
#' @param extension file estension used for asccii recovery map files. It must contains \code{'.'} as the first character. Defaut is \code{".asc"} . 
#' @param formatter string character for the the decimal formatter to be used. Default is \code{"L\%04d"}.
#' @param nsoillayers number of soil layers used in the GEOtop simulation.
#' @param layersFromDir logical value. If is \code{TRUE} the number of soil/snow (vertical) layers used in the GEOtop simulation is automatically calculated and cannot be assigned through \code{nsoillayers}.
#' @param ... further arguments 
#' 
#' @return a \code{list} object containining all recovery raster maps. 
#' 
#' @note This function has been used with the built 1.225-9 of GEOtop . 
#' @seealso \code{\link{brick.decimal.formatter}},
#' 
#' \code{\link[raster]{raster}},\code{\link{set.geotop.recovery.state}},
#' 
#' \code{\link{write.vectorized.geotop.recovery}},\code{\link{read.vectorized.geotop.recovery}}
#' 
#' @export
#' @examples
#' library(geotopbricks)
#' example_Rscript <- system.file('template/example.geotop.recovery.state.R',package="geotopbricks")
#' example_Rscript
#' 
#' # Not Run because it elapses too long time!!! 
#' # Please Uncomment the following line to run by yourself!!!
#' # source(example_Rscript)
#' 
# ##' recFolder <- '/home/ecor/Dropbox/R-packages/geotopbricks__/rec_after1day' 
# rec <- get.geotop.recovery.state(recFolder) 
# 
# newRecFolder  <- '/home/ecor/Dropbox/R-packages/geotopbricks__/new_rec_after1day__' 
# set.geotop.recovery.state(rec,newRecFolder=newRecFolder)
#  newRecFolder  <- '/home/ecor/Dropbox/R-packages/geotopbricks__/ba' 
# set.geotop.recovery.state(rec,newRecFolder=newRecFolder)
# 
 




get.geotop.recovery.state <- function(recFolder,xx="0000",formatter="L%04d",extension=".asc",nsoillayers=10,layersFromDir=FALSE,...) {


	
	
	names<-c("RainOnCanopy","SnowAge","SnowIceContent","SnowLayersNumber","SnowLiqWaterContent","SnowOnCanopy","SnowTemperature","SnowThickness","SoilChannelIceContent","SoilChannelPressure","SoilChannelTemperature","SoilIceContent","SoilPressure","SoilTemperature","VegTemperature",
			"GlacIceContent","GlacLayersNumber","GlacLiqWaterContent","GlacTemperature","GlacThickness")      
#	xx <- "0000"
    layer <- array(formatter,length(names))
	
	# 4 Groups of rasterbricks or rasters !!! 
	noLayers <- (names %in% c("RainOnCanopy","VegTemperature","SnowAge","SnowLayersNumber","SnowOnCanopy","GlacLayersNumber"))	
	soilLayersWithZero <- (names %in% c("SoilPressure","SoilChannelPressure"))
	soilLayers <- (str_detect(names,"Soil")) & (!soilLayersWithZero) & (!noLayers)
	snowLayers <- str_detect(names,"Snow") & (!noLayers) & (!soilLayers) & (!soilLayersWithZero)
	glacLayers <- str_detect(names,"Glac") & (!noLayers) & (!soilLayers) & (!soilLayersWithZero) &(!snowLayers)
	
	layer[noLayers] <- ""
	files <- paste(names,xx,layer,extension,sep="")
	files_w <- paste(recFolder,files,sep="/")
#	
	out <- list()
	
	if (layersFromDir==TRUE)  {
		
		nsoillayers <-  "FromDir"
		nsnowlayers <-  "FromDir"
		nglaclayers <-  "FromDir"
	} 
		
	
	for (it in names[noLayers]) {
		
		

		x <- as.character(files_w[names==it])
		
		if (file.exists(x)==TRUE) { 
			
			out[[it]] <- raster(x)
		
		}  else {
			
			out[[it]] <- NULL
		}
		
		
	}
	
	for (it in names[soilLayersWithZero]) {
		
		x <- as.character(files_w[names==it])
		
		out[it] <- brick.decimal.formatter(file=x,nlayers=nsoillayers,start.from.zero=TRUE)
	
	}
	for (it in names[soilLayers]) {
	
		x <- as.character(files_w[names==it])
		
		out[it] <- brick.decimal.formatter(file=x,nlayers=nsoillayers,start.from.zero=FALSE)
	
	}
	
	# number of snow layers is detected by raster layer 'out$SnowLayersNumber'
	if (!is.null(out[["SnowLayersNumber"]])) {
		
		if (layersFromDir==FALSE) {
		
	
			out$SnowLayersNumber <- setMinMax(out$SnowLayersNumber)
			nsnowlayers <- maxValue(out$SnowLayersNumber)
			nsnowlayers[nsnowlayers<1] <- 1
		}
		for (it in names[snowLayers]) {
		

			x <- as.character(files_w[names==it])
			
			out[it] <- brick.decimal.formatter(file=x,nlayers=nsnowlayers,start.from.zero=FALSE)
		
		}
		
	}
	
	##############
	if (!is.null(out[["GlacLayersNumber"]])) {
		if (layersFromDir==FALSE) {
		
		
			out$GlacLayersNumber <- setMinMax(out$GlacLayersNumber)
			nglaclayers <- maxValue(out$GlacLayersNumber)
			nglaclayers[nglaclayers<1] <- 1
		}
		for (it in names[glacLayers]) {
		
		
			x <- as.character(files_w[names==it])
			##print(it)
			##print(x)
			out[it] <- brick.decimal.formatter(file=x,nlayers=nglaclayers,start.from.zero=FALSE)
		
		
		
		}
	}
	
	
	
	
	################
	
	
	ii <- which(names %in% names(out))
	
	
	out$names <- names[ii]
	out$files <- files[ii]
	
	out$noLayers <- noLayers[ii]
	out$soilLayersWithZero <- soilLayersWithZero[ii]
	out$soilLayers <- soilLayers[ii]
	out$snowLayers <- snowLayers[ii]
	out$glacLayers <- glacLayers[ii]
	###############################
	
	
	
	
	
	
	
	
	
	
	
	#################################
	
	
	
	
	return(out)
	
}

############
#
#
#
#"GlacIceContent0000L0001.asc"       
#   
#     "GlacLayersNumber0000.asc"        
#[17] "GlacLiqWaterContent0000L0001.asc"  "GlacTemperature0000L0001.asc"    
#  
#[47] "GlacThickness0000L0001.asc"      
#
#

#
#
#"GlacIceContent,"GlacLayersNumber","GlacLiqWaterContent","GlacTemperature","GlacThickness")      
#
#








