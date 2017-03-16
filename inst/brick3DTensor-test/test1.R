# TODO: Add comment
# 
# Author: ecor
###############################################################################




library(geotopbricks)

source('/home/ecor/Dropbox/R-packages/geotopbricks/R/listFromOutput3DTensor.R')
# The data containing in the link are only for educational use
wpath <- '/home/ecor/Dropbox/R-packages/geotopbricks_simulation/rendena'
x <- "SoilLiqContentTensorFile"
tz <-  "Etc/GMT-1"
when <- as.POSIXct("2014-03-10",tz=tz)

# Not Run because it elapses too long time!!! 
# Please Uncomment the following lines to run by yourself!!!##
### b <- brickFromOutputSoil3DTensor(x,when=when,wpath=wpath,tz=tz,use.read.raster.from.url=TRUE)

# a 2D map: 
x_e <- "SnowDepthMapFile"
# Not Run: uncomment the following line
 m <- rasterFromOutput2DMap(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",
                            tz=tz,use.read.raster.from.url=TRUE)
## NOTE: set use.read.raster.from.url=FALSE (default) 
# if the "wpath" directorty is in the local file system.
# Not Run: uncomment the following line
# plot(m)

m <- rasterFromOutput2DMap(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",
		tz=tz,use.read.raster.from.url=FALSE)


me <- listFromOutputSoil3DTensor(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",
		tz=tz,use.read.raster.from.url=TRUE,one.layer=TRUE)


ms <- rasterFromOutput2DMap(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",
		tz=tz,use.read.raster.from.url=FALSE,only.map.filename=TRUE)

