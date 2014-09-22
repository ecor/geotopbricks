
#
#
#
#  Emanuele Cordano 
#
#


library(geotopbricks)
library(geographyTrentinoVis)

wpath <- '/home/ecor/attivita/2014/caprioli/rendena100m_sim'
wpath <- '/media/GONGOLO/caprioli/rendena100m_20140807_v2'

tz <-  "Etc/GMT+1"

whensummer <- c("%s-07-02")
whenwinter <- c("%s-02-02")

whensummer <- sprintf(whensummer,2011:2013)
whenwinter <- sprintf(whenwinter,2012:2014)
####when <- c("2012-05-05","2012-05-06","2012-05-07","2012-05-08","2012-05-09","2012-05-10","2012-05-15","2012-05-20")
when <- sprintf("2014-04-%02d",26:30)
###when2 <- sprintf("2013-12-%02d",24:2)

#when <- c(whensummer,whenwinter,"2011-08-01",add)

when <- sort(when)

when <- as.POSIXct(when,tz=tz) #### c("2011-07-02","2012-02-07","2012-07-02","2014-02-14"),tz=tz)

# a 2D map:
x_e <- "SnowDepthMapFile"
x_surftemp <- "SurfaceTempMapFile"
x_airtemp <-  "AirTempMapFile"
x_prec <- "PrecipitationMapFile"

# Not Run: uncomment the following line
m <- rasterFromOutput2DMap(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",tz=tz,use.read.raster.from.url=FALSE)
m_surftemp <- rasterFromOutput2DMap(x_surftemp,when=when,wpath=wpath,timestep="OutputSnowMaps",tz=tz,use.read.raster.from.url=FALSE)
m_airtemp <- rasterFromOutput2DMap(x_airtemp,when=when,wpath=wpath,timestep="OutputSnowMaps",tz=tz,use.read.raster.from.url=FALSE)
m_precTOTAL <- rasterFromOutput2DMap(x_prec,when=when,wpath=wpath,timestep="OutputSnowMaps",tz=tz,use.read.raster.from.url=FALSE,secondary.suffix="TOTAL")
m_precSNOW <- rasterFromOutput2DMap(x_prec,when=when,wpath=wpath,timestep="OutputSnowMaps",tz=tz,use.read.raster.from.url=FALSE,secondary.suffix="SNOW")
## NOTE: set use.read.raster.from.url=FALSE (default) if the "wpath" directorty is in the local file system.
# Not Run: uncomment the following line
# plot(m)

##  Pra Rodont 

xmin <- 637750
xmax <- 637950

ymin <- 5114000
ymax <- 5114200

extent <- extent(xmin,xmax,ymin,ymax)

#title <- "Snow Depth"
#label <- "Snow Depth[mm]"
#
#map_rendena  = get_map(location = "rendena", zoom = 10) 
#scale <- scale_colour_continuous() ## scale_fill_gradient(name=label,low=low,high=high,limits=range)+scale_color_gradient(name=label,low=low,high=high,limits=range)
#plotOn(x=brick(m),map=map_rendena,label=label,title=title,alpha=0.1,scale.fill.gradient=FALSE)





