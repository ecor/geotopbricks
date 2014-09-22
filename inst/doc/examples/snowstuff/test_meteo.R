
#
#
#
#  Emanuele Cordano 
#
#


library(geotopbricks)
library(geographyTrentinoVis)

wpath <- '/home/ecor/attivita/2014/caprioli/rendena100m_sim'

tz <-  "Etc/GMT+1"

MeteoStationCode <- str_split(get.geotop.inpts.keyword.value("MeteoStationCode",wpath=wpath),pattern=",")[[1]] ###T0167,T0169,T0175,T0177,T0179,T0373,T0382,T0411,T0412,T0413,T0414,T0426,T0433,T0435,T0436

MeteoStationName <- str_split(get.geotop.inpts.keyword.value("MeteoStationName",wpath=wpath),pattern=",")[[1]]
names(MeteoStationName) <- MeteoStationCode


start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="A")
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="A")

nmeteo <- get.geotop.inpts.keyword.value("NumberOfMeteoStations",numeric=TRUE,wpath=wpath)
level <- 1:nmeteo

# Not Run: uncomment the following lines to calculate "meteo"
meteo <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
         level=level,start_date=start,end_date=end,tz=tz)
#
meteo_old <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
		level=level,start_date=start,end_date=end,file_extension=".txt.old",tz=tz)

names(meteo) <- MeteoStationCode
names(meteo_old) <- MeteoStationCode
##### end set meteo data












stop()
tz <-  "Etc/GMT+1"

whensummer <- c("%s-07-02")
whenwinter <- c("%s-02-02")

whensummer <- sprintf(whensummer,2011:2013)
whenwinter <- sprintf(whenwinter,2012:2014)
when <- c("2012-05-05","2012-05-06","2012-05-07","2012-05-08","2012-05-09","2012-05-10","2012-05-15","2012-05-20")
####when <- c(whensummer,whenwinter,"2011-08-01",add)

when <- sort(when)

when <- as.POSIXct(when,tz=tz) #### c("2011-07-02","2012-02-07","2012-07-02","2014-02-14"),tz=tz)

# a 2D map:
x_e <- "SnowDepthMapFile"

# Not Run: uncomment the following line
m <- rasterFromOutput2DMap(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",tz=tz,use.read.raster.from.url=FALSE)
## NOTE: set use.read.raster.from.url=FALSE (default) if the "wpath" directorty is in the local file system.
# Not Run: uncomment the following line
# plot(m)


title <- "Snow Depth"
label <- "Snow Depth[mm]"

map_rendena  = get_map(location = "rendena", zoom = 10) 
scale <- scale_colour_continuous() ## scale_fill_gradient(name=label,low=low,high=high,limits=range)+scale_color_gradient(name=label,low=low,high=high,limits=range)
plotOn(x=m,map=map_rendena,label=label,title=title,alpha=0.1,scale.fill.gradient=FALSE,scale=scale)




