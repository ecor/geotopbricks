# TODO: Add comment
# 
# Author: ecor
###############################################################################

library(geotopbricks)
library(reshape2)

wpath <- '/home/ecor/Dropbox/R-packages/geotopbricks_simulation/rendena'

alt_map <-  get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath)
aspect_map <- get.geotop.inpts.keyword.value("AspectMapFile",raster=TRUE,wpath=wpath)
aspect_map[aspect_map>360-45/2] <- aspect_map[aspect_map>360-45/2]-360

aspect_cat <- 1:8
names(aspect_cat) <- aspect_class <- c("N","NE","E","SE","S","SW","W","NW")

aspect_cat_map <- (as.integer(aspect_map/45))+1



tz <- "Etc/GMT-1"
start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz) 
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz) 
dt <-  get.geotop.inpts.keyword.value("OutputSnowMaps",numeric=TRUE,wpath=wpath,tz=tz)*3600


when <- seq(from=start,to=end,by=dt)[-1]

x_e <- "SnowDepthMapFile"
m <- rasterFromOutput2DMap(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",
		tz=tz,use.read.raster.from.url=FALSE)


me <- lapply(X=m,FUN=function(x,alt,asp,aspcat,valmin){
			
			
			ov <- alt[x>valmin]		
			aspv <- names(aspect_cat)[asp[x>valmin]]
			o <- tapply(X=ov,INDEX=aspv,FUN=min,na.rm=TRUE)	
			###o <- min(o,na.rm=TRUE)
			o <- o[names(aspect_cat)]
			return(o)
			
			
			
		},alt=alt_map,asp=aspect_cat_map,aspcat=aspect_cat,valmin=100)


df <- melt(me)
names(df)[names(df)=="Var1"] <- "aspect"
names(df)[names(df)=="L1"] <- "time"
df$time <- as.Date(df$time,format="DATE-TIME %Y-%m-%d 00:00")
df$aspect <- as.character(df$aspect)

df <- df[,c("time","aspect","value")]



#stop("LO SCRIPT FINISCE QU!!!")
#
#
#aggr <- c("q25","median","q75","mean")
#mapaggr <- list()
#
#mapv <- list.files(mapdir,pattern=".asc",full.name=TRUE)
#names(mapv) <- list.files(mapdir,pattern=".asc",full.name=FALSE)
#names(mapv) <- str_replace(names(mapv),".asc","")
#names(mapv) <- str_replace(names(mapv),"SnowDepthMapFile-","")
#
#funm <- sapply(X=str_split(names(mapv),"-"),FUN=function(x){x[[1]]})
#time <- sapply(X=str_split(names(mapv),"-",n=2),FUN=function(x){x[[2]]})
#
#mapv <- mapv[funm %in% aggr]
#time <- time[funm %in% aggr]
#funm <- funm[funm %in% aggr]
#alt_map <-  get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath)
#
#valmin <- 250 ## 50
#time <- paste(time,15,sep="-")
#time <- as.Date(time,format="%Y-%m-%d")
#df <- data.frame <- data.frame(Time=time,MonthlyAggrFunc=funm,SnowAlt=NA*0,stringsAsFactors=FALSE)
#
#alt_snow <- lapply(X=mapv,FUN=function(x,alt,asp,valmin){
#			
#			message(x)
#			o <- raster(x)
#			aspv <- names(aspect_cat)[asp[o>valmin]]
#		##	print(o)
#		##	print(alt)
#		##	print(asp)
#			ov <- alt[o>valmin]		
#		##	str(aspv)
#		##	str(ov)
#			o <- tapply(X=ov,INDEX=aspv,FUN=min,na.rm=TRUE)	
#			##o <- min(o,na.rm=TRUE)
#			
#			return(o)
#			
#		},alt=alt_map,asp=aspect_cat_map,valmin=valmin)
#
#stop("qui")
#df <- df[1:length(alt_snow),]
#df$SnowAlt <- alt_snow

file <- '/home/ecor/Dropbox/R-packages/geotopbricks/inst/brick3DTensor-test/output/alt_snow_vs_time_daily.csv' 

write.table(x=df,file=file,sep=",",row.names=FALSE,quote=FALSE)
