# TODO: Add comment
# 
# Author: ecor
###############################################################################

library(geotopbricks)


wpath <- '/home/ecor/Dropbox/R-packages/geotopbricks_simulation/rendena'

alt_map <-  get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath)

tz <- "Etc/GMT-1"
start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz) 
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz) 
dt <-  get.geotop.inpts.keyword.value("OutputSnowMaps",numeric=TRUE,wpath=wpath,tz=tz)*3600


when <- seq(from=start,to=end,by=dt)[-1]

x_e <- "SnowDepthMapFile"
m <- rasterFromOutput2DMap(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",
		tz=tz,use.read.raster.from.url=FALSE)


me <- lapply(X=m,FUN=function(x,alt,valmin){
			
			
			o <- alt[x>valmin]		
			o <- min(o,na.rm=TRUE)
			
			return(o)
			
			
			
		},alt=alt_map,valmin=250)
stop("HERE")







aggr <- c("q25","median","q75","mean")
mapaggr <- list()

mapv <- list.files(mapdir,pattern=".asc",full.name=TRUE)
names(mapv) <- list.files(mapdir,pattern=".asc",full.name=FALSE)
names(mapv) <- str_replace(names(mapv),".asc","")
names(mapv) <- str_replace(names(mapv),"SnowDepthMapFile-","")

funm <- sapply(X=str_split(names(mapv),"-"),FUN=function(x){x[[1]]})
time <- sapply(X=str_split(names(mapv),"-",n=2),FUN=function(x){x[[2]]})

mapv <- mapv[funm %in% aggr]
time <- time[funm %in% aggr]
funm <- funm[funm %in% aggr]
alt_map <-  get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath)

valmin <- 250 ## 50
time <- paste(time,15,sep="-")
time <- as.Date(time,format="%Y-%m-%d")
df <- data.frame <- data.frame(Time=time,MonthlyAggrFunc=funm,SnowAlt=NA*0,stringsAsFactors=FALSE)

alt_snow <- sapply(X=mapv,FUN=function(x,alt,valmin){
			
			message(x)
			o <- raster(x)
			o <- alt[o>valmin]		
			o <- min(o,na.rm=TRUE)
			
			return(o)
			
		},alt=alt_map,valmin=valmin)

df <- df[1:length(alt_snow),]
df$SnowAlt <- alt_snow

file <- '/home/ecor/activity/2017/eurodeer/Rscript/output/alt_snow_vs_time.csv' 

write.table(x=df,file=file,sep=",",row.names=FALSE,quote=FALSE)
