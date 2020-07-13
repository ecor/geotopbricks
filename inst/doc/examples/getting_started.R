library(geotopbricks)

#Simulation working path
## Not run: 

sims_url <- 'https://www.rendena100.eu/public/geotopbricks/simulations'


sim <- "panola13_run2xC_test3"
simzip <- sim
##extension(simzip) <- ".zip"
url <-  paste(sims_url,sim,sep="/")
wpath <- paste(tempdir(),sim,sep="/")
zip <- wpath
extension(from) <- "zip"
extension(zip) <- extension(from)
download.file(from,zip)
stop("HERE")

####wpath <- "https://github.com/ecor/geotopbricks_doc/tree/master/simulations/panola13_run2xC_test3"
wpath <- "https://raw.githubusercontent.com/ecor/geotopbricks_doc/master/simulations/panola13_run2xC_test3"
prefix <- get.geotop.inpts.keyword.value("SoilLiqWaterPressTensorFile",wpath=wpath)

slope <- get.geotop.inpts.keyword.value("SlopeMapFile",raster=TRUE,wpath=wpath) 
bedrock_depth <- get.geotop.inpts.keyword.value("BedrockDepthMapFile",raster=TRUE,wpath=wpath) 

layers <- get.geotop.inpts.keyword.value("SoilLayerThicknesses",numeric=TRUE,wpath=wpath)
names(layers) <- paste("L",1:length(layers),sep="")

##### set van genuchten parameters to estimate water volume 
theta_sat <- get.geotop.inpts.keyword.value("ThetaSat",numeric=TRUE,wpath=wpath)
theta_res <- get.geotop.inpts.keyword.value("ThetaRes",numeric=TRUE,wpath=wpath)
alphaVG <-  get.geotop.inpts.keyword.value("AlphaVanGenuchten",
                                           numeric=TRUE,wpath=wpath) # expressed in mm^-1

nVG <-  get.geotop.inpts.keyword.value("NVanGenuchten",numeric=TRUE,wpath=wpath) 


##### end set van genuchten parameters to estimate water volume


##### set meteo data

start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="A") 
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="A") 

nmeteo <- get.geotop.inpts.keyword.value("NumberOfMeteoStations",numeric=TRUE,wpath=wpath)
level <- 1:nmeteo

# Uncomment the following lises to run the R code: 

## set meteo data

## End(Not run)

## Not run: 
meteo <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
                                        level=level,start_date=start,end_date=end)

## End(Not run)

##### end set meteo data

## IMPORTING AN OUTPUT SOIL MOISTURE PROFILE: 

wpath <- 'https://www.rendena100.eu/public/geotopbricks/simulations/Muntatschini_pnt_1_225_B2_004'

## Not run: 
SMC  <- get.geotop.inpts.keyword.value("SoilLiqContentProfileFile",
                                       wpath=wpath,data.frame=TRUE,date_field="Date12.DDMMYYYYhhmm.",
                                       formatter="%04d")

SMCz  <- get.geotop.inpts.keyword.value("SoilLiqContentProfileFile",
                                        wpath=wpath,data.frame=TRUE,date_field="Date12.DDMMYYYYhhmm.",
                                        formatter="%04d",zlayer.formatter="z%04d")

## End(Not run)