geotopbricks
============
R plug-in 
Development of CRAN R package geotopbricks (http://cran.r-project.org/web/packages/geotopbricks)

To install this package on R fram R console:

```{r}

>library(devtools)

>library(reotes)

>install_github("ecor/geotopbricks")

```

![](inst/sticker/sticker_geotopbricks.png){width=0.50/width}

## Getting Started 






```{r}
library(geotopbricks)

#Simulation working path



wpath <-  'https://raw.githubusercontent.com/ecor/geotopbricks_doc/master/simulations/panola13_run2xC_test3'
## wpath (RAW VERSION) of https://github.com/ecor/geotopbricks_doc/tree/master/simulations/panola13_run2xC_test3
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


tz <- "Etc/GMT-1"  ## See help(timezones) In particular:
## Most platforms support time zones of the form Etc/GMT+n and Etc/GMT-n (possibly also without prefix Etc/), 
## which assume a fixed offset from UTC (hence no DST). Contrary to some expectations 
## (but consistent with names such as PST8PDT), negative offsets are times ahead of (east of) UTC, 
## positive offsets are times behind (west of) UTC.
start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz) 
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz=tz) 

nmeteo <- get.geotop.inpts.keyword.value("NumberOfMeteoStations",numeric=TRUE,wpath=wpath)
level <- 1:nmeteo

## set meteo data

meteo <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
                                        level=level,start_date=start,end_date=end,tz=tz)

##### end set meteo data

## IMPORTING AN OUTPUT SOIL MOISTURE PROFILE: 


wpath <-  'https://raw.githubusercontent.com/ecor/geotopbricks_doc/master/simulations/Muntatschini_pnt_1_225_B2_004'
## wpath (RAW VERSION) of https://github.com/ecor/geotopbricks_doc/tree/master/simulations/Muntatschini_pnt_1_225_B2_004
## Not run: 
SMC  <- get.geotop.inpts.keyword.value("SoilLiqContentProfileFile",
                                       wpath=wpath,data.frame=TRUE,date_field="Date12.DDMMYYYYhhmm.",
                                       formatter="%04d")

SMCz  <- get.geotop.inpts.keyword.value("SoilLiqContentProfileFile",
                                        wpath=wpath,data.frame=TRUE,date_field="Date12.DDMMYYYYhhmm.",
                                        formatter="%04d",zlayer.formatter="z%04d")

## End(Not run)



```