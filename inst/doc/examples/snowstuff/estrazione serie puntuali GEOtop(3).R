##########################################################
# VISUALIZZAZIONE SERIE PUNTUALI DA GEOTOP
##########################################################

rm(list=ls())

library(devtools)
install_github("geotoppoints",username="ecor")
install_github("geotopbricks",username="ecor")


library(geotopbricks)
library(reshape2)
library(ggplot2)
#library(RClimMAWGEN)
#library(gridExtra)
library(geotoppoints)
library(scales)

# vedere su https://github.com/ecor/geotoppoints
# poi inst / examples

wpath <- "F:/idroclima_simulazioni/Trentino_500_dstr_GEOtop_2_012" # disco "backup Emanuele C. e Annalisa"
wpath <- "F:/IDROCLIMA/simulazioni_geotop/Trentino_500_dstr_GEOtop_2_012" # Gongolo
wpath <- "http://cassiopea.intra.ismaa.it/idroclima/idroclim_trentino_2/Trentino_500_dstr_GEOtop_2_012"

ContinuousRecovery <- 1 # nr. di interruzioni della simulazione (vedere nella cartella tabs i file di tipo "xxxxxx_crec0001.txt" e cercare il nr. pi� alto)

points <- c("BONDONE-VIOTE","LAVARONE","SAVIGNANO","SAN MICHELE A.A.","MEZZOLOMBARDO")
points_meteoid <- c("T0327","T0082","T0147","SMICH","T0090") ## T0147 ROVERETO 

#points <- c("BONDONE-VIOTE","LAVARONE","SAVIGNANO","MEZZOLOMBARDO")
#points_meteoid <- c("T0327","T0082","T0147","T0090") ## T0147 ROVERETO 


CoordinatePointX <- get.geotop.inpts.keyword.value("CoordinatePointX",wpath=wpath,numeric=TRUE)[1:max]
CoordinatePointY <- get.geotop.inpts.keyword.value("CoordinatePointY",wpath=wpath,numeric=TRUE)[1:max]
CoordinatePointCode <- str_split(get.geotop.inpts.keyword.value("CoordinatePointCode",wpath=wpath),pattern=",")[[1]][1:max]
CoordinatePointName <- str_split(get.geotop.inpts.keyword.value("CoordinatePointName",wpath=wpath),pattern=",")[[1]][1:max]
level <- 1:length(CoordinatePointX)

names(CoordinatePointX) <- points
names(CoordinatePointY) <- points
names(level) <- points

## Set time parameters

tz <-  "Etc/GMT+1"
start <- as.POSIXct("2006-01-01 UTC",tz=tz)
end <- as.POSIXct("2008-12-31 UTC",tz=tz)

start <- as.POSIXct("1990-04-01 UTC",tz=tz)
end <- as.POSIXct("2012-12-31 UTC",tz=tz)


day <- 3600*24

# tensione
psiliq <- get.geotop.inpts.keyword.value("SoilLiqWaterPressProfileFile",wpath=wpath,data.frame=TRUE,level=level,start_date=start,end_date=end,date_field="Date12.DDMMYYYYhhmm.",isNA = -9*10^6,ContinuousRecovery=ContinuousRecovery)
psiliq <- lapply(X=psiliq,FUN=function(x){
  		out <- x[,str_detect(names(x),"X")]
			names(out) <- str_replace(names(out),"X","psi_z")
			return(out)
		})
names(psiliq) <- names(level)

# cont. acqua
thetaliq <- get.geotop.inpts.keyword.value("SoilLiqContentProfileFile",wpath=wpath,data.frame=TRUE,level=level,start_date=start,end_date=end,date_field="Date12.DDMMYYYYhhmm.",isNA = -9*10^6,ContinuousRecovery=ContinuousRecovery)
thetaliq <- lapply(X=thetaliq,FUN=function(x){
  out <- x[,str_detect(names(x),"X")]
  names(out) <- str_replace(names(out),"X","theta_z")
  return(out)
})
names(thetaliq) <- names(level)

# temp. suolo
tempsoil <- get.geotop.inpts.keyword.value("SoilAveragedTempProfileFile",wpath=wpath,data.frame=TRUE,level=level,start_date=start,end_date=end,date_field="Date12.DDMMYYYYhhmm.",isNA = -9*10^6,ContinuousRecovery=ContinuousRecovery)
tempsoil <- lapply(X=tempsoil,FUN=function(x){
  out <- x[,str_detect(names(x),"X")]
  names(out) <- str_replace(names(out),"X","temp_z")
  return(out)
})
names(tempsoil) <- names(level)


# per vedere i nomi delle variabili:
#names(pointvalues[[1]])

# altre grandezze, senza profondit� in strati
pointvalues <- get.geotop.inpts.keyword.value("PointOutputFile",wpath=wpath,data.frame=TRUE,level=level,start_date=start,end_date=end,date_field="Date12.DDMMYYYYhhmm.",ContinuousRecovery=ContinuousRecovery)
names(pointvalues) <- names(level)

id <- names(pointvalues[[1]]) # tutte
id <- c("Prain_over_canopy.mm.","Prain_under_canopy.mm.","LE.W.m2.","Evap_surface.mm.","Trasp_canopy.mm.")
id <- c("Evap_surface.mm.","Trasp_canopy.mm.")
id <- c("SWin.W.m2.", "LE.W.m2.")
id <- c("Wind_speed.m.s.", "Wind_speed_top_canopy.m.s.")

# tiene solo quelle contenute in id
pointvalues_rev <- lapply(X=pointvalues,FUN=function(x,id){x[,id]},id=id)

time_aggregation <- 3600*24*(365.25)/12 # daily or multi-daily  aggregation - equally spaced months  [seconds]
time_aggregation <- 3600*24 # daily or multi-daily  aggregation- days [seconds]

# creo i melt
psiliq_melt <- meltFromZooList(x=psiliq,aggregate=time_aggregation,FUN=mean)
psithr <- -5000
psiliq_melt$value[psiliq_melt$value<psithr] <- psithr
psiliq_melt$value[psiliq_melt$value > 10] <- 10

thetaliq_melt <- meltFromZooList(x=thetaliq,aggregate=time_aggregation,FUN=mean)

tempsoil_melt <- meltFromZooList(x=tempsoil,aggregate=time_aggregation,FUN=mean)


# valori mensili:
# somma
pointvalues_melt <- meltFromZooList(x=pointvalues_rev,aggregate=time_aggregation,FUN=sum)
# media
pointvalues_melt <- meltFromZooList(x=pointvalues_rev,aggregate=time_aggregation,FUN=mean)
# solo se devo unire con i valori delle variabili sugli strati (brick):
pointvalues_melt <- rbind(pointvalues_melt,psiliq_melt,thetaliq_melt, tempsoil_melt)

# se uso solo i dati sugli strati:
pointvalues_melt <- rbind(psiliq_melt,thetaliq_melt, tempsoil_melt)

variables <- unique(pointvalues_melt$variable)
locations <- unique(pointvalues_melt$L1)

# scegli
variable <- c("SWin.W.m2.")
variable <- c("Wind_speed.m.s.", "Wind_speed_top_canopy.m.s.")
variable <- c("Evap_surface.mm.","Trasp_canopy.mm.")
variable <- c("Evap_surface.mm.","Trasp_canopy.mm.","EP.mm")
variable <- c("psi_z580.000000","psi_z1430.000000","psi_z3530.000000","psi_z16030.000000")
variable <- c("theta_z80.000000","theta_z230.000000","theta_z580.000000","theta_z1430.000000")
variable <- c("theta_z80.000000","theta_z230.000000","theta_z580.000000")
variable <- c("psi_z80.000000","psi_z230.000000","psi_z580.000000")
variable <- c("temp_z80.000000","temp_z230.000000","temp_z580.000000")
#variable <- c("Prain_over_canopy.mm.","Prain_under_canopy.mm.","EP.mm")



start <- as.POSIXct("1990-04-01 UTC",tz=tz)
end <- as.POSIXct("2012-12-30 UTC",tz=tz)

start <- as.POSIXct("2006-01-01 UTC",tz=tz)
end <- as.POSIXct("2008-12-31 UTC",tz=tz)

pointvalues_plot <- pointvalues_melt[pointvalues_melt$variable %in% variable & pointvalues_melt$Time >= start & pointvalues_melt$Time <= end,]
# qui solo psi:
pointvalues_plot <- psiliq_melt[psiliq_melt$variable %in% variable & psiliq_melt$Time >= start & psiliq_melt$Time <= end,]
# qui solo theta:
pointvalues_plot <- thetaliq_melt[thetaliq_melt$variable %in% variable & thetaliq_melt$Time >= start & thetaliq_melt$Time <= end,]

# escludo eventualmente siti - altrimenti saltare!
#location <- locations[4:5] 
#pointvalues_plot<-pointvalues_plot[pointvalues_plot$L1 %in% location,]

qp <- qplot(Time, value, data = pointvalues_plot, geom = "line", group = variable) +
  facet_grid(variable ~ L1, scale = "fixed") + scale_x_datetime(breaks=date_breaks("3 year"), labels=date_format("%Y")) 

# alternativa: l'ho salvato avendolo creato altrove
#load("D:/Documents and Settings/eccele/Documenti/OTC/IdroClima/simulazioni/010 grafici puntiformi/evapotrasp punt si. 11 2006-2007")

print(qp)