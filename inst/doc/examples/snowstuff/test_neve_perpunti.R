##########################################################
# VISUALIZZAZIONE SERIE PUNTUALI DA GEOTOP
##########################################################

rm(list=ls())

#library(devtools)
#install_github("geotoppoints",username="ecor")
#install_github("geotopbricks",username="ecor")


library(geotopbricks)
library(reshape2)
library(ggplot2)
#library(RClimMAWGEN)
#library(gridExtra)
library(geotoppoints)
library(scales)

# vedere su https://github.com/ecor/geotoppoints
# poi inst / examples

#wpath <- "F:/idroclima_simulazioni/Trentino_500_dstr_GEOtop_2_012" # disco "backup Emanuele C. e Annalisa"
#wpath <- "F:/IDROCLIMA/simulazioni_geotop/Trentino_500_dstr_GEOtop_2_012" # Gongolo
wpath <- "/home/ecor/attivita/2014/caprioli/simulazioni/rendena100m_20140807_v2"
wpath <- "/Volumes/GONGOLO/caprioli/rendena100m_20140807_v2"

ContinuousRecovery <- 3 # nr. di interruzioni della simulazione (vedere nella cartella tabs i file di tipo "xxxxxx_crec0001.txt" e cercare il nr. pi� alto)

max <- 20


CoordinatePointX <- get.geotop.inpts.keyword.value("CoordinatePointX",wpath=wpath,numeric=TRUE)[1:max]
CoordinatePointY <- get.geotop.inpts.keyword.value("CoordinatePointY",wpath=wpath,numeric=TRUE)[1:max]
CoordinatePointCode <- str_split(get.geotop.inpts.keyword.value("CoordinatePointCode",wpath=wpath),pattern=",")[[1]][1:max]
CoordinatePointName <- str_split(get.geotop.inpts.keyword.value("CoordinatePointName",wpath=wpath),pattern=",")[[1]][1:max]
CoordinatePointLandCover<- get.geotop.inpts.keyword.value("CoordinatePointLandCover",wpath=wpath,numeric=TRUE)[1:max]
CoordinatePointElevation <- get.geotop.inpts.keyword.value("CoordinatePointElevation",wpath=wpath,numeric=TRUE)[1:max]


CoordinatePointNumber <- 1:length(CoordinatePointCode)

names(CoordinatePointCode) <- CoordinatePointCode
names(CoordinatePointX) <- CoordinatePointCode
names(CoordinatePointY) <- CoordinatePointCode
names(CoordinatePointName) <- CoordinatePointCode
names(CoordinatePointNumber) <- CoordinatePointCode
names(CoordinatePointLandCover) <- CoordinatePointCode
names(CoordinatePointElevation) <- CoordinatePointCode


level <- c("FEM11","FEM90","FEM23", "T0179","T0175") ###,"FEM90","T0433")
level <- CoordinatePointNumber[level]
level <- sort(level)


tz <-  "Etc/GMT+1"


start <- as.POSIXct("2013-11-01",tz=tz)
end <- as.POSIXct("2014-05-01",tz=tz)




pointvalues <- get.geotop.inpts.keyword.value("PointOutputFile",wpath=wpath,data.frame=TRUE,level=level,start_date=start,end_date=end,date_field="Date12.DDMMYYYYhhmm.",ContinuousRecovery=ContinuousRecovery)
names(pointvalues) <- names(level)

#id <- names(pointvalues[[1]]) # tutte
#id <- c("Prain_over_canopy.mm.","Prain_under_canopy.mm.","LE.W.m2.","Evap_surface.mm.","Trasp_canopy.mm.")
#id <- c("Evap_surface.mm.","Trasp_canopy.mm.")
#id <- c("SWin.W.m2.", "LE.W.m2.")
#id <- c("Wind_speed.m.s.", "Wind_speed_top_canopy.m.s.")


#[1] "JulianDayFromYear0.days."      "TimeFromStart.days."          
#[3] "Simulation_Period"             "Run"                          
#[5] "IDpoint"                       "Psnow_over_canopy.mm."        
#[7] "Prain_over_canopy.mm."         "Psnow_under_canopy.mm."       
#[9] "Prain_under_canopy.mm."        "Prain_rain_on_snow.mm."       
#[11] "Wind_speed.m.s."               "Wind_direction.deg."          
#[13] "Relative_Humidity..."          "Pressure.mbar."               
#[15] "Tair.C."                       "Tdew.C."                      
#[17] "Tsurface.C."                   "Tvegetation.C."               
#[19] "Tcanopyair.C."                 "Surface_Energy_balance.W.m2." 
#[21] "Soil_heat_flux.W.m2."          "SWin.W.m2."                   
#[23] "SWbeam.W.m2."                  "SWdiff.W.m2."                 
#[25] "LWin.W.m2."                    "LWin_min.W.m2."               
#[27] "LWin_max.W.m2."                "SWnet.W.m2."                  
#[29] "LWnet.W.m2."                   "H.W.m2."                      
#[31] "LE.W.m2."                      "Canopy_fraction..."           
#[33] "LSAI.m2.m2."                   "z0veg.m."                     
#[35] "d0veg.m."                      "Estored_canopy.W.m2."         
#[37] "SWv.W.m2."                     "LWv.W.m2."                    
#[39] "Hv.W.m2."                      "LEv.W.m2."                    
#[41] "Hg_unveg.W.m2."                "LEg_unveg.W.m2."              
#[43] "Hg_veg.W.m2."                  "LEg_veg.W.m2."                
#[45] "Evap_surface.mm."              "Trasp_canopy.mm."             
#[47] "Water_on_canopy.mm."           "Snow_on_canopy.mm."           
#[49] "Qvegetation..."                "Qsurface..."                  
#[51] "Qair..."                       "Qcanopyair..."                
#[53] "LObukhov.m."                   "LObukhovcanopy.m."            
#[55] "Wind_speed_top_canopy.m.s."    "Decay_of_K_in_canopy..."      
#[57] "SWup.W.m2."                    "LWup.W.m2."                   
#[59] "Hup.W.m2."                     "LEup.W.m2."                   
#[61] "snow_depth.mm."                "snow_water_equivalent.mm."    
#[63] "snow_density.kg.m3."           "snow_temperature.C."          
#[65] "snow_melted.mm."               "snow_subl.mm."                
#[67] "snow_blown_away.mm."           "snow_subl_while_blown.mm."    
#[69] "glac_depth.mm."                "glac_water_equivalent.mm."    
#[71] "glac_density.kg.m3."           "glac_temperature.C."          
#[73] "glac_melted.mm."               "glac_subl.mm."                
#[75] "lowest_thawed_soil_depth.mm."  "highest_thawed_soil_depth.mm."
#[77] "lowest_water_table_depth.mm."  "highest_water_table_depth.mm."




id <- c("snow_depth.mm.","snow_water_equivalent.mm.","snow_temperature.C.","snow_subl.mm.","Tair.C.","Tsurface.C.","Tvegetation.C.","Prain_over_canopy.mm.","Psnow_under_canopy.mm.","Prain_under_canopy.mm.","Prain_rain_on_snow.mm.","Psnow_over_canopy.mm.","snow_water_equivalent.mm.","Psnow_under_canopy.mm.","SWin.W.m2.","snow_density.kg.m3.", "Tsurface.C.", "Surface_Energy_balance.W.m2." )     


# only id columuns are saved
pointvalues_rev <- lapply(X=pointvalues,FUN=function(x,id){
						
						out <- x[,id]
						
						str(out)
						return(out)
						
		},id=id)


#####  VERIFICA TEMPERATURE 
checkMeteoStation=TRUE
if (checkMeteoStation) {
	MeteoStationCode <- str_split(get.geotop.inpts.keyword.value("MeteoStationCode",wpath=wpath),pattern=",")[[1]]
	names(MeteoStationCode) <- MeteoStationCode
	MeteoStationIndex <- 1:length(MeteoStationCode)
	names(MeteoStationIndex) <- MeteoStationCode


	level_meteo <- MeteoStationIndex[names(level)]

	


	meteovalues <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
        level=level_meteo,start_date=start,end_date=end)
	names(meteovalues) <- names(level_meteo)
	
	
	
	
	meteovalues <- lapply(X=meteovalues,FUN=function(x){
				out <- x[,c("Iprec","AirT")]
				names(out) <- c("P_obs.mm.","Tair_obs.C.")
				return(out)
				
			})

}
##### END VERIFICA TEMPERATURE 




time_aggregation <- 3600 ## hourly
# creo i melt

# media
pointvalues_melt <- meltFromZooList(x=pointvalues_rev,aggregate=time_aggregation,FUN=mean)
meteovalues_melt <- meltFromZooList(x=meteovalues,aggregate=time_aggregation,FUN=mean)

##pointvalues_melt <- rbind(meteovalues_melt,pointvalues_melt)
variables <- unique(pointvalues_melt$variable)
locations <- unique(pointvalues_melt$L1)


# scegli
#variable <- c("SWin.W.m2.")
#variable <- c("Wind_speed.m.s.", "Wind_speed_top_canopy.m.s.")
#variable <- c("Evap_surface.mm.","Trasp_canopy.mm.")
#variable <- c("Evap_surface.mm.","Trasp_canopy.mm.","EP.mm")
#variable <- c("psi_z580.000000","psi_z1430.000000","psi_z3530.000000","psi_z16030.000000")
#variable <- c("theta_z80.000000","theta_z230.000000","theta_z580.000000","theta_z1430.000000")
#variable <- c("theta_z80.000000","theta_z230.000000","theta_z580.000000")
#variable <- c("psi_z80.000000","psi_z230.000000","psi_z580.000000")
#variable <- c("temp_z80.000000","temp_z230.000000","temp_z580.000000")
#variable <- c("Prain_over_canopy.mm.","Prain_under_canopy.mm.","EP.mm")

variable <- c("Tair.C.","snow_depth.mm.","Psnow_under_canopy.mm.","Prain_under_canopy.mm.","snow_water_equivalent.mm.", "Tsurface.C." , "Surface_Energy_balance.W.m2.")


pointvalues_plot <- pointvalues_melt[pointvalues_melt$variable %in% variable & pointvalues_melt$Time >= start & pointvalues_melt$Time <= end,]

# escludo eventualmente siti - altrimenti saltare!
#location <- locations[4:5] 
#pointvalues_plot<-pointvalues_plot[pointvalues_plot$L1 %in% location,]





qp <- qplot(Time, value, data = pointvalues_plot, geom = "line", group = variable) +
  facet_grid(variable ~ L1, scale = "free_y") ##+ scale_x_datetime(breaks=date_breaks("3 year"), labels=date_format("%Y")) 

# alternativa: l'ho salvato avendolo creato altrove
#load("D:/Documents and Settings/eccele/Documenti/OTC/IdroClima/simulazioni/010 grafici puntiformi/evapotrasp punt si. 11 2006-2007")

print(qp)
pdf <- "./plot/n_"
suffix <- paste(as.character(start),as.character(end),sep="_")
pdf <- paste(pdf,suffix,".pdf",sep="")
dev.off()

pdf(pdf)
print(qp)
dev.off()


