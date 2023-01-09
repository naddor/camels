###===============================###===============================###
### CAMELS-CH: computation of human influence characteristics
### Manuela Brunner
### 06.04.2022
### ETH Zurich and SLF
### manuela.brunner@env.etz.ch
###===============================###===============================###
###===============================###===============================###
### (1) Read in catchment shapes
### (2) compute human influence characteristics
###===============================###===============================###

###===============================###===============================###
### load packages
###===============================###===============================###
### loading shape files
library("maptools")
library(raster)
library(rgeos)
library(sp)
library(rgdal)

### list environmental variables
# Sys.getenv()

### use individual variables
# setwd(Sys.getenv('CAMELS_DIR_DATA'))
###===============================###===============================###
### (1) Read in catchment shapes and reservoir information
###===============================###===============================###
### read in CAMELS-CH catchment shapes
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'),'/CAMELS_CH_EZG_95_v6',sep=''))
catch <- readOGR('CAMELS_CH_EZG_95_v6.shp',encoding='UTF-8',use_iconv = TRUE)
plot(catch)
catch@data

### load reservoir data provided by BFE
setwd(Sys.getenv('CAMELS_DIR_DATA'))
reservoirs <- readOGR('reservoirs_BFE_incl_usable.shp',use_iconv = TRUE, encoding = "UTF-8")
### transform
reservoirs <- spTransform(reservoirs,crs(catch))
plot(reservoirs,add=TRUE,col='blue')
reservoirs@data
length(reservoirs)
###===============================###===============================###
### (2) compute human influence characteristics
###===============================###===============================###
### num_reservoir:	number of reservoirs in the catchment
### count number of reservoirs
###===============================###===============================###
### reservoir_cap: total storage capacity of reservoirs in the catchment in megalitres
### use total volume
### reservoirs@data$Totalvolum
### documentation see BAFU report or Brunner et al. 2019 water scarcity
###===============================###===============================###
### check unit with a well-known reservoir
# reservoirs@data[which(reservoirs@data$FacilityNa=='Mauvoisin'),]$Totalvolum
### Wikipedia total capacity Mauvoisin: 211,500,000 m3
### original unit is Mio m3
### 1m3 = 0.001 ML
### convert total volumes from Mio m3 to ML
reservoirs@data$vol_ML <- reservoirs@data$Totalvolum*(1000)^2/1000

### compute human-influence characteristics
###===============================###===============================###
num_reservoirs <- capacity_reservoirs <- perc_hp <- perc_flood <- perc_irr <- perc_nodata<- c()
reservoir_year_first <- reservoir_year_last <- c()
setwd(Sys.getenv('CAMELS_DIR_RESULTS'))
pdf('reservoirs_per_catchment.pdf',width=12,height=10)
par(mfrow=c(4,4),mar=c(1,1,1,1))
for(l in 1:length(catch)){
  reservoir_crop <- crop(reservoirs,catch[l,])
  if(length(crop(reservoirs,catch[l,]))>0){
    plot(catch[l,],main=catch[l,]@data$ID_BAFU)
    plot(reservoir_crop,add=TRUE,col='blue')
    ### count number of reservoirs in catchment
    ###===============================###===============================###
    num_reservoirs[l] <- length(reservoir_crop)
    ### compute total storage capacity in ML
    ###===============================###===============================###
    capacity_reservoirs[l] <- sum(reservoir_crop@data$vol_ML)
    ### reservoir_he:	percentage of total reservoir storage in catchment used for hydroelectricty
    ###===============================###===============================###
    ### use Zweck1 == H (Hydroelektrizität)
    ### identify all hydropower reservoirs
    ids_hp <- which(reservoir_crop@data$Zweck1=='H (Hydroelektrizität)')
    ### compute total storage capacity of hydropower reservoirs
    vol_hp <- sum(reservoir_crop@data$vol_ML[ids_hp])
    ### percentage of total storage
    perc_hp[l] <- vol_hp/capacity_reservoirs[l]
    
    ### reservoir_fs:	percentage of total reservoir storage in catchment used for flood storage
    ### use Zweck1 == C (Hochwasserrückhalt, Geschiebesperre)
    ###===============================###===============================###
    ### identify all flood retention
    ids_flood <- which(reservoir_crop@data$Zweck1=='C (Hochwasserrückhalt, Geschiebesperre)')
    ### compute total storage capacity of hydropower reservoirs
    vol_flood <- sum(reservoir_crop@data$vol_ML[ids_flood])
    ### percentage of total storage
    perc_flood[l] <- vol_flood/capacity_reservoirs[l]
    
    ### not there in UK dataset but maybe of interest: percentage of total reservoir storage in catchment for irrigation
    #### I (Bewässerung)
    ###===============================###===============================###
    ### identify all irrigation reservoirs
    ids_irr <- which(reservoir_crop@data$Zweck1=='I (Bewässerung)')
    ### compute total storage capacity of hydropower reservoirs
    vol_irr <- sum(reservoir_crop@data$vol_ML[ids_irr])
    ### percentage of total storage
    perc_irr[l] <- vol_irr/capacity_reservoirs[l]
    
    ### reservoir_nousedata: percentage of total reservoir storage where no use data were available
    ###===============================###===============================###
    ### necessary?, going to be 0 everywhere 
    perc_nodata[l] <- 0
    
    ### reservoir_year_first:	year the first reservoir in the catchment was built
    ### see year of construction reservoirs@data$Constructi
    ###===============================###===============================###
    reservoir_year_first[l] <- min(as.numeric(reservoir_crop@data$Constructi))
    
    ### reservoir_year_last: year the last reservoir in the catchment was built
    ###===============================###===============================###
    reservoir_year_last[l] <- max(as.numeric(reservoir_crop@data$Constructi))
  }else{
    ### 0 values if no reservoirs are present
    num_reservoirs[l] <- 0
    capacity_reservoirs[l] <- 0
    perc_hp[l] <- NA
    perc_flood[l] <- NA
    perc_irr[l] <- NA
    perc_nodata[l] <- NA
    reservoir_year_first[l] <- NA
    reservoir_year_last[l] <- NA
  }
}
dev.off()

### save extracted characteristics
setwd(Sys.getenv('CAMELS_DIR_RESULTS'))
save(file='human_impact_char.RData', num_reservoirs, capacity_reservoirs, perc_hp, perc_flood,
     perc_irr, perc_nodata, reservoir_year_first, reservoir_year_last)

### combine information in dataframe
df_reservoirs <- data.frame('gauge_id'=catch@data$ID_BAFU,'num_reservoir'=num_reservoirs,
                            'reservoir_cap'=capacity_reservoirs,'reservoir_he'=perc_hp,
                            'reservoir_fs'=perc_flood,'reservoir_irr'=perc_irr,
                            'reservoir_nousedata'=perc_nodata,'reservoir_year_first'=reservoir_year_first,
                            'reservoir_year_last'=reservoir_year_last)

### write to file, use semicolon for separation
setwd(Sys.getenv('CAMELS_DIR_RESULTS'))
write.table(file='CAMELS_CH_humaninfluence_attributes.txt',df_reservoirs,sep=';')


