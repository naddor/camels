rm(list=ls())

library(maps)
library(RColorBrewer)
require(fields) # for color bar
require(ncdf4)

# SET GENERIC PATHS
hostname<-system('hostname',intern=TRUE)

if(hostname=='eugene'|substr(hostname,1,3)=='vpn'){

  source('/Volumes/d1/naddor/hc1_home/scripts/r_scripts/set_paths.R')

} else {

  source('/home/naddor/scripts/r_scripts/set_paths.R')

}

# SET CAMELS PATHS
camels_version<-'2.1'
set_camels_paths(camels_version)

if(!dir.exists(dir_camels_attr)){
  dir.create(dir_camels_attr)
  dir.create(dir_camels_attr_temp)
}

# LOAD FUNCTIONS
#source(paste(dir_r_scripts,'camels/clim/clim_indices.R',sep=''))
#source(paste(dir_r_scripts,'camels/maps/plot_maps_camels.R',sep=''))  # for maps
#source(paste(dir_r_scripts,'camels/time/time_tools.R',sep=''))  # for maps

# DEFINE DATA SET ATTRBIUTES
# V1.2 of the time series: Updated Daymet and NLDAS basin mean forcing through 12/31/2014 for all basins.
# V1.2 of the time series: Updated Daymet and NLDAS Snow-17/SAC model output through 12/31/2014 (or end of observed record) for all basins.

dataset_start<-as.Date('19801001','%Y%m%d') # start of water year
dataset_end<-as.Date('20140930','%Y%m%d')   # end of water year
dataset_days<-seq(dataset_start,dataset_end,by='day')

# EXTRACT ATTRIBUTES
source(paste(dir_r_scripts,'camels/extract/extract_name_topo.R',sep=''),echo=TRUE)               # catch_name.Rdata, catch_topo.Rdata

print('Extracting metadata on hydrological reccord and SAC accuracy...')
source(paste(dir_r_scripts,'camels/extract/extract_hydro_metadata_sac.R',sep=''))      # catch_hydro_metadata.Rdata, catch_accuracy_sac.Rdata

print('Extracting climate indices and hydrological signatures...')
source(paste(dir_r_scripts,'camels/extract/extract_climate_hydro_sign.R',sep=''))      # catch_clim.Rdata, catch_hydro_obs.Rdata, catch_hydro_sac.Rdata

print(paste('Pearson correlation coefficient for fraction of precip falling as snow =',round(cor(catch_clim$frac_snow_sine,catch_clim$frac_snow_daily),2)))

print('Extracting soil attributes...')
source(paste(dir_r_scripts,'camels/extract/extract_soil.R',sep=''))                    # catch_soil_statsgo.Rdata, catch_soil_pelletier.Rdata, catch_soil.Rdata

print('Extracting vegetation attributes...')
source(paste(dir_r_scripts,'camels/extract/extract_vege.R',sep=''))                    # catch_vege.Rdata

# COMBINE METADATA
# source(paste(dir_r_scripts,'camels/extract/extract_metadata.R',sep=''))              # catch_name.Rdata, catch_topo.Rdata

# CLUSTERING
clus_berg<-data.frame(gage_id=catch_clim$gage_id,clus=apply(catch_clim[,2:4],1,function(x) compute_cluster_berghuijs(x[1],x[2],x[3])))

# MERGE ALL THE TABLES
list_df=list(catch_name,catch_topo,catch_soil,catch_vege,catch_clim,catch_hydro_obs,catch_hydro_sac,catch_accuracy_sac,catch_metadata)
catch_all<-Reduce(function(...) merge(..., all=TRUE,by='gage_id'), list_df)

# SAVE ALL THE ATTRIBUTES IN SPEARATE TABLES BUT IN THE SAME FILE
save(catch_name,catch_topo,catch_soil,catch_vege,catch_clim,catch_hydro_obs,catch_hydro_sac,catch_accuracy_sac,catch_metadata,
     catch_all,
     file=paste(dir_catch_attr,'catch_all.Rdata',sep=''))

write.table(catch_all,file=paste(dir_catch_attr,'catch_all.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')
