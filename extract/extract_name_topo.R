rm(list = ls())

library(dotenv)
library(here)

### Set paths
require(maps)
require(stringr) # To remove leading blank spaces

source(here::here('extract/extract_elev_bands.R'))

### Load gauge information and basin characteristics for all basins

# Load gauge information - USGS data
file_usgs_data <- paste0(Sys.getenv('CAMELS_DIR_DATA'), 'gauge_information.txt')
gauge_table <- read.table(file_usgs_data, sep = '\t', quote = '', skip = 1, header = FALSE,
                          colClasses = c(rep("factor", 3), rep("numeric", 3)))

if (readLines(file_usgs_data, 1) ==
  'HUC_02  GAGE_ID\t\t\tGAGE_NAME\t\t\t\t\tLAT\t\tLONG\t\tDRAINAGE AREA (KM^2)') {
  # Reformat (add) header
  colnames(gauge_table) <- c('huc_02', 'gauge_id', 'gauge_name', 'gauge_lat', 'gauge_lon', 'area_usgs')
} else {
  stop(paste('Unexpected header in', file_usgs_data))
}

gauge_table$gauge_name <- str_trim(gauge_table$gauge_name, 'left') # Remove leading white spaces

# Attempt to separate catchment name from state
# catchment_name<-rapply(strsplit(as.character(gauge_table$gauge_name),','),function(x) x[1])
# catchment_state<-rapply(strsplit(as.character(gauge_table$gauge_name),','),function(x) x[2])

# Load basin physical characteristics - produced by Andy
file_catchment_table <- paste0(Sys.getenv('CAMELS_DIR_DATA'), '/basin_physical_characteristics.txt')
catchment_table <- read.table(file_catchment_table, header = TRUE,
                              colClasses = c(rep("factor", 2), rep("numeric", 4)))

if (readLines(file_catchment_table, 1) ==
  'BASIN_HUC BASIN_ID Size(km2) Elevation(m) Slope(m_km-1) Frac_Forest(percent)') {
  # Make names nicer and remove units
  colnames(catchment_table) <- c('huc_02', 'gauge_id', 'area_geospa_fabric', 'basin_mean_elev',
                                 'mean_slope', 'frac_forest')
} else {
  stop(paste('Unexpected header in', file_usgs_data))
}

# Merge the two tables
camels_merge <- merge(gauge_table, catchment_table, by = 'gauge_id')
camels_merge <- camels_merge[order(camels_merge$gauge_id),] # Sort catchments by ID
rm(gauge_table, catchment_table)

### Create camels_name
camels_name <- camels_merge[, c('gauge_id', 'huc_02.x', 'gauge_name')]
colnames(camels_name)[2] <- 'huc_02'
#save(camels_name,file=paste(Sys.getenv('CAMELS_DIR_RESULTS'),'camels_name.Rdata',sep=''))
#write.table(camels_name,file=paste(Sys.getenv('CAMELS_DIR_RESULTS'),'camels_name.txt',sep=''),
#            row.names=FALSE,quote=FALSE,sep=';')

### Create camels_topo
camels_topo <- camels_merge[, c('gauge_id', 'gauge_lat', 'gauge_lon', 'basin_mean_elev', 'area_usgs',
                                'area_geospa_fabric', 'mean_slope')]
camels_topo <- data.frame(camels_topo, abs_rel_error_area = abs((camels_topo$area_geospa_fabric -
                          camels_topo$area_usgs) / camels_topo$area_usgs))
#save(camels_topo,file=paste(Sys.getenv('CAMELS_DIR_RESULTS'),'camels_topo.Rdata',sep=''))
#write.table(camels_topo,file=paste(Sys.getenv('CAMELS_DIR_RESULTS'),'camels_topo.txt',sep=''),
#            row.names=FALSE,quote=FALSE,sep=';')

### Create camels_vege_usgs
#camels_vege_usgs<-camels_merge[,c('gauge_id','frac_forest')]
#save(camels_vege_usgs,file=paste(Sys.getenv('CAMELS_DIR_TMP'),'camels_vege_usgs.Rdata',sep=''))

### Create camels_meta_data_elev_bands
if (any(camels_topo$gauge_id != camels_name$gauge_id)) {

  stop('gauge_id in camels_topo and camels_name do not match!')
}

rerr_area <- array() # Relative error in the area
aerr_elev <- array() # Absolute error in the elevation

for (e in 1:dim(camels_name)[1]) {

  id <- camels_name$gauge_id[e]
  huc <- camels_name$huc_02[e]

  # Retrieve catchment area by adding area elevation bands
  elev_tab_format <- extract_elev_bands(id = id, huc = huc, keep_absolute_area = TRUE)
  total_area_elev_bands <- sum(elev_tab_format$area_m2)
  mean_elev <- sum(elev_tab_format$mid_point_elevation * elev_tab_format$area_fraction)

  # Compute area and elevation errors
  rerr_area[e] <- (camels_topo$area_geospa_fabric[camels_topo$gauge_id == id] -
    total_area_elev_bands / 1E6) / camels_topo$area_geospa_fabric[camels_topo$gauge_id == id]
  aerr_elev[e] <- camels_topo$basin_mean_elev[camels_topo$gauge_id == id] - mean_elev

}

par(mfrow = c(2, 2))

summary(rerr_area)
hist(rerr_area)
map('state')
points(camels_topo$gauge_lon, camels_topo$gauge_lat, cex = rerr_area, pch = 16)

summary(aerr_elev)
hist(aerr_elev)
map('state')
points(camels_topo$gauge_lon, camels_topo$gauge_lat, cex = aerr_elev / 75, pch = 16)

#camels_metadata_elev_bands <- data.frame(gauge_id = camels_name$gauge_id,
#                                         elev_bands_rerr_area = rerr_area,
#                                         elev_bands_aerr_elev = aerr_elev)
#save(camels_metadata_elev_bands,
#     file = paste0(Sys.getenv('CAMELS_DIR_TMP'), 'camels_metadata_elev_bands.Rdata'))
