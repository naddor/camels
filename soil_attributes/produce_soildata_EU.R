###===============================###===============================###
### CAMELS-CH: computation of soil attributes
### Martina Kauzlaric
### 28.11.2022
### University of Bern
### martina.kauzlaric@giub.unibe.ch and kauzlaric.martina@gmail.com
###===============================###===============================###
###===============================###===============================###
### (1) Read in catchment shapes
### (2) Compute soil attributes
###===============================###===============================###

###===============================###===============================###
### load packages
###===============================###===============================###
### loading shape files
library(sf)
library(reshape)
library(dplyr)
library(raster)
library(exactextractr)

###===============================###===============================###
### (1) Read in catchment shapes
###===============================###===============================###
### read in CAMELS-CH catchment shapes
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Catchments', 'CAMELS_CH_EZG_7.6', sep = '/'))
catch <- st_read('CAMELS_CH_EZG_76.shp')

#Plot catchments' overview
plot(st_geometry(catch))
#Look at data attached
head(catch)

###===============================###===============================###
### (2) Compute soil attributes
###===============================###===============================###
# 18 attributes (11 actually, as some are reported x2 because of different data sources):
# *sand percentage[%]  <=> Weight percentage of the sand particles (0.05-2 mm) SNDPPT [%]
# *sand percentage_ESDD[%]  <=> Sand content STU_EU_SAND [%]
# *silt percentage [%]  <=> Weight percentage of the silt particles (0.0002-0.05 mm) SLTPPT [%]
# *silt percentage_ESDD [%]  <=> Sand content STU_EU_SILT [%]
# *clay percentage[%]  <=> Weight percentage of the clay particles (<0.0002 mm) CLYPPT [%]
# *clay percentage_ESDD[%]  <=> Sand content STU_EU_CLAY [%]
# *percentage organic content [%]  <=> 	Soil organic carbon content ORCDRC [permille]/10
# *percentage organic content_ESDD [%]  <=> 	Organic carbon content STU_EU_OC [%]
# *bulk density [g/cm3]  <=> bulk density BLDFIE [kg/m3]/1000
# *bulk density_ESDD [g/cm3]  <=> bulk density STU_EU_BD [g/m3]
# *total available water content [mm]  <=> AWCh1 [%]/100*1000
# *total available water content_ESDD [mm]  <=> Total available water content from PTF STU_EU_TAWC[mm]
# *porosity [-] <=> saturated volumetric water content THS [cm3/cm3] *100 / 100 
# *conductivity [cm/h] <=> saturated hydraulic conductivity KS [cm/day] *100 /(100*24)
# *root depth [m] <=> depth to bedrock (R horizon) up to 200cm BDRICM [cm]/10
# *root depth_ESDD [m] <=> depth available to roots STU_EU_DEPTH_ROOTS [cm]/100
# *soil depth [m] <=> absolute depth to bedrock BDTICM [cm]/10
# *coarse fragments_ESDD [%]  <=> Coarse fragments  STU_EU_GRAVEL [%]

###===============================###===============================###
# for each attribute we compute average;5-,25-,50-,75-,95-percentiles;
# skewness and the percentage of no data within the catchment
###===============================###===============================###

### set working directory
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData', Sys.getenv('CAMELS_COUNTRY'), sep = '/'))
wdir <- getwd()

### set attribute names
attr.nm <- c('sand_perc', 'sand_perc_ESDD',
             'silt_perc', 'silt_perc_ESDD',
             'clay_perc', 'clay_perc_ESDD',
             'organic_perc', 'organic_perc_ESDD',
             'bulkdens', 'bulkdens_ESDD',
             'tawc', 'tawc_ESDD',
             'porosity', 'conductivity',
             'root_depth', 'root_depth_ESDD',
             'soil_depth', 'coarse_fragm_perc_ESDD')
attr.nm.spec <- c('', '_5', '_25', '_50', '_75', '_90', '_skewness', '_missing')
attr.nm.all <- sapply(X = attr.nm, FUN = function(x) paste0(x, attr.nm.spec))
attr.nm.all <- melt(attr.nm.all)
attr.nm.all <- as.character(attr.nm.all$value)

### preallocate data frame to store soil attributes
df_soilattr <- as.data.frame(matrix(NA, dim(catch)[1], 1 + length(attr.nm) * length(attr.nm.spec)),
                             stringsAsFactors = FALSE)
df_soilattr[, 1] <- catch$gauge_id
colnames(df_soilattr) <- c('gauge_id', attr.nm.all)
rm(attr.nm.spec)

### function for computing all parameters per attribute
### i.e. average, percentiles, skewness, percentage of missing data

## set percentiles
perc <- c(0.05, 0.25, 0.50, 0.75, 0.95)

ext.param.attr <- function(rs, perc, col.nm) {

  df <- df_soilattr[, match(col.nm, colnames(df_soilattr))]

  ## NOTE on exact_extract function:
  ## In all of the summary operations, NA values in the the primary raster (x) raster are ignored (i.e., na.rm = TRUE.)

  df[, 1] <- exact_extract(rs, st_transform(catch, crs(rs)), 'mean')

  df[, 2:6] <- exact_extract(rs, st_transform(catch, crs(rs)), 'quantile', quantiles = perc)

  df[, 7] <- (df[, 1] - df[, grep('_50', colnames(df))]) / exact_extract(rs, st_transform(catch, crs(rs)), 'stdev')

  df[, 8] <- 100 - round(exact_extract(rs, st_transform(catch, crs(rs)), 'count', coverage_area = TRUE) /
                           as.numeric(st_area(st_transform(catch, crs(rs)))), 4) * 100

  return(df)

}

### set variables needed for computing soil attributes 
variables <- c('SNDPPT', 'SLTPPT', 'CLYPPT', 'ORCDRC', 'BLDFIE', 'AWCh1', 'THS',
               'KS', 'BDTICM', 'BDRICM', 'SAND', 'SILT', 'CLAY', 'OC', 'BD',
               'GRAVEL', 'TAWC', 'DEPTH_ROOTS')

for (var.nm in variables) {

  if (var.nm == 'SNDPPT') {

    raster.nm <- list.files(wdir)[grep('_M_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('sand_perc', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('sand_perc', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'SAND') {

    raster.nm <- list.files(wdir)[grep('_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep('STU_EU_M', raster.nm)]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('sand_perc_ESDD', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('sand_perc_ESDD', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'SLTPPT') {

    raster.nm <- list.files(wdir)[grep('_M_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('silt_perc', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('silt_perc', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'SILT') {

    raster.nm <- list.files(wdir)[grep('_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep('STU_EU_M', raster.nm)]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('silt_perc_ESDD', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('silt_perc_ESDD', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'CLYPPT') {

    raster.nm <- list.files(wdir)[grep('_M_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('clay_perc', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('clay_perc', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'CLAY') {

    raster.nm <- list.files(wdir)[grep('_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep('STU_EU_M', raster.nm)]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('clay_perc_ESDD', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('clay_perc_ESDD', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'ORCDRC') {

    raster.nm <- list.files(wdir)[grep('_M_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('organic_perc', colnames(df_soilattr))]

    rs <- raster(raster.nm) / 10

    df_soilattr[, grep('organic_perc', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'OC') {

    raster.nm <- list.files(wdir)[grep('_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep('STU_EU_M', raster.nm)]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('organic_perc_ESDD', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('organic_perc_ESDD', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'BLDFIE') {

    raster.nm <- list.files(wdir)[grep('_M_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('bulkdens', colnames(df_soilattr))]

    rs <- raster(raster.nm) / 1000

    df_soilattr[, grep('bulkdens', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'BD') {

    raster.nm <- list.files(wdir)[grep('_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep('STU_EU_M', raster.nm)]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('bulkdens_ESDD', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('bulkdens_ESDD', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'AWCh1') {

    raster.nm <- list.files(wdir)[grep('_M_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('tawc', colnames(df_soilattr))]

    rs <- raster(raster.nm) * 10

    df_soilattr[, grep('tawc', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'TAWC') {

    raster.nm <- list.files(wdir)[grep('_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep('STU_EU_M', raster.nm)]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('tawc_ESDD', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('tawc_ESDD', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'THS') {

    raster.nm <- list.files(wdir)[grep('_M_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('porosity', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('porosity', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'KS') {

    raster.nm <- list.files(wdir)[grep('_M_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('conductivity', colnames(df_soilattr))]

    rs <- raster(raster.nm) / 24

    df_soilattr[, grep('conductivity', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'BDRICM') {

    raster.nm <- list.files(wdir)[grep('_M_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('root_depth', colnames(df_soilattr))]

    rs <- raster(raster.nm) / 10

    df_soilattr[, grep('root_depth', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

    'root_depth'

  }

  if (var.nm == 'DEPTH_ROOTS') {

    raster.nm <- list.files(wdir)[grep('_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep('STU_EU', raster.nm)]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('root_depth_ESDD', colnames(df_soilattr))]

    rs <- raster(raster.nm) / 100

    df_soilattr[, grep('root_depth_ESDD', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'BDTICM') {

    raster.nm <- list.files(wdir)[grep('_M_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('soil_depth', colnames(df_soilattr))]

    rs <- raster(raster.nm) / 10

    df_soilattr[, grep('soil_depth', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

  if (var.nm == 'GRAVEL') {

    raster.nm <- list.files(wdir)[grep('_camelsCH.tif', list.files(wdir))]
    raster.nm <- raster.nm[grep('STU_EU_M', raster.nm)]
    raster.nm <- raster.nm[grep(var.nm, raster.nm)]
    col.nm <- colnames(df_soilattr)[grep('coarse_fragm_perc_ESDD', colnames(df_soilattr))]

    rs <- raster(raster.nm)

    df_soilattr[, grep('coarse_fragm_perc_ESDD', colnames(df_soilattr))] <- ext.param.attr(rs, perc, col.nm)

  }

}

### save extracted attributes
setwd(Sys.getenv('CAMELS_DIR_RESULTS'))
save(file = 'soil_attributes.RData', df_soilattr)

### write to file, use semicolon for separation
setwd(Sys.getenv('CAMELS_DIR_RESULTS'))

write.table(file = 'CAMELS_CH_soil_attributes.csv',
            round(df_soilattr, 3),
            sep = ';', quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")

### clear workspace
rm(list = ls())
