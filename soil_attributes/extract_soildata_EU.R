###===============================###===============================###
### CAMELS-CH: preprocessing of soil data
### Martina Kauzlaric
### 28.11.2022
### University of Bern
### martina.kauzlaric@giub.unibe.ch and kauzlaric.martina@gmail.com
###===============================###===============================###
###===============================###===============================###
# First get the data you need and save these in a folder called SoilData
# in your 'CAMELS_DIR_DATA' directory

setwd(paste(Sys.getenv('CAMELS_DIR_DATA')))
dir.create('Soil')
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil', sep = '/'))
dir.create('SoilData')

# **For SoilGrid (global coverage)
# Either download manually from
# https://files.isric.org/soilgrids/former/2017-03-10/data/
# (Refer also to https://github.com/ISRICWorldSoil/SoilGrids250m)

# or download data using WCS from
# https://soilgrids.org/
# See https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/wcs_from_R.md for a tutorial

# Save data in Sys.getenv('CAMELS_DIR_DATA')/Soil/SoilData/SoilGrid/data

# **For EU-SoilHydroGrids (3D soil hydraulic database of Europe)
# Submit request
# https://esdac.jrc.ec.europa.eu/content/3d-soil-hydraulic-database-europe-1-km-and-250-m-resolution

# Save data in Sys.getenv('CAMELS_DIR_DATA')/SoilData/EU_SoilHydroGrids_250m
# and/or Sys.getenv('CAMELS_DIR_DATA')/SoilData/EU_SoilHydroGrids_1km

# **For European Soil Database Derived data (Europe, 1km resolution, soil topological units; ESDD)
# Submit request
# https://esdac.jrc.ec.europa.eu/content/european-soil-database-derived-data (European Soil Database Derived data)

# Save data in Sys.getenv('CAMELS_DIR_DATA')/Soil/SoilData/STU_EU_Layers

# from SoilGrid we get
#-------------------------
#@for 7 layers resp. depths (0 cm,5 cm,15 cm,30 cm,60 cm,100 cm,200 cm)
# *sand percentage[%]  <=> Weight percentage of the sand particles (0.05-2 mm) SNDPPT [%]
# *silt percentage [%]  <=> Weight percentage of the silt particles (0.0002-0.05 mm) SLTPPT [%]
# *clay percentage[%]  <=> Weight percentage of the clay particles (<0.0002 mm) CLYPPT [%]
# *percentage organic content [%]  <=> 	Soil organic carbon content ORCDRC [permille]/10
# *bulk density [g/cm3]  <=> bulk density BLDFIE [kg/m3]*1000
# *total available water content [mm]  <=> AWCh1 [%]/100*1000
#  AWCh1 [%] Available soil water capacity (volumetric fraction) with FC = pF 2.0
#  (see also https://groups.google.com/g/global-soil-information/c/NNwCT3mqXRM/m/vP-qjJpdBQAJ)
#  Total available water (TAW) is defined as the difference between the water content at field capacity (FC) and at wilting point (WP)
#  (FC = AWCH1 + WWP; see also https://groups.google.com/g/global-soil-information/c/SMJzAXBhS08/m/Lc15MjMyBQAJ)
#@for the whole soil profile (1 layer)
# *soil depth [m] <=> absolute depth to bedrock BDTICM [cm]/10
# *root depth [m] <=> depth to bedrock (R horizon) up to 200cm BDRICM [cm]/10

# See also https://www.isric.org/explore/soilgrids/faq-soilgrids-2017 for more information (there are also more available data)

# from EU-SoilHydroGrids we get
#-------------------------------
#@for 7 layers resp. depths (0 cm,5 cm,15 cm,30 cm,60 cm,100 cm,200 cm)
# *porosity [-] <=> saturated volumetric water content THS [cm3/cm3] *100 / 100 
# *conductivity [cm/h] <=> saturated hydraulic conductivity KS [cm/day] *100 /(100*24)

# from European Soil Database Derived data we get
#-------------------------------------------------
#@for topsoil(T; first 30 cm) and subsoil(S; root depth-30cm)
# *sand percentage[%]  <=> Sand content STU_EU_T&S_SAND [%]
# *silt percentage [%]  <=> Sand content STU_EU_T&S_SILT [%]
# *clay percentage[%]  <=> Sand content STU_EU_T&S_CLAY [%]
# *percentage organic content [%]  <=> 	Organic carbon content STU_EU_T&S_OC [%]
# *bulk density [g/cm3]  <=> bulk density STU_EU_T&S_BD [g/m3]
# *total available water content [mm]  <=> Total available water content from PTF STU_EU_T&S_TAWC[mm]
# *coarse fragments [%]  <=> Coarse fragments  STU_EU_T&S_GRAVEL [%]
#@for the whole soil profile (1 layer)
# *root depth [m] <=> depth available to roots STU_EU_DEPTH_ROOTS [cm]/100

#For Europe alternative data sources for some attributes can be found also here: 
# https://esdac.jrc.ec.europa.eu/content/spade-14

###===============================###===============================###
###===============================###===============================###
### (1) Read in catchment shapes
### (2) Get and crop soil data
### (3) Make weigthed average of multilayer data
###===============================###===============================###

###===============================###===============================###
### load packages
###===============================###===============================###
### loading shape files
#library(rgdal) => if using readOGR
library(sf)
library(raster)
library(exactextractr)

### define soil grid resolution
SoilGrid.res <- '250m'

###===============================###===============================###
### (1) Read in catchment shapes and get their extent
###===============================###===============================###
### read in CAMELS-CH catchment shapes
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Catchments', 'CAMELS_CH_EZG_7.6', sep = '/'))
catch <- st_read('CAMELS_CH_EZG_76.shp')
#plot catchments' overview
plot(st_geometry(catch))
#Look at data attached
head(catch)

#Swiss projections
#CH1903: crs("+init=epsg:21781")
#PROJ4: +init=epsg:21781 +proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs
#CH1903+/LV95: crs("+init=epsg:2056")
#PROJ4: +proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs

#European projections
#ETRS89: crs("+init=epsg:3035")
#PROJ4: +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs

#World projections
#WGS84: crs("+init=epsg:4326")
#PROJ4: +proj=longlat +datum=WGS84 +no_defs +type=crs
#WGS84 with Azimuthal Equidistant projection
#PROJ4: +proj=aeqd +lat_0=53 +lon_0=24 +x_0=5837287.81977 +y_0=2121415.69617 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 

### generate mask of all catchments
catch.mask <- st_union(catch)
catch.bb <- st_buffer(st_as_sfc(st_bbox(catch.mask)), 1000)
#plot mask
plot(st_geometry(catch.mask))
plot(st_geometry(catch.bb), add = TRUE)

###===============================###===============================###
### (2) Get and crop soil data to catchments' bounding box
###===============================###===============================###

### create directory to store data needed to extract attributes
dir.create(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData',
                 Sys.getenv('CAMELS_COUNTRY'), sep = '/'))

#**=========***=========***
#** EU-SoilHydroGrids
#**=========***=========***

### define variales and layers to be extracted from EU-SoilHydroGrids
variables <- c('KS', 'THS')
layers <- c('sl1', 'sl2', 'sl3', 'sl4', 'sl5', 'sl6', 'sl7')

### extract data for catch.mask
if (SoilGrid.res == '250m') {

  ### load tiles of EU_SoilHydroGrids and get those covering your country (resp. your catch.mask)

  setwd(paste(Sys.getenv('CAMELS_DIR_DATA'),
              'Soil/SoilData', 'LandscapeGeoinformatics-EU-SoilHydroGrids_tiles', 'data', sep = '/'))

  tiles.EUSoilHydroGrids <- st_read('grid_cells_250m_wgs84.shp')


  dirs <- as.character(tiles.EUSoilHydroGrids$grid_id[
                         st_intersects(st_transform(catch.mask, crs(tiles.EUSoilHydroGrids)),
                                       tiles.EUSoilHydroGrids)[[1]]])

  setwd(paste(Sys.getenv('CAMELS_DIR_DATA'),
              'Soil/SoilData', 'EU_SoilHydroGrids_250m', sep = '/'))

  ### copy all tiles covering your catchment to the folder where you will further process data

  for (dir in dirs) {
    data.list <- list.files(dir, full.names = TRUE)
    file.copy(data.list[grep(paste(variables, collapse = "|"), data.list)],
              paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData',
                    Sys.getenv('CAMELS_COUNTRY'), sep = '/'))
  }

  ### merge copied data to one single file
  ### (you will produce 1 raster for each of the 7 layers)  
  setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData',
              Sys.getenv('CAMELS_COUNTRY'), sep = '/'))
  data.list <- list.files()

  for (v in 1:length(variables)) {
    for (l in 1:length(layers)) {

      r.list <- lapply(data.list[grep(variables[v], data.list)]
                       [grep(layers[l], data.list[grep(variables[v], data.list)])],
                       raster)

      writeRaster(do.call(merge, r.list),
                  filename = paste0(variables[v], '_M_', layers[l], '_camels',
                                    Sys.getenv('CAMELS_COUNTRY'), '.tif'))
    }
  }

  file.remove(data.list)
  rm(tiles.EUSoilHydroGrids); rm(dirs); rm(r.list); rm(data.list)

} else if (SoilGrid.res == '1km') {

  setwd(paste(Sys.getenv('CAMELS_DIR_DATA'),
              'Soil/SoilData', 'EU_SoilHydroGrids_1km', sep = '/'))

  ### copy all data needed to the folder where you will further process data
  data.list <- list.files(paste(Sys.getenv('CAMELS_DIR_DATA'),
                                'Soil/SoilData', 'EU_SoilHydroGrids_1km', sep = '/'),
                          full.names = TRUE)
  file.copy(data.list[grep(paste(variables, collapse = "|"), data.list)],
            paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData',
                  Sys.getenv('CAMELS_COUNTRY'), sep = '/'))

  ### extract and save only data covering your catchment
  setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData',
              Sys.getenv('CAMELS_COUNTRY'), sep = '/'))

  for (v in 1:length(variables)) {

    for (l in 1:length(layers)) {

      r.EU <- raster(data.list[grep(variables[v], data.list)]
                     [grep(layers[l], data.list[grep(variables[v], data.list)])])

      ext.catch <- st_bbox(st_transform(catch.mask, crs(r.EU)))

      ext.catch <- extent(x = c(ext.catch['xmin'],
                                ext.catch['xmax'],
                                ext.catch['ymin'],
                                ext.catch['ymax']))

      r.ext <- crop(r.EU, ext.catch)

      writeRaster(r.ext, filename = paste0(variables[v], '_', layers[l], '_camels',
                                           Sys.getenv('CAMELS_COUNTRY'), '.tif'))
    }
  }

  file.remove(data.list)
  rm(data.list); rm(r.EU); rm(r.ext); rm(ext.catch)

}

#**====***====***
#** SoilGrids
#**====***====***
### NOTE: this might take long, as the data are very large (global coverage with 250m resolution)

setwd(paste(Sys.getenv('CAMELS_DIR_DATA'),
            'Soil/SoilData', 'SoilGrid', 'data', sep = '/'))

### define variables and layers to be extracted from SoilGrids
### extract first variables with multiple layers (7 layers)
variables <- c('SNDPPT', 'SLTPPT', 'CLYPPT', 'ORCDRC', 'BLDFIE', 'AWCh1')
layers <- c('sl1', 'sl2', 'sl3', 'sl4', 'sl5', 'sl6', 'sl7')

### list all data needed to be further processed

data.list <- list.files(paste(Sys.getenv('CAMELS_DIR_DATA'),
                              'Soil/SoilData', 'SoilGrid', 'data', sep = '/'), full.names = TRUE)

data.list <- data.list[grep(paste(variables, collapse = "|"), data.list)]
data.list <- data.list[grep('.tif', data.list)]

### extract and save only data covering your catchment
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData',
            Sys.getenv('CAMELS_COUNTRY'), sep = '/'))

for (v in 1:length(variables)) {

  for (l in 1:length(layers)) {

    r.EU <- raster(data.list[grep(variables[v], data.list)]
                   [grep(layers[l], data.list[grep(variables[v], data.list)])])

    ext.catch <- st_bbox(st_transform(catch.mask, crs(r.EU)))

    ext.catch <- extent(x = c(ext.catch['xmin'],
                              ext.catch['xmax'],
                              ext.catch['ymin'],
                              ext.catch['ymax']))

    r.ext <- crop(r.EU, ext.catch)

    writeRaster(r.ext, filename = paste0(variables[v], '_M_', layers[l], '_camels',
                                         Sys.getenv('CAMELS_COUNTRY'), '.tif'))
  }
}

file.remove(data.list)
rm(data.list); rm(r.EU); rm(r.ext); rm(ext.catch); rm(layers); rm(l)

### finally extract variables with one single layer
variables <- c('BDTICM', 'BDRICM')

### list all data needed to be further processed

data.list <- list.files(paste(Sys.getenv('CAMELS_DIR_DATA'),
                              'Soil/SoilData', 'SoilGrid', 'data', sep = '/'), full.names = TRUE)

data.list <- data.list[grep(paste(variables, collapse = "|"), data.list)]
data.list <- data.list[grep('.tif', data.list)]

### extract and save only data covering your catchment
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData',
            Sys.getenv('CAMELS_COUNTRY'), sep = '/'))

for (v in 1:length(variables)) {

  r.EU <- raster(data.list[grep(variables[v], data.list)])

  ext.catch <- st_bbox(st_transform(catch.mask, crs(r.EU)))

  ext.catch <- extent(x = c(ext.catch['xmin'],
                            ext.catch['xmax'],
                            ext.catch['ymin'],
                            ext.catch['ymax']))

  r.ext <- crop(r.EU, ext.catch)

  writeRaster(r.ext, filename = paste0(variables[v], '_M_camels',
                                       Sys.getenv('CAMELS_COUNTRY'), '.tif'))
}

### finally extract variables with one single layer
variables <- c('BDTICM', 'BDRICM')

### list all data needed to be further processed

data.list <- list.files(paste(Sys.getenv('CAMELS_DIR_DATA'),
                              'Soil/SoilData', 'SoilGrid', 'data', sep = '/'), full.names = TRUE)

data.list <- data.list[grep(paste(variables, collapse = "|"), data.list)]
data.list <- data.list[grep('.tif', data.list)]

### extract and save only data covering your catchment
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData',
            Sys.getenv('CAMELS_COUNTRY'), sep = '/'))

for (v in 1:length(variables)) {

  r.EU <- raster(data.list[grep(variables[v], data.list)])

  ext.catch <- st_bbox(st_transform(catch.mask, crs(r.EU)))

  ext.catch <- extent(x = c(ext.catch['xmin'],
                            ext.catch['xmax'],
                            ext.catch['ymin'],
                            ext.catch['ymax']))

  r.ext <- crop(r.EU, ext.catch)

  writeRaster(r.ext,
              filename = paste0(variables[v], '_M_camels',
                                Sys.getenv('CAMELS_COUNTRY'), '.tif'))

}

#**=================***=================***
#** European Soil Database Derived data
#**=================***=================***

setwd(paste(Sys.getenv('CAMELS_DIR_DATA'),
            'Soil/SoilData', 'STU_EU_Layers', sep = '/'))

### define variables and layers to be extracted from European Soil Database Derived data
### extract first variables with multiple layers (2 layers)
variables <- c('SAND', 'SILT', 'CLAY', 'OC', 'BD', 'GRAVEL', 'TAWC')
layers <- c('STU_EU_T', 'STU_EU_S')

### list all data needed to be further processed

data.list <- list.files(paste(Sys.getenv('CAMELS_DIR_DATA'),
                              'Soil/SoilData', 'STU_EU_Layers', sep = '/'), full.names = TRUE)

data.list <- data.list[grep('STU_EU', gsub('STU_EU_Layers', '', data.list))]
data.list <- data.list[grep(paste(variables, collapse = "|"), data.list)]
data.list <- data.list[grep('.rst', data.list)]
data.list <- data.list[-1]

### extract and save only data covering your catchment
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData',
            Sys.getenv('CAMELS_COUNTRY'), sep = '/'))

for (v in 1:length(variables)) {

  for (l in 1:length(layers)) {

    r.EU <- raster(data.list[grep(variables[v], data.list)]
                   [grep(layers[l], data.list[grep(variables[v], data.list)])])
    #crs(r.EU) <- crs("+init=epsg:3035")
    crs(r.EU) <- crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

    ext.catch <- st_bbox(st_transform(catch.mask, crs(r.EU)))

    ext.catch <- extent(x = c(ext.catch['xmin'],
                              ext.catch['xmax'],
                              ext.catch['ymin'],
                              ext.catch['ymax']))

    r.ext <- crop(r.EU, ext.catch)

    writeRaster(r.ext, filename = paste0(layers[l], '_', variables[v], '_camels',
                                         Sys.getenv('CAMELS_COUNTRY'), '.tif'))
  }
}

### finally extract variables with one single layer

variables <- c('DEPTH_ROOTS')

### list all data needed to be further processed

data.list <- list.files(paste(Sys.getenv('CAMELS_DIR_DATA'),
                              'Soil/SoilData', 'STU_EU_Layers', sep = '/'), full.names = TRUE)

data.list <- data.list[grep('STU_EU', gsub('STU_EU_Layers', '', data.list))]
data.list <- data.list[grep(paste(variables, collapse = "|"), data.list)]
data.list <- data.list[grep('.rst', data.list)]


### extract and save only data covering your catchment
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Soil/SoilData',
            Sys.getenv('CAMELS_COUNTRY'), sep = '/'))

for (v in 1:length(variables)) {

  r.EU <- raster(data.list[grep(variables[v], data.list)])
  #crs(r.EU) <- crs("+init=epsg:3035")
  crs(r.EU) <- crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

  ext.catch <- st_bbox(st_transform(catch.mask, crs(r.EU)))

  ext.catch <- extent(x = c(ext.catch['xmin'],
                            ext.catch['xmax'],
                            ext.catch['ymin'],
                            ext.catch['ymax']))

  r.ext <- crop(r.EU, ext.catch)

  writeRaster(r.ext, filename = paste0('STU_EU_', variables[v], '_camels',
                                       Sys.getenv('CAMELS_COUNTRY'), '.tif'))

}

file.remove(data.list)
rm(data.list); rm(r.EU); rm(r.ext); rm(ext.catch); rm(layers); rm(l)

###===============================###===============================###
### (3) Make weigthed average of multilayer data
###===============================###===============================###

#**====***====***
#** SoilGrids
#**====***====***

### define variales to be averaged
variables <- c('SNDPPT', 'SLTPPT', 'CLYPPT', 'ORCDRC', 'BLDFIE', 'AWCh1', 'THS', 'KS')

### set working directory
wdir <- getwd()

for (var.nm in variables) {

  raster.nm <- list.files(wdir)
  raster.nm <- raster.nm[grep(var.nm, raster.nm)]
  r1 <- raster(raster.nm[1])
  allrasters <- lapply(raster.nm, raster)

  ### get data of whole soil profile
  var.brick <- brick(allrasters)
  rm(allrasters)

  ### set all NA to 0 so that it we can integrate over all layers
  var.brick[is.na(var.brick[])] <- 0

  ### make weighted average with trapezoidal integration
  # obtain length of variable of integration and integrand
  n <- nlayers(var.brick)
  d <- c(0, 5, 15, 30, 60, 100, 200) / 100
  # integrate using the trapezoidal rule
  var.soil.ti <- 0.5 * sum((d[2:n] - d[1:(n - 1)]) * (var.brick[[2:n]] + var.brick[[1:(n - 1)]])) / (d[n] - d[1])

  ### reset all NA that are NA at the surface (at 0cm, resp. layer 1)
  var.soil.ti[is.na(r1)] <- NA

  writeRaster(var.soil.ti, filename = paste0(var.nm, '_M_camels',
                                             Sys.getenv('CAMELS_COUNTRY'), '.tif'))

  rm(var.brick); rm(var.soil.ti);

}


#**=================***=================***
#** European Soil Database Derived data
#**=================***=================***

### define variales to be averaged
variables <- c('SAND', 'SILT', 'CLAY', 'OC', 'BD', 'GRAVEL', 'TAWC')
layers <- c('STU_EU_T', 'STU_EU_S')
sd <- 'STU_EU_DEPTH_ROOTS'

### set working directory
wdir <- getwd()

### obtain weights of the two layers: these are the same for all attributes
# top soil=30cm 
r.SD <- raster(list.files(wdir)[grep('STU_EU_DEPTH_ROOTS', list.files(wdir))])
w.T <- 30 / r.SD
#if root depth resp.soil depth =0 cm => weight=0
w.T[r.SD == 0] <- 0
#if root depth resp.soil depth <30cm => weight=1
w.T[r.SD < 30] <- 1
#Check weights
#plot(w.T)
w <- brick(w.T, 1 - w.T)
rm(w.T)

for (var.nm in variables) {

  raster.nm <- list.files(wdir)
  raster.nm <- raster.nm[grep('STU_EU', raster.nm)]
  raster.nm <- raster.nm[grep(var.nm, raster.nm)]
  raster.nm <- rev(raster.nm)
  allrasters <- lapply(raster.nm, raster)

  ### get data of whole soil profile
  var.brick <- brick(allrasters)
  rm(allrasters)

  ### make weighted mean
  var.soil.wm <- weighted.mean(var.brick, w, na.rm = FALSE)

  ### In ESSD where there are no data (e.g. glacierized areas)
  ### values are set to 0, we need to set these to NA to get the right statistics
  ### in the next step. Assumption: where soil depth=0 => NA
  var.soil.wm[r.SD == 0] <- NA

  writeRaster(var.soil.wm, filename = paste0('STU_EU_M_', var.nm, '_camels',
                                             Sys.getenv('CAMELS_COUNTRY'), '.tif'))

  rm(var.brick); rm(var.soil.wm);
}

### clear workspace
rm(list = ls())

