rm(list = ls())

library(dotenv)
library(maps)
library(rgdal)
library(raster)
library(rgeos)

# The input file geodatabase and read the feature class
# to List all feature classes in a file geodatabase use:
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
# print(fc_list)

### LiMW
fgdb <- file.path(Sys.getenv('CAMELS_DIR_DATA'), 'LiMW_GIS_2015.gdb')
fc <- readOGR(dsn = fgdb, layer = "GLiM_export")

# Change projection system
fc_wgs84 <- spTransform(fc, CRS("+proj=longlat +datum=WGS84"))
save.image(file.path(Sys.getenv('CAMELS_DIR_DATA'), 'LiMW.Rdata'))

# Crop to regional domains
limw_wgs84_us <- crop(fc_wgs84, extent(-125, -66, 23, 50))
save(limw_wgs84_us, file = file.path(Sys.getenv('CAMELS_DIR_DATA'), 'limw_wgs84_us.Rdata'))

limw_wgs84_uk <- crop(fc_wgs84, extent(-11, 2.5, 49, 59))
save(limw_wgs84_uk, file = file.path(Sys.getenv('CAMELS_DIR_DATA'), 'limw_wgs84_uk.Rdata'))

limw_wgs84_es <- crop(fc_wgs84, extent(-10, 3.5, 35.5, 44))
save(limw_wgs84_es, file = file.path(Sys.getenv('CAMELS_DIR_DATA'), 'limw_wgs84_es.Rdata'))

limw_wgs84_cl <- crop(fc_wgs84, extent(-76, -64, -56, -16.5))
save(limw_wgs84_cl, file = file.path(Sys.getenv('CAMELS_DIR_DATA'), 'limw_wgs84_cl.Rdata'))

limw_wgs84_us_pnw <- crop(fc_wgs84, extent(-125, -120, 45, 50))
save(limw_wgs84_us_pnw, file = file.path(Sys.getenv('CAMELS_DIR_DATA'), 'limw_wgs84_us_pnw.Rdata'))
