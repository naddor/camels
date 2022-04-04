### Run this on geyser only!

rm(list = ls())

require(maps)
require(rgdal)
require(raster)
require(rgeos)

dir_r_data <- '/glade/scratch/naddor/data/r_data/'

# The input file geodatabase and read the feature class
# to List all feature classes in a file geodatabase use:
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
# print(fc_list)

### GLHYPMS
fgdb <- '/glade/scratch/naddor/data/GLHYMPS/GLHYMPS.gdb/'
fc <- readOGR(dsn = fgdb, layer = "Final_GLHYMPS_Polygon")

# Change projection system
fc_wgs84 <- spTransform(fc, CRS("+proj=longlat +datum=WGS84"))
save.image(paste0(dir_r_data, 'GLHYMPS.Rdata'))

# Crop to regional domains
glhymps_wgs84_us <- crop(fc_wgs84, extent(-125, -66, 23, 50))
save(glhymps_wgs84_us, file = paste0(dir_r_data, 'glhymps_wgs84_us.Rdata'))

glhymps_wgs84_uk <- crop(fc_wgs84, extent(-11, 2.5, 49, 59))
save(glhymps_wgs84_uk, file = paste0(dir_r_data, 'glhymps_wgs84_uk.Rdata'))

glhymps_wgs84_es <- crop(fc_wgs84, extent(-10, 3.5, 35.5, 44))
save(glhymps_wgs84_es, file = paste0(dir_r_data, 'glhymps_wgs84_es.Rdata'))

glhymps_wgs84_cl <- crop(fc_wgs84, extent(-76, -64, -56, -16.5))
save(glhymps_wgs84_cl, file = paste0(dir_r_data, 'glhymps_wgs84_cl.Rdata'))

glhymps_wgs84_us_pnw <- crop(fc_wgs84, extent(-125, -120, 45, 50))
save(glhymps_wgs84_us_pnw, file = paste0(dir_r_data, 'glhymps_wgs84_us_pnw.Rdata'))

### LiMW
fgdb <- '/glade/scratch/naddor/data/LiMW_GIS_2015.gdb/'
fc <- readOGR(dsn = fgdb, layer = "GLiM_export")

# Change projection system
fc_wgs84 <- spTransform(fc, CRS("+proj=longlat +datum=WGS84"))
save.image(paste0(dir_r_data, 'LiMW.Rdata'))

# Crop to regional domains
limw_wgs84_us <- crop(fc_wgs84, extent(-125, -66, 23, 50))
save(limw_wgs84_us, file = paste0(dir_r_data, 'limw_wgs84_us.Rdata'))

limw_wgs84_uk <- crop(fc_wgs84, extent(-11, 2.5, 49, 59))
save(limw_wgs84_uk, file = paste0(dir_r_data, 'limw_wgs84_uk.Rdata'))

limw_wgs84_es <- crop(fc_wgs84, extent(-10, 3.5, 35.5, 44))
save(limw_wgs84_es, file = paste0(dir_r_data, 'limw_wgs84_es.Rdata'))

limw_wgs84_cl <- crop(fc_wgs84, extent(-76, -64, -56, -16.5))
save(limw_wgs84_cl, file = paste0(dir_r_data, 'limw_wgs84_cl.Rdata'))

limw_wgs84_us_pnw <- crop(fc_wgs84, extent(-125, -120, 45, 50))
save(limw_wgs84_us_pnw, file = paste0(dir_r_data, 'limw_wgs84_us_pnw.Rdata'))
