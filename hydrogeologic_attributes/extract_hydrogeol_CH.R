###===============================###===============================###
### CAMELS-CH: preprocessing of hydrogeological data
### Daniel Viviroli, Ursula Schönenberg and Martina Kauzlaric
### 13.02.2023
### University of Zürich, daniel.viviroli@geo.uzh.ch
### EAWAG, ursula.schoenenberger@eawag.ch
### University of Bern, martina.kauzlaric@giub.unibe.ch
### martina.kauzlaric@giub.unibe.ch and kauzlaric.martina@gmail.com
###===============================###===============================###
###===============================###===============================###
# First get the data you need and save these in a folder called 
# Hydrogeology in your 'CAMELS_DIR_DATA' directory

dir_data <- Sys.getenv('CAMELS_DIR_DATA')

setwd(dir_data)
dir.create('Hydrogeology')

# For Switzerland there is a hydrogeological map 1:500'000
# covering most of its hydrological extent, but with a minor part 
# missing North East, upstream of the Bodensee.

# **For CH
# Download manually from
# Hydrogeological Map of Switzerland: Groundwater resources 1:500'000 either here for V1_1
# https://www.swisstopo.admin.ch/en/geodata/geology/maps/gk500/vector.html
# or also here (here you get both, vectorised and raster data)
# https://data.geo.admin.ch/ch.swisstopo.geologie-hydrogeologische_karte-grundwasservorkommen/

# Save data in dir_data/Hydrogeology/GK500_V1_3_DE
# You find the data you need in the subdirectory LV95/Shapes_LV95
# shapefile PY_Basis_Flaechen.shp

# In this shapefile the relevant field for defining hydrogeological properties is H2_ID 
# it contains information on the groundwater resources: the aquifer, the hydrogeology and the productivity
# => please refer to the table on sheet H2_ID in Attributtabellen.xlsx in subdirectory Tabellen

# We reclassify here this field as follows:
# H2_ID   ATTRIBUTE NAME                DESCRIPTION AND DETAILS
# 0       hygeol_null_perc              not defined --> polygons without defined hydrogeology
# 1,2     hygeol_unconsol_coarse_perc   unconsolidated coarse-grained material --> well-permeable gravel in valley bottoms
# 3       hygeol_unconsol_medium_perc   unconsolidated medium-grained material --> permeable gravel outside of valley bottoms,
#                                                                                  sandy gravel,medium- to coarse-grained gravel
# 4,5     hygeol_unconsol_fine_perc     unconsolidated fine-grained material --> loamy gravel, fine- to medium-grained debris, moraines
# 6       hygeol_unconsol_imperm_perc   impermeable, unconsolidated material --> clay, silt, fine sands and loamy moraines
# 8       hygeol_karst_perc             karstic rock --> carbonate rock: limestone, dolomite, rauhwacke;
#                                                        sulphate-containing rock: gypsum, anhydrite
# 9,10    hygeol_hardrock_perc          hard rock --> fissured and porous, non-karstic hard rock: conglomerates,
#                                                     sandstone, limestone with marl layers; crystalline rock: granite,
#                                                     granodiorites, tonalite.
# 11      hygeol_hardrock_imperm_perc   impermeable hard rock --> marl, shale, gneiss and cemented sandstone
# 98,99   hygeol_water_perc             water --> glaciers, firn, surface waters

#Please refer also to table ** un Supplementary Material ***

# **For DE
# Download manually from
# Hydrogeological Map of Germany 1:250'000
# https://gdk.gdi-de.org/geonetwork/srv/api/records/61ac4628-6b62-48c6-89b8-46270819f0d6
# For a procuct description see
# https://www.bgr.bund.de/DE/Themen/Wasser/Projekte/laufend/Beratung/Huek200/huek200_projektbeschr.html

# Save data in dir_data/Hydrogeology/Karst/huek250_v103
# You find the data you need in the subdirectory shp
# shapefile huek250__25832_v103_poly.shp

###===============================###===============================###
###===============================###===============================###
### (1) Read in catchment shapes
### (2) Get swiss hydrogeological data and re-class them
### (3) Get and crop german hydrogeological data, merge the karst area
###     with the swiss karst area
###===============================###===============================###

###===============================###===============================###
### load packages
###===============================###===============================###
### loading shape files
library(sf)
library(dplyr)

###===============================###===============================###
### (1) Read in catchment shapes
###===============================###===============================###
### read in CAMELS-CH catchment shapes
setwd(paste(dir_data, 'Catchments', 'CAMELS_CH_EZG_7.6', sep = '/'))
catch <- st_read('CAMELS_CH_EZG_76.shp')

#Plot catchments' overview
plot(st_geometry(catch))

#Look at data attached
head(catch)

### generate mask of all catchments
catch.mask <- st_union(catch)
catch.bb <- st_buffer(st_as_sfc(st_bbox(catch.mask)), 1000)
#plot mask
plot(st_geometry(catch.mask))
plot(st_geometry(catch.bb), add = TRUE)

###===============================###===============================###
### (2) Get swiss hydrogeological data and re-class them
###===============================###===============================###

### read hydrogrological map for Switzerland
hydrogeo.CH <- st_read(paste(dir_data, 'Hydrogeology', 'GK500_V1_3_DE',
                             'LV95/Shapes_LV95', 'PY_Basis_Flaechen.shp', sep = '/'))

# Projected CRS: CH1903+ / LV95

#Select field needed for further analysis H2_ID
hydrogeo.CH <- hydrogeo.CH %>% select(H2_ID)

#Define table to reclassify data
reclass.table <- setNames(cbind.data.frame(c(0, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 98, 99),
                                           c('hygeol_null',
                                             'hygeol_unconsol_coarse',
                                             'hygeol_unconsol_coarse',
                                             'hygeol_unconsol_medium',
                                             'hygeol_unconsol_fine',
                                             'hygeol_unconsol_fine',
                                             'hygeol_unconsol_imperm',
                                             'hygeol_karst',
                                             'hygeol_hardrock',
                                             'hygeol_hardrock',
                                             'hygeol_hardrock_imperm',
                                             'hygeol_water',
                                             'hygeol_water'), stringsAsFactors = FALSE),
                          c('H2_ID', 'CAMELS.hygeol'))

# Lookup table as needed by dplyr to recode data (named vector)
lu.table <- reclass.table$CAMELS.hygeol
names(lu.table) <- reclass.table$H2_ID

#Add field with the CAMELS classses
hydrogeo.CH <- hydrogeo.CH %>% mutate(CAMELS.hygeol = recode(H2_ID, !!!lu.table))

#Aggregate data to the CAMELS classses 
hydrogeo.CH <- hydrogeo.CH %>%
  group_by(CAMELS.hygeol) %>%
  summarise(geometry = st_union(geometry))

#Check data visually
plot(hydrogeo.CH)

###===============================###===============================###
### (3) Get and crop german hydrogeological data, merge the karst area
###     with the swiss karst area
###===============================###===============================###
### read hydrogrological map for Germany
hydrogeo.DE <- st_read(paste(dir_data, 'Hydrogeology', 'Karst/huek250_v103',
                             'shp', 'huek250__25832_v103_poly.shp', sep = '/'))

#Projected CRS: ETRS89 / UTM zone 32N

### Select only relevant attributes, i.e.
### CH: H2_ID == 8 resp. now CAMELS.hygeol == 'hygeol_karst'
### DE: I_CompMat1 == limestone

#Select field needed for further analysis I_CompMat1
hydrogeo.DE <- hydrogeo.DE %>% select(I_CompMat1)

### Reproject german data to swiss data projection
hydrogeo.DE.proj <- st_transform(hydrogeo.DE, crs = st_crs(hydrogeo.CH))
rm(hydrogeo.DE)

### Retain only german data not overlapping with swiss data
hydrogeo.DE.HydroCH <- st_difference(
  st_crop(hydrogeo.DE.proj, st_bbox(st_buffer(st_as_sfc(st_bbox(catch.mask)), 100))),
  st_union(hydrogeo.CH))

rm(hydrogeo.DE.proj)

hydrogeo.DE.HydroCH <- hydrogeo.DE.HydroCH %>% mutate(
  CAMELS.hygeol = 'hygeol_null', .before = I_CompMat1)
hydrogeo.DE.HydroCH <- hydrogeo.DE.HydroCH %>% mutate(
  CAMELS.hygeol = replace(CAMELS.hygeol, I_CompMat1 == 'limestone', 'hygeol_karst'))

### Aggregate data to the CAMELS classses 
hydrogeo.DE.HydroCH <- hydrogeo.DE.HydroCH %>%
  group_by(CAMELS.hygeol) %>%
  summarise(geometry = st_union(geometry))


# Check data visually
plot(st_geometry(hydrogeo.CH))
plot(st_geometry(hydrogeo.DE.HydroCH), col = 'red', add = TRUE)
plot(st_geometry(catch.mask),
     col = sf.colors(categorical = TRUE, alpha = 0.5), add = TRUE)


### Generate sf object for data missing in France and Italy (minor parts)
missing.data <- st_difference(catch, st_union(hydrogeo.CH))
missing.data <- st_difference(missing.data, st_union(hydrogeo.DE.HydroCH))
missing.data <- missing.data %>% mutate(CAMELS.hygeol = NA, .before = geometry)
missing.data <- missing.data %>% select(CAMELS.hygeol)
missing.data <- missing.data %>% group_by(CAMELS.hygeol) %>% summarise(geometry = st_union(geometry))

### Merge all data together
hydrogeo.HydroCH <- rbind(hydrogeo.CH, hydrogeo.DE.HydroCH, missing.data)
rm(hydrogeo.CH); rm(hydrogeo.DE.HydroCH); rm(missing.data)

hydrogeo.HydroCH <- hydrogeo.HydroCH %>% group_by(CAMELS.hygeol) %>% summarise(geometry = st_union(geometry))

### Write data to a shapefile 
st_write(st_zm(hydrogeo.HydroCH),
         paste(dir_data, 'Hydrogeology', 'hydrogeoHydroCH.shp', sep = '/'),
         layer_options = "ENCODING=UTF-8")

### clear workspace
rm(list = ls())


