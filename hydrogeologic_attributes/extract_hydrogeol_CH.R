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

setwd(paste(Sys.getenv('CAMELS_DIR_DATA')))
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

# Save data in Sys.getenv('CAMELS_DIR_DATA')/Hydrogeology/GK500_V1_3_DE
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

# Save data in Sys.getenv('CAMELS_DIR_DATA')/Hydrogeology/Karst/huek250_v103
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

### list environmental variables
# Sys.getenv()

### use individual variables
# setwd(Sys.getenv('CAMELS_DIR_DATA'))
###===============================###===============================###
### (1) Read in catchment shapes
###===============================###===============================###
### read in CAMELS-CH catchment shapes
setwd(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Catchments', 'CAMELS_CH_EZG_7.6', sep = '/'))
catch <- st_read('CAMELS_CH_EZG_76.shp')

#Plot catchments' overview
plot(st_geometry(catch))
#plot(catch)
#Look at data attached
head(catch)
#head(catch@data)

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
hydrogeo.CH <- st_read(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Hydrogeology', 'GK500_V1_3_DE',
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

#  Define function to clip hydrogeological map over CAMELS catchments
#catchments_clip <- function (catch, hydrogeo_map){
# extract hydrogeo data for each catchment
#  df_all <- NULL

#  for(i in 1:nrow(catch)){
#    df_i <- NULL
#    id_i <- NULL
#    print(i)
#    catch_i <- catch[i,]

#    hg_i<-try(st_intersection(hydrogeo_map,catch_i ) )
# check if the catchment catch_i result any clipping, if not fill with "NA". if yes, aggregate clipping result
#    if(nrow(hg_i)==0){
#      hg_agg[1:nrow(hg_agg),2]<-NaN
#    }else{
#      hg_agg <- aggregate(st_area(hg_i) ~ H2_ID, FUN = sum, data = hg_i, na.rm = TRUE)
#    } 

# build dataframe
#    df_i <- data.frame(hg_agg)

#insert catchment ID
#    id_i <- catch$gauge_id[i]
#    colnames(df_i) <- c("H2_ID",id_i)

#    if (i==1) {
#      df_all <- df_i
#    } else {df_all <- merge.data.frame(df_all,df_i, all.x=TRUE, all.y=TRUE) }

#  }

#  df_all
#}

# Define fuction to reclassify data
#reclass_table <- function (hydrogeo_table){
# insert extra column for new classes
#  reclass_table<-cbind(data.frame("camels_class"=c(rep("na",nrow(hydrogeo_table)))),hydrogeo_table)
## loop for new class
#  for(i in 1:nrow(hydrogeo_table)){
#    class_i <- reclass_table$H2_ID[i]

#    if (class_i==0) {
#      reclass_table$camels_class[i]<-"hygeol_null_perc"
#    }
#    if (class_i==1) {
#      reclass_table$camels_class[i]<-"hygeol_unconsol_coarse_perc"
#    }
#    if (class_i==2) {
#      reclass_table$camels_class[i]<-"hygeol_unconsol_coarse_perc"
#    }
#    if (class_i==3) {
#      reclass_table$camels_class[i]<-"hygeol_unconsol_medium_perc"
#    }
#    if (class_i==4) {
#      reclass_table$camels_class[i]<-"hygeol_unconsol_fine_perc"
#    }
#    if (class_i==5) {
#      reclass_table$camels_class[i]<-"hygeol_unconsol_fine_perc"
#    }
#    if (class_i==6) {
#      reclass_table$camels_class[i]<-"hygeol_unconsol_imperm_perc"
#    }
#    if (class_i==8) {
#      reclass_table$camels_class[i]<-"hygeol_karst_perc"
#    }
#    if (class_i==9) {
#      reclass_table$camels_class[i]<-"hygeol_hardrock_perc"
#    }
#    if (class_i==10) {
#      reclass_table$camels_class[i]<-"hygeol_hardrock_perc"
#    }
#    if (class_i==11) {
#      reclass_table$camels_class[i]<-"hygeol_hardrock_imperm_perc"
#    }
#    if (class_i==98) {
#      reclass_table$camels_class[i]<-"hygeol_water_perc"
#    }
#    if (class_i==99) {
#      reclass_table$camels_class[i]<-"hygeol_water_perc"
#    }

#  }
#  reclass_table<-reclass_table[,-2]
#}

# Define fuction to aggregate by the new CAMELS-CH classes
#reaggregate <- function (recl_catch){
# aggregate over new classes
#  hg_aggr<-NULL

#  for(i in 2:(ncol(recl_catch))){
#    hg_aggr_i<-NULL
#    hg_aggr_i<-aggregate(recl_catch[,i]~camels_class, recl_catch, sum, na.rm = TRUE, na.action=NULL)

#    hg_aggr_i <- data.frame(hg_aggr_i)
#    id_i <- colnames(recl_catch[i])
#    colnames(hg_aggr_i) <- c("camels_class",id_i)

#    if (i==2) {
#      hg_aggr <- hg_aggr_i
#    } else {hg_aggr <- merge.data.frame(hg_aggr,hg_aggr_i, all.x=TRUE, all.y=TRUE) }
#  }

# create row names from content in last column (with new classes) and delete last column
#  rownames(hg_aggr) <- c(hg_aggr[,1])
#  hg_aggr<-hg_aggr[-c(1)]
#}

# Clip hydrogeological map over CAMELS catchments
#hydrogeo.CH.clip <- catchments_clip(catch,hydrogeo.CH)

# Reclassify data to CAMELS hydrogeology classes
#hydrogeo.CH.recl <- reclass_table(hydrogeo.CH.clip)

# Aggregate by the new CAMELS-CH classes
#hydrogeo.CAMELS.CH <- reaggregate(hydrogeo.CH.recl)


###===============================###===============================###
### (3) Get and crop german hydrogeological data, merge the karst area
###     with the swiss karst area
###===============================###===============================###
### read hydrogrological map for Germany
hydrogeo.DE <- st_read(paste(Sys.getenv('CAMELS_DIR_DATA'), 'Hydrogeology', 'Karst/huek250_v103',
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
hydrogeo.DE.HydroCH <- st_difference(st_crop(hydrogeo.DE.proj,
                                             st_bbox(st_buffer(st_as_sfc(st_bbox(catch.mask)), 100))),
                                     st_union(hydrogeo.CH))

rm(hydrogeo.DE.proj)

hydrogeo.DE.HydroCH <- hydrogeo.DE.HydroCH %>% mutate(CAMELS.hygeol = 'hygeol_null', .before = I_CompMat1)
hydrogeo.DE.HydroCH <- hydrogeo.DE.HydroCH %>% mutate(CAMELS.hygeol = replace(CAMELS.hygeol,
                                                                              I_CompMat1 == 'limestone',
                                                                              'hygeol_karst'))

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
         paste(Sys.getenv('CAMELS_DIR_DATA'), 'Hydrogeology', 'hydrogeoHydroCH.shp', sep = '/'), layer_options = "ENCODING=UTF-8")

### clear workspace
rm(list = ls())


