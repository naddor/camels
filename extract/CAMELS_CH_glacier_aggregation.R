# ==============================================================================
# Aggregation of glacier volume, mass and area to CAMELS-CH catchments
# 
# Eawag, Switzerland, Jan. 2023
#
# marvin.hoege@eawag.ch 
# usula.schoenenberger@eawag.ch
# ==============================================================================


if ( ! require(rgdal) )     { install.packages("rgdal");     library(rgdal) }
if ( ! require(rgeos) )     { install.packages("rgeos");     library(rgeos) }
if ( ! require(raster))     { install.packages("raster");    library(raster)}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# make sure to have input files in the same folder as script

#-------------------------------------------------------------------------------
# USER OPTIONS

optional_nonnested_hydrol_CH_analysis = TRUE #FALSE
save_results = TRUE


# ==============================================================================
# Load data

# load catchment shape files
EZG_CH <- readOGR(dsn = "./EZG", layer = "CAMELS_CH_EZG_76")


# load sgi-glacier files from 2016 and 1973
glaciers_sgi_2016 <- readOGR(dsn = "./Glamos_2016", layer = "SGI_2016_glaciers")


glaciers_sgi_1973 <- readOGR(dsn = "./Glamos_1973", layer = "SGI_1973")
# adjust Glamos 1973 data:
glaciers_sgi_1973$area_km2 <- area(glaciers_sgi_1973)/1000000  # m^2 to km^2
glaciers_sgi_1973$sgi.id <- glaciers_sgi_1973$SGI              # rename glacier id label

# load gi-glacier file from 2003 and 2015
glaciers_gi_2015 <- readOGR(dsn = "./GI_2015/", layer = "GI2015_Paul_korr5")
glaciers_gi_2015$sgi.id <- glaciers_gi_2015$CAMELS_ID           # rename glacier id label

glaciers_gi_2003 <- readOGR(dsn = "./GI_2003", layer = "GI2003_Paul_korr5")
glaciers_gi_2003$sgi.id <- glaciers_gi_2003$CAMELS_ID           # rename glacier id label


# load glacier masses and areas from sgi
df_glacier_sgi_volumes = data.frame(read.table("volume_evolution_single.dat", header=TRUE, skip=1))
colnames(df_glacier_sgi_volumes) <- c(1980:2021)

df_glacier_sgi_areas = data.frame(read.table("area_evolution_single.dat", header=TRUE, skip=1))
colnames(df_glacier_sgi_areas) <- c(1980:2021)

glacier_names_sgi = rownames(df_glacier_sgi_volumes)          # get names of glaciers of sgi table

# load glacier areas from gi
df_glacier_gi_areas = data.frame(read.table("area_evolution_single_gi.dat", header=TRUE, skip=1))
glacier_names_gi = df_glacier_gi_areas[,1]                    # get names of glaciers of gi table
rownames(df_glacier_gi_areas) <- glacier_names_gi             # rearrange table
df_glacier_gi_areas <- df_glacier_gi_areas[,-1]
colnames(df_glacier_gi_areas) <- c(1980:2021)

# turn to data frames
df_glaciers_sgi_2016 = data.frame(glaciers_sgi_2016)
df_glaciers_sgi_1973 = data.frame(glaciers_sgi_1973)
df_glaciers_gi_2015 = data.frame(glaciers_gi_2015)
df_glaciers_gi_2003 = data.frame(glaciers_gi_2003)

df_EZG = data.frame(EZG_CH)

if (optional_nonnested_hydrol_CH_analysis == TRUE){

  # subset selection of hydrologic Switzerland w/o nested basins - covered by the following basins:
  #  Weil-Rhein, Chancy Vers Vaux, Kajetansbruecke-Inn, Miorina-Ticino, Le Prese-Poschiavino 
  #  M?stair-Rom, Ocourt, Chiasso (Ponte di Polenta), Soglio, Boncourt (Frontiere)
  coverage_hydrol_CH_ID = c(2613, 2217, 3033, 6011,2617, 2078, 2210, 2349, 2620, 2485)
  EZG_coverage_hydrol_CH = EZG_CH[EZG_CH$gauge_id %in% coverage_hydrol_CH_ID, ]
  

  df_EZG_hydrol_CH = data.frame(EZG_coverage_hydrol_CH)
  
  # plot
  plot(EZG_coverage_hydrol_CH)
  lines(glaciers, col = "blue")
  
}

# ==============================================================================
# Obtain intersections between catchments and glaciers via shape files


# function to clip glacier shape files with basin shape files 
catchments_glaciers_clip <- function (shp_basin, shp_glacier, area_scaling){
  
  EZG_glacier_clip_alle<-NULL
  testbool<-TRUE
  
  for(i in 1:length(shp_basin)){
    print(i)
    intrsct<-try(intersect(shp_basin[i,], shp_glacier) )
    if(!is.null(ncol(intrsct))){
      if(testbool==TRUE) {
        EZG_glacier_clip = intrsct
        testbool<-FALSE
      }else{
        EZG_glacier_clip = rbind(EZG_glacier_clip,intrsct)
      } 
      
    }
    next
  }
  
  data_EZG_glacier_clip = data.frame(EZG_glacier_clip)
  
  # add clip area to data frame 
  data_EZG_glacier_clip$area_clip <- area(EZG_glacier_clip)/area_scaling
  
  # check which original data was used
  # 2016 data
  if (is.element("area_km2", names(data_EZG_glacier_clip))){
    glacier_area = data_EZG_glacier_clip$area_km2
    # 1976 data
  #}else if(is.element("Shape_Area.1", names(data_EZG_glacier_clip))){
  #  glacier_area = (data_EZG_glacier_clip$Shape_Area.1/area_scaling)
  }else{
    print("Invalid field names in input data.")
    break
  }
  
  # add area fraction of glacier coverage fraction to respective catchment-clip
  data_EZG_glacier_clip$glacier_frac_area <- data_EZG_glacier_clip$area_clip/glacier_area
  
  # rename and extract relevant columns
  #if (is.element("area_km2", names(data_EZG_glacier_clip))){
  #  data_EZG_glacier_clip = data_EZG_glacier_clip[, c("gauge_id", "sgi.id", "area_km2","area_clip", "glacier_frac_area")]
  #}else{
  #  print("Invalid field names in input data.")
  #  break
  #}
  
  # return
  data_EZG_glacier_clip
}


# clip shape files
data_EZG_glacier_sgi_2016_clip = catchments_glaciers_clip(EZG_CH, glaciers_sgi_2016, 1000000)
data_EZG_glacier_sgi_1973_clip = catchments_glaciers_clip(EZG_CH, glaciers_sgi_1973, 1000000)
data_EZG_glacier_gi_2015_clip = catchments_glaciers_clip(EZG_CH, glaciers_gi_2015, 1000000)
data_EZG_glacier_gi_2003_clip = catchments_glaciers_clip(EZG_CH, glaciers_gi_2003, 1000000)

if (optional_nonnested_hydrol_CH_analysis == TRUE){
  data_EZG_glacier_sgi_2016_hydrol_CH_clip = catchments_glaciers_clip(EZG_coverage_hydrol_CH, glaciers_sgi_2016, 1000000)
  data_EZG_glacier_sgi_1973_hydrol_CH_clip = catchments_glaciers_clip(EZG_coverage_hydrol_CH, glaciers_sgi_1973, 1000000)
  data_EZG_glacier_gi_2015_hydrol_CH_clip = catchments_glaciers_clip(EZG_coverage_hydrol_CH, glaciers_gi_2015, 1000000)
  data_EZG_glacier_gi_2003_hydrol_CH_clip = catchments_glaciers_clip(EZG_coverage_hydrol_CH, glaciers_gi_2003, 1000000)
}

if (save_results == TRUE){
  save(data_EZG_glacier_sgi_2016_clip,file="data_EZG_glacier_sgi_2016_clip.Rda")
  save(data_EZG_glacier_sgi_1973_clip,file="data_EZG_glacier_sgi_1973_clip.Rda")
  save(data_EZG_glacier_gi_2015_clip,file="data_EZG_glacier_gi_2015_clip.Rda")
  save(data_EZG_glacier_gi_2003_clip,file="data_EZG_glacier_gi_2003_clip.Rda")
  if (optional_nonnested_hydrol_CH_analysis == TRUE){
    save(data_EZG_glacier_sgi_2016_hydrol_CH_clip,file="data_EZG_glacier_sgi_2016_hydrol_CH_clip.Rda")
    save(data_EZG_glacier_sgi_1973_hydrol_CH_clip,file="data_EZG_glacier_sgi_1973_hydrol_CH_clip.Rda")
    save(data_EZG_glacier_gi_2015_hydrol_CH_clip,file="data_EZG_glacier_gi_2015_hydrol_CH_clip.Rda")
    save(data_EZG_glacier_gi_2003_hydrol_CH_clip,file="data_EZG_glacier_gi_2003_hydrol_CH_clip.Rda")
  }
}


# =============================================================================
# Allocate Glacier Masses and Areas to Catchments

aggregate_glacier_features <- function(data, df_EZG, glacier_volumes,glacier_areas,glacier_names){
  frac_aggregate = aggregate(data$glacier_frac_area, by=list(Category.1=data$sgi.id, Category.2=data$gauge_id), FUN=sum)
  
  # prepare container for glacier area per catchment
  glacier_area_catchment = data.frame(matrix(nrow = length(df_EZG$gauge_id), 
                                             ncol = ncol(glacier_areas)))
  colnames(glacier_area_catchment) <- c(1980:2021) 
  rownames(glacier_area_catchment) <- df_EZG$gauge_id
  
  # container to record lost glacier parts
  lost_glacier_parts = numeric(2)
  
  # get list of relevant basins that have glaciers
  basins_unique = unique(data$gauge_id)
  
  # aggregate glacier area per basin
  for (i_basin in basins_unique){
    
    # get indices of current ID
    catch_i <-subset(data, data$gauge_id == i_basin)
    loc_i_basin<-unique(catch_i$sgi.id)
    # containers for glacier area per catchment
    area_tot_per_basin = rep(0.0, times = ncol(glacier_areas))
    
    for (j in loc_i_basin){
      glac_j = subset(catch_i, catch_i$sgi.id == j)
      
      # due to numerical error from clipping, the sum of aggregated glacier fractions
      # can exceed 1.0. In order to avoid artifacts, these numbers are rescaled to 1.0
      aggr_frac <-sum(glac_j$glacier_frac_area)
      if (aggr_frac > 1.0){
        aggr_frac=1
      }
      
      if (is.element(glac_j$sgi.id[1],glacier_names)){
        glacier_area_evol = as.numeric(as.vector(glacier_areas[glac_j$sgi.id[1],]))
        area_tot_per_basin = round(area_tot_per_basin +glacier_area_evol*aggr_frac,6)
      }else{
        #print(paste("MISSING ",glacier_id, "in", i_basin))
        lost_glacier_parts <- rbind(lost_glacier_parts, c(i_basin, data$area_clip[j]))
      }  
    }
    
    # assign glacier evolution to respective catchment
    glacier_area_catchment[which(df_EZG$gauge_id == i_basin),] <- area_tot_per_basin
    
  }
  
  # check for volume data
  if(is.numeric(ncol(glacier_volumes))){
    # prepare container for glacier volume per catchment
    glacier_volume_catchment = data.frame(matrix(nrow = length(df_EZG$ID), 
                                                 ncol = ncol(glacier_volumes)))
    colnames(glacier_volume_catchment) <- c(1980:2021) 
    rownames(glacier_volume_catchment) <- df_EZG$gauge_id
    
    # aggregate glacier volume per basin
    for (i_basin in basins_unique){
      
      # get indices of current ID
      catch_i <-subset(data, data$gauge_id == i_basin)
      loc_i_basin<-unique(catch_i$sgi.id)
      # containers for glacier volume per catchment
      vol_tot_per_basin = rep(0.0, times = ncol(glacier_volumes))
      
      for (j in loc_i_basin){
        glac_j = subset(catch_i, catch_i$sgi.id == j)
        
        # due to numerical error from clipping, the sum of aggregated glacier fractions
        # can exceed 1.0. In order to avoid artifacts, these numbers are rescaled to 1.0
        aggr_frac <-sum(glac_j$glacier_frac_area)
        if (aggr_frac > 1.0){
          aggr_frac=1
        }
        
        if (is.element(glac_j$sgi.id[1],glacier_names)){
          glacier_vol_evol = as.numeric(as.vector(glacier_volumes[glac_j$sgi.id[1],]))
          vol_tot_per_basin = vol_tot_per_basin + glacier_vol_evol*aggr_frac
        }
      }
      
      # assign glacier evolution to respective catchment
      glacier_volume_catchment[which(df_EZG$gauge_id == i_basin),] <- vol_tot_per_basin
    }
  }else{
    glacier_volume_catchment<-NA
  }
  
  # return
  return(list("volume" = glacier_volume_catchment,"area" = glacier_area_catchment, "lost_glacier_parts" = lost_glacier_parts))
  
}


# aggregate glacier data for catchments
catchment_glacier_sgi_2016 = aggregate_glacier_features(data_EZG_glacier_sgi_2016_clip , df_EZG, df_glacier_sgi_volumes, df_glacier_sgi_areas,glacier_names_sgi)
catchment_glacier_sgi_1973 = aggregate_glacier_features(data_EZG_glacier_sgi_1973_clip , df_EZG, df_glacier_sgi_volumes, df_glacier_sgi_areas,glacier_names_sgi)
catchment_glacier_gi_2015 = aggregate_glacier_features(data_EZG_glacier_gi_2015_clip , df_EZG, NULL, df_glacier_gi_areas,glacier_names_gi)
catchment_glacier_gi_2003 = aggregate_glacier_features(data_EZG_glacier_gi_2003_clip , df_EZG, NULL, df_glacier_gi_areas,glacier_names_gi)

if (optional_nonnested_hydrol_CH_analysis == TRUE){
  catchment_hydrol_CH_glacier2016 = aggregate_glacier_features(data_EZG_glacier_sgi_2016_hydrol_CH_clip , df_EZG_hydrol_CH, df_glacier_sgi_volumes, df_glacier_sgi_areas)
  catchment_hydrol_CH_glacier1973 = aggregate_glacier_features(data_EZG_glacier_sgi_1973_hydrol_CH_clip , df_EZG_hydrol_CH, df_glacier_sgi_volumes, df_glacier_sgi_areas)
  catchment_hydrol_CH_glacier2015 = aggregate_glacier_features(data_EZG_glacier_gi_2015_hydrol_CH_clip , df_EZG_hydrol_CH, NULL, df_glacier_areas_gi)
  catchment_hydrol_CH_glacier2003 = aggregate_glacier_features(data_EZG_glacier_gi_2003_hydrol_CH_clip , df_EZG_hydrol_CH, NULL, df_glacier_areas_gi)
}


# ==============================================================================
# Check for catchments in which glacier disappeared:
# sgi: If not available in 2016 data, check and insert from 1973 if available
# gi: If not available in 2015 data, check and insert from 2003 if available

replace_expiring_glaciers <- function(ts_old, ts_new){
  ts_all<-ts_new
  
  catch_i <-NULL
  catchold_i<-NULL
  
  # check if 2016 produced data
  for (i in 1:nrow(ts_all)){
    
    catch_i <-ts_all[i,]
    catchold_i<-ts_old[i,]
    if (is.na(catch_i[,1])==TRUE){
      if (sum(catchold_i)>0 && !is.na(catchold_i[1,1])){
        ts_all[i,] <- catchold_i
      }
    }
  }
  ts_all <- ts_all[order(as.numeric(row.names(ts_all))), ]
  ts_all[is.na(ts_all)] <- 0

  return(ts_all)
  
}

final_glacier_volume_catchment_CH = replace_expiring_glaciers(catchment_glacier_sgi_1973$volume, catchment_glacier_sgi_2016$volume)
final_glacier_area_catchment_CH = replace_expiring_glaciers(catchment_glacier_sgi_1973$area, catchment_glacier_sgi_2016$area)
final_glacier_area_catchment_CH_neighbours = replace_expiring_glaciers(catchment_glacier_gi_2003$area, catchment_glacier_gi_2015$area)


if (optional_nonnested_hydrol_CH_analysis == TRUE){
  final_glacier_volume_catchment_hydrol_CH = replace_expiring_glaciers(catchment_hydrol_CH_glacier1973$volume, catchment_hydrol_CH_glacier2016$volume)
  final_glacier_area_catchment_hydrol_CH = replace_expiring_glaciers(catchment_hydrol_CH_glacier1973$area, catchment_hydrol_CH_glacier2016$area)
  final_glacier_area_catchment_hydrol_CH_neighbours = replace_expiring_glaciers(catchment_hydrol_CH_glacier2003$area, catchment_hydrol_CH_glacier2015$area)
}



# ==============================================================================
# SAVE matrices

if (save_results == TRUE){
  write.table(final_glacier_volume_catchment_CH,"glacier_volume_catchment_camels_ch.csv",sep=";",quote=FALSE)
  # header: annual glacier volume (km3) evolution per catchment between 1980 and 2021 
  
  write.table(final_glacier_area_catchment_CH,"glacier_area_catchment_camels_ch.csv",sep=";",quote=FALSE)
  write.table(final_glacier_area_catchment_CH_neighbours,"glacier_area_catchment_camels_ch_neighbours.csv",sep=";",quote=FALSE)
  # header: annual glacier area (km2) evolution per catchment between 1980 and 2021 
  
  # volume km3 to m3, then times 850 kg/m3, then to mega tons
  final_glacier_mass_catchment_CH = final_glacier_volume_catchment_CH*850
  write.table(final_glacier_mass_catchment_CH,"glacier_mass_catchment_camels_ch.csv",sep=";",quote=FALSE)
  # header: annual glacier mass (mega tons) evolution per catchment between 1980 and 2021 
  
  
  if (optional_nonnested_hydrol_CH_analysis == TRUE){
    write.table(final_glacier_volume_catchment_hydrol_CH,"glacier_volume_catchment_camels_hydrol_ch.csv",sep=";",quote=FALSE)
    # header: annual glacier volume (km3) evolution per catchment between 1980 and 2021 
    
    write.table(final_glacier_area_catchment_hydrol_CH,"glacier_area_catchment_camels_hydrol_ch.csv",sep=";",quote=FALSE)
    write.table(final_glacier_area_catchment_hydrol_CH_neighbours,"glacier_area_catchment_camels_hydrol_ch_neighbours.csv",sep=";",quote=FALSE)
    # header: annual glacier area (km2) evolution per catchment between 1980 and 2021 
    
    # volume km3 to m3, then times 850 kg/m3, then to mega tons
    final_glacier_mass_catchment_hydrol_CH = final_glacier_volume_catchment_hydrol_CH*1e9*850
    write.table(final_glacier_mass_catchment_hydrol_CH,"glacier_mass_catchment_camels_hydrol_ch.csv",sep=";",quote=FALSE)
    # header: annual glacier mass (mega tons) evolution per catchment between 1980 and 2021 
  }
  
}

# ==============================================================================
# create static attribute table for data from year 2000

# round data
area_ch<-round(final_glacier_area_catchment_CH$"2000",4)
vol_ch<-round(final_glacier_volume_catchment_CH$"2000",5)
mass_ch<-round(final_glacier_mass_catchment_CH$"2000",3)
area_neighbours<-round(final_glacier_area_catchment_CH_neighbours$"2000",4)

# bind data
stat_attr<-cbind(area_ch,vol_ch,mass_ch,area_neighbours)

# add column names and row names
colnames(stat_attr)[1:4] <- c("glac_area","glac_vol","glac_mass","glac_area_neighbours")
rownames(stat_attr)<-rownames(final_glacier_area_catchment_CH)

# save as csv file
# header: Glacier attributes about area (km2), volume (km3) and mass (mega tons) and area in neighbouring countries (km2) based on the Swiss glacier inventory (GLAMOS) and GI from Paul et al, 2020.
write.table(stat_attr,"CAMELS_CH_glacier_attributes.csv",sep=";", quote=FALSE)  
