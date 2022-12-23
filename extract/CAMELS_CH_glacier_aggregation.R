# ==============================================================================
# Aggregation of glacier volume, mass and area to CAMELS-CH catchments
# 
# Eawag, Switzerland, Dec. 2022
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
EZG_CH <- readOGR(dsn = "./EZG/CAMELS_EZG_7.4", layer = "CAMELS_CH_EZG_LV95_v74")


# load glacier files from 2016 and 1973
glaciers <- readOGR(dsn = "./Glamos_2016", layer = "SGI_2016_glaciers")

glaciers1973 <- readOGR(dsn = "./Glamos_1973", layer = "SGI_1973")
# adjust Glamos 1973 data:
glaciers1973$area_km2 <- area(glaciers1973)/1000000  # km^2 to m^2
glaciers1973$sgi.id <- glaciers1973$SGI              # rename glacier id label


# load glacier masses and areas
df_glacier_volumes = data.frame(read.table("volume_evolution_single.dat", header=TRUE, skip=1)) 
colnames(df_glacier_volumes) <- c(1980:2021)

df_glacier_areas = data.frame(read.table("area_evolution_single.dat", header=TRUE, skip=1)) 
colnames(df_glacier_areas) <- c(1980:2021)

glacier_names = rownames(df_glacier_volumes)



# turn to data frames
df_glaciers = data.frame(glaciers)
df_glaciers1973 = data.frame(glaciers1973)
df_EZG = data.frame(EZG_CH)

if (optional_nonnested_hydrol_CH_analysis == TRUE){

  # subset selection of hydrologic Switzerland w/o nested basins - covered by the following basins:
  #  Weil-Rhein, Chancy Vers Vaux, Kajetansbruecke-Inn, Miorina-Ticino, Le Prese-Poschiavino 
  #  M?stair-Rom, Ocourt, Chiasso (Ponte di Polenta), Soglio, Boncourt (Frontiere)
  coverage_hydrol_CH_ID = c(2613, 2217, 3033, 6011,2617, 2078, 2210, 2349, 2620, 2485)
  EZG_coverage_hydrol_CH = EZG_CH[EZG_CH$ID %in% coverage_hydrol_CH_ID, ]
  

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
  #  data_EZG_glacier_clip = data_EZG_glacier_clip[, c("ID", "sgi.id", "area_km2","area_clip", "glacier_frac_area")]
  #}else{
  #  print("Invalid field names in input data.")
  #  break
  #}
  
  # return
  data_EZG_glacier_clip
}


# clip shape files
data_EZG_glacier_clip = catchments_glaciers_clip(EZG_CH, glaciers, 1000000)
data_EZG_glacier1973_clip = catchments_glaciers_clip(EZG_CH, glaciers1973, 1000000)

if (optional_nonnested_hydrol_CH_analysis == TRUE){
  data_EZG_glacier_hydrol_CH_clip = catchments_glaciers_clip(EZG_coverage_hydrol_CH, glaciers, 1000000)
  data_EZG_glacier1973_hydrol_CH_clip = catchments_glaciers_clip(EZG_coverage_hydrol_CH, glaciers1973, 1000000)
}

if (save_results == TRUE){
  save(data_EZG_glacier_clip,file="data_basins_glacier_clipped.Rda")
  save(data_EZG_glacier1973_clip,file="data_basins_glacier1973_clipped.Rda")
  if (optional_nonnested_hydrol_CH_analysis == TRUE){
    save(data_EZG_glacier_hydrol_CH_clip,file="data_basins_glacier_hydrol_CH_clipped.Rda")
    save(data_EZG_glacier1973_hydrol_CH_clip,file="data_basins_glacier1973_hydrol_CH_clipped.Rda")
  }
}


# =============================================================================
# Allocate Glacier Masses and Areas to Catchments

aggregate_glacier_features <- function(data, df_EZG, glacier_volumes, glacier_areas){
  frac_aggregate = aggregate(data$glacier_frac_area, by=list(Category.1=data$sgi.id, Category.2=data$ID), FUN=sum)
  # prepare container for glacier masses per catchment
  glacier_volume_catchment = data.frame(matrix(nrow = length(df_EZG$ID), 
                                               ncol = ncol(glacier_volumes)))
  colnames(glacier_volume_catchment) <- c(1980:2021) 
  rownames(glacier_volume_catchment) <- df_EZG$ID
  
  # prepare container for glacier masses per catchment
  glacier_area_catchment = data.frame(matrix(nrow = length(df_EZG$ID), 
                                             ncol = ncol(glacier_areas)))
  colnames(glacier_area_catchment) <- c(1980:2021) 
  rownames(glacier_area_catchment) <- df_EZG$ID
  
  # container to record lost glacier parts
  lost_glacier_parts = numeric(2)
  
  # get list of relevant basins that have glaciers
  basins_unique = unique(data$ID)
  
  # aggregate glacier volumes per basin
  for (i_basin in basins_unique){
    
    # get indices of current ID
    catch_i <-subset(data, data$ID == i_basin)
    loc_i_basin<-unique(catch_i$sgi.id)
    # containers for glacier volume and area per catchment
    vol_tot_per_basin = rep(0.0, times = ncol(glacier_volumes))
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
        glacier_vol_evol = as.numeric(as.vector(glacier_volumes[glac_j$sgi.id[1],]))
        vol_tot_per_basin = vol_tot_per_basin + glacier_vol_evol*aggr_frac
        
        glacier_area_evol = as.numeric(as.vector(glacier_areas[glac_j$sgi.id[1],]))
        area_tot_per_basin = area_tot_per_basin +glacier_area_evol*aggr_frac
      }else{
        #print(paste("MISSING ",glacier_id, "in", i_basin))
        lost_glacier_parts <- rbind(lost_glacier_parts, c(i_basin, data$area_clip[j]))
      }  
    }
    
    # assign glacier evolution to respective catchment
    glacier_volume_catchment[which(df_EZG$ID == i_basin),] <- vol_tot_per_basin
    glacier_area_catchment[which(df_EZG$ID == i_basin),] <- area_tot_per_basin
    
  }
  
  # return
  return(list("volume" = glacier_volume_catchment, "area" = glacier_area_catchment, "lost_glacier_parts" = lost_glacier_parts))
  
}


# aggregate glacier data for catchments
catchment_glacier = aggregate_glacier_features(data_EZG_glacier_clip , df_EZG, df_glacier_volumes, df_glacier_areas)
catchment_glacier1973 = aggregate_glacier_features(data_EZG_glacier1973_clip , df_EZG, df_glacier_volumes, df_glacier_areas)

if (optional_nonnested_hydrol_CH_analysis == TRUE){
  catchment_hydrol_CH_glacier = aggregate_glacier_features(data_EZG_glacier_hydrol_CH_clip , df_EZG_hydrol_CH, df_glacier_volumes, df_glacier_areas)
  catchment_hydrol_CH_glacier1973 = aggregate_glacier_features(data_EZG_glacier1973_hydrol_CH_clip , df_EZG_hydrol_CH, df_glacier_volumes, df_glacier_areas)
}


# ==============================================================================
# Check for catchments in which glacier disappeared:
# If not available in 2016 data, check and insert from 1973 if available

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

  return(ts_all)
  
}

final_glacier_volume_catchment = replace_expiring_glaciers(catchment_glacier1973$volume, catchment_glacier$volume)
final_glacier_area_catchment = replace_expiring_glaciers(catchment_glacier1973$area, catchment_glacier$area)

if (optional_nonnested_hydrol_CH_analysis == TRUE){
  final_glacier_volume_catchment_hydrol_CH = replace_expiring_glaciers(catchment_hydrol_CH_glacier1973$volume, catchment_hydrol_CH_glacier$volume)
  final_glacier_area_catchment_hydrol_CH = replace_expiring_glaciers(catchment_hydrol_CH_glacier1973$area, catchment_hydrol_CH_glacier$area)
}



# ==============================================================================
# SAVE matrices

if (save_results == TRUE){

  write.table(final_glacier_volume_catchment,"glacier_volume_catchment_camels_ch3.txt",sep=";")
  # header: annual glacier volume (km3) evolution per catchment between 1980 and 2021 
  
  write.table(final_glacier_area_catchment,"glacier_area_catchment_camels_ch3.txt",sep=";")
  # header: annual glacier area (km2) evolution per catchment between 1980 and 2021 
  
  # volume km3 to m3, then times 850 kg/m3, then to metric tons
  final_glacier_mass_catchment = final_glacier_volume_catchment*1e9*850/1000
  write.table(final_glacier_mass_catchment,"glacier_mass_catchment_camels_ch3.txt",sep=";")
  # header: annual glacier mass (metric tons) evolution per catchment between 1980 and 2021 
}


  
