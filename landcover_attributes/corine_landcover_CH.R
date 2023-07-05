# ==============================================================================
# aggregate landcover data from clc shapefiles (copernicus server) by catchments
# 
# Eawag, Switzerland, Jan. 2023
#
# usula.schoenenberger@eawag.ch
# jan.schwanbeck@giub.unibe.ch
# ==============================================================================


library(sf)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mainDir <- getwd()


# read catchment file and change coordinate system to 
camels_catch <- st_read("EZG/CAMELS_CH_catchments.shp")
camels_catch <- st_transform(camels_catch, "EPSG:3035")

# create boundary
camels_boundary <- st_boundary(st_union(camels_catch))
wkt <- st_as_text(st_geometry(camels_boundary))

######################################################
# functions

# clip clc over catchments
catchments_clc_clip <- function(camels_catch, shp_clc) {
  # extract clc data for each catchment
  df_all <- NULL

  for (i in 1:nrow(camels_catch)) {
    print(i)
    catch_i <- camels_catch[i,]
    clc_i <- st_intersection(shp_clc, catch_i)
    clc_agg <- aggregate(st_area(clc_i) ~ Code, FUN = sum, data = clc_i, na.rm = TRUE)

    df_i <- data.frame(clc_agg)
    id_i <- camels_catch$gauge_id[i]
    colnames(df_i) <- c("Code", id_i)

    if (i == 1) {
      df_all <- df_i
    } else { df_all <- merge.data.frame(df_all, df_i, all.x = TRUE, all.y = TRUE) }
  }
  df_all
}

# reclass 
reclass_clip <- function(clc_clip) {
  # insert extra column for new classes
  reclass_table <- cbind(clc_clip, data.frame("camels_class" = c(rep("na", nrow(clc_clip)))))

  ## loop for new class
  for (i in 1:nrow(reclass_table)) {
    reclass_i <- NULL
    class_i <- reclass_table$Code[i]
    if (class_i == 111) {
      reclass_table$camels_class[i] <- "urban_perc"
    }
    if (class_i == 112) {
      reclass_table$camels_class[i] <- "urban_perc"
    }
    if (class_i == 121) {
      reclass_table$camels_class[i] <- "urban_perc"
    }
    if (class_i == 122) {
      reclass_table$camels_class[i] <- "urban_perc"
    }
    if (class_i == 123) {
      reclass_table$camels_class[i] <- "urban_perc"
    }
    if (class_i == 124) {
      reclass_table$camels_class[i] <- "urban_perc"
    }
    if (class_i == 131) {
      reclass_table$camels_class[i] <- "loose_rock_perc"
    }
    if (class_i == 132) {
      reclass_table$camels_class[i] <- "loose_rock_perc"
    }
    if (class_i == 133) {
      reclass_table$camels_class[i] <- "loose_rock_perc"
    }
    if (class_i == 141) {
      reclass_table$camels_class[i] <- "grass_perc"
    }
    if (class_i == 142) {
      reclass_table$camels_class[i] <- "urban_perc"
    }
    if (class_i == 211) {
      reclass_table$camels_class[i] <- "crop_perc"
    }
    if (class_i == 212) {
      reclass_table$camels_class[i] <- "crop_perc"
    }
    if (class_i == 213) {
      reclass_table$camels_class[i] <- "crop_perc"
    }
    if (class_i == 221) {
      reclass_table$camels_class[i] <- "scrub_perc"
    }
    if (class_i == 222) {
      reclass_table$camels_class[i] <- "scrub_perc"
    }
    if (class_i == 223) {
      reclass_table$camels_class[i] <- "scrub_perc"
    }
    if (class_i == 231) {
      reclass_table$camels_class[i] <- "grass_perc"
    }
    if (class_i == 241) {
      reclass_table$camels_class[i] <- "crop_perc"
    }
    if (class_i == 242) {
      reclass_table$camels_class[i] <- "crop_perc"
    }
    if (class_i == 243) {
      reclass_table$camels_class[i] <- "crop_perc"
    }
    if (class_i == 244) {
      reclass_table$camels_class[i] <- "scrub_perc"
    }
    if (class_i == 311) {
      reclass_table$camels_class[i] <- "dwood_perc"
    }
    if (class_i == 312) {
      reclass_table$camels_class[i] <- "ewood_perc"
    }
    if (class_i == 313) {
      reclass_table$camels_class[i] <- "mixed_wood_perc"
    }
    if (class_i == 321) {
      reclass_table$camels_class[i] <- "grass_perc"
    }
    if (class_i == 322) {
      reclass_table$camels_class[i] <- "wetlands_perc"
    }
    if (class_i == 323) {
      reclass_table$camels_class[i] <- "scrub_perc"
    }
    if (class_i == 324) {
      reclass_table$camels_class[i] <- "scrub_perc"
    }
    if (class_i == 331) {
      reclass_table$camels_class[i] <- "loose_rock_perc"
    }
    if (class_i == 332) {
      reclass_table$camels_class[i] <- "rock_perc"
    }
    if (class_i == 333) {
      reclass_table$camels_class[i] <- "loose_rock_perc"
    }
    if (class_i == 334) {
      reclass_table$camels_class[i] <- "loose_rock_perc"
    }
    if (class_i == 335) {
      reclass_table$camels_class[i] <- "ice_perc"
    }
    if (class_i == 411) {
      reclass_table$camels_class[i] <- "wetlands_perc"
    }
    if (class_i == 412) {
      reclass_table$camels_class[i] <- "wetlands_perc"
    }
    if (class_i == 421) {
      reclass_table$camels_class[i] <- "wetlands_perc"
    }
    if (class_i == 422) {
      reclass_table$camels_class[i] <- "wetlands_perc"
    }
    if (class_i == 423) {
      reclass_table$camels_class[i] <- "wetlands_perc"
    }
    if (class_i == 511) {
      reclass_table$camels_class[i] <- "inwater_perc"
    }
    if (class_i == 512) {
      reclass_table$camels_class[i] <- "inwater_perc"
    }
    if (class_i == 521) {
      reclass_table$camels_class[i] <- "inwater_perc"
    }
    if (class_i == 522) {
      reclass_table$camels_class[i] <- "inwater_perc"
    }
    if (class_i == 523) {
      reclass_table$camels_class[i] <- "inwater_perc"
    }
    if (class_i == 999) {
      reclass_table$camels_class[i] <- "blank_perc"
    }
    if (class_i == 990) {
      reclass_table$camels_class[i] <- "blank_perc"
    }
    if (class_i == 995) {
      reclass_table$camels_class[i] <- "inwater_perc"
    }
  }
  reclass_table
}

# aggregate by new classes
reaggregate <- function(recl_catch) {
  # aggregate over new classes
  clc_aggr <- NULL

  for (i in 2:(ncol(recl_catch) - 1)) {
    clc_aggr_i <- aggregate(recl_catch[, i] ~ camels_class, recl_catch, sum, na.rm = TRUE, na.action = NULL)

    clc_aggr_i <- data.frame(clc_aggr_i)
    id_i <- colnames(recl_catch[i])
    colnames(clc_aggr_i) <- c("camels_class", id_i)

    if (i == 2) {
      clc_aggr <- clc_aggr_i
    } else { clc_aggr <- merge.data.frame(clc_aggr, clc_aggr_i, all.x = TRUE, all.y = TRUE) }
  }

  # create row names from last column with new classes, delete last column and sort from row names
  rownames(clc_aggr) <- c(clc_aggr[, 1])
  clc_aggr <- clc_aggr[-c(1)]
}

# calculate percentage
calculate_percentage <- function(reagg_catch) {
  # create dataframe and fill it with catchment area with the dimensions of clc_aggr
  catch_area <- st_area(camels_catch)
  nr <- nrow(reagg_catch)

  df_row <- as.data.frame(t(data.frame(row = catch_area)))
  df_sum <- df_row

  for (m in 1:(nr - 1)) {
    df_row <- as.data.frame(t(data.frame(row = catch_area)))
    df_sum <- rbind(df_sum, df_row)
  }

  # create percentage table
  df_perc <- reagg_catch / df_sum * 100
  # df_perc<-round(df_perc,2)
  df_perc <- df_perc[, sort(names(df_perc))]
}

# create static attribute table with dominant land cover
create_static_attribute <- function(perc_table) {
  dom_land_cover <- rownames(perc_00[which.max(perc_00[, 1]),])

  for (i in 2:ncol(perc_00)) {
    dom_i <- rownames(perc_00[which.max(perc_00[, i]),])
    dom_land_cover <- c(dom_land_cover, dom_i)
  }
  end_table <- data.frame(cbind(round(t(perc_00), 2), dom_land_cover))
  return(end_table)
}

####################################################

## clc 2018
# read clc-shapefile within boundaries from catchment file
subDir <- "/u2018_clc2018_v2020_20u1_fgdb/u2018_clc2018_v2020_20u1_fgdb/DATA/"
setwd(file.path(mainDir, subDir))
clc18 <- sf::st_read("U2018_CLC2018_V2020_20u1.gdb", wkt_filter = wkt)
clc18 <- st_cast(clc18, "MULTIPOLYGON")
colnames(clc18)[1] <- "Code"

# run functions
clc_clip_18 <- catchments_clc_clip(camels_catch, clc18)
recl_18 <- reclass_clip(clc_clip_18)
reagg_catch_18 <- reaggregate(recl_18)
perc_18 <- calculate_percentage(reagg_catch_18)

# save output
setwd(mainDir)
cat("# Land cover attributes as area percentages based on the CORINE land cover databases 2018 by the European Copernicus Land Monitoring Service.\n", file = "clc_2018_perc.csv")
write.table(perc_18, file = "clc_2018_perc.csv", append = TRUE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")
####################################################

## clc 2012
# read clc-shapefile within boundaries from catchment file
subDir <- "/u2018_clc2012_v2020_20u1_fgdb/DATA/"
setwd(file.path(mainDir, subDir))
clc12 <- sf::st_read("U2018_CLC2012_V2020_20u1.gdb", wkt_filter = wkt)
clc12 <- st_cast(clc12, "MULTIPOLYGON")
colnames(clc12)[1] <- "Code"

# run functions
clc_clip_12 <- catchments_clc_clip(camels_catch, clc12)
recl_12 <- reclass_clip(clc_clip_12)
reagg_catch_12 <- reaggregate(recl_12)
perc_12 <- calculate_percentage(reagg_catch_12)

# save output
setwd(mainDir)
cat("# Land cover attributes as area percentages based on the CORINE land cover databases 2012 by the European Copernicus Land Monitoring Service..\n", file = "clc_2012_perc.csv")
write.table(perc_12, file = "clc_2012_perc.csv", append = TRUE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")
####################################################

## clc 2006
# read clc-shapefile within boundaries from catchment file
subDir <- "/u2012_clc2006_v2020_20u1_fgdb/DATA/"
setwd(file.path(mainDir, subDir))
clc06 <- sf::st_read("U2012_CLC2006_V2020_20u1.gdb", wkt_filter = wkt)
clc06 <- st_cast(clc06, "MULTIPOLYGON")
colnames(clc06)[1] <- "Code"

# run functions
clc_clip_06 <- catchments_clc_clip(camels_catch, clc06)
recl_06 <- reclass_clip(clc_clip_06)
reagg_catch_06 <- reaggregate(recl_06)
perc_06 <- calculate_percentage(reagg_catch_06)

# save output
setwd(mainDir)
cat("# Land cover attributes as area percentages based on the CORINE land cover databases 2006 by the European Copernicus Land Monitoring Service.\n", file = "clc_2006_perc.csv")
write.table(perc_06, file = "clc_2006_perc.csv", append = TRUE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")
####################################################

## clc 2000
# read clc-shapefile within boundaries from catchment file
subDir <- "/u2006_clc2000_v2020_20u1_fgdb/DATA/"
setwd(file.path(mainDir, subDir))
clc00 <- sf::st_read("U2006_CLC2000_V2020_20u1.gdb", wkt_filter = wkt)
clc00 <- st_cast(clc00, "MULTIPOLYGON")
colnames(clc00)[1] <- "Code"

# run functions
clc_clip_00 <- catchments_clc_clip(camels_catch, clc00)
recl_00 <- reclass_clip(clc_clip_00)
reagg_catch_00 <- reaggregate(recl_00)
perc_00 <- calculate_percentage(reagg_catch_00)

# save output
setwd(mainDir)
cat("# Land cover attributes as area percentages based on the CORINE land cover databases 2000 by the European Copernicus Land Monitoring Service.\n", file = "clc_2000_perc.csv")
write.table(perc_00, file = "clc_2000_perc.csv", append = TRUE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")


## create and save static attribute data
setwd(mainDir)
perc_00_static <- create_static_attribute(perc_00)

# new order of columns
perc_00_static_2 <- cbind(rownames(perc_00_static), perc_00_static[, c(1, 4, 10, 2, 8, 3, 12, 6, 5, 7, 9, 11, 13)])
colnames(perc_00_static_2) <- c("gauge_id", colnames(perc_00_static_2)[2:14])

# save output of attribute file with dominant landcover
cat("# Land cover attributes as area percentages based on the CORINE land cover databases 2000 by the European Copernicus Land Monitoring Service.\n", file = "CAMELS_CH_landcover_attributes.csv")
write.table(perc_00_static_2, file = "CAMELS_CH_landcover_attributes.csv", append = TRUE, row.names = FALSE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")


####################################################

## clc 1990
# read clc-shapefile within boundaries from catchment file
subDir <- "/u2000_clc1990_v2020_20u1_fgdb/DATA/"
setwd(file.path(mainDir, subDir))
clc90 <- sf::st_read("U2000_CLC1990_V2020_20u1.gdb", wkt_filter = wkt)
clc90 <- st_cast(clc90, "MULTIPOLYGON")
colnames(clc90)[1] <- "Code"

# run functions
clc_clip_90 <- catchments_clc_clip(camels_catch, clc90)
recl_90 <- reclass_clip(clc_clip_90)
reagg_catch_90 <- reaggregate(recl_90)
perc_90 <- calculate_percentage(reagg_catch_90)

# save output
setwd(mainDir)
cat("# Land cover attributes as area percentages based on the CORINE land cover databases 1990 by the European Copernicus Land Monitoring Service.\n", file = "clc_1990_perc.csv")
write.table(perc_90, file = "clc_1990_perc.csv", append = TRUE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")
