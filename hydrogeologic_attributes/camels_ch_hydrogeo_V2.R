# ==============================================================================
# aggregate hydrogeological data by catchments
# 
# Eawag, Switzerland, Jan. 2023
#
# usula.schoenenberger@eawag.ch
# daniel.viviroli@geo.uzh.ch
# ==============================================================================

## creating the hydrogeological percentage of all catchments.
## the hydrogeol map is not covering the whole area of camels_ch catchments: some catchments 
## are less then 100% in their sum, because parts are outside of the hydrogeol map.


library(sf)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# open catchment shapefiles
camels_catch <- st_read("EZG/CAMELS_CH_EZG_74.shp")
hydrogeo_basis <- st_read("Basisflaechen/PY_Basis_Flaechen.shp")

# change projection to LV95
hydrogeo_basis <- st_transform(hydrogeo_basis, "EPSG:2056")

# clip hydrogeo over catchments
catchments_hg_clip <- function(catch, hydrogeo_map) {
  # extract hydrogoe data for each catchment
  df_all <- NULL

  for (i in 1:nrow(catch)) {
    df_i <- NULL
    id_i <- NULL
    print(i)
    catch_i <- catch[i,]

    hg_i <- try(st_intersection(hydrogeo_map, catch_i))
    # check if the catchment catch_i result any clipping, if not fill with "NA". if yes, aggregate clipping result
    if (nrow(hg_i) == 0) {
      hg_agg[1:nrow(hg_agg), 2] <- NA
    }else {
      hg_agg <- aggregate(st_area(hg_i) ~ H2_ID, FUN = sum, data = hg_i, na.rm = TRUE)
    }

    # build dataframe
    df_i <- data.frame(hg_agg)

    #insert catchment ID
    id_i <- catch$ID[i]
    colnames(df_i) <- c("H2_ID", id_i)

    if (i == 1) {
      df_all <- df_i
    } else { df_all <- merge.data.frame(df_all, df_i, all.x = TRUE, all.y = TRUE) }

  }

  df_all
}

# reclass 
reclass_table <- function(hydrogeo_table) {
  # insert extra column for new classes
  reclass_table <- cbind(data.frame("camels_class" = c(rep("na", nrow(hydrogeo_table)))), hydrogeo_table)
  ## loop for new class
  for (i in 1:nrow(hydrogeo_table)) {
    class_i <- reclass_table$H2_ID[i]

    if (class_i == 0) {
      reclass_table$camels_class[i] <- "hygeol_null_perc"
    }
    if (class_i == 1) {
      reclass_table$camels_class[i] <- "hygeol_unconsol_coarse_perc"
    }
    if (class_i == 2) {
      reclass_table$camels_class[i] <- "hygeol_unconsol_coarse_perc"
    }
    if (class_i == 3) {
      reclass_table$camels_class[i] <- "hygeol_unconsol_medium_perc"
    }
    if (class_i == 4) {
      reclass_table$camels_class[i] <- "hygeol_unconsol_fine_perc"
    }
    if (class_i == 5) {
      reclass_table$camels_class[i] <- "hygeol_unconsol_fine_perc"
    }
    if (class_i == 6) {
      reclass_table$camels_class[i] <- "hygeol_unconsol_imperm_perc"
    }
    if (class_i == 8) {
      reclass_table$camels_class[i] <- "hygeol_karst_perc"
    }
    if (class_i == 9) {
      reclass_table$camels_class[i] <- "hygeol_hardrock_perc"
    }
    if (class_i == 10) {
      reclass_table$camels_class[i] <- "hygeol_hardrock_perc"
    }
    if (class_i == 11) {
      reclass_table$camels_class[i] <- "hygeol_hardrock_imperm_perc"
    }
    if (class_i == 98) {
      reclass_table$camels_class[i] <- "hygeol_water_perc"
    }
    if (class_i == 99) {
      reclass_table$camels_class[i] <- "hygeol_water_perc"
    }

  }
  reclass_table <- reclass_table[, -2]
}

# aggregate by new classes
reaggregate <- function(recl_catch) {
  # aggregate over new classes
  hg_aggr <- NULL

  for (i in 2:(ncol(recl_catch))) {
    hg_aggr_i <- NULL
    hg_aggr_i <- aggregate(recl_catch[, i] ~ camels_class, recl_catch, sum, na.rm = TRUE, na.action = NULL)

    hg_aggr_i <- data.frame(hg_aggr_i)
    id_i <- colnames(recl_catch[i])
    colnames(hg_aggr_i) <- c("camels_class", id_i)

    if (i == 2) {
      hg_aggr <- hg_aggr_i
    } else { hg_aggr <- merge.data.frame(hg_aggr, hg_aggr_i, all.x = TRUE, all.y = TRUE) }
  }

  # create row names from content in last column (with new classes) and delete last column
  rownames(hg_aggr) <- c(hg_aggr[, 1])
  hg_aggr <- hg_aggr[-c(1)]
}

# calculate percentage
calculate_percentage <- function(reagg_catch) {
  # create dataframe with the dimensions of "clc_aggr" and fill it with catchment area values
  catch_area <- st_area(camels_catch)
  nr <- nrow(reagg_catch)
  df_sum <- NULL

  df_row <- as.data.frame(t(data.frame(row = catch_area)))
  df_sum <- df_row

  for (m in 1:(nr - 1)) {
    df_row <- NULL
    df_row <- as.data.frame(t(data.frame(row = catch_area)))
    df_sum <- rbind(df_sum, df_row)
  }

  # create percentage table
  df_perc <- reagg_catch / df_sum * 100
  df_perc <- t(df_perc)

  # create dataframe with percentage of areas outside the map and bind it
  df_diff <- 100 - rowSums(df_perc)
  df_perc <- cbind(df_perc, data.frame("hygeol_external" = df_diff))

  # format the resulting dataframe
  df_perc <- df_perc[order(as.numeric(row.names(df_perc))),]
  df_perc <- round(df_perc, 2)
}


# run the functions
hg_clip <- catchments_hg_clip(camels_catch, hydrogeo_basis)
hg_reclass <- reclass_table(hg_clip)
hg_aggr <- reaggregate(hg_reclass)
hg_perc <- calculate_percentage(hg_aggr)

# save as file
file_name <- paste("camels_ch_hydrogeo_perc", ".txt")
first_row_text <- paste0("hydrogeological data [%]")
cat(first_row_text, "\n", file = file_name)
write.table(hg_perc, file = file_name, append = TRUE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")


