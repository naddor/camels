# ==============================================================================
# Creating annual time series per catchment for clc and glacier data
# 
# Eawag, Switzerland, Dec. 2022
#
# usula.schoenenberger@eawag.ch
# ==============================================================================

## creating the hydrogeological percentage of all catchments.
## the hydrogeol map is not covering the whole area of camels_ch: some catchments 
## are less then 100% in their sum, because parts are outside of the hydrogeol map.


library(sf)

# open shape-files
setwd("/Users/...")

camels_catch <- st_read("Catchment/EZG/CAMELS_CH_EZG_74.shp")
hydrogeo <- st_read("Hydrogeologie/Vorbereitung/hydrogeol/hydrogeol.shp")

# change projection to LV95
hydrogeo <- st_transform(hydrogeo, "EPSG:2056")

# create df over all catchments of aggregated hydrogeo data as absolut areas
df_all <- NULL
catch_count <- nrow(camels_catch)

for (i in 1:catch_count) {
  df_i <- NULL
  id_i <- NULL

  # clip hydrogeo map for catchment i
  catch_i <- camels_catch[i,]
  hydrogeo_i <- st_intersection(hydrogeo, catch_i)

  # aggregate hydrogeo data for catchment i
  if (nrow(hydrogeo_i) == 0) {
    for (z in 1:nrow(hydrogeo_agg))
      hydrogeo_agg[z, 2] <- NA
  }else {
    hydrogeo_agg <- aggregate(st_area(hydrogeo_i) ~ ID2, FUN = sum, data = hydrogeo_i, na.rm = TRUE)
  }

  # create df of hydrogeo data  for catchment i
  df_i <- data.frame(hydrogeo_agg)
  id_i <- camels_catch$ID[i]
  colnames(df_i) <- c("ID2", id_i)

  # merge df_i with df_all
  if (i == 1) {
    df_all <- df_i
  } else { df_all <- merge.data.frame(df_all, df_i, all.x = TRUE, all.y = TRUE) }
}

# create rownames
rownames(df_all) <- c(df_all[, 1])
df_all <- df_all[-c(1)]

# create df_sum with catchment areas
catch_area <- st_area(camels_catch)
nr <- (nrow(df_all) - 1)
df_sum <- NULL

df_row <- as.data.frame(t(data.frame(row = catch_area)))
df_sum <- df_row

for (m in 1:nr) {
  df_row <- NULL
  df_row <- as.data.frame(t(data.frame(row = catch_area)))
  df_sum <- rbind(df_sum, df_row)
}

# create percentage data
df_perc <- round(df_all / df_sum * 100, 2)
df_perc <- t(df_perc)
df_perc <- df_perc[order(as.numeric(row.names(df_perc))),]

# save as file
file_name <- paste("camels_ch_hydrogeo_perc_neu", ".txt")
first_row_text <- paste0("hydrogeological data [%]")
cat(first_row_text, "\n", file = file_name)
write.table(df_perc, file = file_name, append = TRUE, quote = FALSE, sep = ";", fileEncoding = "UTF-8")

