rm(list = ls())

require(raster)
require("rgdal") # for readOGR
require(RColorBrewer)
require(maps)
require(maptools)
require(mapdata)  # contains the hi-resolution points that mark out the countries - leaves this afer require(maps)

# set paths
hostname <- system('hostname', intern = TRUE)

if (hostname == 'uncertainty.rap.ucar.edu' | substr(hostname, 1, 3) == 'vpn') {

  source('/Volumes/d1/naddor/hc1_home/scripts/r_scripts/set_paths.R')  # set all paths

} else {

  source('/home/naddor/scripts/r_scripts/set_paths.R')  # set all paths

}

# set country
country = 'cl'

# load GLiM data previously clipped for the country of interest
# and saved in R format
load(paste0('/d7/naddor/data/geol/limw_wgs84_', country, '.Rdata'))

if (country == 'us') {

  # load catchment attributes
  load_camels_data('2.0')

  # load shapefiles
  load(file = '/home/naddor/data/shp_catch_wgs84.Rdata')

  # rename geol to a standard name
  limw_wgs84 <- limw_wgs84_us
  shp_catch <- shp_catch

  # define PDF dimensions
  width_pdf <- 20
  height_pdf <- 16

} else if (country == 'cl') {

  # load shapefiles
  shp_catch <- readShapePoly('/d7/naddor/data/camels_cl/catchments_chile_cag_v2.shp')
  crs(shp_catch) # no crs...
  crs(shp_catch) <- "+proj=longlat +datum=WGS84"

  # extract catchment name and area
  catch_topo <- shp_catch@data
  names(catch_topo)[1] <- 'gauge_id'

  # rename to standard names
  limw_wgs84 <- limw_wgs84_cl
  names(shp_catch@data)[1] <- c('gauge_id')

  # define PDF dimensions
  width_pdf <- 6
  height_pdf <- 20

}

# define colors for GLiM
table_glim_classes <- read.table(paste0(dir_data, 'GLiM/GLiM_classes_colors.txt'), sep = ';', header = TRUE)
table_glim_classes$short_name <- as.factor(table_glim_classes$short_name)

glim_classes = data.frame(short_name = limw_wgs84@data$xx, order = 1:length(limw_wgs84@data$xx))
glim_classes_col = merge(glim_classes, table_glim_classes, sort = FALSE) # add column with color

# merge does not maintain order, so data must be sorted using the column "order" created for this purpose
glim_classes_col = glim_classes_col[order(glim_classes_col$order),]

# plot a map with catchments
pdf(paste0(dir_plots, 'camels/geol/glim_', country, '_overview.pdf', sep = ''), width = width_pdf, height = height_pdf)

plot(limw_wgs84, col = as.character(glim_classes_col$R_col), border = NA)

if (country == 'us') {
  map('state', col = 'gray36', lwd = 1, add = TRUE)
  map('worldHires', 'USA', add = TRUE)
}else if (country == 'cl') {
  map('worldHires', 'Chile', lwd = 1, col = 'gray36', add = TRUE)
  plot(shp_catch, add = TRUE, lwd = 0.5)
}

legend('bottomleft', col = as.character(table_glim_classes$R_col), legend = table_glim_classes$long_name, ncol = 2, pch = 15, bty = 'n', cex = 0.7)

dev.off()

# declare vectors
glim_1st_class <- array()
glim_1st_frac <- array()
glim_2nd_class <- array()
glim_2nd_frac <- array()
glim_carbonate_rocks_frac <- array()

pdf(paste0(dir_plots, 'camels/geol/glim_', country, '_extraction.pdf', sep = ''), width = width_pdf, height = height_pdf)

plot(limw_wgs84, col = as.character(glim_classes_col$R_col), border = NA)

if (country == 'us') {
  map('state', col = 'black', lwd = 1, add = TRUE)
  map('worldHires', 'USA', add = TRUE)
}else if (country == 'cl') {
  map('worldHires', 'Chile', add = TRUE)
}

legend('bottomleft', col = as.character(table_glim_classes$R_col), legend = table_glim_classes$long_name, ncol = 2, pch = 15, bty = 'n', cex = 0.7)

for (e in 1:nrow(catch_topo)) {

  catch_id = catch_topo$gauge_id[e]
  print(paste(e, catch_id))

  # determime area of polygons overlapping with each catchment
  inter <- intersect(limw_wgs84, shp_catch[shp_catch@data$gauge_id == catch_id,])
  inter_data <- inter@data
  inter_data <- data.frame(inter_data, area = area(inter), area_frac = area(inter) / sum(area(inter)), sort = 1:nrow(inter_data))
  inter_data <- merge(inter_data, table_glim_classes, by.x = 'xx', by.y = 'short_name') # does not preserve row order
  inter_data <- inter_data[order(inter_data$sort),] # re-sort data to match order in inter

  #plot(inter,col=as.character(inter_data$R_color),add=TRUE,lwd=0.1)

  # compute error in area_

  if (country == 'us') {

    area_gf = catch_topo[catch_topo$gage_id == catch_id, 'area_geospa_fabric']
    rel_area_error = sum(inter_data$area) / 1E6 / area_gf - 1

  }else if (country == 'cl') {

    rel_area_error = sum(inter_data$area) / 1E6 / catch_topo$area_km2[e] - 1

  }

  # sort data my area_frac to determine dominant geology
  dom_geol <- rapply(split(inter_data$area_frac, inter_data$long_name), sum)

  # compute fraction of carbonate rocks
  glim_carbonate_rocks_frac[e] <- as.numeric(dom_geol[names(dom_geol) == 'Carbonate sedimentary rocks'])

  # clean up and sort
  dom_geol <- dom_geol[dom_geol > 1E-6] # remove geological classes absent from this catchment
  dom_geol <- sort(dom_geol, decreasing = TRUE)

  if (1E-6 < abs(sum(dom_geol) - 1)) {

    stop('Error in the dom_geol')

  }

  # isolate shapefile of catchment to plot
  shp_catch_to_plot <- shp_catch[shp_catch@data$gauge_id == catch_id,]
  catch_box <- shp_catch_to_plot@bbox # extract coordinates of box around the catchment

  text(mean(catch_box['x',]), mean(catch_box['y',]), labels = catch_id, cex = 0.1) # add catch name

  plot(shp_catch_to_plot, add = TRUE,
       col = NA, border = 'black', lty = 1, lwd = 0.5)

  plot(shp_catch_to_plot, add = TRUE,
       col = NA, border = as.character(table_glim_classes$R_color[table_glim_classes$long_name == names(dom_geol[1])]), lty = 1, lwd = 0.25)

  # save dominant geology class and fraction
  glim_1st_class[e] <- names(dom_geol[1])
  glim_1st_frac[e] <- as.numeric(dom_geol[1])

  if (length(dom_geol) > 1) {

    glim_2nd_class[e] <- names(dom_geol[2])
    glim_2nd_frac[e] <- as.numeric(dom_geol[2])

  } else {

    glim_2nd_class[e] <- NA
    glim_2nd_frac[e] <- 0

  }

}

dev.off()

# only keep significant decimals
glim_1st_frac <- round(glim_1st_frac, 3)
glim_2nd_frac <- round(glim_2nd_frac, 3)
glim_carbonate_rocks_frac <- round(glim_carbonate_rocks_frac, 3)

catch_geol_glim <- data.frame(gauge_id = catch_topo$gauge_id, glim_1st_class, glim_1st_frac, glim_2nd_class, glim_2nd_frac, glim_carbonate_rocks_frac)

# save data to temp directory
save(catch_geol_glim, file = paste(dir_catch_attr_temp, 'catch_geol_glim_', country, '.Rdata', sep = ''))

write.table(catch_geol_glim, file = paste(dir_catch_attr, 'catch_geol_glim_', country, '.txt', sep = ''),
            row.names = FALSE, quote = FALSE, sep = ';')
