rm(list = ls())

library(dotenv)

extract_elev_bands <- function(id, huc, keep_absolute_area = FALSE) {

  # Locate file in which elevations bands are stored
  file_elev <- paste0(Sys.getenv('CAMELS_DIR_DATA'), 'daymet/', huc, '/', id, '.list')

  # Get number of elevation zones from first line
  n_elevation_zones <- as.numeric(read.table(file_elev, header = FALSE, nrows = 1))
  elev_tab <- read.table(file_elev, skip = 1, header = FALSE)

  # Some elevations bands have negative area - this is a a bug
  if (any(elev_tab[, 2] < 0)) {
    elev_tab[, 2] <- abs(elev_tab[, 2]) # Remove eventual minus signs
  }

  # Check number of elevation bands - consistency check
  if (dim(elev_tab)[1] != n_elevation_zones) {
    stop('Problem with the number of elevation bands')
  }

  # Compute total area as the sum of the elevation bands
  total_area_elev_bands <- sum(as.numeric(elev_tab[, 2])) # in m^2

  # Create table in FUSE format
  elev_tab_format <- data.frame(array(dim = c(n_elevation_zones, 4)))

  colnames(elev_tab_format) <- c('indice_elevation_zone', 'mid_point_elevation', 'area_fraction', 'area_m2')

  for (z in 1:n_elevation_zones) {
    # First colum: indice of elevation zone
    elev_tab_format$indice_elevation_zone[z] <- z
    elev_code <- as.numeric(strsplit(as.character(elev_tab[z, 1]), '_')[[1]][4])

    # Second colum: mid-point elevation
    elev_tab_format$mid_point_elevation[z] <- elev_code * 100 + 50

    # Third column: fraction of the area in this elevation band
    elev_tab_format$area_fraction[z] <- elev_tab[z, 2] / total_area_elev_bands

    # Fourth column: area in this elevation band - not needed by FUSE, only for verification purposes
    elev_tab_format$area_m2[z] <- elev_tab[z, 2]
  }

  if (!keep_absolute_area) {
    elev_tab_format <- elev_tab_format[, -4]
  }

  # Sort rows by mean elevation
  if (n_elevation_zones > 1) {
    elev_tab_format <- elev_tab_format[order(elev_tab_format[, 2]),]
    elev_tab_format[, 1] <- 1:n_elevation_zones
  }

  if (any(diff(elev_tab_format[, 2]) != 100)) {
    stop('Unexpected distance between two successive elevation bands')
  }

  elev_tab_format
}
