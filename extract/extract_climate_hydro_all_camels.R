rm(list = ls())

library(dotenv)
library(here)

# Load functions
source(here::here('clim/clim_indices.R'))
source(here::here('hydro/hydro_signatures.R'))
source(here::here('maps/plot_maps_camels.R'))

# Define directory for files and list of catchment IDs
if (Sys.getenv('CAMELS_COUNTRY') == 'US') {

} else if (Sys.getenv('CAMELS_COUNTRY') == 'GB') {

  # Set preferences
  hydro_year_cal <- 'oct'
  tol <- 0.85 # Gem asked for no restriction at first (see email from 10 Dec 2019) but the code
  # crashes (streamflow elasticity) when there is only a year of available data, which happens for
  # catchments not in CAMELS-GB, now tolerating 85% of missing values (see email from 16 Dec 2019)

  list_files <- system(paste0('ls ', Sys.getenv('CAMELS_DIR_DATA')), intern = TRUE)
  list_catch <- rapply(strsplit(list_files, '_'), function(x) x[5])

  # Define period over which indices and signatures will be computed
  # 1st Oct 1970 to 30th Sept 2015
  per_str <- 'hy1971-2015' # String used to name output files
  per_start <- as.Date('1970-10-01')
  per_end <- as.Date('2015-09-30')
  per_all <- seq(per_start, per_end, by = 'day')

} else if (Sys.getenv('CAMELS_COUNTRY') == 'BR') {

  # preferences
  hydro_year_cal <- 'sep'
  tol <- 0.05
  list_files <- system(paste0('ls ', Sys.getenv('CAMELS_DIR_DATA')), intern = TRUE)
  list_catch <- rapply(strsplit(list_files, '_'), function(x) x[1])

  # Define period over which indices and signatures will be computed
  per_str <- 'hy1990-2009' # String used to name output files
  per_start <- as.Date('1989-09-01')
  per_end <- as.Date('2009-08-31')
  per_all <- seq(per_start, per_end, by = 'day')

  # Load gauge coordinates
  camels_topo <- read.table(paste0(Sys.getenv('CAMELS_DIR_DATA'), 'brazil_gauges_coordinates.txt'), header = TRUE)

} else if (Sys.getenv('CAMELS_COUNTRY') == 'CH') {

  # Set preferences
  hydro_year_cal <- 'oct'
  tol <- ...
  list_files <- system(paste0('ls ', Sys.getenv('CAMELS_DIR_DATA')), intern = TRUE)
  list_catch <- rapply(strsplit(list_files, '_'), function(x) x[1])

  # Define period over which indices and signatures will be computed
  per_str <- 'hy1980-2020' # String used to name output files
  per_start <- as.Date('1979-10-01')
  per_end <- as.Date('2020-09-30')
  per_all <- seq(per_start, per_end, by = 'day')

  # Load gauge coordinates
  camels_topo <- ...

} else {
  stop(paste('Country code unknown:', Sys.getenv('CAMELS_COUNTRY')))
}

# Create data.frames
camels_clim <- data.frame(stringsAsFactors = FALSE)
camels_hydro_obs <- data.frame(stringsAsFactors = FALSE)

# Loop through catchments, load data and compute CI and HS
for (i in seq_along(list_catch)) {

  catch_id <- list_catch[i]

  print(paste0(i, ') ', catch_id))

  # Load data
  if (Sys.getenv('CAMELS_COUNTRY') == 'US') {

  } else if (Sys.getenv('CAMELS_COUNTRY') == 'GB') {

    dat <- read.csv(paste0(Sys.getenv('CAMELS_DIR_DATA'), '/CAMELS_GB_hydromet_timeseries_', catch_id,
                           '_19701001-20150930.txt'),
                    header = TRUE,
                    na.strings = 'NaN'
    )

    day <- as.Date(dat$date)

    prec <- dat$precipitation
    temp <- dat$temperature
    pet <- dat$pet
    q_obs <- dat$discharge_spec

  } else if (Sys.getenv('CAMELS_COUNTRY') == 'BR') {

    dat <- read.table(paste0(Sys.getenv('CAMELS_DIR_DATA'), '/', catch_id, '_pet_p_t_q.txt'),
                      header = TRUE,
                      na.strings = 'NaN'
    )

    dat$DD <- sprintf('%02d', dat$DD); dat$MM <- sprintf('%02d', dat$MM)

    day <- as.Date(apply(dat[, 1:3], 1, paste, collapse = '-'))

    prec <- dat$P
    temp <- dat$T
    pet <- dat$PET
    q_obs <- dat$Q

  } else if (Sys.getenv('CAMELS_COUNTRY') == 'CH') {



  }

  # Select sub-period over which indices will be computed
  if (min(day) > per_start || max(day) < per_end) {
    stop('The period over which the indices should be computed is not fully covered by the data')
  }

  in_period <- day >= per_start & day <= per_end

  prec <- prec[in_period]
  temp <- temp[in_period]
  pet <- pet[in_period]
  q_obs <- q_obs[in_period]
  day <- day[in_period]

  # Compute climate indices
  camels_clim[i, 'gauge_id'] <- as.character(catch_id)
  dat <- compute_clim_indices_camels(temp = temp, prec = prec, pet = pet, day = day, tol = tol)
  camels_clim[i, names(dat)] <- dat

  levels(camels_clim$high_prec_timing) <- c('djf', 'mam', 'jja', 'son')
  levels(camels_clim$low_prec_timing) <- c('djf', 'mam', 'jja', 'son')

  # Compute hydrological signatures for observed Q
  camels_hydro_obs[i, 'gauge_id'] <- as.character(catch_id)
  dat <- compute_hydro_signatures_camels(q = q_obs, p = prec, d = day, tol = tol, hydro_year_cal = hydro_year_cal)
  camels_hydro_obs[i, names(dat)] <- dat

}

# Save
save(camels_clim, camels_hydro_obs, list_catch,
     file = paste0(Sys.getenv('CAMELS_DIR_DATA'), 'ci_hs_camels_', Sys.getenv('CAMELS_COUNTRY'), '_', per_str, '_NAtol', tol, '.Rdata'))

write.table(camels_clim,
            file = paste0(Sys.getenv('CAMELS_DIR_DATA'), 'clim_indices_camels_', Sys.getenv('CAMELS_COUNTRY'), '_', per_str, '_NAtol', tol, '.txt'),
            row.names = FALSE,
            quote = FALSE,
            sep = ';'
)

write.table(camels_hydro_obs,
            file = paste0(Sys.getenv('CAMELS_DIR_DATA'), 'hydro_sign_camels_', Sys.getenv('CAMELS_COUNTRY'), '_', per_str, '_NAtol', tol, '.txt'),
            row.names = FALSE,
            quote = FALSE,
            sep = ';'
)

# Plot maps
camels_clim <- merge(camels_topo, camels_clim)
camels_hydro_obs <- merge(camels_topo, camels_hydro_obs)

pdf(paste0(Sys.getenv('CAMELS_DIR_PLOTS'), 'clim.pdf'), 6, 6, useDingbats = FALSE)

for (my_var in names(camels_clim[c(-1, -2, -3)])) {

  qual <- is.factor(camels_clim[, my_var])

  plot_points(x = camels_clim$gauge_lon,
              y = camels_clim$gauge_lat,
              z = camels_clim[, my_var],
              text_legend = my_var,
              country = Sys.getenv('CAMELS_COUNTRY'),
              qual = qual,
              col_scheme = ifelse(qual, 'seas', 'RdYlBu'),
              color_bar = TRUE
  )

}

dev.off()

pdf(paste0(Sys.getenv('CAMELS_DIR_PLOTS'), 'hydro.pdf'), 6, 6, useDingbats = FALSE)

for (my_var in names(camels_hydro_obs[c(-1, -2, -3)])) {

  qual <- is.factor(camels_hydro_obs[, my_var])

  plot_points(x = camels_clim$gauge_lon,
              y = camels_clim$gauge_lat,
              z = camels_hydro_obs[, my_var],
              text_legend = my_var,
              country = Sys.getenv('CAMELS_COUNTRY'),
              qual = qual,
              col_scheme = ifelse(qual, 'seas', 'RdYlBu'),
              color_bar = !qual
  )

}

dev.off()
