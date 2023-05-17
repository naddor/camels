rm(list = ls())

library(dotenv)
library(here)

# Load functions
source(here::here('compute/climate_indices.R'))
source(here::here('compute/hydro_signatures.R'))
source(here::here('utils/plot_maps_camels.R'))

# Define directory for files and list of catchment IDs
if (Sys.getenv('CAMELS_COUNTRY') == 'US') {

  source(here::here('extract/camels_us_hydromet.R'))
  source(here::here('compute/hydro_accuracy.R'))

  tol_na <- 0.0502 # Two CAMELS catchments have ~5.01% of missing values, this threshold includes them

  # Choose time period for computation of climate indices and hydrological signatures
  per_start <- as.Date('1989-10-01')
  per_end <- as.Date('2009-09-30')
  start_date_indices <- '19891001'
  end_date_indices <- '20090930'

} else if (Sys.getenv('CAMELS_COUNTRY') == 'GB') {

  # Set preferences
  hydro_year_cal <- 'oct'
  tol_na <- 0.85 # Gem asked for no restriction at first (see email from 10 Dec 2019) but the code
  # crashes (streamflow elasticity) when there is only a year of available data, which happens for
  # catchments not in CAMELS-GB, now tolerating 85% of missing values (see email from 16 Dec 2019)

  list_files <- system(paste('ls', Sys.getenv('CAMELS_DIR_DATA')), intern = TRUE)
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
  tol_na <- 0.05
  list_files <- system(paste('ls', Sys.getenv('CAMELS_DIR_DATA')), intern = TRUE)
  list_catch <- rapply(strsplit(list_files, '_'), function(x) x[1])

  # Define period over which indices and signatures will be computed
  per_str <- 'hy1990-2009' # String used to name output files
  per_start <- as.Date('1989-09-01')
  per_end <- as.Date('2009-08-31')
  per_all <- seq(per_start, per_end, by = 'day')

  # Load gauge coordinates
  camels_topo <- read.table(file.path(Sys.getenv('CAMELS_DIR_DATA'), 'brazil_gauges_coordinates.txt'), header = TRUE)

} else if (Sys.getenv('CAMELS_COUNTRY') == 'CH') {

  # Set preferences
  hydro_year_cal <- 'oct'
  tol_na <- ...
  list_files <- system(paste('ls', Sys.getenv('CAMELS_DIR_DATA')), intern = TRUE)
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
if (Sys.getenv('CAMELS_COUNTRY') == 'US') {
  camels_hydro_sac <- data.frame(stringsAsFactors = FALSE)
}

# Loop through catchments, load data and compute CI and HS
for (i in seq_along(list_catch)) {

  catch_id <- list_catch[i]

  print(paste0(i, ') ', catch_id))

  # Load data
  if (Sys.getenv('CAMELS_COUNTRY') == 'US') {

    # Import forcing data, obs and sim discharge and collect metadata
    dat <- get_catchment_data_dataframe(huc = list_catch$huc_02[i],
                                        id = catch_id,
                                        date_start = start_date_indices,
                                        date_end = end_date_indices,
                                        forcing_dataset = 'daymet',
                                        ens_method = 'best'
    )

    day <- as.Date(dat$date)

    prec <<- dat$prec
    temp <<- (dat$temp_min + dat$temp_max) / 2
    pet <<- dat$pet
    q_obs <<- dat$q_obs
    q_sim_sac <<- dat$q_sim_sac

  } else if (Sys.getenv('CAMELS_COUNTRY') == 'GB') {

    dat <- read.csv(file.path(Sys.getenv('CAMELS_DIR_DATA'),
                              paste0('CAMELS_GB_hydromet_timeseries_',
                                     catch_id, '_19701001-20150930.txt')),
                    header = TRUE,
                    na.strings = 'NaN'
    )

    day <- as.Date(dat$date)

    prec <- dat$precipitation
    temp <- dat$temperature
    pet <- dat$pet
    q_obs <- dat$discharge_spec

  } else if (Sys.getenv('CAMELS_COUNTRY') == 'BR') {

    dat <- read.table(file.path(Sys.getenv('CAMELS_DIR_DATA'),
                                paste0(catch_id, '_pet_p_t_q.txt')),
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
  dat <- compute_clim_indices_camels(temp = temp, prec = prec, pet = pet, day = day, tol = tol_na)
  camels_clim[i, names(dat)] <- dat

  levels(camels_clim$high_prec_timing) <- c('djf', 'mam', 'jja', 'son')
  levels(camels_clim$low_prec_timing) <- c('djf', 'mam', 'jja', 'son')

  # Compute hydrological signatures for observed Q
  camels_hydro_obs[i, 'gauge_id'] <- as.character(catch_id)
  dat <- compute_hydro_signatures_camels(q = q_obs, p = prec, d = day, tol = tol_na, hydro_year_cal = hydro_year_cal)
  camels_hydro_obs[i, names(dat)] <- dat

  if (Sys.getenv('CAMELS_COUNTRY') == 'US') {
    # Compute hydrological signatures for simulated Q
    camels_hydro_sac[i, 'gauge_id'] <- as.character(catch_id)
    dat <- compute_hydro_signatures_camels(q = q_sim_sac, p = prec, d = day, tol = tol_na, hydro_year_cal = hydro_year_cal)
    camels_hydro_sac[i, names(dat)] <- dat
  }
}

# Save
save(camels_clim, camels_hydro_obs, list_catch,
     file = file.path(Sys.getenv('CAMELS_DIR_RESULTS'),
                      paste0('ci_hs_camels_', Sys.getenv('CAMELS_COUNTRY'), '_',
                             per_str, '_NAtol', tol_na, '.Rdata')))

write.table(camels_clim,
            file = file.path(Sys.getenv('CAMELS_DIR_RESULTS'),
                             paste0('clim_indices_camels_', Sys.getenv('CAMELS_COUNTRY'), '_',
                                    per_str, '_NAtol', tol_na, '.txt')),
            row.names = FALSE,
            quote = FALSE,
            sep = ';'
)

write.table(camels_hydro_obs,
            file = file.path(Sys.getenv('CAMELS_DIR_RESULTS'),
                             paste0('hydro_sign_camels_', Sys.getenv('CAMELS_COUNTRY'), '_',
                                    per_str, '_NAtol', tol_na, '.txt')),
            row.names = FALSE,
            quote = FALSE,
            sep = ';'
)

if (Sys.getenv('CAMELS_COUNTRY') == 'US') {
  write.table(camels_hydro_sac,
              file = file.path(Sys.getenv('CAMELS_DIR_RESULTS'), 'camels_us_hydro_sac.txt'),
              row.names = FALSE,
              quote = FALSE,
              sep = ';'
  )
}

# Plot maps
camels_clim <- merge(camels_topo, camels_clim)
camels_hydro_obs <- merge(camels_topo, camels_hydro_obs)

pdf(file.path(Sys.getenv('CAMELS_DIR_PLOTS'), 'clim.pdf'), 6, 6, useDingbats = FALSE)

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

pdf(file.path(Sys.getenv('CAMELS_DIR_PLOTS'), 'hydro.pdf'), 6, 6, useDingbats = FALSE)

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
