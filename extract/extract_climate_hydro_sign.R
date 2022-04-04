rm(list = ls())

library(dotenv)
library(here)

source(here::here('extract/read_camels_hydromet.R'))
source(here::here('clim/clim_indices.R'))
source(here::here('hydro/hydro_signatures.R'))
source(here::here('hydro/hydro_accuracy.R'))

# Choose time period for computation of climate indices and hydrological signatures
start_date_indices <- '19891001'
end_date_indices <- '20090930'

# Create tables
camels_clim <- data.frame()
camels_hydro_obs <- data.frame()
camels_hydro_sac <- data.frame()

# Signatures and climate indices

tol_na <- 0.0502 # Two CAMELS catchments have ~5.01% of missing values, this threshold includes them

for (i in 1:dim(camels_name)[1]) {

  print(paste(i, as.character(camels_name$gauge_name[i])))

  # Import forcing data, obs and sim discharge and collect metadata
  get_catchment_data_arrays(huc = camels_name$huc_02[i],
                            id = camels_name$gauge_id[i],
                            date_start = start_date_indices,
                            date_end = end_date_indices,
                            forcing_dataset = 'daymet',
                            ens_method = 'best'
  )

  # Compute climate indices
  camels_clim[i, 'gauge_id'] <- as.character(camels_name$gauge_id[i])
  dat <- compute_clim_indices_camels(temp = temp, prec = prec, pet = pet, day = day)
  camels_clim[i, names(dat)] <- dat

  # Compute hydrological signatures for observed and simulated Q
  camels_hydro_obs[i, 'gauge_id'] <- as.character(camels_name$gauge_id[i])
  dat <- compute_hydro_signatures_camels(q = q_obs, p = prec, d = day, tol = tol_na)
  camels_hydro_obs[i, names(dat)] <- dat

  camels_hydro_sac[i, 'gauge_id'] <- as.character(camels_name$gauge_id[i])
  dat <- compute_hydro_signatures_camels(q = q_sim_sac, p = prec, d = day, tol = tol_na)
  camels_hydro_sac[i, names(dat)] <- dat

}

camels_clim$gauge_id <- as.factor(camels_clim$gauge_id)
camels_hydro_obs$gauge_id <- as.factor(camels_hydro_obs$gauge_id)
camels_hydro_sac$gauge_id <- as.factor(camels_hydro_sac$gauge_id)

# Save
write.table(camels_clim,
            file = file.path(Sys.getenv('CAMELS_DIR_RESULTS'), 'camels_clim.txt'),
            row.names = FALSE,
            quote = FALSE,
            sep = ';'
)

write.table(camels_hydro_obs,
            file = file.path(Sys.getenv('CAMELS_DIR_RESULTS'), 'camels_hydro.txt'),
            row.names = FALSE,
            quote = FALSE,
            sep = ';'
)

write.table(camels_hydro_sac,
            file = file.path(Sys.getenv('CAMELS_DIR_RESULTS'), 'camels_hydro_sac.txt'),
            row.names = FALSE,
            quote = FALSE,
            sep = ';'
)
