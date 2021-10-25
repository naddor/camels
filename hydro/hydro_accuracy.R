source(paste0(dir_r_scripts, 'camels/time/time_tools.R')) # for find_avail_data_df

### NSE

compute_nse <- function(obs, sim, tol = 0.05) {

  if (length(obs) != length(sim)) { stop('the length of OBS and SIM differ') }

  avail_data <- find_avail_data_df(cbind(obs, sim), tol) # time steps for which obs and sim are available

  nse <- 1 - sum((sim[avail_data] - obs[avail_data])^2) / sum((obs[avail_data] - mean(obs[avail_data]))^2)

  return(nse)

}

### RMSE

compute_rmse <- function(obs, sim, tol = 0.05) {

  avail_data <- find_avail_data_df(cbind(obs, sim), tol) # time steps for which obs and sim are available

  sqrt(sum((sim[avail_data] - obs[avail_data])^2) / length(obs[avail_data]))

}

### ERROR IN WATER BALANCE

compute_dv <- function(obs, sim, tol = 0.05) {

  avail_data <- find_avail_data_df(cbind(obs, sim), tol) # time steps for which obs and sim are available

  (sum(sim[avail_data]) - sum(obs[avail_data])) / sum(obs[avail_data])

}

### KLING GUPTA EFFICIENCY

compute_kge <- function(obs, sim, tol = 0.05, return_decomp = FALSE) {

  avail_data <- find_avail_data_df(cbind(obs, sim), tol) # time steps for which obs and sim are available

  r <- cor(obs[avail_data], sim[avail_data], use = "everything")
  alpha <- sd(sim[avail_data]) / sd(obs[avail_data])
  beta <- mean(sim[avail_data]) / mean(obs[avail_data])

  kge <- 1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)

  if (return_decomp) {

    return(data.frame(kge = kge, r = r, alpha = alpha, beta = beta))

  } else {

    return(kge)

  }

}
