source(paste(dir_r_scripts,'camels/read_camels_hydromet.R',sep=''))
source(paste(dir_r_scripts,'camels/clim/clim_indices.R',sep=''))
source(paste(dir_r_scripts,'camels/hydro/hydro_signatures.R',sep=''))
source(paste(dir_r_scripts,'camels/hydro/hydro_accuracy.R',sep=''))

### CHOSE TIME PERIOD FOR COMPUTATION OF CLIMATE INDICES AND HYDROLOGICAL SIGNATURES
start_date_indices<-'19891001'
end_date_indices<-'20090930'

### CREATE TABLES
camels_clim<-data.frame('gauge_id' = character(),
                       'p_mean'=numeric(),
                       'pet_mean'=numeric(),
                       'seasonality'=numeric(),
                       'frac_snow_sine'=numeric(),
                       'frac_snow_daily'=numeric(),
                       'aridity'=numeric(),
                       'hp_freq'=numeric(),
                       'hp_dur'=numeric(),
                       'hp_timing'=character(),
                       'hp_timing_sign'=logical(),
                       'lp_freq'=numeric(),
                       'lp_dur'=numeric(),
                       'lp_timing'=character(),
                       'lp_timing_sign'=logical(),
                       stringsAsFactors=FALSE)

camels_hydro_obs<-data.frame(gauge_id=camels_name$gauge_id)
camels_hydro_sac<-data.frame(gauge_id=camels_name$gauge_id)

### SIGNATURES AND CLIMATE INDICES

tol_na=0.0502 # two catchments have ~5.01% of missing values, this threshold includes them

for(i in 1:dim(camels_name)[1]){

  print(paste(i,as.character(camels_name$gauge_name[i])))

  ### IMPORT FORCING DATA, OBS AND SIM DISCHARGE AND COLLECT METADATA
  get_catchment_data_arrays(huc=camels_name$huc_02[i],id=camels_name$gauge_id[i],
                            date_start=format(dataset_start,'%Y%m%d'),date_end=format(dataset_end,'%Y%m%d'),
                            forcing_dataset='daymet',ens_method='best')

  ### COMPUTE CLIMATE SIGNATURES
  camels_clim[i,'gauge_id']<-as.character(camels_name$gauge_id[i])

  climate_signatures_berghuijs<-compute_climate_indices_berghuijs(temp,prec,pet,day)

  camels_clim[i,'seasonality']<-climate_signatures_berghuijs$seasonality
  camels_clim[i,'frac_snow_sine']<-climate_signatures_berghuijs$frac_snow_sine
  camels_clim[i,'frac_snow_daily']<-climate_signatures_berghuijs$frac_snow_daily
  camels_clim[i,'aridity']<-climate_signatures_berghuijs$aridity
  camels_clim[i,'p_mean']<-climate_signatures_berghuijs$p_mean
  camels_clim[i,'pet_mean']<-climate_signatures_berghuijs$pet_mean

  climate_signatures_dry_wet<-compute_dry_wet_climate_indices(prec,day,rel_hp_thres=5,abs_lp_thres=1)
  camels_clim[i,'hp_freq']<-climate_signatures_dry_wet$hp_freq
  camels_clim[i,'hp_dur']<-climate_signatures_dry_wet$hp_dur
  camels_clim[i,'hp_timing']<-as.character(climate_signatures_dry_wet$hp_timing)
  camels_clim[i,'hp_timing_sign']<-climate_signatures_dry_wet$hp_timing_sign
  camels_clim[i,'lp_freq']<-climate_signatures_dry_wet$lp_freq
  camels_clim[i,'lp_dur']<-climate_signatures_dry_wet$lp_dur
  camels_clim[i,'lp_timing']<-as.character(climate_signatures_dry_wet$lp_timing)
  camels_clim[i,'lp_timing_sign']<-climate_signatures_dry_wet$lp_timing_sign

  ### COMPUTE SEASONAL HYDROLOGICAL SIGNATURES FOR OBSERVED AND SIMULATED Q
  hydro_signatures_seas_obs<-compute_hydro_signatures_seas(q=q_obs,d=day,tol=tol_na)
  hydro_signatures_seas_sac<-compute_hydro_signatures_seas(q=q_sim_sac,d=day,tol=tol_na)

  camels_hydro_obs[i,'q_mean_yea']<-hydro_signatures_seas_obs$q_mean_yea
  camels_hydro_obs[i,'q_mean_djf']<-hydro_signatures_seas_obs$q_mean_djf
  camels_hydro_obs[i,'q_mean_jja']<-hydro_signatures_seas_obs$q_mean_jja
  camels_hydro_obs[i,'q_seas']<-hydro_signatures_seas_obs$q_seas
  camels_hydro_obs[i,'q_peak']<-hydro_signatures_seas_obs$q_peak

  camels_hydro_sac[i,'q_mean_yea']<-hydro_signatures_seas_sac$q_mean_yea
  camels_hydro_sac[i,'q_mean_djf']<-hydro_signatures_seas_sac$q_mean_djf
  camels_hydro_sac[i,'q_mean_jja']<-hydro_signatures_seas_sac$q_mean_jja
  camels_hydro_sac[i,'q_seas']<-hydro_signatures_seas_sac$q_seas
  camels_hydro_sac[i,'q_peak']<-hydro_signatures_seas_sac$q_peak

  ### COMPUTE HYDROLOGICAL SIGNATURES FROM SAWICZ
  hydro_signatures_sawicz_obs<-compute_hydro_signatures_sawicz(q=q_obs,p=prec,t=temp,d=day,tol=tol_na)
  hydro_signatures_sawicz_sac<-compute_hydro_signatures_sawicz(q=q_sim_sac,p=prec,t=temp,d=day,tol=tol_na)

  camels_hydro_obs[i,'r_qp']<-hydro_signatures_sawicz_obs$r_qp
  camels_hydro_obs[i,'sfdc_yadav_2007']<-hydro_signatures_sawicz_obs$sfdc_yadav_2007
  camels_hydro_obs[i,'sfdc_sawicz_2010']<-hydro_signatures_sawicz_obs$sfdc_sawicz_2010
  camels_hydro_obs[i,'sfdc_mcmillan_2017']<-hydro_signatures_sawicz_obs$sfdc_mcmillan_2017
  camels_hydro_obs[i,'sfdc_addor_2017']<-hydro_signatures_sawicz_obs$sfdc_addor_2017
  camels_hydro_obs[i,'i_bf']<-hydro_signatures_sawicz_obs$i_bf
  camels_hydro_obs[i,'e_qp_sawicz']<-hydro_signatures_sawicz_obs$e_qp_sawicz
  camels_hydro_obs[i,'e_qp_sanka']<-hydro_signatures_sawicz_obs$e_qp_sanka
  #camels_hydro_obs[i,'r_sd']<-hydro_signatures_sawicz_obs$r_sd
  #camels_hydro_obs[i,'r_ld']<-hydro_signatures_sawicz_obs$r_ld

  camels_hydro_sac[i,'r_qp']<-hydro_signatures_sawicz_sac$r_qp
  camels_hydro_sac[i,'sfdc_yadav_2007']<-hydro_signatures_sawicz_sac$sfdc_yadav_2007
  camels_hydro_sac[i,'sfdc_sawicz_2010']<-hydro_signatures_sawicz_sac$sfdc_sawicz_2010
  camels_hydro_sac[i,'sfdc_mcmillan_2017']<-hydro_signatures_sawicz_sac$sfdc_mcmillan_2017
  camels_hydro_sac[i,'sfdc_addor_2017']<-hydro_signatures_sawicz_sac$sfdc_addor_2017
  camels_hydro_sac[i,'i_bf']<-hydro_signatures_sawicz_sac$i_bf
  camels_hydro_sac[i,'e_qp_sawicz']<-hydro_signatures_sawicz_sac$e_qp_sawicz
  camels_hydro_sac[i,'e_qp_sanka']<-hydro_signatures_sawicz_sac$e_qp_sanka
  #camels_hydro_sac[i,'r_sd']<-hydro_signatures_sawicz_sac$r_sd
  #camels_hydro_sac[i,'r_ld']<-hydro_signatures_sawicz_sac$r_ld

  ### COMPUTE HYDROLOGICAL SIGNATURES FROM IDA
  hydro_signatures_westerberg_obs<-compute_hydro_signatures_westerberg(q=q_obs,d=day,tol=tol_na)
  hydro_signatures_westerberg_sac<-compute_hydro_signatures_westerberg(q=q_sim_sac,d=day,tol=tol_na)

  camels_hydro_obs[i,'q95']<-hydro_signatures_westerberg_obs$q95
  camels_hydro_obs[i,'q5']<-hydro_signatures_westerberg_obs$q5
  camels_hydro_obs[i,'hf_freq']<-hydro_signatures_westerberg_obs$hf_freq
  camels_hydro_obs[i,'hf_dur']<-hydro_signatures_westerberg_obs$hf_dur
  camels_hydro_obs[i,'lf_freq']<-hydro_signatures_westerberg_obs$lf_freq
  camels_hydro_obs[i,'lf_dur']<-hydro_signatures_westerberg_obs$lf_dur

  camels_hydro_sac[i,'q95']<-hydro_signatures_westerberg_sac$q95
  camels_hydro_sac[i,'q5']<-hydro_signatures_westerberg_sac$q5
  camels_hydro_sac[i,'hf_freq']<-hydro_signatures_westerberg_sac$hf_freq
  camels_hydro_sac[i,'hf_dur']<-hydro_signatures_westerberg_sac$hf_dur
  camels_hydro_sac[i,'lf_freq']<-hydro_signatures_westerberg_sac$lf_freq
  camels_hydro_sac[i,'lf_dur']<-hydro_signatures_westerberg_sac$lf_dur

  ### COMPUTE MISC HYDROLOGICAL SIGNATURES
  hydro_signatures_misc_obs<-compute_hydro_signatures_misc(q=q_obs,d=day,thres=0,tol=tol_na)
  hydro_signatures_misc_sac<-compute_hydro_signatures_misc(q=q_sim_sac,d=day,thres=0,tol=tol_na)

  camels_hydro_obs[i,'no_flow']<-hydro_signatures_misc_obs$no_flow
  camels_hydro_obs[i,'hfd_mean']<-hydro_signatures_misc_obs$hfd_mean
  camels_hydro_obs[i,'hfd_sd']<-hydro_signatures_misc_obs$hfd_sd

  camels_hydro_sac[i,'no_flow']<-hydro_signatures_misc_sac$no_flow
  camels_hydro_sac[i,'hfd_mean']<-hydro_signatures_misc_sac$hfd_mean
  camels_hydro_sac[i,'hfd_sd']<-hydro_signatures_misc_sac$hfd_sd

}

camels_clim$hp_timing<-as.factor(camels_clim$hp_timing)
camels_clim$lp_timing<-as.factor(camels_clim$lp_timing)
camels_clim$gauge_id<-as.factor(camels_clim$gauge_id)

# SAVE
save(camels_clim,file=paste(dir_camels_attr,'camels_clim.Rdata',sep=''))
write.table(camels_clim,file=paste(dir_camels_attr,'camels_clim.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')

save(camels_hydro_obs,file=paste(dir_camels_attr,'camels_hydro_obs.Rdata',sep=''))
write.table(camels_hydro_obs,file=paste(dir_camels_attr,'camels_hydro_obs.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')

save(camels_hydro_sac,file=paste(dir_camels_attr,'camels_hydro_sac.Rdata',sep=''))
write.table(camels_hydro_sac,file=paste(dir_camels_attr,'camels_hydro_sac.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')
