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

camels_hydro_obs<-data.frame()
camels_hydro_sac<-data.frame()

### SIGNATURES AND CLIMATE INDICES

tol_na=0.0502 # two catchments have ~5.01% of missing values, this threshold includes them

for(i in 1:dim(camels_name)[1]){

  print(paste(i,as.character(camels_name$gauge_name[i])))

  ### IMPORT FORCING DATA, OBS AND SIM DISCHARGE AND COLLECT METADATA
  get_catchment_data_arrays(huc=camels_name$huc_02[i],id=camels_name$gauge_id[i],
                            date_start=start_date_indices,date_end=end_date_indices,
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

  ### COMPUTE HYDROLOGICAL SIGNATURES FOR OBSERVED AND SIMULATED Q

  camels_hydro_obs[i,'gauge_id']<-as.character(camels_name$gauge_id[i])
  dat<-compute_hydro_signatures_camels(q=q_obs,p=prec,d=day,tol=tol_na)
  camels_hydro_obs[i,names(dat)]<-dat

  camels_hydro_sac[i,'gauge_id']<-as.character(camels_name$gauge_id[i])
  dat<-compute_hydro_signatures_camels(q=q_sim_sac,p=prec,d=day,tol=tol_na)
  camels_hydro_sac[i,names(dat)]<-dat

}

camels_clim$hp_timing<-as.factor(camels_clim$hp_timing)
camels_clim$lp_timing<-as.factor(camels_clim$lp_timing)
camels_clim$gauge_id<-as.factor(camels_clim$gauge_id)

# SAVE
write.table(camels_clim,file=paste(dir_camels_attr,'camels_clim.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')

write.table(camels_hydro_obs,file=paste(dir_camels_attr,'camels_hydro.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')

write.table(camels_hydro_sac,file=paste(dir_camels_attr,'camels_hydro_sac.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')
