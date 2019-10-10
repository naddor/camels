source(paste(dir_r_scripts,'camels/read_camels_hydromet.R',sep=''))
source(paste(dir_r_scripts,'camels/clim/clim_indices.R',sep=''))
source(paste(dir_r_scripts,'camels/hydro/hydro_signatures.R',sep=''))
source(paste(dir_r_scripts,'camels/hydro/hydro_accuracy.R',sep=''))

### CHOSE TIME PERIOD FOR COMPUTATION OF CLIMATE INDICES AND HYDROLOGICAL SIGNATURES
start_date_indices<-'19891001'
end_date_indices<-'20090930'

### CREATE TABLES
camels_clim<-data.frame()
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

  ### COMPUTE CLIMATE INDICES
  camels_clim[i,'gauge_id']<-as.character(camels_name$gauge_id[i])
  dat<-compute_clim_indices_camels(temp=temp,prec=prec,pet=pet,day=day)
  camels_clim[i,names(dat)]<-dat

  ### COMPUTE HYDROLOGICAL SIGNATURES FOR OBSERVED AND SIMULATED Q
  camels_hydro_obs[i,'gauge_id']<-as.character(camels_name$gauge_id[i])
  dat<-compute_hydro_signatures_camels(q=q_obs,p=prec,d=day,tol=tol_na)
  camels_hydro_obs[i,names(dat)]<-dat

  camels_hydro_sac[i,'gauge_id']<-as.character(camels_name$gauge_id[i])
  dat<-compute_hydro_signatures_camels(q=q_sim_sac,p=prec,d=day,tol=tol_na)
  camels_hydro_sac[i,names(dat)]<-dat

}

camels_clim$gauge_id<-as.factor(camels_clim$gauge_id)
camels_hydro_obs$gauge_id<-as.factor(camels_hydro_obs$gauge_id)
camels_hydro_sac$gauge_id<-as.factor(camels_hydro_sac$gauge_id)

# SAVE
write.table(camels_clim,file=paste(dir_camels_attr,'camels_clim.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')

write.table(camels_hydro_obs,file=paste(dir_camels_attr,'camels_hydro.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')

write.table(camels_hydro_sac,file=paste(dir_camels_attr,'camels_hydro_sac.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')
