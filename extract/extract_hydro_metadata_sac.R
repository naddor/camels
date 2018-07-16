
# NOTE: THIS SCRIPT COMPUTES COMPUTES SAC ACCURACY FOR CALIB
# AND EVAL OF PERIODS WHICH ARE BASIN-DEPENDENT, I.E. THIS
# CAN INDUCE ARTIFICAL REGIONAL VARIATIONS

source(paste(dir_r_scripts,'camels/read_camels_hydromet.R',sep=''))
source(paste(dir_r_scripts,'camels/hydro/hydro_accuracy.R',sep=''))
source(paste(dir_r_scripts,'camels/hydro/hydro_signatures.R',sep=''))

### CREATE TABLES
camels_hydro_metadata<-data.frame(gauge_id=camels_name$gauge_id)
camels_accuracy_sac<-data.frame(gauge_id=camels_name$gauge_id)

### COMPUTE FRACTION OF MISSING DISCHARGE MEASURMENTS
start_hyear<-as.numeric(format(dataset_start,'%Y'))

if(format(dataset_start,'%m')%in%c(10,11,12)){
  start_hyear<-start_hyear+1
}

stop_hyear<-as.numeric(format(dataset_end,'%Y'))

if(format(dataset_end,'%m')%in%c(10,11,12)){
  stop_hyear<-stop_hyear+1
}

# DETERMINE CENTER YEAR FOR ALL 10, 20, 25 AND 30-YEAR PERIODS
start_hyear_10<-start_hyear:(stop_hyear-9)
start_hyear_20<-start_hyear:(stop_hyear-19)
start_hyear_25<-start_hyear:(stop_hyear-24)
start_hyear_30<-start_hyear:(stop_hyear-29)

# CREATE TABLES TO SAVE FRACTION OF MISSING DISCHARGE MEASURMENTS
perc_q_na_10<-array(dim=c(dim(camels_name)[1],length(start_hyear_10)));colnames(perc_q_na_10)<-start_hyear_10
perc_q_na_20<-array(dim=c(dim(camels_name)[1],length(start_hyear_20)));colnames(perc_q_na_20)<-start_hyear_20
perc_q_na_25<-array(dim=c(dim(camels_name)[1],length(start_hyear_25)));colnames(perc_q_na_25)<-start_hyear_25
perc_q_na_30<-array(dim=c(dim(camels_name)[1],length(start_hyear_30)));colnames(perc_q_na_30)<-start_hyear_30
perc_q_na_tot<-array(dim=dim(camels_name)[1])

for(i in 1:dim(camels_name)[1]){ # loop through catchments

  print(paste(i,as.character(camels_name$gauge_name[i])))
  get_catchment_data_arrays(huc=camels_name$huc_02[i],id=camels_name$gauge_id[i],
                            date_start=format(dataset_start,'%Y%m%d'),date_end=format(dataset_end,'%Y%m%d'),
                            forcing_dataset='daymet',ens_method='best')

  hy<-as.numeric(format(day,'%Y'))
  hy[format(day,'%m')%in%c(10,11,12)]<-hy[format(day,'%m')%in%c(10,11,12)]+1

  # FOR EACH PERIOD, COMPUTE FRACTION OF MISSING VALUES
  for (p in 1:length(start_hyear_10)){
    q_period<-q_obs[hy>=start_hyear_10[p]&hy<start_hyear_10[p]+10]
    perc_q_na_10[i,p]<-sum(is.na(q_period))/length(q_period)
  }

  for (p in 1:length(start_hyear_20)){
    q_period<-q_obs[hy>=start_hyear_20[p]&hy<start_hyear_20[p]+20]
    perc_q_na_20[i,p]<-sum(is.na(q_period))/length(q_period)
  }

  for (p in 1:length(start_hyear_25)){
    q_period<-q_obs[hy>=start_hyear_25[p]&hy<start_hyear_25[p]+20]
    perc_q_na_25[i,p]<-sum(is.na(q_period))/length(q_period)
  }

  for (p in 1:length(start_hyear_30)){
    q_period<-q_obs[hy>=start_hyear_30[p]&hy<start_hyear_30[p]+30]
    perc_q_na_30[i,p]<-sum(is.na(q_period))/length(q_period)
  }

  perc_q_na_tot[i]<-sum(is.na(q_obs))/length(q_obs)

  ### COMPUTE HYDRO METADATA
  camels_hydro_metadata[i,'start_discharge_record']<-start_discharge_record # start_discharge_record is a global variable defined in get_catchment_data_arrays
  camels_hydro_metadata[i,'end_discharge_record']<-end_discharge_record
  camels_hydro_metadata[i,'prop_na_discharge_record']<-prop_na_q_obs
  camels_hydro_metadata[i,'prop_est_discharge_record']<-prop_est_q_obs

  ### COMPUTE SAC ACCURACY
  camels_accuracy_sac[i,'start_hydro_sim']<-start_hydro_sim
  camels_accuracy_sac[i,'end_hydro_sim']<-end_hydro_sim

  ### DERTMINE CALIBRATION AND EVALUATION PERIODS
  if(format(as.Date(start_hydro_sim,'%Y%m%d'),'%m%d')!='1001'){error('Hydro simulation does not start on Oct. 1st')}

  end_calib<-as.Date(paste(as.numeric(substr(start_hydro_sim,1,4))+15,'0930',sep=''),'%Y%m%d') # first 15 hydrological years used for SAC calibration

  dataset_calib<-dataset_days>=as.Date(start_hydro_sim,'%Y%m%d')&dataset_days<=end_calib  # portion of the time period used for calibration
  dataset_eval<-dataset_days>end_calib&dataset_days<=as.Date(end_hydro_sim,'%Y%m%d')      # portion of the time period used for evaluation
  dataset_full_hy<-dataset_days>=as.Date(start_hydro_sim,'%Y%m%d')&dataset_days<=as.Date(end_hydro_sim,'%Y%m%d') # portion used for evaluation and calib

  ### COMPUTE ACCURACY FOR WHOLE SIMULATION PERIOD AND CALIB AND EVAL SUB-PERIODS
  camels_accuracy_sac[i,'nse_run']<-round(compute_nse(obs=q_obs,sim=q_sim_sac),2)
  camels_accuracy_sac[i,'nse_calib']<-round(compute_nse(obs=q_obs[dataset_calib],sim=q_sim_sac[dataset_calib]),2)
  camels_accuracy_sac[i,'nse_eval']<-round(compute_nse(obs=q_obs[dataset_eval],sim=q_sim_sac[dataset_eval]),2)

  camels_accuracy_sac[i,'kge_run']<-round(compute_kge(obs=q_obs,sim=q_sim_sac),2)
  camels_accuracy_sac[i,'kge_calib']<-round(compute_kge(obs=q_obs[dataset_calib],sim=q_sim_sac[dataset_calib]),2)
  camels_accuracy_sac[i,'kge_eval']<-round(compute_kge(obs=q_obs[dataset_eval],sim=q_sim_sac[dataset_eval]),2)

  camels_accuracy_sac[i,'rmse_run']<-round(compute_rmse(obs=q_obs,sim=q_sim_sac),2)
  camels_accuracy_sac[i,'rmse_calib']<-round(compute_rmse(obs=q_obs[dataset_calib],sim=q_sim_sac[dataset_calib]),2)
  camels_accuracy_sac[i,'rmse_eval']<-round(compute_rmse(obs=q_obs[dataset_eval],sim=q_sim_sac[dataset_eval]),2)

  camels_accuracy_sac[i,'dv_run']<-round(compute_dv(obs=q_obs,sim=q_sim_sac),2)
  camels_accuracy_sac[i,'dv_calib']<-round(compute_dv(obs=q_obs[dataset_calib],sim=q_sim_sac[dataset_calib]),2)
  camels_accuracy_sac[i,'dv_eval']<-round(compute_dv(obs=q_obs[dataset_eval],sim=q_sim_sac[dataset_eval]),2)

}

camels_hydro_metadata$start_discharge_record<-as.factor(camels_hydro_metadata$start_discharge_record)
camels_hydro_metadata$end_discharge_record<-as.factor(camels_hydro_metadata$end_discharge_record)
camels_accuracy_sac$start_hydro_sim<-as.factor(camels_accuracy_sac$start_hydro_sim)
camels_accuracy_sac$end_hydro_sim<-as.factor(camels_accuracy_sac$end_hydro_sim)

# SAVE

save(camels_hydro_metadata,file=paste(dir_camels_attr,'camels_hydro_metadata.Rdata',sep=''))
write.table(camels_hydro_metadata,file=paste(dir_camels_attr,'camels_hydro_metadata.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')

save(camels_accuracy_sac,file=paste(dir_camels_attr,'camels_accuracy_sac.Rdata',sep=''))
write.table(camels_accuracy_sac,file=paste(dir_camels_attr,'camels_accuracy_sac.txt',sep=''),
            row.names=FALSE,quote=FALSE,sep=';')

### CREATE PDF TO CHOOSE PERIOD FOR COMPUTATION OF CLIMATE INDICES AND HYDROLOGICAL SIGNATURES

pdf(paste0(dir_data,'basin_attributes/maps_charac/na_discharge_data.pdf'),width=9,height=5,useDingbats=FALSE)

par(mar=c(4,5,1,1))
plot(c(1985,2010),c(525,675),'n',xlab='Center year of the time series',ylab='Number of basins with less than 1% (crosses)\nand 5% (circles) of missing values')
na_thres_list<-c(1,0.95,0.9,0.99,0.8)
abline(h=671*na_thres_list,lty=3)
text(rep(1985,4),671*na_thres_list,paste(na_thres_list*100,'%'),pos=3)

for(na_thres in c(0.05,0.01)){

  perc_q_na_10_tot<-colSums(perc_q_na_10<na_thres)
  perc_q_na_20_tot<-colSums(perc_q_na_20<na_thres)
  #perc_q_na_25_tot<-colSums(perc_q_na_25<na_thres)
  perc_q_na_30_tot<-colSums(perc_q_na_30<na_thres)
  perc_q_na_tot_tot<-sum(perc_q_na_tot<na_thres)

  points(5+as.numeric(names(perc_q_na_10_tot)),perc_q_na_10_tot,pch=ifelse(na_thres==0.05,1,4),col='firebrick3',type='b',lwd=1.5)
  points(10+as.numeric(names(perc_q_na_20_tot)),perc_q_na_20_tot,pch=ifelse(na_thres==0.05,1,4),col='dodgerblue2',type='b',lwd=1.5)
  #points(12.5+as.numeric(names(perc_q_na_25_tot)),perc_q_na_25_tot,pch=ifelse(na_thres==0.05,1,4),col='dodgerblue2',type='b',lwd=1.5)
  points(15+as.numeric(names(perc_q_na_30_tot)),perc_q_na_30_tot,pch=ifelse(na_thres==0.05,1,4),col='springgreen4',type='b',lwd=1.5)
  points(floor((start_hyear+stop_hyear)/2),perc_q_na_tot_tot,pch=ifelse(na_thres==0.05,1,4),col='black',type='b',lwd=1.5)

}

polygon(c(1999.5,2000.5,2000.5,1999.5),c(500,500,685,685))

legend('bottomright',legend = c('10 years','20 years','30 years','34 years'),col=c('firebrick3','dodgerblue2','springgreen4','black'),lwd=1.5)

dev.off()
