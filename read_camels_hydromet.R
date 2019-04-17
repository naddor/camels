
### LOAD CATCHMENT METADATA TO RETRIVE CATCHMENT AREA (USGS ESTIMATES) AND COMPUTE SPECIFIC DISCHARGE

gauge_table<-read.table(paste(dir_basin_dataset,'basin_metadata/gauge_information.txt',sep=''),sep='\t',quote='',header=FALSE,skip=1,colClasses=c(rep("factor",3),rep("numeric",3)))
colnames(gauge_table)<-c('huc_02','gage_id','gage_name','gage_lat','gage_lon','area_usgs_km2') # I didn't manage to import the header from the original file because of spaces in "DRAINAGE AREA (KM^2)"
#gauge_table<<-gauge_table[order(gauge_table$gage_id),] # sort catchments by ID

### LOAD DATA FOR DESIRED CATCHMENT INTO INDIVIDUAL ARRAYS (prec, temp, etc)

get_catchment_data_arrays<-function(huc,id,date_start,date_end,
                                    forcing_dataset='daymet',ens_method='mean'){

  # ARGUMENTS
  # ens_method: should the SAC runs be averaged ('mean') or should only the best one be used ('best')?

  catch_data<-get_catchment_data_dataframe(huc,id,date_start,date_end,forcing_dataset,ens_method)

  prec<<-catch_data$prec
  temp<<-(catch_data$temp_min+catch_data$temp_max)/2
  pet<<-catch_data$pet
  q_obs<<-catch_data$q_obs
  q_sim_sac<<-catch_data$q_sim_sac
  day<<-as.Date(catch_data$date,'%Y%m%d')

}

### RETURN DATA FOR DESIRED CATCHMENT INTO A SINGLE DATAFRAME (with columns prec, temp, etc)
### AND SAVE DAY AS GLOBAL ARRAY

get_catchment_data_dataframe<-function(huc,id,date_start='19801001',date_end='20080930',
                                       forcing_dataset='daymet',ens_method='mean'){

  # IMPORT FORCING DATA
  if(forcing_dataset=='daymet'){

    forcing_table<-read.table(paste(dir_basin_dataset,'basin_mean_forcing/daymet/',huc,'/',id,'_lump_cida_forcing_leap.txt',sep=''),skip=3,header=TRUE)

  } else if(forcing_dataset=='maurer'){

    if(id%in%c('02108000','05120500','07067000','09492400')){ # header is incomplete in original files

      forcing_table<-read.table(paste(dir_basin_dataset,'basin_mean_forcing/maurer/',huc,'/',id,'_lump_maurer_forcing_leap.txt',sep=''),skip=4,header=FALSE)
      colnames(forcing_table)<-c("year","mnth","day","hr","dayl.s.","prcp.mm.day.","srad.w.m2.","swe.mm.","tmax.c.","tmin.c.","vp.pa.")

    }else{

      forcing_table<-read.table(paste(dir_basin_dataset,'basin_mean_forcing/maurer/',huc,'/',id,'_lump_maurer_forcing_leap.txt',sep=''),skip=3,header=TRUE)

    }

  }else{

    stop(paste('Unkown forcing forcing data set:',forcing_dataset))

  }

  # rename forcing variables
  colnames(forcing_table)<-tolower(colnames(forcing_table)) # converting to lower case, as Daymet and Maurer files use different upper/lower case combinations

  if(all(colnames(forcing_table)==c("year","mnth","day","hr","dayl.s.","prcp.mm.day.","srad.w.m2.","swe.mm.","tmax.c.","tmin.c.","vp.pa."))){

    colnames(forcing_table)<-c('year','month','day','hour','dayl(s)','prcp(mm/day)','srad(W/m2)','swe(mm)','tmax(C)','tmin(C)','vp(Pa)')

  } else{

    stop('Unexpected header of the forcing file')

  }

  t_forcing<-as.Date(paste(forcing_table$year,sprintf('%02d',as.numeric(forcing_table$month)),sprintf('%02d',as.numeric(forcing_table$day)),sep=''),'%Y%m%d')

  # IMPORT STREAMFLOW DATA
  # A ->  streaflow value is certified by USGS as the actual daily mean flow
  # A:e -> streamflow value is certified by the USGS as the actual ESTIMATED daily mean flow
  streamflow_table<-read.table(paste(dir_basin_dataset,'usgs_streamflow/',huc,'/',id,'_streamflow_qc.txt',sep=''),header=FALSE,col.names=c('ID','Y','M','D','Q','QC_FLAG'),fill=TRUE) # fill=TRUE handles cases QC_FLAG is missing
  t_streamflow<-as.Date(paste(streamflow_table$Y,sprintf('%02d',as.numeric(streamflow_table$M)),sprintf('%02d',as.numeric(streamflow_table$D)),sep=''),'%Y%m%d')

  streamflow_table$Q[streamflow_table$Q==-999]<-NA # missing values: change -999 to NA
  if(sum(streamflow_table$QC_FLAG=='M')!=sum(is.na(streamflow_table$Q))){stop('Inconsistency between number of M flags and number of -999 values')}

  # determine the actual start and end date of the streamflow observations - the first day and last day which are not NA
  q_obs_na<-is.na(streamflow_table$Q)
  i_qobs_start<-min(which(!q_obs_na))
  i_qobs_end<-max(which(!q_obs_na))
  start_discharge_record<<-t_streamflow[i_qobs_start]
  end_discharge_record<<-t_streamflow[i_qobs_end]

  # proportion of NA and estimated values
  prop_na_q_obs<<-round(sum(streamflow_table$QC_FLAG=='M')/length(t_streamflow),2)
  prop_est_q_obs<<-round(sum(streamflow_table$QC_FLAG=='A:e')/length(t_streamflow),2)

  # convert streamflow to mm/day
  streamflow<-streamflow_table$Q*(0.3048^3)
  streamflow<-streamflow*3600*24*1000/(camels_topo$area_geospa_fabric[camels_name$gauge_id==id]*1E6) # convert m^3/sec to mm/day
  #  streamflow<-streamflow*3600*24*1000/(gauge_table$area_usgs_km2[gauge_table$gage_id==id]*1E6) # convert m^3/sec to mm/day
  #  streamflow<-streamflow*3600*24*1000/(camels_topo$area_gages2[camels_name$gauge_id==id]*1E6) # convert m^3/sec to mm/day

  # IMPORT ET AND PET FROM SACRAMENTO OUTPUT
  output_hydro_files<-system(paste('ls ',dir_basin_dataset,'model_output/flow_timeseries/',forcing_dataset,'/',huc,'/',id,'_??_model_output.txt',sep=''),intern = TRUE)

  if(length(output_hydro_files)!=10){stop('Unexpected number of hydrological output files')}

  first_file<-TRUE

  for(f in output_hydro_files){ # loop through the 10 files and store their content in arrays

    hydro_sim<-read.table(f,header=TRUE)

    if(first_file){

      et_ens<-hydro_sim$ET
      pet_ens<-hydro_sim$PET
      q_sim_sac_ens<-hydro_sim$MOD_RUN
      q_obs_sac<-hydro_sim$OBS_RUN
      t_hydro_sim<-as.Date(paste(hydro_sim$YR,sprintf('%02d',hydro_sim$MNTH),sprintf('%02d',hydro_sim$DY),sep=''),format='%Y%m%d')

      first_file<-FALSE

    }else{

      et_ens<-cbind(et_ens,hydro_sim$ET)
      pet_ens<-cbind(pet_ens,hydro_sim$PET)
      q_sim_sac_ens<-cbind(q_sim_sac_ens,hydro_sim$MOD_RUN)

      if(any(q_obs_sac!=hydro_sim$OBS_RUN)){stop('OBS discharge in different model output files do not match')}
      if(any(t_hydro_sim!=as.Date(paste(hydro_sim$YR,sprintf('%02d',hydro_sim$MNTH),sprintf('%02d',hydro_sim$DY),sep=''),format='%Y%m%d'))){
        stop('The model runs cover different periods')
      }
    }
  }

  start_hydro_sim<<-t_hydro_sim[1]
  end_hydro_sim<<-t_hydro_sim[length(t_hydro_sim)]

  if(ens_method=='mean'){

      et<-rowMeans(et_ens)
      pet<-rowMeans(pet_ens)
      q_sim_sac<-rowMeans(q_sim_sac_ens)

  }else if(ens_method=='best'){

    # COMPUTE RMSE AND FOR ALL MODEL RUNS OVER THE CALIBRATION PERIOD
    # For the spin up, the first year was run repeatedly until the soil moisture stabilized. Simulations over this
    # spin up period were not saved in the output files, i.e. the output files only contain the post-spin up period.
    # Model calibration was performed over the first 15 years

    start_cal<-t_hydro_sim[1] # start of calibration period
    end_cal<-seq(start_cal,by='year',length.out=16)[16]-1 # end of calibration period
    cal_period<-t_hydro_sim%in%seq(start_cal,end_cal,by='day')

    sac_rmse<-apply(q_sim_sac_ens[cal_period,],2,compute_rmse,obs=q_obs_sac[cal_period])
    sac_nse<-apply(q_sim_sac_ens[cal_period,],2,compute_nse,obs=q_obs_sac[cal_period])
    best_ps_rmse<-which.min(sac_rmse) # find index of best parameter set

    et<-et_ens[,best_ps_rmse]
    pet<-pet_ens[,best_ps_rmse]
    q_sim_sac<-q_sim_sac_ens[,best_ps_rmse]

  }else{

    stop(paste('Unkown method for the aggregation of the SAC runs:',ens_method))

  }

  ### 3 MAR 2016: PET computed by Andy can sometimes negative (e.g. 12054000 min PET is -0.13)
  ### 14 MAR 2016: Now this is fixed in version 1.2, but still checking

  if(any(pet<0)){

    stop('Some PET values are negative')

  }

  ### 14 MAR 2016: There used to be missing days in the simulations, now this is fixed in version 1.2, but still checking
  ### 8 MAR 2018: Two entries for 2008/12/31 in Maurer simulations (last two entries of each file)

  if(forcing_dataset=='maurer'){

    i_20081231<-which(t_hydro_sim==as.Date('2008-12-31'))

    if(length(i_20081231)==2){

      # remove second entry for 2008/12/31
      t_hydro_sim<-t_hydro_sim[-i_20081231[2]]
      et<-et[-i_20081231[2]]
      pet<-pet[-i_20081231[2]]
      q_sim_sac<-q_sim_sac[-i_20081231[2]]
      q_obs_sac<-q_obs_sac[-i_20081231[2]]

    }
  }

  if(any(diff(t_hydro_sim)!=1)){

    stop('There are missing or duplicated days in the simulated time series')

  }

  ### EXTRACT DESIRED PERIOD FROM EACH TIME SERIES AND SAVE DAY AS GLOBAL ARRAY
  t_input<<-seq(as.Date(date_start,'%Y%m%d'),as.Date(date_end,'%Y%m%d'),by='day')

  ### FORCING: TRIM DATA IF NECESSARY

  if(min(t_forcing)<=min(t_input)&max(t_forcing)>=max(t_input)){

    forcing_table<-forcing_table[t_forcing>=min(t_input)&t_forcing<=max(t_input),]

    if(any(t_forcing[t_forcing>=min(t_input)&t_forcing<=max(t_input)]!=t_input)){

      stop('t_forcing and t_input differ')

    }

  } else if(min(t_forcing)>min(t_input)){

    stop(paste('Forcing data start on ',min(t_forcing),' so forcing for ',min(t_input),' cannot be extracted.',sep=''))

  } else if(max(t_forcing)<max(t_input)){

    stop(paste('Forcing data end on ',max(t_forcing),' so forcing for ',max(t_input),' cannot be extracted.',sep=''))

  }

  ### STREAMFLOW: TRIM OR ADD NA USING MERGE - # all.x adds NA when obs not available

  streamflow_input<-merge(data.frame(t_input),data.frame(streamflow,t_streamflow),by.x='t_input',by.y='t_streamflow',all.x=TRUE)

  ### SAC HYDROLOGICAL SIMULATIONS: TRIM OR ADD NA USING MERGE - all.x adds NA when obs not available
  pet_input<-merge(data.frame(t_input),data.frame(pet,t_hydro_sim),by.x='t_input',by.y='t_hydro_sim',all.x=TRUE)
  et_input<-merge(data.frame(t_input),data.frame(et,t_hydro_sim),by.x='t_input',by.y='t_hydro_sim',all.x=TRUE)
  q_obs_sac_input<-merge(data.frame(t_input),data.frame(q_obs_sac,t_hydro_sim),by.x='t_input',by.y='t_hydro_sim',all.x=TRUE)
  q_sim_sac_input<-merge(data.frame(t_input),data.frame(q_sim_sac,t_hydro_sim),by.x='t_input',by.y='t_hydro_sim',all.x=TRUE)

  # check consistence of q_obs and sac_q_obs
  # if(any(abs(q_obs_sac-streamflow)>1,na.rm=TRUE)){stop('q_obs_sac and streamflow do not match')}

  # create table with all data
  output_table<-data.frame(date=format(t_input,'%Y%m%d'),
                           day_length=forcing_table[,'dayl(s)'],
                           prec=forcing_table[,'prcp(mm/day)'],
                           sw_inc_rad=forcing_table[,'srad(W/m2)'],
                           temp_min=forcing_table[,'tmin(C)'],
                           temp_max=forcing_table[,'tmax(C)'],
                           vapor_pressure=forcing_table[,'vp(Pa)'],
                           pet=pet_input$pet,
                           et=et_input$et,
                           q_obs=streamflow_input$streamflow,
                           q_obs_sac=q_obs_sac_input$q_obs_sac,
                           q_sim_sac=q_sim_sac_input$q_sim_sac)

  return(output_table)

}
