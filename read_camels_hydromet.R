
### LOAD CATCHMENT METADATA TO RETRIVE CATCHMENT AREA (USGS ESTIMATES) AND COMPUTE SPECIFIC DISCHARGE

gauge_table<-read.table(paste(dir_basin_dataset,'basin_metadata/gauge_information.txt',sep=''),sep='\t',quote='',header=FALSE,skip=1,colClasses=c(rep("factor",3),rep("numeric",3)))
colnames(gauge_table)<-c('huc_02','gage_id','gage_name','gage_lat','gage_lon','area_usgs_km2') # I didn't manage to import the header from the original file because of spaces in "DRAINAGE AREA (KM^2)"
#gauge_table<<-gauge_table[order(gauge_table$gage_id),] # sort catchments by ID

### LOAD DATA FOR DESIRED CATCHMENT INTO INDIVIDUAL ARRAYS (prec, temp, etc)

get_catchment_data_arrays<-function(huc,id,date_start,date_end,forcing_dataset='daymet'){

  catch_data<-get_catchment_data_dataframe(huc,id,date_start,date_end,forcing_dataset)

  prec<<-catch_data$prec
  temp<<-(catch_data$temp_min+catch_data$temp_max)/2
  pet<<-catch_data$pet
  q_obs<<-catch_data$q_obs
  q_sim_sac<<-catch_data$q_sim_sac
  day<<-as.Date(catch_data$date,'%Y%m%d')

}

### RETURN DATA FOR DESIRED CATCHMENT INTO A SINGLE DATAFRAME (with columns prec, temp, etc)
### AND SAVE DAY AS GLOBAL ARRAY
### ALSO SAVE missing_days_sim, prop_na_obs, prop_est_obs AS GLOBAL ARRAY (the last two correspond to the poportion of NA and estimate values in discharge measurments
### between the beginning and the end of the streamflow reccord, i.e. NA values added at the beginning and end of the time series if it's too short are negelected)

get_catchment_data_dataframe<-function(huc,id,date_start='19801001',date_end='20080930',forcing_dataset='daymet'){

  # IMPORT FORCING DATA
  if(forcing_dataset=='daymet'){

    forcing_table<-read.table(paste(dir_basin_dataset,'basin_mean_forcing/daymet/',huc,'/',id,'_lump_cida_forcing_leap.txt',sep=''),skip=3,header=TRUE)

  } else if(forcing_dataset=='maurer'){

    if(id%in%c('02108000','05120500','07067000','09492400')){ # header is incomplete in orginial files

      forcing_table<-read.table(paste(dir_basin_dataset,'basin_mean_forcing/maurer/',huc,'/',id,'_lump_maurer_forcing_leap.txt',sep=''),skip=4,header=FALSE)
      colnames(forcing_table)<-c("year","mnth","day","hr","dayl.s.","prcp.mm.day.","srad.w.m2.","swe.mm.","tmax.c.","tmin.c.","vp.pa.")

    }else{

      forcing_table<-read.table(paste(dir_basin_dataset,'basin_mean_forcing/maurer/',huc,'/',id,'_lump_maurer_forcing_leap.txt',sep=''),skip=3,header=TRUE)

    }

  } else {

    stop(paste('Unkown forcing forcing data set',forcing_dataset))

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
  # q_obs_na<-is.na(streamflow_table$Q)
  # i_qobs_start<-min(which(!q_obs_na))
  # i_qobs_end<-max(which(!q_obs_na))

  # trim observed discharge time series
  # streamflow_table<-streamflow_table[i_qobs_start:i_qobs_end,]
  # t_streamflow<-t_streamflow[i_qobs_start:i_qobs_end]

  prop_na_q_obs<<-round(sum(streamflow_table$QC_FLAG=='M')/length(t_streamflow),2)
  prop_est_q_obs<<-round(sum(streamflow_table$QC_FLAG=='A:e')/length(t_streamflow),2)

  # convert streamflow to mm/day
  streamflow<-streamflow_table$Q*(0.3048^3) # convert ft^3/sec to m^3/sec
  #  streamflow<-streamflow*3600*24*1000/(gauge_table$area_usgs_km2[gauge_table$gage_id==id]*1E6) # convert m^3/sec to mm/day
  #  streamflow<-streamflow*3600*24*1000/(camels_topo$area_gages2[camels_name$gauge_id==id]*1E6) # convert m^3/sec to mm/day
  streamflow<-streamflow*3600*24*1000/(camels_topo$area_geospa_fabric[camels_name$gauge_id==id]*1E6) # convert m^3/sec to mm/day

  # IMPORT ET AND PET FROM SACRAMENTO OUTPUT - COMPUTE MEAN ACCROSS TEN MEMBERS
  output_hydro_files<-system(paste('ls ',dir_basin_dataset,'model_output/flow_timeseries/',forcing_dataset,'/',huc,'/',id,'_??_model_output.txt',sep=''),intern = TRUE)

  if(length(output_hydro_files)!=10){stop('Unexpected number of hydrological output files')}

  et<-array(0)
  pet<-array(0)
  q_obs_sac<-array(0)
  q_sim_sac<-array(0)
  first_file<-TRUE

  for(f in output_hydro_files){

    hydro_sim<-read.table(f,header=TRUE)
    et<-et+hydro_sim$ET
    pet<-pet+hydro_sim$PET
    q_sim_sac<-q_sim_sac+hydro_sim$MOD_RUN

    if(first_file){

      q_obs_sac<-hydro_sim$OBS_RUN
      first_file<-FALSE

    } else {

      if(any(q_obs_sac!=hydro_sim$OBS_RUN)){stop('OBS discharge in different model output files do not match')}

    }
  }

  et<-et/10
  pet<-pet/10
  q_sim_sac<-q_sim_sac/10

  ### 3 MAR 2016: PET computed by Andy can sometimes negative (e.g. 12054000 min PET is -0.13)
  ### 14 MAR 2016: Now this is fixed in version 1.2, but still checking

  if(any(pet<0)){

    stop('Some PET values are negative')

  }

  t_hydro_sim<-as.Date(paste(hydro_sim$YR,sprintf('%02d',hydro_sim$MNTH),sprintf('%02d',hydro_sim$DY),sep=''),format='%Y%m%d')

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

  ### TRIM FORCING DATA

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

  #plot(t_input[1:1000],streamflow_input$streamflow[1:1000],type='l')
  #lines(t_streamflow[1:1000],streamflow[1:1000],col='orange')

  ### SAC HYDROLOGICAL SIMULATIONS: TRIM OR ADD NA USING MERGE - all.x adds NA when obs not available

  pet_input<-merge(data.frame(t_input),data.frame(pet,t_hydro_sim),by.x='t_input',by.y='t_hydro_sim',all.x=TRUE)
  et_input<-merge(data.frame(t_input),data.frame(et,t_hydro_sim),by.x='t_input',by.y='t_hydro_sim',all.x=TRUE)
  q_obs_sac_input<-merge(data.frame(t_input),data.frame(q_obs_sac,t_hydro_sim),by.x='t_input',by.y='t_hydro_sim',all.x=TRUE)
  q_sim_sac_input<-merge(data.frame(t_input),data.frame(q_sim_sac,t_hydro_sim),by.x='t_input',by.y='t_hydro_sim',all.x=TRUE)

  # check consistence of q_obs and sac_q_obs
  # if(any(abs(q_obs_sac-streamflow)>1,na.rm=TRUE)){stop('q_obs_sac and streamflow do not match')}

  # i_max_error<-which.max(q_obs_sac-streamflow)
  # half_window<-50
  # plot(streamflow[i_max_error+(-half_window:half_window)],type='l')
  # lines(q_obs_sac[i_max_error+(-half_window:half_window)],type='l',col='orange')
  # lines(q_sim_sac[i_max_error+(-half_window:half_window)],type='l',col='blue')

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
