rm(list=ls())

# load functions
source(paste(dir_r_scripts,'camels/clim/clim_indices.R',sep=''))
source(paste(dir_r_scripts,'camels/hydro/hydro_signatures.R',sep=''))
source(paste(dir_r_scripts,'camels/maps/plot_maps_camels.R',sep=''))

# Define the country
country='us'
#country='gb'
#country='br'

# Define directory for files and list of catchment IDs
if(country=='us'){

}else if(country=='gb'){

  # set dir and preferences
  hy_cal='oct_us_gb'
  tol=0.85 # Gem asked for no restriction at first (see email from 10 Dec 2019) but the code crashes (streamflow elasticity)
           # when there is only a year of available data, which happens for catchments not in CAMELS-GB, now tolerating 85%
           # of missing values (see email from 16 Dec 2019)
  
  dir_dat<-'/Volumes/d1/data/camels_gb/'
  list_files<-system(paste0('ls ',dir_dat,'timeseries_2dp/'),intern=TRUE)
  list_catch<-rapply(strsplit(list_files,'_'),function(x) x[5])

  # define period over which indices and signatures will be computed
  # 1st Oct 1970 to 30th Sept 2015
  per_str<-'hy1971-2015' # string used to name output files
  per_start<-as.Date('1970-10-01')
  per_end<-as.Date('2015-09-30')
  per_all<-seq(per_start,per_end,by='day')

}else if(country=='br'){

  # set dir and preferences
  hy_cal='sep_br'
  tol=0.05
  dir_dat<-'/Volumes/d1/data/camels_br/'
  dir_plots<-'/Volumes/d1/data/camels_br/plots/'
  list_files<-system(paste0('ls ',dir_dat,'Brazil_complete_series/'),intern=TRUE)
  list_catch<-rapply(strsplit(list_files,'_'),function(x) x[1])

  # define period over which indices and signatures will be computed
  per_str<-'hy1990-2009' # string used to name output files
  per_start<-as.Date('1989-09-01')
  per_end<-as.Date('2009-08-31')
  per_all<-seq(per_start,per_end,by='day')

  # load gauge coordinates
  camels_topo<-read.table(paste0(dir_dat,'brazil_gauges_coordinates.txt'),header=TRUE)

}else{
  stop(paste('Country code unknown:',country))
}

# create data.frames
camels_clim<-data.frame(stringsAsFactors=FALSE)
camels_hydro_obs<-data.frame(stringsAsFactors=FALSE)

# loop through catchments, load data and compute CI and HS
for(i in 1:length(list_catch)){

  catch_id<-list_catch[i]

  print(paste0(i,') ',catch_id))

  # load data
  if(country=='us'){

  }else if(country=='gb'){

    dat<-read.csv(paste0(dir_dat,'timeseries_2dp/CAMELS_GB_hydromet_timeseries_',catch_id,'_19701001-20150930.txt'),header=TRUE,na.strings='NaN')
    day<-as.Date(dat$date)

    prec<-dat$precipitation
    temp<-dat$temperature
    pet<-dat$pet
    q_obs<-dat$discharge_spec

  }else if(country=='br'){

    dat<-read.table(paste0(dir_dat,'Brazil_complete_series/',catch_id,'_pet_p_t_q.txt'),header=TRUE,na.strings='NaN')
    dat$DD<-sprintf('%02d',dat$DD); dat$MM<-sprintf('%02d',dat$MM)

    day<-as.Date(apply(dat[,1:3],1,paste,collapse='-'))

    prec<-dat$P
    temp<-dat$T
    pet<-dat$PET
    q_obs<-dat$Q

  }

  ### SELECT SUB-PERIOD OVER WHICH INDICES WILL BE COMPUTED
  if(min(day)>per_start|max(day)<per_end){
    stop('The period over which the indices should be computed is not fully covered by the data')
  }

  in_period<-day>=per_start&day<=per_end

  prec<-prec[in_period]
  temp<-temp[in_period]
  pet<-pet[in_period]
  q_obs<-q_obs[in_period]
  day<-day[in_period]

  ### COMPUTE CLIMATE INDICES
  camels_clim[i,'gauge_id']<-as.character(catch_id)
  dat<-compute_clim_indices_camels(temp=temp,prec=prec,pet=pet,day=day,tol=tol)
  camels_clim[i,names(dat)]<-dat

  levels(camels_clim$high_prec_timing)<-c('djf','mam','jja','son')
  levels(camels_clim$low_prec_timing)<-c('djf','mam','jja','son')

  ### COMPUTE HYDROLOGICAL SIGNATURES FOR OBSERVED Q
  camels_hydro_obs[i,'gauge_id']<-as.character(catch_id)
  dat<-compute_hydro_signatures_camels(q=q_obs,p=prec,d=day,tol=tol,hy_cal=hy_cal)
  camels_hydro_obs[i,names(dat)]<-dat

}

### SAVE
save(camels_clim,camels_hydro_obs,list_catch,file=paste0(dir_dat,'ci_hs_camels_',country,'_',per_str,'_NAtol',tol,'.Rdata'))
write.table(camels_clim,file=paste0(dir_dat,'clim_indices_camels_',country,'_',per_str,'_NAtol',tol,'.txt'),row.names=FALSE,quote=FALSE,sep=';')
write.table(camels_hydro_obs,file=paste0(dir_dat,'hydro_sign_camels_',country,'_',per_str,'_NAtol',tol,'.txt'),row.names=FALSE,quote=FALSE,sep=';')

### PLOT MAPS
camels_clim<-merge(camels_topo,camels_clim)
camels_hydro_obs<-merge(camels_topo,camels_hydro_obs)

pdf(paste0(dir_plots,'clim.pdf'),6,6,useDingbats=FALSE)

  for(my_var in names(camels_clim[c(-1,-2,-3)])){

    qual=is.factor(camels_clim[,my_var])

    plot_points(x=camels_clim$gauge_lon,y=camels_clim$gauge_lat,z=camels_clim[,my_var],text_legend=my_var,
      country='br',qual= qual,col_scheme=ifelse(qual,'seas','RdYlBu'),color_bar=TRUE)

  }

dev.off()

pdf(paste0(dir_plots,'hydro.pdf'),6,6,useDingbats=FALSE)

  for(my_var in names(camels_hydro_obs[c(-1,-2,-3)])){

    qual=is.factor(camels_hydro_obs[,my_var])

    plot_points(x=camels_clim$gauge_lon,y=camels_clim$gauge_lat,z=camels_hydro_obs[,my_var],text_legend=my_var,
      country='br',qual= qual,col_scheme=ifelse(qual,'seas','RdYlBu'),color_bar=!qual)

  }

dev.off()
