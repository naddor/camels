rm(list=ls())

dir_r_scripts<-'/Volumes/d1/scripts/r_scripts/'

source(paste(dir_r_scripts,'camels/clim/clim_indices.R',sep=''))
source(paste(dir_r_scripts,'camels/hydro/hydro_signatures.R',sep=''))
source(paste(dir_r_scripts,'camels/hydro/hydro_accuracy.R',sep=''))
source(paste(dir_r_scripts,'camels/clim/clim_indices.R',sep=''))

# Define the country
#country='us'
#country='gb'
country='br'

# Define directory for files and list of catchment IDs
if(country=='us'){
  
}else if(country=='gb'){
  
}else if(country=='br'){
  
  dir_input<-'/Volumes/d1/data/camels_br/Brazil_complete_series/'
  list_files<-system(paste0('ls ',dir_input),intern=TRUE)
  list_catch<-rapply(strsplit(list_files,'_'),function(x) x[1])
  
}else{
  stop(paste('Country code unknown:',country))
}

# create data.frames
camels_clim<-data.frame()
camels_hydro_obs<-data.frame()

# loop through catchments, load data and compute CI and HS
for(i in 1:length(list_catch)){
  
  catch_id<-list_catch[i]
  
  # load data
  if(country=='us'){
    
  }else if(country=='gb'){
    
  }else if(country=='br'){
    
    dat<-read.table(paste0(dir_input,'/',catch_id,'_pet_p_t_q.txt'),header=TRUE)
    dat$DD<-sprintf('%02d',dat$DD); dat$MM<-sprintf('%02d',dat$MM)
    day<-as.Date(apply(dat[,1:3],1,paste,collapse='-'))
    
    prec<-dat$P
    temp<-dat$T
    pet<-dat$PET
    q_obs<-dat$Q
    
  }
  
  ### COMPUTE CLIMATE INDICES
  camels_clim[i,'gauge_id']<-as.character(catch_id)
  dat<-compute_clim_indices_camels(temp=temp,prec=prec,pet=pet,day=day)
  camels_clim[i,names(dat)]<-dat
  
  ### COMPUTE HYDROLOGICAL SIGNATURES FOR OBSERVED AND SIMULATED Q
  camels_hydro_obs[i,'gauge_id']<-as.character(camels_name$gauge_id[i])
  dat<-compute_hydro_signatures_camels(q=q_obs,p=prec,d=day,tol=tol_na)
  camels_hydro_obs[i,names(dat)]<-dat

}