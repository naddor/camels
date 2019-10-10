### PURPOSE

# This document contains R functions to compute climatic indices. These functions have been used and are still used to produce the CAMELS datasets. The wrapper compute_clim_indices_camels enables the computation of the indices selected for the original CAMELS paper (Addor et al., 2017, HESS).

# For some indices, several formulations have been implemented and the resulting estimates are returned as a # data.frame. Alternative formulations can be added. The objective is to assess the sensitvity of the results to
# the formulation of the hydrological signatures.

### LOAD FUNCTIONS

source(paste(dir_r_scripts,'camels/time/time_tools.R',sep='')) # for month2sea

### WRAPPER TO COMPUTE STANDARD CAMELS CLIMATIC INDICES

compute_clim_indices_camels<-function(temp,prec,pet,day){

  ind_berghuijs<-compute_climate_indices_berghuijs(temp,prec,pet,day)
  ind_extreme_precip<-compute_extreme_precip_indices(prec,day,rel_hp_thres=5,abs_lp_thres=1)

  return(data.frame(p_mean           = ind_berghuijs$p_mean,
                    pet_mean         = ind_berghuijs$pet_mean,
                    aridity          = ind_berghuijs$aridity,
                    p_seasonality    = ind_berghuijs$p_seasonality,
                    frac_snow        = ind_berghuijs$frac_snow_daily,
                    high_prec_freq   = ind_extreme_precip$high_prec_freq,
                    high_prec_dur    = ind_extreme_precip$high_prec_dur,
                    high_prec_timing = as.character(ind_extreme_precip$high_prec_timing),
                    low_prec_freq    = ind_extreme_precip$low_prec_freq,
                    low_prec_dur     = ind_extreme_precip$low_prec_dur,
                    low_prec_timing  = as.character(ind_extreme_precip$low_prec_timing)))

}

### FUNCTIONS FOR INDIVIDUAL SIGNATURES

# input variables:
# temp: temperature time series
# prec: precipitation time series
# pet: potential evapotranspiration time series
# day: date array of class "Date"
# tol: tolerated proportion of NA values in time series

compute_climate_indices_berghuijs<-function(temp,prec,pet,day){

  # the combination of aridity, fraction of precipitation falling as snow and precipitation
  # seasonality was proposed by Berghuijs et al., 2014, WRR, doi:10.1002/2014WR015692

  # aridity
  p_mean<-mean(prec,na.rm=TRUE)
  pet_mean<-mean(pet,na.rm=TRUE)
  aridity<-pet_mean/p_mean

  # estimate annual temperature and precipitation cycles using sine curves
  t_julian<-strptime(format(day,'%Y%m%d'),'%Y%m%d')$yday # extract day of year
  s_p_first_guess<-90-which.max(rapply(split(prec,format(day,'%m')),mean))*30
  s_p_first_guess<-ifelse(s_p_first_guess<0,s_p_first_guess+360,s_p_first_guess)

  fit_temp = nls(temp ~ mean(temp)+delta_t*sin(2*pi*(t_julian-s_t)/365.25),start=list(delta_t=5,s_t=-90))
  fit_prec = nls(prec ~ mean(prec)*(1+delta_p*sin(2*pi*(t_julian-s_p)/365.25)),start=list(delta_p=0.4,s_p=s_p_first_guess))

  s_p<-summary(fit_prec)$par['s_p','Estimate']
  delta_p<-summary(fit_prec)$par['delta_p','Estimate']
  s_t<-summary(fit_temp)$par['s_t','Estimate']
  delta_t<-summary(fit_temp)$par['delta_t','Estimate']

  # seasonality and timing of precipitation
  delta_p_star<-delta_p*sign(delta_t)*cos(2*pi*(s_p-s_t)/365.25)

  # fraction of precipitation falling as snow - using sine curves
  # see original paper by Woods, 2009, Advances in Water Resources, 10.1016/j.advwatres.2009.06.011
  t_0<-1 # temp thershold [°C]
  t_star_bar<-(mean(temp)-t_0)/abs(delta_t)

  if (t_star_bar>1){ # above freezing all year round

    f_s<-0

  } else if (t_star_bar<(-1)){ # below freezing all year round

    f_s<-1

  } else {

    # there is a square in the original Woods paper (Eq. 13) lacking in the Berghuijs paper (Eq. 6a)
    f_s<-1/2-asin(t_star_bar)/pi-delta_p_star/pi*sqrt(1-t_star_bar^2)

  }

  # fraction of precipitation falling as snow - using daily temp and precip values
  f_s_daily<-sum(prec[temp<=0])/sum(prec)

  return(data.frame(aridity=aridity,p_mean=p_mean,pet_mean=pet_mean,
                    p_seasonality=delta_p_star,frac_snow_sine=f_s,frac_snow_daily=f_s_daily))

}

compute_extreme_precip_indices<-function(prec,day,rel_hp_thres,abs_lp_thres){

  # input variables:
  # rel_hp_thres: the high precipitation threshold is relative [mean daily precipitation]
  # abs_lp_thres: the low precipitation threshold is absolute [mm/day]

  if(any(diff(day)>1)){stop('The time series must be continious')}

  # extract season and hydrological year
  s<-as.factor(month2sea(format(day,'%m')))
  hy<-get_hydro_year(day)

  # frequency and duration of high intensity precipitation events
  hp<-prec>=rel_hp_thres*mean(prec)
  hp_length<-nchar(strsplit(paste(ifelse(hp,'T','F'),collapse=''),'F')[[1]]) # compute number of consecutive high precip days
  hp_length<-hp_length[hp_length>0]
  if(sum(hp_length)!=sum(hp)){stop('Unexpected total number of high precip days')}
  hp_sea<-rapply(split(hp[hp],s[hp],drop=TRUE),length)

  hp_freq<-sum(hp)/length(hp)*365.25
  hp_dur<-mean(hp_length)
  hp_timing<-names(hp_sea)[which.max(hp_sea)]

  # frequency and duration of low intensity precipitation events
  lp<-prec<abs_lp_thres
  lp_length<-nchar(strsplit(paste(ifelse(lp,'T','F'),collapse=''),'F')[[1]]) # compute number of consecutive low precip days
  lp_length<-lp_length[lp_length>0]
  if(sum(lp_length)!=sum(lp)){stop('Unexpected total number of low precip days')}
  lp_sea<-rapply(split(lp[lp],s[lp],drop=TRUE),length)

  lp_freq<-sum(lp)/length(lp)*365.25
  lp_dur<-mean(lp_length)
  lp_timing<-names(lp_sea)[which.max(lp_sea)]

  return(data.frame(high_prec_freq=hp_freq,high_prec_dur=hp_dur,high_prec_timing=hp_timing,
                    low_prec_freq=lp_freq,low_prec_dur=lp_dur,low_prec_timing=lp_timing))

}
