
source(paste(dir_r_scripts,'camels/time/time_tools.R',sep='')) # for month2sea

compute_climate_indices_berghuijs<-function(temp,prec,pet,day){

  # Berghuijs, W. R., Sivapalan, M., Woods, R. A. and Savenije, H. H. G.:
  # Patterns of similarity of seasonal water balances: A window into streamflow
  # variability over a range of time scales, Water Resour. Res., 50, 5638–5661,
  # doi:10.1002/2014WR015692, 2014.

  # aridity
  p_mean<-mean(prec,na.rm=TRUE)
  pet_mean<-mean(pet,na.rm=TRUE)
  aridity<-pet_mean/p_mean

  # estimate annual cycles using sine curves
  t_julian<-strptime(format(day,'%Y%m%d'),'%Y%m%d')$yday

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

  # fraction of precipitation falling as snow
  # Woods, 2009, "Analytical model of seasonal climate impacts on snow hydrology: Continuous snowpacks"
  t_0<-1 # tempthershold [°C]
  t_star_bar<-(mean(temp)-t_0)/abs(delta_t)

  if (t_star_bar>1){ # above freezing all year round

    f_s<-0

  } else if (t_star_bar<(-1)){ # below freezing all year round

    f_s<-1

  } else {

    f_s<-1/2-asin(t_star_bar)/pi-delta_p_star/pi*sqrt(1-t_star_bar^2) # there is a square in the originial paper by Woods

  }

  # fraction of precipitation falling as snow using daily time series

  f_s_daily<-sum(prec[temp<t_0])/sum(prec)

  return(data.frame(aridity=aridity,seasonality=delta_p_star,frac_snow_sine=f_s,frac_snow_daily=f_s_daily,p_mean=p_mean,pet_mean=pet_mean))

}

compute_dry_wet_climate_indices<-function(prec,day,rel_hp_thres=5,abs_lp_thres=1,hem){
  if (missing(hem)){hem<-'n'}

  if(any(diff(day)>1)){stop('Time series must be continious')}

  s<-as.factor(month2sea(format(day,'%m')))
  hy<-get_hydro_year(day,hem)

  ### FREQUENCY AND DURATION OF HIGH INTENSITY PRECIPITATION EVENTS
  hp<-prec>=rel_hp_thres*mean(prec)
  hp_length<-nchar(strsplit(paste(ifelse(hp,'T','F'),collapse=''),'F')[[1]]) # compute number of consecutive high precip days
  hp_length<-hp_length[hp_length>0]
  if(sum(hp_length)!=sum(hp)){stop('Unexpected total number of high precip days')}
  hp_sea<-rapply(split(hp[hp],s[hp],drop=TRUE),length)

  hp_freq<-sum(hp)/length(hp)*365.25
  hp_dur<-mean(hp_length)

  ### FREQUENCY AND DURATION OF LOW INTERNSITY PRECIPITATION EVENTS
  lp<-prec<abs_lp_thres
  lp_length<-nchar(strsplit(paste(ifelse(lp,'T','F'),collapse=''),'F')[[1]]) # compute number of consecutive low precip days
  lp_length<-lp_length[lp_length>0]
  if(sum(lp_length)!=sum(lp)){stop('Unexpected total number of low precip days')}
  lp_sea<-rapply(split(lp[lp],s[lp],drop=TRUE),length)

  lp_freq<-sum(lp)/length(lp)*365.25
  lp_dur<-mean(lp_length)
  lp_timing<-names(lp_sea)[which.max(lp_sea)]

  ### COMPUTE TIMIING SIGNIFICANCE
  hp_count<-data.frame()
  lp_count<-data.frame()
  sea_order<-c('djf','jja','mam','son')

  for(my_hy in unique(hy)){

    hp_count<-rbind(hp_count,rapply(split(hp[hp&hy==my_hy],s[hp&hy==my_hy]),length)[sea_order])
    lp_count<-rbind(lp_count,rapply(split(lp[lp&hy==my_hy],s[lp&hy==my_hy]),length)[sea_order])

  }

  colnames(hp_count)<-sea_order
  colnames(lp_count)<-sea_order

  if(sum(hp_count)!=sum(hp)){stop('Unexpected total number of high precip days after computing yearly statistics')}
  if(sum(lp_count)!=sum(lp)){stop('Unexpected total number of low precip days after computing yearly statistics')}

  hp_count_sort<-hp_count[,rev(order(colSums(hp_count)))]
  lp_count_sort<-lp_count[,rev(order(colSums(lp_count)))]

  hp_timing<-names(hp_count_sort)[1]
  lp_timing<-names(lp_count_sort)[1]

  # are there significantly more high (resp. low) precipitation events during the season with the highest total number
  # than during the season with the second highest number?

  hp_timing_sign<-(t.test(hp_count_sort[,1],hp_count_sort[,2],alternative='greater')$p.value<0.05)
  lp_timing_sign<-(t.test(lp_count_sort[,1],lp_count_sort[,2],alternative='greater')$p.value<0.05)

  return(data.frame(hp_freq=hp_freq,hp_dur=hp_dur,hp_timing=hp_timing,hp_timing_sign=hp_timing_sign,
                    lp_freq=lp_freq,lp_dur=lp_dur,lp_timing=lp_timing,lp_timing_sign=lp_timing_sign))

}
