source(paste(dir_r_scripts,'camels/time/time_tools.R',sep='')) # for month2sea

# Based on Sawicz, K., Wagener, T., Sivapalan, M., Troch, P. a. and Carrillo, G.:
# Catchment classification: Empirical analysis of hydrologic similarity based
# on catchment function in the eastern USA, Hydrol. Earth Syst. Sci.,
# 15(9), 2895–2911, doi:10.5194/hess-15-2895-2011, 2011.

# Wrapper to compute all signatures at once

compute_hydro_signatures_sawicz<-function(q,p,t,d,tol=0.05){

  r_qp<-comp_r_qp(q,p,tol)
  s_fdc<-comp_s_fdc(q,tol)
  s_fdc_log<-comp_s_fdc_log(q,tol)
  i_bf<-comp_i_bf_landson(q)
  e_qp<-comp_e_qp(q,p,d,tol)
  r_sd<-comp_r_sd(t,p,tol)

  # r_ld<-comp_r_ld(q,tol) # commented out because takes a while to run

  return(data.frame(r_qp=r_qp,s_fdc=s_fdc,s_fdc_log=s_fdc_log,i_bf=i_bf,e_qp,r_sd))

}

# I. Runoff ratio - ratio of long-term average streamflow to long-term average precipitation

comp_r_qp<-function(q,p,tol=0.05){

  # input variables
  # q: discharge time series
  # p: precipitation time series
  # tol: tolerated proportion of NA values in time series

  avail_data<-find_avail_data_matrix(cbind(q,p),tol) # time steps for which obs and sim are available

  r_qp<-mean(q[avail_data])/mean(p[avail_data])

  if((!is.na(r_qp))&r_qp>1){

    warning(paste('Runoff ratio is greater than 1:',r_qp))

  }

  return(r_qp)

}

# II. Slope of FDC in log space between 33rd and 66th percentile

comp_s_fdc_log<-function(q,tol=0.05){

  # input variables
  # q: discharge time series
  # tol: tolerated proportion of NA values in time series

  avail_data<-find_avail_data_array(q,tol) # time steps for which obs and sim are available

  q33<-as.numeric(quantile(q[avail_data],0.33,na.rm=TRUE)) # use empirical quantiles
  q66<-as.numeric(quantile(q[avail_data],0.66,na.rm=TRUE))

  # plot FDC
  quant<-seq(0,1,0.001)
  fdc<-rev(as.numeric(quantile(q[avail_data],quant,na.rm=TRUE))) # rev because probability of exceedance
  #plot(quant,fdc,log='y',ylab='Discharge [mm/day]',xlab='Probability of exceedance',type='l')

  i33<-which(quant==0.33)
  i66<-which(quant==0.66)

  #points(quant[c(i33,i66)],fdc[c(i33,i66)],col='orange',pch=1)

  if(q33==0|is.na(q33)){ # if more than a thrid of data are 0, log(q33) can't be computed

    return(NA)

  }else{

    s_fdc<-(log(q66)-log(q33))/(0.66-0.33)

    return(s_fdc)

  }

}

comp_s_fdc<-function(q,tol=0.05){

  # input variables
  # q: discharge time series
  # tol: tolerated proportion of NA values in time series

  avail_data<-find_avail_data_array(q,tol) # time steps for which obs and sim are available

  q33<-as.numeric(quantile(q[avail_data],0.33,na.rm=TRUE)) # use empirical quantiles
  q66<-as.numeric(quantile(q[avail_data],0.66,na.rm=TRUE))
  qmean<-mean(q[avail_data],na.rm=TRUE)

  s_fdc<-(q66-q33)/(qmean*(0.66-0.33)) # slope of normalized

  return(s_fdc)


}

# III. Baseflow index - long-term baseflow to total streamflow

# This implementation is by Tony Ladson
# https://tonyladson.wordpress.com/2013/10/01/a-standard-approach-to-baseflow-separation-using-the-lyne-and-hollick-filter/#comments

source('https://raw.github.com/TonyLadson/BaseflowSeparation_LyneHollick/master/BFI.R')

comp_i_bf_landson<-function(q,alpha=0.925,passes=3){

  i_bf<-BFI(q,alpha,passes,ReturnQbase=TRUE)$BFI
  #q_base<-BFI(q,alpha,passes,ReturnQbase=TRUE)$Qbase

  return(i_bf)

}

comp_i_bf<-function(q,tol=0.05){

  # input variables
  # q: discharge time series
  # tol: tolerated proportion of NA values in time series

  avail_data<-find_avail_data_array(q,tol) # time steps for which obs and sim are available

  q_d<-comp_q_d(q) # compute direct flow
  q_b<-q-q_d       # compute baseflow

  i_bf<-sum(q_b)/sum(q)

  return(i_bf)

}

# Estimate direct flow using a one-parameter single pass digital filter method

comp_q_d<-function(q,c_const=0.925,tol=0.05){

  # input variables
  # q: discharge time series
  # tol: tolerated proportion of NA values in time series

  test_na(q,tol)

  q_d<-q*NA
  q_d[1]<-0         # to initialize the method, assuming there is no direct flow (only baseflow) at the fist time step

  for (i in 2:length(q)){

    q_d[i]<-c_const*q_d[i-1]+(1+c_const)*(q[i]-q[i-1])/2 # Eq. 1 in Arnold et al. (1995), but different from Eq. 5 in Eckhardt et al. (2008) which expresses baseflow

    if(q_d[i]<=0){q_d[i]<-0} # replace negative direct flow values by 0

  }

  return(q_d)

}

# IV. Streamflow elasticity: sensitivity of a catchment’s streamflow response to changes in precipitation at the annual time scale

comp_e_qp<-function(q,p,d,tol=0.05){

  # input variables
  # q: discharge time series
  # p: precipitation time series
  # d: date array of class "Date"
  # tol: tolerated proportion of NA values in time series

  avail_data<-find_avail_data_matrix(cbind(q,p),tol) # time steps for which obs and sim are available

  if(length(q)!=length(d)|length(p)!=length(d)){stop('P, Q and D must have the same length')}

  hy<-get_hydro_year(d)

  if(any(table(hy)<365)){stop('Not all the hydrological years are complete')}

  mp_tot<-mean(p[avail_data],na.rm=TRUE) # mean long-term precip
  mq_tot<-mean(q[avail_data],na.rm=TRUE) # mean long-term discharge

  mp<-rapply(split(p[avail_data],hy[avail_data]),mean,na.rm=TRUE) # mean annual precip
  mq<-rapply(split(q[avail_data],hy[avail_data]),mean,na.rm=TRUE) # mean annual discharge

  # Anomaly computed with respect to previous year (Sawicz et al., 2011, HESS)
  dp_sawicz<-diff(mp) # precip difference between two consecutive years
  dq_sawicz<-diff(mq) # discharge difference between two consecutive years

  e_qp_sawicz<-median((dq_sawicz/mq_tot)/(dp_sawicz/mp_tot))

  # Anomaly computed with respect to long-term mean (Sankarasubramanian et al., 2001, WRR)
  dp_sanka<-mp-mp_tot
  dq_sanka<-mq-mq_tot

  e_qp_sanka<-median((dq_sanka/mq_tot)/(dp_sanka/mp_tot))

  # Return both estimates
  e_qp<-data.frame(e_qp_sawicz=e_qp_sawicz,e_qp_sanka=e_qp_sanka)

  return(e_qp)

}

# V. Snow day ratio - the number of days that experience precipitation when the average daily air temperature is below 2°C,
# divided by the total number of days per year with precipitation. Note that it does not account for the amount of precipiation.

comp_r_sd<-function(t,p,tol=0.05){

  # input variables
  # t: temperature time series
  # p: precipitation time series
  # tol: tolerated proportion of NA values in time series

  avail_data<-find_avail_data_matrix(cbind(t,p),tol) # time steps for which obs and sim are available

  p_threshold=0 # minimum precipitation amount [mm/day] to consider a day as a rainy day
  t_threshold=2 # temperature [°C] below which a rainy day becomes a snowy day

  r_sd<-sum(p[avail_data]>=p_threshold&t[avail_data]<t_threshold)/sum(p[avail_data]>=p_threshold)

  return(r_sd)

}

# VI. Rising limb density estimated after Appendix A of Morin et al. (2002), Objective, observations-based,
# automatic estimation of the catchment response timescale, WRR, doi:10.1029/2001WR000808.

# This method is iterative: at each iteration the noise parameter 'eps' is increased, which leads
# to a decrease of the number of peaks considered as such, until only one peak remains

# The code takes a long time to run and would need some tuning. It is not shared at this point.

### Mix of signatures and indicators

compute_hydro_signatures_seas<-function(q,d,tol=0.05){

  q_mean<-compute_q_mean(q,d,tol)
  q_seas<-compute_q_seas(q,d,tol)
  q_peak<-compute_q_peak(q,d,tol)

  return(data.frame(q_mean,q_seas,q_peak))

}

compute_q_mean<-function(q,d,tol=0.05){

  avail_data<-find_avail_data_array(q,tol)

  if(any(!is.na(avail_data))){

      sea<-month2sea(format(d[avail_data],'%m')) # determine season
      table_sea<-table(sea)

      if(abs(table_sea[['djf']]-table_sea[['jja']])>0.05*table_sea[['djf']]){
        stop('Seasonal discharge cannot be computed because number of days in DJF and JJA differ significantly')
      }

      q_sea<-rapply(split(q[avail_data],sea),mean)

      q_mean_yea<-mean(q[avail_data])
      q_mean_djf<-q_sea['djf']
      q_mean_jja<-q_sea['jja']

  } else {

    q_mean_yea<-NA
    q_mean_djf<-NA
    q_mean_jja<-NA

  }

  q_mean<-data.frame(q_mean_yea,q_mean_djf,q_mean_jja,row.names='')

  return(q_mean)

}

compute_q_seas<-function(q,d,tol=0.05){

  avail_data<-find_avail_data_array(q,tol)

  q_year<-compute_q_mean(q,d,tol)$q_mean_yea
  q_mon<-rapply(split(q[avail_data],format(d[avail_data],'%m')),mean)

  return(sum(abs(q_mon-q_year))/(q_year*12))

}

compute_q_peak<-function(q,d,tol=0.05){

  avail_data<-find_avail_data_array(q,tol)

  if(all(is.na(q[avail_data]))){

    return(NA)

  } else{

    which.max(rapply(split(q[avail_data],format(d[avail_data],'%m')),mean))

  }

}

# Wrapper to compute all misc signatures at once

compute_hydro_signatures_misc<-function(q,d,thres=0,tol=0.05){

  no_flow<-compute_no_flow(q,thres,tol)
  hfd<-compute_hfd_mean_sd(q,d,tol)

  return(data.frame(no_flow,hfd))

}

# Proportion of time series with dischare below or at a given threshold (0 by default)

compute_no_flow<-function(q,thres=0,tol=0.05){

  avail_data<-find_avail_data_array(q,tol)

  return(sum(q[avail_data]<=thres)/length(q[avail_data]))

}

# Half flow date (Court, 1962): the date on which the cumulative discharge since the beginning of the hydrological year (starting on 1 October) reaches half of the annual discharge

compute_hfd_mean_sd<-function(q,d,tol=0.05){

  avail_data<-find_avail_data_array(q,tol)

  hy<-get_hydro_year(d)

  hy_q<-split(q[avail_data],hy[avail_data]) # discharge for each hydrological year

  q_yearly_sum<-rapply(hy_q,sum)

  hfd<-rapply(hy_q,function(x)

    if(sum(x)==0){

      return(NA) # hfd can't be computed if annual discharge is 0

    } else {

        if(length(x)>(1-tol)*365){ # at 1-tol% of data needed for each year
          return(min(which(cumsum(x)>0.5*sum(x))))
        }else{
          return(NA)
        }
    })

  if(any(hfd<0|hfd>365,na.rm=TRUE)){

    stop(paste('Unexpected value half flow date value:',hfd))

  }

  return(data.frame(hfd_mean=mean(hfd,na.rm=TRUE),hfd_sd=sd(hfd,na.rm=TRUE)))

}

### Based on Westerger and McMillan (2015), HESS, "Uncertainty in hydrological signatures"

# Wrapper to compute all signatures at once

compute_hydro_signatures_westerberg<-function(q,d,tol=0.05){

  qXX<-compute_qXX(q,thres=c(0.95,0.05),tol)
  hf_freq_dur<-compute_hf_freq_dur(q,d,tol)
  lf_freq_dur<-compute_lf_freq_dur(q,d,tol)

  return(data.frame(qXX,hf_freq_dur,lf_freq_dur))

}

# Flow precentiles, i.e. Q95 is exceeded 95% of the time

compute_qXX<-function(q,thres,tol=0.05){

  if(any(thres<0|thres>1)){stop('Threshold must be between 0 and 1')}

  avail_data<-find_avail_data_array(q,tol) # time steps for which obs and sim are available

  if(all(is.na(avail_data))){ # if there is more than tol% of missing value

    qXX<-data.frame(t(rep(NA,length(thres))))

  }else{

    qXX<-data.frame(t(quantile(q[avail_data],1-thres)))

  }

  names(qXX)=paste('q',thres*100,sep='')

  return(qXX)

}

# Frequency and duration of high flows

compute_hf_freq_dur<-function(q,d,tol=0.05){

  avail_data<-find_avail_data_array(q,tol)   # time steps for which obs and sim are available

  if(all(is.na(avail_data))){

    return(data.frame(hf_freq=NA,hf_dur=NA))

  } else {

    hf<-q[avail_data]>9*median(q[avail_data])  # time steps considered as high flows

    if(any(hf)){

      # in dry conditions, the median of q can be 0, so when using >, days with no discharge are high flow...
      # when using > instead not >=, every day with some discharge is a high flow...

      # Mean duration of daily high flow events
      hf_bin<-paste(as.numeric(hf),collapse='')        # a string where one or more 1 indicate a high flow event
      hf_dur_noise<-rapply(strsplit(hf_bin,0),nchar)   # use strsplit to isolate successive time steps with high discharge
      hf_dur<-mean(hf_dur_noise[hf_dur_noise>0])       # mean duration

      # Average number of daily high-flow events per year
      # Note: I used to split the time series into hydrological years but this has 2 drawbacks:
      # 1: it can split extreme events (esp. low flows) in two
      # 2: when there is missing data, they are not accounted for when the mean is computed over all the years
      # Hence, I compute the number of time steps considered as high flow for the whole period and then
      # divide it by the number of time steps with available data

      # hf_hy<-split(hf,get_hydro_year(d[avail_data])) # old method: split time series into hydrological years
      # hf_freq<-mean(rapply(hf_hy,sum))               # compute mean number of time steps considered as high flow per year

      hf_freq<-sum(hf)/sum(avail_data)*365.25

    } else {

      hf_freq<-0
      hf_dur<-0

    }

    return(data.frame(hf_freq,hf_dur))

  }
}

# Frequency and duration of low flows

compute_lf_freq_dur<-function(q,d,tol=0.05){

  avail_data<-find_avail_data_array(q,tol)    # time steps for which obs and sim are available

  if(all(is.na(avail_data))){

    return(data.frame(lf_freq=NA,lf_dur=NA))

  } else {

    lf<-q[avail_data]<=0.2*mean(q[avail_data])  # time steps considered as low flows

      if(any(lf)){

      # Mean duration of daily high flow events
      lf_bin<-paste(as.numeric(lf),collapse='')        # a string where one or more 1 indicate a low flow event
      lf_dur_noise<-rapply(strsplit(lf_bin,0),nchar)   # use strsplit to isolate successive time steps with low discharge
      lf_dur<-mean(lf_dur_noise[lf_dur_noise>0])       # mean duration

      # Average number of daily low flow events per year - see commment in compute_lf_freq_dur
      # lf_hy<-split(lf,get_hydro_year(d[avail_data]))  # old method: split time series into hydrological years
      # lf_freq<-mean(rapply(lf_hy,sum))                # compute mean number of time steps considered as high flow per year

      lf_freq<-sum(lf)/sum(avail_data)*365.25

    } else {

      lf_freq=0
      lf_dur=0

    }

    return(data.frame(lf_freq,lf_dur))

  }
}
