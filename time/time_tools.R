# Modified by Camila Alvarez-Garreton on November 2017 to account for water years in southern hemisphere and to allow a tolerance NA portion as input variable.

get_hydro_year<-function(d,hem){
  # input variables: 
  # d:  array of dates of class Date
  # hem: optional argument, character 'n' for northern hemisphere (default value) or 's' for southern hemisphere 

  if (missing(hem)){hem<-'n'}
  
  if (hem=='n'){
    if(class(d)!='Date'){stop('d should be of class Date - use as.Date')}
  
    m<-as.numeric(format(d,'%m')) # extract month
    y<-as.numeric(format(d,'%Y')) # extract year
    hy<-y                         # create array for hydrological year
    hy[m>=10]<-(hy[m>=10]+1)      # hydrological year starts on oct 1. st
  
  return(hy)
  }
  
  else if (hem=='s'){ 
    if(class(d)!='Date'){stop('d should be of class Date - use as.Date')}
      
      m<-as.numeric(format(d,'%m')) # extract month
      y<-as.numeric(format(d,'%Y')) # extract year
      hy<-y                         # create array for hydrological year
      hy[(m)<=3]<-(hy[(m)<=3]-1)    # hydrological year starts on apr 1. st and finishes mar 31st (of the following year)
      
      return(hy)
  }
  
  else{stop('hem should be "n" (northern hemisphere), "s" (southern hemisphere), or left blank ("n" is taken as default value)')}
}

month2season<-function(m,hem){
  # input variables: 
  # m:  array of months
  # hem: optional argument, character 'n' for northern hemisphere (default value) or 's' for southern hemisphere 
  
  if (missing(hem)){hem<-'n'}
  
  if (hem=='n'){
    if(!is.numeric(m)){m<-as.numeric(m)}

    s<-m
    s[m%in%c(12,1,2)]<-'winter'
    s[m%in%3:5]<-'spring'
    s[m%in%6:8]<-'summer'
    s[m%in%9:11]<-'autumn'

    return(as.factor(s))
  }

  else if (hem=='s'){ 
    if(!is.numeric(m)){m<-as.numeric(m)}
      
    s<-m
    s[m%in%c(12,1,2)]<-'summer'
    s[m%in%3:5]<-'autumn'
    s[m%in%6:8]<-'winter'
    s[m%in%9:11]<-'spring'
      
    return(as.factor(s))
  }
  
  else{stop('hem should be "n" (northern hemisphere), "s" (southern hemisphere), or left blank ("n" is taken as default value)')}
}

month2sea<-function(m){
  # input variables: 
  # m: array of months

  if(!is.numeric(m)){m<-as.numeric(m)}

  s<-m
  s[m%in%c(12,1,2)]<-'djf'
  s[m%in%3:5]<-'mam'
  s[m%in%6:8]<-'jja'
  s[m%in%9:11]<-'son'

  return(as.factor(s))
}

### NA IN TIME SERIES
# Takes a time series as input, determine the time steps for which data not NA,
# and check if the NA fraction is below a given tolerance threshold

find_avail_data_array<-function(x,tol){
  # input variables: 
  # x:  time-series object
  # tol:  tolerance fraction of NA values to perform calculation

  avail_data<-!(is.na(x)) # time steps for which data are available

  if(sum(!avail_data)>=tol*length(x)){ # more than tol*100 % of the time series is missing

    return(x*NA)

  } else {

    return(avail_data) # return a vector of TRUE/FALSE values

  }

}

# Take several time series covering the same period and orgranized in a matrix as input,
# determine the time steps for which data are available for all the time series (joint availibility),
# and check for this joint vector if the NA fraction is below a given tolerance threshold

find_avail_data_matrix<-function(x,tol){
  # input variables: 
  # x:  time-series object
  # tol:  tolerance fraction of NA values to perform calculation
  
  if(dim(x)[2]>dim(x)[1]){ # if time series organized as rows, transpose the matrix

    x<-t(x)

  }

  joint_avail<-rowSums(x) # check the joint availibility of the time series: for each day,
                          # returns NA if at least one of the time serie has non-available data on that day
                          # otherwise returns a non-interpretable number (e.g. sum of a temparture and a discharge measurment)

  avail_data<-find_avail_data_array(joint_avail,tol)

  return(avail_data)

}
