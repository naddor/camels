# This file contains R functions to perform simple date-related operations which
# are used in CAMELS scripts.

# Determine the hydrological year for different countries

get_hydro_year <- function(d, hy_cal) {

  # input variables:
  # d: array of dates of class Date
  # hy_cal: hydrological year calendar, current options are 'oct_us_gb', 'sep_br' and 'apr_cl'

  if (class(d) != 'Date') { stop('d should be of class Date - use as.Date') }

  m <- as.numeric(format(d, '%m')) # extract month
  y <- as.numeric(format(d, '%Y')) # extract year
  hy <- y                         # create array for hydrological year

  if (hy_cal == 'oct_us_gb') {      # USA and Great Britian

    hy[m >= 10] <- (hy[m >= 10] + 1)    # hydrological year 2010 starts on Oct 1st 2009 and finishes on Sep 30th 2010

  } else if (hy_cal == 'sep_br') {  # Brazil

    hy[m >= 9] <- (hy[m >= 9] + 1)      # hydrological year 2010 starts on Sep 1st 2009 and finishes on Aug 31st 2010

  } else if (hy_cal == 'apr_cl') {  # Chile

    hy[m <= 3] <- (hy[m <= 3] - 1)      # hydrological year 2010 starts on Apr 1st 2010 and finishes on Mar 31st 2011

  } else {

    stop(paste0('Unkown hydrological year calendar:', hy_cal))

  }

  return(hy)

}

get_hydro_year_stats <- function(d, hy_cal) {

  # note: this function includes get_hydro_year and should be used instead

  # input variables:
  # d: array of dates of class Date
  # hy_cal: hydrological year calendar, current options are 'oct_us_gb', 'sep_br' and 'apr_cl'

  if (class(d) != 'Date') { stop('d should be of class Date - use as.Date') }

  m <- as.numeric(format(d, '%m')) # extract month
  y <- as.numeric(format(d, '%Y')) # extract year
  hy <- y                         # create array for hydrological year

  if (hy_cal == 'oct_us_gb') {      # USA and Great Britian

    hy[m >= 10] <- (hy[m >= 10] + 1)    # hydrological year 2010 starts on Oct 1st 2009 and finishes on Sep 30th 2010
    start_hy <- as.Date(paste0(hy - 1, '-10-01'))

  } else if (hy_cal == 'sep_br') {  # Brazil

    hy[m >= 9] <- (hy[m >= 9] + 1)      # hydrological year 2010 starts on Sep 1st 2009 and finishes on Aug 31st 2010
    start_hy <- as.Date(paste0(hy - 1, '-09-01'))

  } else if (hy_cal == 'apr_cl') {  # Chile

    hy[m <= 3] <- (hy[m <= 3] - 1)      # hydrological year 2010 starts on Apr 1st 2010 and finishes on Mar 31st 2011
    start_hy <- as.Date(paste0(hy, '-04-01'))

  } else {

    stop(paste0('Unkown hydrological year calendar:', hy_cal))

  }

  day_of_hy <- as.numeric(d - start_hy + 1) # days since the beginning of the hydro year

  if (any(day_of_hy < 1 | day_of_hy > 366)) {

    stop('Error when computing day of hydro year')

  }

  return(data.frame(hy, day_of_hy))

}

# Determine the season based on the month - returns full season name

month2season <- function(m) {

  if (!is.numeric(m)) { m <- as.numeric(m) }

  s <- m
  s[m %in% c(12, 1, 2)] <- 'winter'
  s[m %in% 3:5] <- 'spring'
  s[m %in% 6:8] <- 'summer'
  s[m %in% 9:11] <- 'autumn'

  return(as.factor(s))

}

# Determine the season based on the month - returns season abbreviation

month2sea <- function(m) {

  if (!is.numeric(m)) { m <- as.numeric(m) }

  s <- m
  s[m %in% c(12, 1, 2)] <- 'djf'
  s[m %in% 3:5] <- 'mam'
  s[m %in% 6:8] <- 'jja'
  s[m %in% 9:11] <- 'son'

  return(as.factor(s))

}

# Deal with missing values in an array

find_avail_data_array <- function(x, tol) {

  # input variables:
  # x: array (i.e. time series) to scrutinise
  # tol: tolerated fraction of missing values (e.g. 0.05 for 5%)

  # purpose:
  # determine the time steps for which data are available (i.e. not NA) and check
  # if the fraction of NA elements is greater than the tolerance threshold tol

  # returns:
  # - an array of TRUE/FALSE values indicating for which array elements data are available
  # OR
  # - an array of NAs if the fraction of NA elements exceeds the tolerance threshold

  avail_data <- !(is.na(x)) # time steps for which data are available

  if (sum(!avail_data) >= tol * length(x)) { # more than tol*100 % of the time series are missing

    return(x * NA)       # return a vector of NA values

  } else {

    return(avail_data) # return a vector of TRUE/FALSE values

  }

}

# Deal with missing values in several arrays organised as a data.frame

find_avail_data_df <- function(x, tol) {

  # input variables:
  # x: data.frame (i.e. several time series covering the same period) to scrutinise
  # tol: tolerated fraction of missing values (e.g. 0.05 for 5%)

  # purpose:
  # determine the time steps for which data are available for all the time series
  # (joint availibility) and check for this joint array if the NA fraction is below
  # the given tolerance threshold - see find_avail_data_array

  # determine time steps for which data are available (i.e. not NA) for all the time series
  joint_avail <- apply(x, 1, function(x) ifelse(all(!is.na(x)), TRUE, NA))

  # compare fraction of missing values to prescribed thershold
  avail_data <- find_avail_data_array(joint_avail, tol)

  return(avail_data)

}
