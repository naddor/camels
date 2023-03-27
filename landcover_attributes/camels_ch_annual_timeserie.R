# ==============================================================================
# Creating annual time series per catchment for clc and glacier data
# 
# Eawag, Switzerland, Jan. 2022
#
# usula.schoenenberger@eawag.ch
# ==============================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##################################
## prepare lists and variables
list_files_clc<-c("crop_perc", "dwood_perc", "ewood_perc", "grass_perc", "ice_perc","inwater_perc", "loose_rock_perc", "mix_wood_perc", "rock_perc", "shrub_perc", "urban_perc", "wetlands_perc","blank_perc")
list_files_glacier<-c("glac_area_ch_[km2]","glac_vol_ch_[km3]","glac_mass_ch_[Mt]","glac_area_neighbours_[km2]")
list_parameter_name<-c(list_files_clc,list_files_glacier)

# set up time sequence
years_clc <- seq(1990,2018,1)
years_all <- seq(1981,2021,1)

##################################
## open clc files, create attribute list for glaciers and add to data_list

# function to open clc files
open_clc_file <- function (clc_name){
  # open clc-file
  file_clc <- read.table(clc_name,skip=1, header=TRUE, sep=";")
  # replace "X" in column names
  colnames(file_clc) <- gsub("^X", "",  colnames(file_clc))
  if(clc_name=="clc_1990_perc.csv"){
    file_clc<-rbind(file_clc[2:13,],file_clc[1,])
    # replace values in catchments which cover less than 95% area
    for(i in 1:ncol(file_clc)){
      if(file_clc[13,i]>5){
        file_clc[,i]<-NaN
      }
    }
  }else{
    file_clc<-rbind(file_clc,rep(0,331))
    row.names(file_clc)<-list_files_clc
  } 
  return(file_clc)
}

# run function "open_clc_file" for all clc-files
file_clc1990 <- open_clc_file("clc_1990_perc.csv")
file_clc2000 <- open_clc_file("clc_2000_perc.csv")
file_clc2006 <- open_clc_file("clc_2006_perc.csv")
file_clc2012 <- open_clc_file("clc_2012_perc.csv")
file_clc2018 <- open_clc_file("clc_2018_perc.csv")

# get catchment IDs
catch_names <- colnames(file_clc1990)

# get number of catchments
catch_num<-ncol(file_clc1990)

# create time serie and data list for non-blank clc
data_list <- NULL

# function to create time series and arrange in data_list
add_clc <- function (list_attr, list_pos, end_list){
  # run loop for all attributes in clc-file
  for(m in 1:length(list_attr)){
    data_1990_2018<-NULL
    data_list_pos<-list_pos[m]
    # extract data from attribute m for catchment j
    for(j in 1:catch_num){
      value_1990<-file_clc1990[m,j]
      value_2000<-file_clc2000[m,j]
      value_2006<-file_clc2006[m,j]
      value_2012<-file_clc2012[m,j]
      value_2018<-file_clc2018[m,j]
      # fill gaps with interpolated data (linear regression)
      if(is.na(value_1990)){
        fill_value_1<-rep(NaN,11)
      }else{if(value_1990==value_2000){
        fill_value_1<-rep(value_2000,11)
      }else{
        fill_value_1<-seq(value_1990,value_2000, by=((value_2000-value_1990)/10))}
      }
      if(value_2000==value_2006){
        fill_value_2<-rep(value_2006,7)
      }else{
        fill_value_2<-seq(value_2000,value_2006, by=((value_2006-value_2000)/6))}
      if(value_2006==value_2012){
        fill_value_3<-rep(value_2012,7)
      }else{
        fill_value_3<-seq(value_2006,value_2012, by=((value_2012-value_2006)/6))}
      if(value_2012==value_2018){
        fill_value_4<-rep(value_2018,7)
      }else{
        fill_value_4<-seq(value_2012,value_2018, by=((value_2018-value_2012)/6))}
      # merge values to time serie and round to two digits
      data_1990_2018<-round(rbind(data_1990_2018,c(fill_value_1[1:10],fill_value_2[1:6],fill_value_3[1:6],fill_value_4[1:7])),2)
    }
    # add row and column names
    data_1990_2018<-data.frame(t(data_1990_2018))
    rownames(data_1990_2018) <- years_clc
    colnames(data_1990_2018) <- catch_names
    # fill data gaps between 1981 and 1989 with NaN, and between 2019 and 2021 with data from 2018
    data_pre <- data.frame(matrix(nrow = 9, ncol = 331),row.names=seq(1981,1989,1))
    data_pre[is.na(data_pre)] <- "NaN"
    colnames(data_pre)<-catch_names
    data_post <- data.frame(matrix(nrow = 3, ncol = 331),row.names=seq(2019,2021,1))
    data_post[1:3,]<-data_1990_2018[29,]
    colnames(data_post)<-catch_names
    # bind time serie
    data_1981_2021<-rbind(data_pre,data_1990_2018,data_post)
    # add to list
    end_list[data_list_pos] <-list(data_1981_2021)
  }
  return(end_list)
}

# run function "add_clc"
data_list_clc<-add_clc(list_files_clc,c(rep(1:13)),data_list)

##################################
## open glacier files, create attribute list for glaciers and add to data_list

# function to insert glacier data into data_list
add_glacier<-function (glacier_file_csv,list_pos,end_list, round_num){
  for(m in 1:length(glacier_file_csv)){
    glacier_file <- read.table(glacier_file_csv[m], header=TRUE, sep=";")
    
    # transpose dataframe
    glacier_type_t <- data.frame(t(glacier_file[,2:42]))
    # add column names
    colnames(glacier_type_t) <- catch_names
    # round to digits "round_num"
    glacier_type_t<-round(glacier_type_t, round_num[m])
    glacier_type_r<-glacier_type_t
    # add to list
    end_list[list_pos[m]] <-list(glacier_type_r)
  }
  return(end_list)
}

# create list with all glacier-files
glacier_file_csv<-c("glacier_area_catchment_camels_ch.csv","glacier_volume_catchment_camels_ch.csv","glacier_mass_catchment_camels_ch.csv","glacier_area_catchment_camels_ch_neighbours.csv")

# run function "glacier_file_csv" for glacier-files
data_list_all<-add_glacier(glacier_file_csv,c(14,15,16,17),data_list_clc, c(4,5,3,4))

##################################
## extract data per catchment and save them in folder "results"

# check for folder "results" and create it if missing
mainDir <- getwd()
subDir <- "results"
if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

# prepare container for for output file
data_catchment = data.frame(matrix(nrow = 41, ncol = length(list_parameter_name)))
colnames(data_catchment) <- list_parameter_name
rownames(data_catchment) <- years_all

#extract data from data_list for each catchment k and attribute j and create output file
for (k in 1:length(catch_names)){
  catch_ID<-catch_names[k]
  for (j in 1:length(list_parameter_name) ){
    param_daten <- data.frame(data_list_all[j])
    data_catchment[,j] <- param_daten[,k]
  }
  # change order of columns and add column names (attributes)
  colnames(data_catchment) <- list_parameter_name
  data_catchment_2<-cbind(rownames(data_catchment),data_catchment[,c(1,4,10,2,8,3,12,6,5,7,9,11,13,14,15,16,17)])
  colnames(data_catchment_2)<-c("year",colnames(data_catchment_2)[2:17])
  # save as file
  file_name<-(paste0("CAMELS_CH_annual_data_",catch_ID,".csv"))
  first_row_text<-paste0("gauge_id=",catch_ID,", Land cover attributes as area percentages based on the CORINE land cover databases by the European Copernicus Land Monitoring Service: 1990, 2000, 2006, 2012 and 2018. The years inbetween were interpolated by linear regression. In 1990 only areas outside Switzerland were covered by clc_1990, therefore only catchments with more then 95% data in clc_1990 were calculated. glaciers source: Swiss Glacier Inventory 1973/2016 and Paul et al, 2020. glacier_mass = glacier_volume*850.")
  cat(first_row_text,"\n", file=file_name)
  write.table(data_catchment_2, file = file_name, append=TRUE, row.names=FALSE, quote=FALSE,sep=";",fileEncoding="UTF-8")
}

