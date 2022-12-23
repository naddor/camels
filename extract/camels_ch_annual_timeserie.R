# ==============================================================================
# Creating annual time series per catchment for clc and glacier data
# 
# Eawag, Switzerland, Dec. 2022
#
# usula.schoenenberger@eawag.ch
# ==============================================================================

setwd("/Users/...")
##################################
# prepare lists and variables
list_files<-c("crop", "dwood", "ewood", "grass", "ice", "inwater", "loose_rock", "mixed_wood", "rock", "scrub", "urban", "wetlands","blank")
list_files_glacier<-c("glac_area","glac_vol","glac_mass")
list_parameter_name<-c("crop_perc", "ewood_perc", "dwood_perc", "grass_perc", "ice_perc","inwater_perc", "loose_rock_perc", "mix_wood_perc", "rock_perc", "shrub_perc", "urban_perc", "wetlands_perc","blank_perc","glac_area","glac_vol","glac_mass")
list_files_all<-c(list_files,list_files_glacier)

# set up time sequence
years_clc <- seq(1990,2018,1)
years_all <- seq(1981,2021,1)

##################################
# open clc files, create attribute list for glaciers and add to data_list
# function to open clc files
open_clc_file <- function (clc_name){
  file_clc <- read.table(clc_name,skip=1, header=TRUE, sep=";")
  colnames(file_clc) <- gsub("^X", "",  colnames(file_clc))
  if(clc_name=="clc_1990_perc.txt"){
    file_clc<-rbind(file_clc[2:13,],file_clc[1,])
    # replace values in catchments which cover less than 95% area
    for(i in 1:ncol(file_clc)){
      if(file_clc[13,i]>5){
        file_clc[,i]<-NA
      }
    }
  }else{
    file_clc<-rbind(file_clc,rep(0,331))
    row.names(file_clc)<-list_files
  } 
  return(file_clc)
}

file_clc1990 <- open_clc_file("clc_1990_perc.txt")
file_clc2000 <- open_clc_file("clc_2000_perc.txt")
file_clc2006 <- open_clc_file("clc_2006_perc.txt")
file_clc2012 <- open_clc_file("clc_2012_perc.txt")
file_clc2018 <- open_clc_file("clc_2018_perc.txt")

# get catchment IDs
catch_names <- colnames(file_clc1990)
catch_num<-ncol(file_clc1990)

# create time serie and data list for non-blank clc
data_list <- NULL
# function to create time series and arrange in data_list
add_clc <- function (list_attr, list_pos, end_list){
  for(m in 1:length(list_attr)){
    ablage<-NULL
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
        fill_value_1<-rep(NA,11)
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
      # merge values to time serie
      ablage<-rbind(ablage,c(fill_value_1[1:10],fill_value_2[1:6],fill_value_3[1:6],fill_value_4[1:7]))
    }
    # add row and column names
    ablage<-data.frame(t(ablage))
    rownames(ablage) <- years_clc
    colnames(ablage) <- catch_names
    # fill data gaps 1981 till 1989 and 2019 till 2021 with data from 2018
    vorjahr <- data.frame(matrix(nrow = 9, ncol = 331),row.names=seq(1981,1989,1))
    colnames(vorjahr)<-catch_names
    nachjahr <- data.frame(matrix(nrow = 3, ncol = 331),row.names=seq(2019,2021,1))
    nachjahr[1:3,]<-ablage[29,]
    colnames(nachjahr)<-catch_names
    # bind time serie catchment for catchment and add to data_list
    ablage<-rbind(vorjahr,ablage,nachjahr)
    ablage<-round(ablage,2)
    end_list[data_list_pos] <-list(ablage)
  }
  return(end_list)
}
data_list_clc<-add_clc(list_files,c(rep(1:13)),data_list)

##################################
# open glacier files, create attribute list for glaciers and add to data_list
# function to round
round_value <- function (lookup_value){
  catch_data<-NULL
  value_round<-lookup_value
  for(i in 1:ncol(lookup_value)){
    catch_data<-lookup_value[,i]
    if (catch_data[41]>0&&!is.na(catch_data[41])){
      round_i<-as.integer(-log10(catch_data[41])+4)
      value_round[,i]<-round(catch_data,round_i)
    }else  {value_round[,i]<-catch_data}
  }
  return(value_round)
}

# function to insert glacier data into data_list
add_glacier<-function (glacier_file_txt,list_pos,end_list){
  for(m in 1:length(glacier_file_txt)){
    glacier_file <- read.table(glacier_file_txt[m], header=TRUE, sep=";")
    glacier_type_t <- data.frame(t(glacier_file[,2:42]))
    colnames(glacier_type_t) <- catch_names
    glacier_type_r<-round_value(glacier_type_t)
    end_list[list_pos[m]] <-list(glacier_type_r)
  }
  return(end_list)
}

glacier_file_txt<-c("glacier_area_catchment_camels_ch3.txt","glacier_volume_catchment_camels_ch3.txt","glacier_mass_catchment_camels_ch3.txt")
data_list_all<-add_glacier(glacier_file_txt,c(14,15,16),data_list_clc)

##################################
# extract data per catchment and save
setwd("/Users/...")

# prepare container for for output file
data_catchment = data.frame(matrix(nrow = 41, ncol = 16))
colnames(data_catchment) <- list_parameter_name
rownames(data_catchment) <- years_all

#extract data from data_list for each catchment k and attribute j
for (k in 1:length(catch_names)){
  catch_ID<-catch_names[k]

  for (j in 1:length(list_files_all) ){
    param_daten <- data.frame(data_list_all[j])
    data_catchment[,j] <- param_daten[,k]
  }
  # add column names (attributes)
  colnames(data_catchment) <- list_parameter_name
  # save as file
  file_name<-(paste0("CAMELS_CH_annual_data_",catch_ID,".txt"))
  first_row_text<-paste0("ID=",catch_ID,", landcover [%], glacier_area [km2], glacier_volume [km3], glacier_mass [To]. clc source: copernicus: 1990, 2000, 2006, 2012 and 2018. The years inbetween were interpolated by linear regression. In 1990 only areas outside Switzerland were covered by clc_1990, therefore only catchments with more then 95% data in clc_1990 were calculated. glaciers source: Swiss Glacier Inventory 1973/2016. glacier_mass = glacier_volume*1e9*850/1000.")
  cat(first_row_text,"\n", file=file_name)
  write.table(data_catchment, file = file_name, append=TRUE, quote=FALSE,sep=";",fileEncoding="UTF-8")
}

