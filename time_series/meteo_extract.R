#######################################################
######Extract Netcdf data for catchment with shapefile(mask)
######R.Siber, eawag, updated:Feb 2022
######for daily data from Meteoschweiz: RhiresD, TabsD, SrelD 
###1.Use netcdf file from folder
###2.Extract data by raster::extract
####### Time: one catchment: for 3 para with 41 years:  less than minute
####### Time: 332 catchments: for 1 para with 2 years:  4 minutes
########################################################
######
options("rgdal_show_exportToProj4_warnings" = "none")
library(raster)
library(rgdal)
library(ncdf4)
library(sp)

####set working directory 
setwd(Sys.getenv('CAMELS_DIR_DATA'))

###########################Input###################3
#########Input: create list in list for netcdf files (in subfolders)
Varlist <- c("RhiresD", "TabsD", "SrelD") ##subfolders
mylist <- list()
for (var in Varlist)
{
  path <- paste("./Meteo_2022", var, sep = "/")
  ##list of netcdf files for each parameter
  Pfiles3 <- c(list.files(path, full.names = T, pattern = "nc$"))
  mylist <- append(mylist, list(Pfiles3)) #list in list 
}


####Input Catchment
shp <- shapefile("./shapefile/CAMELS_EZG_V5.shp")

###########Loop through all subcatchment in the shapefile
for (j in 1:nrow(shp))
{
  poly <- shp[j,]
  poly@data
  id <- poly@data$ID12  ######write out ID
  print(id)
  e <- extent(poly)
  my_date <- c()
  my_id <- c()
  col.names <- c()

  for (k in 1:length(mylist))
  {
    paraname <- Varlist[k]
    Pfiles <- mylist[[k]]
    my_vector <- c()

    for (i in 1:length(Pfiles))
    {
      my_brick <- brick((Pfiles[i]), varname = paraname)
      ###### Crop the raster
      Y_crop <- crop(my_brick, extent(poly), snap = "out")

      Y_extr <- raster::extract(Y_crop, poly, weights = TRUE, fun = mean,
                                na.rm = TRUE, progress = 'text')
      id2 <- rep(id, (length(Y_extr))) ####Repeat id
      T_extr <- t(Y_extr)
      date1 <- rownames(T_extr)
      date2 <- substr(date1, 2, 30)
      date3 <- as.character(as.Date(date2, "%Y.%m.%d"))  ####Date Format
      my_id <- c(my_id, id2)
      my_vector <- c(my_vector, T_extr)
      my_date <- c(my_date, date3) }

    ##write in data.frame
    ###for first run (list) write id and date
    if (k == 1) {
      df <- data.frame(my_id, my_date, my_vector)
      col.names <- c(col.names, paraname)
    }else {
      col.names <- c(col.names, paraname)
      df <- cbind(df, my_vector)
    }
  }

  colnames(df) <- c("ID12", "Date", col.names)
  my_filename <- paste("new_", id, ".csv", sep = "")
  my_file <- paste("Meteo_2022/output", my_filename, sep = "/")
  write.table(file = my_file, x = df, sep = ";", row.names = F)
}
print("done")

