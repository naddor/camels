#######################################################
######Extract time series in ascii for catchments 
######R.Siber, eawag, updated:Feb 2022
######for daily simulation data in .ascii format- from Zappa
###1.ascii to stack
###2.Extract data by raster::extract
#######21 min for 1 catchment one month and 6 variables!!!!!
###### How could this be done faster?????
########################################################
######
"rgdal_show_exportToProj4_warnings" <- "none"
library(raster)
library(rgdal)
library(ncdf4)
library(sp)

####set working directory
setwd(Sys.getenv('CAMELS_DIR_DATA'))

###########################Input###################3
path <- "./zappa_sim"

Nfiles <- c(list.files(path, full.names = T, pattern = "asc$"))
#######create lists for variable: "EI", "EPT" etc

varlist <- c()
for (i in 1:length(Nfiles)) {
  find1 <- which(strsplit(Nfiles[i], "")[[1]] == ".")
  var <- substr(Nfiles[i], find1[2] + 1, find1[3] - 1)
  varlist <- c(varlist, var)
}
varlist2 <- unique(varlist); print(varlist2)

mylist <- list()
for (var in varlist2)
{
  print(var)

  Mfiles <- c()
  for (i in 1:length(Nfiles))
  {
    find1 <- which(strsplit(Nfiles[i], "")[[1]] == ".")
    varNeu <- substr(Nfiles[i], find1[2] + 1, find1[3] - 1)
    if (varNeu == var) {
      Mfiles <- c(Mfiles, Nfiles[i])
    }
  }
  mylist <- append(mylist, list(Mfiles))
}

####Input Catchment
shp <- shapefile("./shapefile/CAMELS_EZG_4.shp")
for (j in 1:nrow(shp))
{
  poly <- shp[j,]
  poly@data
  id <- poly@data$ID12  ######write out ID
  print(id)
  e <- extent(poly)
  my_date <- c()
  my_id <- c()
  my_vector <- c()
  col.names <- c()

  for (k in 1:length(mylist))
  {
    col.names <- c(col.names, varlist2[k]) ###Colnames
    Pfiles <- mylist[[k]]
    for (i in 1:length(Pfiles))
    {
      s <- stack(Pfiles)
      b <- brick(s)
      raster::crs(b) <- "EPSG:21781" ##Define drs
      b_pr <- projectRaster(b, crs = "EPSG:2056"); #Reproject
      Y_crop <- crop(b_pr, extent(poly), snap = "out")
      Y_extr <- raster::extract(Y_crop, poly, weights = TRUE, fun = mean,
                                na.rm = TRUE, progress = 'text')
      T_extr <- t(Y_extr)
      date1 <- rownames(T_extr)
      date2 <- substr(date1, 7, 14)
      my_date <- as.Date(date2, "%Y%m%d")
      id2 <- rep(id, (length(Y_extr))) }
    my_vector <- cbind(my_vector, T_extr)
  }
  df <- data.frame(id2, my_date, my_vector)
  colnames(df) <- c("ID12", "Date", col.names)
  my_filename <- paste("sim_", id, ".csv", sep = "")
  my_file <- paste("zappa_sim/output", my_filename, sep = "/")
  write.table(file = my_file, x = df, sep = ";", row.names = F)
}
print("done")

