###########################################################################
# Snowequivalent data of subcatchment (= SUBEZG) 
# Original data from SLF: Tobias Jonas: Time series extracted for non-overlapping subcatchment
# Data prepare: Header to 1,2,3...326
# rosi.siber@eawag.ch -  November 2022       #
###########################################################################

# make a local copy of this skript in your work directory


setwd("E:/E_Data/Camels/test_R") # adjust this for your work directory
#setwd("C:/r_program/ncdf_M?rz2013")


#### Set parameter:   ######################################################

#######EZG === Catchment table with hierarchy fields###########################   
inputezg <- "EZG_Snow_korr.csv"
data.ezg1 <- read.table(inputezg, sep = ";", header = T)

###input data
inputdata <- "SnowEq_input5.csv"
##data <- read.table(inputdata ,sep=";",header=T) 
data <- read.table(inputdata, sep = ",", header = T)

###output data
outsuffix <- "_EZG.csv"  #"_EZG_P.txt"    #For total ezg
filename <- strsplit(inputdata, ".csv")
outputfile <- paste(filename, outsuffix, sep = "")
##########################################################

###Prepare EZG data
names <- names(data.ezg1); names
data.ezg2 <- data.ezg1[data.ezg1$SWE_exists == 1,]
names <- names(data.ezg1); names
data.ezg <- data.ezg2[, (names == "ID_Short" |
  names == "Area_NEW" |
  names == "H1" |
  names == "H2" |
  names == "SWE_1999")]
names <- names(data.ezg); names
EZG.all <- data.ezg[, (names == "ID_Short")]
EZGlista <- unique(EZG.all)
EZGlist <- sort(EZGlista)
length(EZGlist)

#####Prepare datafile
#copy of input fill with NA
df <- as.data.frame(data)
names2 <- names(df); names2
dim(df)
ncol(df)
nrow(df)
df <- df[1]
col.names <- colnames(df)
##for (j in 1:length(EZGlist))

for (j in 1:length(EZGlist)) {
  ##ezg<-52
  ezg <- EZGlist[j]
  print(ezg)

  data.subezg <- data.ezg[data.ezg$ID_Short == ezg,] ###Selektion f?r eine EZGNO (also alle Teile des SubEZG)
  ##nrow(data.subezg); #print(nrow(data.subezg)) ###Anzahl row

  valh1 <- data.subezg$H1; valh1
  valh2 <- data.subezg$H2; valh2

  data.h1h2 <- data.ezg[((data.ezg$H1 >= valh1) & (data.ezg$H1 < valh2)),]
  # test<-unique(data.h1h2$ID_Short)
  # data.h<-data.h1h2[(unique(data.h1h2$ID_Short))]
  listID <- data.h1h2$ID_Short
  listarea <- data.h1h2$Area_NEW
  ######select columns of subezg
  colnam <- c()
  for (i in listID) { a <- (paste0("X", i)); colnam <- c(colnam, a) }
  data.sel <- as.matrix(data[, c(colnam)]); data.sel ###sonst vector, keine

  ##data.sel[1,]
  # typeof(colnam)
  ######
  ##data.cal<-matrix(NA,nrow(df),nrow(data.subezg))
  data.cal <- matrix(NA, nrow(df), 1) ### one ID (get rid of same Id)
  for (r in 1:nrow(data.sel))
  { temp <- (listarea %*% as.integer(data.sel[r,])) / sum(listarea);
    ##print(temp)
    data.cal[r] <- temp
  }
  col.names <- c(col.names, (paste0("Z_", ezg)))
  df[j + 1] <- data.cal
  print(df[j + 1])
}
print(df)
dim(df)
colnames(df) <- col.names
write.table(df, outputfile, quote = F, sep = ";", row.names = F, col.names = T, append = F)
