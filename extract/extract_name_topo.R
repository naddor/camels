### SET PATHS
require(maps)
require(stringr) # to remove leading blank spaces

source(paste(dir_r_scripts,'camels/extract/extract_elev_bands.R',sep=''))

### LOAD GAUGE INFORMATION AND BASIN CHARACTERISTICS FOR ALL BASINS

# load gauge information - USGS data
file_usgs_data<-paste(dir_basin_dataset,'basin_metadata/gauge_information.txt',sep='')
gauge_table<-read.table(file_usgs_data,sep='\t',quote='',skip=1,header=FALSE,colClasses=c(rep("factor",3),rep("numeric",3)))

if(readLines(file_usgs_data,1)=='HUC_02  GAGE_ID\t\t\tGAGE_NAME\t\t\t\t\tLAT\t\tLONG\t\tDRAINAGE AREA (KM^2)'){
  colnames(gauge_table)<-c('huc_02','gauge_id','gauge_name','gauge_lat','gauge_lon','area_usgs') # reformat (add) header
}else{
  stop(paste('Unexpected header in',file_usgs_data))
}

gauge_table$gauge_name<-str_trim(gauge_table$gauge_name,'left') # remove leading white spaces

# attempt to separate catchment name from state
# catchment_name<-rapply(strsplit(as.character(gauge_table$gauge_name),','),function(x) x[1])
# catchment_state<-rapply(strsplit(as.character(gauge_table$gauge_name),','),function(x) x[2])

# load basin physical characteristics - produced by Andy
file_catchment_table<-paste(dir_basin_dataset,'/basin_metadata/basin_physical_characteristics.txt',sep='')
catchment_table<-read.table(file_catchment_table,header=TRUE,colClasses=c(rep("factor",2),rep("numeric",4)))

if(readLines(file_catchment_table,1)=='BASIN_HUC BASIN_ID Size(km2) Elevation(m) Slope(m_km-1) Frac_Forest(percent)'){
  colnames(catchment_table)<-c('huc_02','gauge_id','area_geospa_fabric','basin_mean_elev','mean_slope','frac_forest') # make names nicer and remove units
}else{
  stop(paste('Unexpected header in',file_usgs_data))
}

# merge the two tables
camels_merge<-merge(gauge_table,catchment_table,by='gauge_id')
camels_merge<-camels_merge[order(camels_merge$gauge_id),] # sort catchments by ID
rm(gauge_table,catchment_table)

### CREATE CAMELS_NAME
camels_name<-camels_merge[,c('gauge_id','huc_02.x','gauge_name')]
colnames(camels_name)[2]<-'huc_02'
#save(camels_name,file=paste(dir_camels_attr,'camels_name.Rdata',sep=''))
#write.table(camels_name,file=paste(dir_camels_attr,'camels_name.txt',sep=''),
#            row.names=FALSE,quote=FALSE,sep=';')

### CREATE CAMELS_TOPO
camels_topo<-camels_merge[,c('gauge_id','gauge_lat','gauge_lon','basin_mean_elev','area_usgs','area_geospa_fabric','mean_slope')]
camels_topo<-data.frame(camels_topo,abs_rel_error_area=abs((camels_topo$area_geospa_fabric-camels_topo$area_usgs)/camels_topo$area_usgs))
#save(camels_topo,file=paste(dir_camels_attr,'camels_topo.Rdata',sep=''))
#write.table(camels_topo,file=paste(dir_camels_attr,'camels_topo.txt',sep=''),
#            row.names=FALSE,quote=FALSE,sep=';')

### CREATE CAMELS_VEGE_USGS
#camels_vege_usgs<-camels_merge[,c('gauge_id','frac_forest')]
#save(camels_vege_usgs,file=paste(dir_camels_attr_temp,'camels_vege_usgs.Rdata',sep=''))

### CREATE CAMELS_META_DATA_ELEV_BANDS
if(any(camels_topo$gauge_id!=camels_name$gauge_id)){

  stop('gauge_id in camels_topo and camels_name do not match!')
}

rerr_area<-array() # relative error in the area
aerr_elev<-array() # absolute error in the elevation

for(e in 1:dim(camels_name)[1]){

  id<-camels_name$gauge_id[e]
  huc<-camels_name$huc_02[e]

  # retrieve catchment area by adding area elevation bands
  elev_tab_format<-extract_elev_bands(id=id,huc=huc,keep_absolute_area=TRUE)
  total_area_elev_bands<-sum(elev_tab_format$area_m2)
  mean_elev<-sum(elev_tab_format$mid_point_elevation*elev_tab_format$area_fraction)

  # compute area and elevation errors
  rerr_area[e]<-(camels_topo$area_geospa_fabric[camels_topo$gauge_id==id]-total_area_elev_bands/1E6)/camels_topo$area_geospa_fabric[camels_topo$gauge_id==id]
  aerr_elev[e]<-camels_topo$basin_mean_elev[camels_topo$gauge_id==id]-mean_elev

}

par(mfrow=c(2,2))

summary(rerr_area)
hist(rerr_area)
map('state')
points(camels_topo$gauge_lon,camels_topo$gauge_lat,cex=rerr_area,pch=16)

summary(aerr_elev)
hist(aerr_elev)
map('state')
points(camels_topo$gauge_lon,camels_topo$gauge_lat,cex=aerr_elev/75,pch=16)

#camels_metadata_elev_bands<-data.frame(gauge_id=camels_name$gauge_id,elev_bands_rerr_area=rerr_area,elev_bands_aerr_elev=aerr_elev)
#save(camels_metadata_elev_bands,file=paste(dir_camels_attr_temp,'camels_metadata_elev_bands.Rdata',sep=''))
