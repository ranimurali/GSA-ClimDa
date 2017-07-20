# get the boundary lat/lon, and lat/lon number of indices
i=1
lon_num =462
lat_num=222

# get a county shapefile for 1/8 degree
if (inp==3) {
#fil_open<-c("F:\\ClimDA\\US\\RCP85\\ccsm4\\pr\\BCCAv2_0.125deg_pr_day_CCSM4_rcp85_r1i1p1_20360101-20451231.nc")
fil_open<-c(paste(sep="",ClimDAloc,"\\US\\RCP85\\ccsm4\\pr\\BCCAv2_0.125deg_pr_day_CCSM4_rcp85_r1i1p1_20260101-20351231.nc"))

# requires maptools
#fil_open_shape<-c("F:\\ClimDa\\US\\cty_shapes\\5m\\cb_2013_us_county_5m.shp")
# can switch to 500k or 20mb
fil_open_shape<-c(paste(sep="",ClimDAloc,"\\US\\cty_shapes\\5m\\cb_2013_us_county_5m.shp"))

state_bndy<-readShapePoly(fil_open_shape)
reg_cutout = state_bndy[match(toupper(statename),toupper(state_bndy$GEOID)),]
}
# get a state shapefile for 1/8 degree
if (inp==2) {
#fil_open<-c("F:\\ClimDA\\US\\RCP85\\ccsm4\\pr\\BCCAv2_0.125deg_pr_day_CCSM4_rcp85_r1i1p1_20360101-20451231.nc")
fil_open<-c(paste(sep="",ClimDAloc,"\\US\\RCP85\\ccsm4\\pr\\BCCAv2_0.125deg_pr_day_CCSM4_rcp85_r1i1p1_20260101-20351231.nc"))

# requires maptools
#fil_open_shape<-c("C:\\Users\\18959\\Desktop\\ClimDa\\data\\cb_2014_us_state_500k.shp")
fil_open_shape<-c(paste(sep="",ClimDAloc,"US\\cty_shapes\\500k\\cb_2014_us_state_500k.shp"))
state_bndy<-readShapePoly(fil_open_shape)
reg_cutout = state_bndy[match(toupper(statename),toupper(state_bndy$NAME)),]
}

pr_1 = nc_open(fil_open) 
pr_val_whole <-ncvar_get(pr_1,"pr")

dat1=list()
dat1$x=seq(-124.5625,by=0.125,len=462)   #pretty sure site has error and states -124.625
dat1$y=seq(24.0625,by=0.125,len=222)
dat1$z=pr_val_whole[,,1]
projprrast<-raster(dat1)
#
remove(dat1)
remove(pr_val_whole)
remove(pr_1)
#
#  switched state_cutout to reg_cutout & state_proj to reg_proj - for 1 climda
#reg_cutout = state_bndy[match(toupper(statename),toupper(state_bndy$GEOID)),]
reg_proj<-crop(projprrast,reg_cutout)
getlatlon<-extent(reg_proj)
# get lan/lon
lon1<-getlatlon[1]  # farthest west
lon2<-getlatlon[2]  # farthest east
lat1<-getlatlon[3]  # most southern
lat2<-getlatlon[4]  # most northern
#
#source("F:\\ClimDA\\Scripts\\US_only\\Precip\\ReadInput\\find_lat_lon_12km.R")
source(c(paste(sep="",ClimDAloc,"\\Scripts\\US_only\\Precip\\ReadInput\\find_lat_lon_12km.R")))

### TOOK OUT: COMMENTED OUT ABOVE SO FORCES IT TO USE SAME LAT/LON AS OBSERVED
lon_num=(abs(lon1)-abs(lon2))/0.125  #1 since 1 degree,add another +1 to get it aligned with gridded obs
lat_num=(lat2-lat1)/0.125  # put back in +1
# convert to indices for pulling out, knowing grid size and 1 deg grids
lon1_ind<-(abs(-124.5625)-abs(lon1))/0.125 + 1  #so if lon1=-125, then first grid, add 2 to get it aligned with gridded obs, seems 1 grid cell out of whack
lon2_ind<-lon1_ind+lon_num-1
lat1_ind<-(lat1-24.0625)/0.125 + 1  # so if lat1=25, then first grid; proj start at lat1=24, but we want 25, so add one more
lat2_ind<-lat1_ind+lat_num-1   # took out -1
print(c(paste(getlatlon[1]," ",getlatlon[2]," ",getlatlon[3]," ",getlatlon[4])))
print(c(paste("Projected lon1, number :",lon1," ",lon_num," ",lon2)))
print(c(paste("         lat1, number :",lat1," ",lat_num," ",lat2)))
