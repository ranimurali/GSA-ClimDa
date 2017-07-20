# get the boundary lat/lon, and lat/lon number of indices
i=1

# if the user hasn't specified lat/lon grids, then assume it's a shapefile
if (usergrid != 1) {
lon_num =462
lat_num=222
# open up any 1/8 degree model to get the correct lat/lon rectangle that fits the user-chosen region (e.g., county, state, region)
mod_loc<-c(paste(sep="",ClimDAloc,"\\US\\RCP85\\ccsm4"))
obs_loc<-c(paste(sep="",ClimDAloc,"\\US\\obs"))
if (inp !=1) {
if (Module_ch=="Precip") {fil_open<-c(paste(sep="",mod_loc,"\\pr\\BCCAv2_0.125deg_pr_day_CCSM4_rcp85_r1i1p1_20260101-20351231.nc"))}
if (Module_ch=="Temp") {fil_open<-c(paste(sep="",mod_loc,"\\tmin\\BCCA_0.125deg_tasmin_day_CCSM4_rcp85_r1i1p1_20260101-20351231.nc"))}
}
# open up any 1 degree model to get correct lat/lon rectangle that fits the user-chosen region
if (inp == 1) {
if (Module_ch=="Precip") {fil_open<-c(paste(sep="",mod_loc,"\\pr\\regridded_1deg_pr_day_CCSM4_rcp85_r1i1p1_20260101-20351231.nc"))}
if ((Module_ch=="Temp") & (flag_obs == 1)) {fil_open<-c(paste(sep="",obs_loc,"\\1_deg\\tmin\\Extraction_tasmin.nc"))}
if ((Module_ch=="Temp") & (flag_obs !=1)) {fil_open<-c(paste(sep="",mod_loc,"\\tmin\\regridded_1deg_tasmin_day_CCSM4_rcp85_r1i1p1_20260101-20351231.nc"))}
}

# get a county shapefile for 1/8 degree
if (inp==3) {
# requires maptools
fil_open_shape<-c(paste(sep="",ClimDAloc,"\\US\\cty_shapes\\5m\\cb_2013_us_county_5m.shp"))
state_bndy<-readShapePoly(fil_open_shape)
reg_cutout = state_bndy[match(toupper(statename),toupper(state_bndy$GEOID)),]
}
# get a state shapefile for 1/8 degree
if (inp==2) {
# requires maptools
fil_open_shape<-c(paste(sep="",ClimDAloc,"\\US\\state_shapes\\cb_2014_us_state_500k.shp"))
state_bndy<-readShapePoly(fil_open_shape)
reg_cutout = state_bndy[match(toupper(statename),toupper(state_bndy$NAME)),]
}

# get regional for 1 degree
if (inp==1){
if(US_region=="Alabama") {regionPT <-c("Alabama")}
if(US_region=="Alaska") {regionPT <-c("Alaska")}
if(US_region=="Arizona") {regionPT <-c("Arizona")}
if(US_region=="Arkansas") {regionPT <-c("Arkansas")}
if(US_region=="California") {regionPT <-c("California")}
if(US_region=="Colorado") {regionPT <-c("Colorado")}
if(US_region=="Connecticut") {regionPT <-c("Connecticut")}
if(US_region=="Delaware") {regionPT <-c("Delaware")}
if(US_region=="Florida") {regionPT <-c("Florida")}
if(US_region=="Georgia") {regionPT <-c("Georgia")}
if(US_region=="Hawaii") {regionPT <-c("Hawaii")}
if(US_region=="Idaho") {regionPT <-c("Idaho")}
if(US_region=="Illinois") {regionPT <-c("Illinois")}
if(US_region=="Indiana") {regionPT <-c("Indiana")}
if(US_region=="Iowa") {regionPT <-c("Iowa")}
if(US_region=="Kansas") {regionPT <-c("Kansas")}
if(US_region=="Kentucky") {regionPT <-c("Kentucky")}
if(US_region=="Louisiana") {regionPT <-c("Louisiana")}
if(US_region=="Maine") {regionPT <-c("Maine")}
if(US_region=="Maryland") {regionPT <-c("Maryland")}
if(US_region=="Massachusetts") {regionPT <-c("Massachusetts")}
if(US_region=="Michigan") {regionPT <-c("Michigan")}
if(US_region=="Minnesota") {regionPT <-c("Minnesota")}
if(US_region=="Mississippi") {regionPT <-c("Mississippi")}
if(US_region=="Missouri") {regionPT <-c("Missouri")}
if(US_region=="Montana") {regionPT <-c("Montana")}
if(US_region=="Nebraska") {regionPT <-c("Nebraska")}
if(US_region=="Nevada") {regionPT <-c("Nevada")}
if(US_region=="New Hampshire") {regionPT <-c("New Hampshire")}
if(US_region=="New Jersey") {regionPT <-c("New Jersey")}
if(US_region=="New Mexico") {regionPT <-c("New Mexico")}
if(US_region=="New York") {regionPT <-c("New York")}
if(US_region=="North Carolina") {regionPT <-c("North Carolina")}
if(US_region=="North Dakota") {regionPT <-c("North Dakota")}
if(US_region=="Ohio") {regionPT <-c("Ohio")}
if(US_region=="Oklahoma") {regionPT <-c("Oklahoma")}
if(US_region=="Oregon") {regionPT <-c("Oregon")}
if(US_region=="Pennsylvania") {regionPT <-c("Pennsylvania")}
if(US_region=="Rhode Island") {regionPT <-c("Rhode Island")}
if(US_region=="South Carolina") {regionPT <-c("South Carolina")}
if(US_region=="South Dakota") {regionPT <-c("South Dakota")}
if(US_region=="Tennessee") {regionPT <-c("Tennessee")}
if(US_region=="Texas") {regionPT <-c("Texas")}
if(US_region=="Utah") {regionPT <-c("Utah")}
if(US_region=="Vermont") {regionPT <-c("Vermont")}
if(US_region=="Virginia") {regionPT <-c("Virginia")}
if(US_region=="Washington") {regionPT <-c("Washington")}
if(US_region=="West Virginia") {regionPT <-c("West Virginia")}
if(US_region=="Wisconsin") {regionPT <-c("Wisconsin")}
if(US_region=="Wyoming") {regionPT <-c("Wyoming")}

if(US_region=="Northeast") {regionPT <- c("Maine", "Vermont", "Massachusetts", "New Hampshire" ,"Connecticut","Rhode Island","New York","Pennsylvania", "New Jersey","Maryland", "Delaware", "Virginia", "West Virginia")}
if(US_region=="Southeast") {regionPT <-c("Alabama","Arkansas","Florida","Georgia","Kentucky","Louisiana","Mississippi","North Carolina","South Carolina","Tennessee","Virginia")}
if(US_region=="Midwest") {regionPT <-c("Illinois","Indiana","Iowa","Michigan","Minnesota","Missouri","Ohio","Wisconsin")}
if(US_region=="GreatPlns") {regionPT <-c("Kansas","Montana","Nebraska","North Dakota","Oklahoma","South Dakota","Texas","Wyoming")}
if(US_region=="Southwest") {regionPT <-c("Arizona","California","Colorado","Nevada","New Mexico","Utah")}
if(US_region=="Northwest") {regionPT <-c("Idaho","Oregon","Washington")}
## Add something here if for all of U.S.
#bring in US shapefile - may take a few minutes to download
us <- getData("GADM", country="USA", level=1)
# list states in us, do this:  > us$NAME_1
reg_cutout = us[match(toupper(regionPT),toupper(us$NAME_1)),]   # toupper - upper/lower case - don't fully understand this line of code
}

if (Module_ch=="Precip") {
pr_1 = nc_open(fil_open) 
var_temp <-ncvar_get(pr_1,"pr")}

if (Module_ch=="Temp") {
temp_1 = nc_open(fil_open) 
var_temp <-ncvar_get(temp_1,"tasmin")}

dat1=list()
if (inp != 1){
# changed below as of 12/2
#dat1$x=seq(-124.5625,by=0.125,len=462)   #pretty sure site has error and states -124.625
#dat1$y=seq(24.0625,by=0.125,len=222)
dat1$x=seq(-124.6875,by=0.125,len=462) 
dat1$y=seq(25.1875,by=0.125,len=222)
}
if ((inp ==1) & (flag_obs==1)) {
dat1$x=seq(-125,by=1,len=58)   # was dat1o$x
dat1$y=seq(25,by=1,len=28)
}
if ((inp==1) & (flag_obs !=1)) {
dat1$x=seq(-125.5,by=1,len=60)
dat1$y=seq(24.5,by=1,len=30)
}

dat1$z<-var_temp[,,1]

projprrast<-raster(dat1)
#
remove(dat1)
if (Module_ch=="Precip") {remove(pr_1)}
if (Module_ch=="Temp")  {remove(temp_1)}
remove(var_temp)
#
#  switched state_cutout to reg_cutout & state_proj to reg_proj - for 1 climda
reg_proj<-crop(projprrast,reg_cutout)
getlatlon<-extent(reg_proj)
# get lan/lon
lon1<-getlatlon[1]  # farthest west
lon2<-getlatlon[2]  # farthest east
lat1<-getlatlon[3]  # most southern
lat2<-getlatlon[4]  # most northern
#
}

source(c(paste(sep="",workdirUS,"\\CommonFunctions\\find_lat_lon_12km.R")))

if (inp != 1) {
lon_num=(abs(lon1)-abs(lon2))/0.125+1  
lat_num=(lat2-lat1)/0.125+1  
# convert to indices for pulling out, knowing grid size and 1 deg grids
#lon1_ind<-(abs(-124.5625)-abs(lon1))/0.125 + 1  #so if lon1=-125, then first grid, add 2 to get it aligned with gridded obs, seems 1 grid cell out of whack
lon1_ind<-(abs(-124.6875)-abs(lon1))/0.125 + 1
lon2_ind<-lon1_ind+lon_num-1
#lat1_ind<-(lat1-24.0625)/0.125 + 1  # so if lat1=25, then first grid; proj start at lat1=24, but we want 25, so add one more
lat1_ind<-(lat1-25.1875)/0.125 + 1
lat2_ind<-lat1_ind+lat_num-1   # took out -1
}
if ((inp == 1) & (flag_obs==1)) {
lon_num=(abs(lon1)-abs(lon2))+1  
lat_num=(lat2-lat1)+1  
# convert to indices for pulling out, knowing grid size and 1 deg grids
lon1_ind<-abs(-125)-abs(lon1) + 1
lon2_ind<-lon1_ind+lon_num-1
lat1_ind<-(lat1-25) + 1
lat2_ind<-lat1_ind+lat_num-1   # took out -1
}
if ((inp == 1) & (flag_obs!=1)) {
lon_num=(abs(lon1)-abs(lon2))+1  
lat_num=(lat2-lat1)+1  
# convert to indices for pulling out, knowing grid size and 1 deg grids
lon1_ind<-abs(-125.5)-abs(lon1) + 1
lon2_ind<-lon1_ind+lon_num-1
lat1_ind<-(lat1-24.5) + 1
lat2_ind<-lat1_ind+lat_num-1   # took out -1
}

print(c(paste(getlatlon[1]," ",getlatlon[2]," ",getlatlon[3]," ",getlatlon[4])))
print(c(paste("Projected lon1, number :",lon1," ",lon_num," ",lon2)))
print(c(paste("         lat1, number :",lat1," ",lat_num," ",lat2)))
