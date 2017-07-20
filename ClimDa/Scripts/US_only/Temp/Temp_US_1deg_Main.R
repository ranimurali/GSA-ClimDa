# For reading in Temperature climate model data that has been regridded for
#  degree resolution  (Tmin and Tmax)
#  April 2016
#-----------------------------------

setwd(c(paste(sep="",workdirUS,"\\Temp\\Functions")))

print("Start processing for 1 degree Temp")
# to run just this script:
#source("Temp_US_1deg_Main.R")

library(ncdf4)  #for older netcdf version
library(maptools)
library(raster)

#--------------------
leap<-c(seq(from=1940,to=2100,by=4))  #includes leap year for extra day
bl_years_tot <- Base_yr_f - Base_yr_s + 1
#days in each month
days_mon <-  c(31,28,31,30,31,30,31,31,30,31,30,31)
days_monlp <-c(31,29,31,30,31,30,31,31,30,31,30,31)

month_st <-c(1,32,60,91,121,152,182,213,244,274,305,335)
month_st_lp <-c(1,32,61,92,122,153,183,214,245,275,306,336)

# figure out the number of days for the variables below within the time period
# accounting for leap year

bl_day_num = 0
for (yr_file in Base_yr_s:Base_yr_f) {
  ifelse((yr_file) %in% leap, daysinyr <- 366, daysinyr<- 365) 
  bl_day_num <-bl_day_num+daysinyr}

flag_obs=1

#-------------------------------------------------------
# OBSERVATIONS
#--------------------------------
# read in & set up precip
ob_lon_num=58
ob_lat_num=28
bl_years_tot <- Base_yr_f - Base_yr_s + 1
years_tot <-bl_years_tot
bl_day_num <- (Base_yr_f - Base_yr_s + 1) *366
day_num<-bl_day_num
Ob_tp_val<-array(0,dim=c(ob_lon_num,ob_lat_num,bl_day_num))

print("Read data from observation temperature file")
temp="Tmin"

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

us <- getData("GADM", country="USA", level=1)  # requires internet - way to put this in folder??
reg_cutout = us[match(toupper(regionPT),toupper(us$NAME_1)),]   # toupper - upper/lower case - don't fully understand this line of code
# Get the x/y grids for the regional analysis & set up variables as such - portion of the obs 
dat1o=list()
dat1o$x=seq(-125,by=1,len=58)
dat1o$y=seq(25,by=1,len=28)
dat1o$z=Ob_tp_val[,,1]
obprrast<-raster(dat1o)
reg_obsp<-crop(obprrast,reg_cutout)   # crop does a rectangle so includes more than just the grids in region
getlatlon<-extent(reg_obsp)
lon1<-getlatlon[1]  # farthest west
lon2<-getlatlon[2]  # farthest east
lat1<-getlatlon[3]  # most southern
lat2<-getlatlon[4]  # most northern
ob_lon1<-lon1
ob_lat1<-lat1
ob_lon_num=abs(lon1)-abs(lon2)+1 #1 since 1 degree
ob_lat_num=lat2-lat1+1   # put in "+1" 
# convert to indices for pulling out, knowing grid size and 1 deg grids
lon1_ind<-abs(-125)-abs(lon1)+1  #so if lon1=-125, then first grid
lon2_ind<-lon1_ind+ob_lon_num-1
lat1_ind<-lat1-25+1  # so if lat1=25, then first grid
lat2_ind<-lat1_ind+ob_lat_num-1 #HAVE REMOVED A MINUS ONE HERE -ALEX LATAILLE

##--- obs variables
Ob_ann_tmax_tot <-array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_ann_tmin_tot <-array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_ann_tavg_tot <-array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_mon_tmin <-array(0,dim=c(ob_lon_num,ob_lat_num,12))  # stores monthly (12) average temp
Ob_mon_tmax <-array(0,dim=c(ob_lon_num,ob_lat_num,12))  # stores monthly (12) average temp
Ob_mon_tavg <-array(0,dim=c(ob_lon_num,ob_lat_num,12))  # stores monthly (12) average temp
Ob_thres_tmax<-array(0,dim=c(ob_lon_num,ob_lat_num,Num_thre))
Ob_thres_ctd<-array(0,dim=c(ob_lon_num,ob_lat_num,Num_thre))
Ob_CDD <-array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_HDD <-array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_drange <-array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_frethaw <- array(0,dim=c(ob_lon_num,ob_lat_num))

# working variables
dum <- array(0,dim=c(ob_lon_num,ob_lat_num))  # dummy array for monthly temp
Ob_ann_temp<-array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_mon_temp<-array(0,dim=c(ob_lon_num,ob_lat_num,12))
Ob_tmin<- array(0,dim=c(ob_lon_num,ob_lat_num,bl_day_num))
Ob_tmax<- array(0,dim=c(ob_lon_num,ob_lat_num,bl_day_num))
Ob_tavg<- array(0,dim=c(ob_lon_num,ob_lat_num,bl_day_num))
Ob_di<-array(0,dim=c(ob_lon_num,ob_lat_num,bl_day_num))

#**** OBSERVATION SIMULATION ANALYSIS
flag_obs=1
print('******Begin Observation Analysis')

lon_num<-ob_lon_num
lat_num<-ob_lat_num
# read obs file
print("Read data from observation temperature files")
temp<-"Tmax"
source(c(paste(sep="",workdirUS,"\\Temp\\ReadInput\\readtemp_obs.R")))

if (Units == 2) {Ob_tp_val<-Ob_tp_val*9/5 + 32}
temp_crop<-array(0,dim=c(lon_num,lat_num,day_num))
temp_crop<-Ob_tp_val[lon1_ind:lon2_ind,lat1_ind:lat2_ind,]
Ob_tmax<-temp_crop  # switch it back
setwd(c(paste(sep="",workdirUS,"\\Temp\\Functions")))

if (Montemp==1){
  print("Calculate average monthly Tmax temp")
  Ob_temp<-Ob_tmax
  source("monthlytemp.R")}
Ob_mon_tmax<-Ob_mon_temp

if (Anntemp==1){
  print("Calculate average annual temperatures")
  Ob_temp<-Ob_tmax
  source("annualtemp.R")}
Ob_ann_tmax_tot<-Ob_ann_temp

if (Tmaxthres ==1){
  print("Calculate maximum temperature thresholds")
  Ob_temp<-Ob_tmax
  source("TempThresholds.R") }

temp<-"Tmin"
source(c(paste(sep="",workdirUS,"\\Temp\\ReadInput\\readtemp_obs.R")))

if (Units == 2) {Ob_tp_val<-Ob_tp_val*9/5 + 32}
temp_crop<-array(0,dim=c(lon_num,lat_num,day_num))
temp_crop<-Ob_tp_val[lon1_ind:lon2_ind,lat1_ind:lat2_ind,]
Ob_tmin<-temp_crop

if (Montemp==1){
  print("Calculate average Tmin temp")
  Ob_temp<-Ob_tmin
  source("monthlytemp.R")
  Ob_mon_tmin<-Ob_mon_temp
  Ob_mon_tavg<-(Ob_mon_tmax+Ob_mon_tmin)/2}

if (Anntemp==1){
  print("Calculate average annual temperatures")
  Ob_temp<-Ob_tmin
  source("annualtemp.R")
  Ob_ann_tmin_tot<-Ob_ann_temp
  Ob_ann_tavg_tot<-(Ob_ann_tmax_tot+Ob_ann_tmin_tot)/2}

if (CDDHDD == 1){
  print("Calculate HDDCDD")
  Ob_tavg<-(Ob_tmax+Ob_tmin)/2  # daily averages
  source("CDDHDD.R")
}

if (drange==1){
  print("Calculate diurnal range")
  Ob_di<-Ob_tmax-Ob_tmin
  source("Diurnal.R")
}

if (frethaw==1){
  print("Calculate freeze thaw")
  source("FreezeThaw.R")
  Ob_frethaw<-fredum}

if (ctd==1){
  print("Calculate consecutive threshold days")
  source("ConsThresDays.R")
}

print("End Observation Analysis")

if (J_obs ==1) {stop("only obs analysis selected")}  # stop computing if only want obs analysis

#-------------------------------------------------------
# PROJECTIONS
#--------------------------------
# set up matrix with lat/lon and indices for the region for the 1deg projections
i=1
lon_num =60
lat_num=30
tag<-1

#years_tot <- Proj_yr_f - Proj_yr_s +1
#day_num <- (Proj_yr_f - Proj_yr_s + 1) *366

day_num = 0
for (yr_file in Proj_yr_s:Proj_yr_f) {
  ifelse((yr_file) %in% leap, daysinyr <- 366, daysinyr<- 365) 
  day_num <-day_num+daysinyr}

i=1

tag<-1
temp_val_in<-array(0,dim=c(lon_num,lat_num,day_num,model_num))
testing = 1
source(c(paste(sep="",workdirUS,"\\Temp\\ReadInput\\readtemp.R")))

if(US_region=="California") {regionPT<-c("California")}
if(US_region=="Northeast") {regionPT <- c("Maine", "Vermont", "Massachusetts", "New Hampshire" ,"Connecticut","Rhode Island","New York","Pennsylvania", "New Jersey","Maryland", "Delaware", "Virginia", "West Virginia")}
if(US_region=="Southeast") {regionPT <-c("Alabama","Arkansas","Florida","Georgia","Kentucky","Louisiana","Mississippi","North Carolina","South Carolina","Tennessee","Virginia")}
if(US_region=="Midwest") {regionPT <-c("Illinois","Indiana","Iowa","Michigan","Minnesota","Missouri","Ohio","Wisconsin")}
if(US_region=="GreatPlns") {regionPT <-c("Kansas","Montana","Nebraska","North Dakota","Oklahoma","South Dakota","Texas","Wyoming")}
if(US_region=="Southwest") {regionPT <-c("Arizona","California","Colorado","Nevada","New Mexico","Utah")}
if(US_region=="Northwest") {regionPT <-c("Idaho","Oregon","Washington")}

us <- getData("GADM", country="USA", level=1)
reg_cutout = us[match(toupper(regionPT),toupper(us$NAME_1)),]
#Get the x/y grids for the regional analysis & set up variables as such - portion of the obs
dat1=list()
dat1$x=seq(-125.5,by=1,len=60)
dat1$y=seq(24.5,by=1,len=30)
dat1$z=temp_val_in[,,1,1]
projprrast<-raster(dat1)
reg_projsp<-crop(projprrast,reg_cutout)   # crop does a rectangle so includes more than just the grids in region
getlatlon<-extent(reg_projsp)
lon1<-getlatlon[1]  # farthest west
lon2<-getlatlon[2]  # farthest east
lat1<-getlatlon[3]  # most southern
lat2<-getlatlon[4]  # most northern

lon_num=abs(lon1)-abs(lon2)+1  #1 since 1 degree,add another +1 to get it aligned with gridded obs
lat_num=lat2-lat1+1  # put back in +1
# convert to indices for pulling out, knowing grid size and 1 deg grids
lon1_ind<-abs(-125.5)-abs(lon1)+1  #so if lon1=-125, then first grid, add 2 to get it aligned with gridded obs, seems 1 grid cell out of whack
lon2_ind<-lon1_ind+lon_num-1
lat1_ind<-lat1-24.5+1  # so if lat1=25, then first grid; proj start at lat1=24, but we want 25, so add one more
lat2_ind<-lat1_ind+lat_num-1   # took out -1
#print(c(paste(getlatlon[1]," ",getlatlon[2]," ",getlatlon[3]," ",getlatlon[4])))
#print(c(paste("Projected lon1, number :",lon1," ",lon_num," ",lon2)))
#print(c(paste("         lat1, number :",lat1," ",lat_num," ",lat2)))

#-- proj variables
Pr_Tmax_val <-array(0,dim=c(lon_num,lat_num,day_num,model_num))  # store Tmax data
Pr_Tmin_val <-array(0,dim=c(lon_num,lat_num,day_num,model_num))  # store Tmin data

Pr_Tmax_ann <-array(0,dim=c(lon_num,lat_num,model_num))
Pr_Tmin_ann <-array(0,dim=c(lon_num,lat_num,model_num))

Pr_Tmax_mon <-array(0,dim=c(lon_num,lat_num,12,model_num))
Pr_Tmin_mon <-array(0,dim=c(lon_num,lat_num,12,model_num))
Pr_Tavg_mon <-array(0,dim=c(lon_num,lat_num,12,model_num))

Pr_Tmax_thres<-array(0,dim=c(lon_num,lat_num,Num_thre,model_num))
Pr_thres_ctd<-array(0,dim=c(lon_num,lat_num,Num_thre,model_num))
Pr_HDD<-array(0,dim=c(lon_num,lat_num,model_num))
Pr_CDD<-array(0,dim=c(lon_num,lat_num,model_num))
Pr_drange<-array(0,dim=c(lon_num,lat_num,model_num))
Pr_frethaw<-array(0,dim=c(lon_num,lat_num,model_num))

#-- baseline variables  [should be Bl not B1 - need to check!!!!]
Bl_Tmax_val <-array(0,dim=c(lon_num,lat_num,bl_day_num,model_num))  # store Tmax data
Bl_Tmin_val <-array(0,dim=c(lon_num,lat_num,bl_day_num,model_num)) 

Bl_Tmax_ann <-array(0,dim=c(lon_num,lat_num,model_num))  # stores annual average temp for each grid
Bl_Tmin_ann <-array(0,dim=c(lon_num,lat_num,model_num))
Bl_Tavg_ann <-array(0,dim=c(lon_num,lat_num,model_num))

Bl_mon_Tmin <-array(0,dim=c(lon_num,lat_num,12,model_num))  # stores monthly (12) average temp
Bl_mon_Tmax <-array(0,dim=c(lon_num,lat_num,12,model_num))  # stores monthly (12) average temp
Bl_mon_Tavg <-array(0,dim=c(lon_num,lat_num,12,model_num))  # stores monthly (12) average temp

Bl_Tmax_thres<-array(0,dim=c(lon_num,lat_num,Num_thre,model_num))
Bl_thres_ctd<-array(0,dim=c(lon_num,lat_num,Num_thre,model_num))
Bl_HDD<-array(0,dim=c(lon_num,lat_num,model_num))
Bl_CDD<-array(0,dim=c(lon_num,lat_num,model_num))
Bl_drange<-array(0,dim=c(lon_num,lat_num,model_num))
Bl_frethaw<-array(0,dim=c(lon_num,lat_num,model_num))

#-- Absolute change of the variables
AC_Tmax_ann <-array(0,dim=c(lon_num,lat_num,model_num))
AC_Tmin_ann <-array(0,dim=c(lon_num,lat_num,model_num))
AC_Tavg_ann <-array(0,dim=c(lon_num,lat_num,model_num))
AC_Tmax_mon <-array(0,dim=c(lon_num,lat_num,12,model_num))  
AC_Tmin_mon <-array(0,dim=c(lon_num,lat_num,12,model_num)) 
AC_Tavg_mon <-array(0,dim=c(lon_num,lat_num,12,model_num)) 

AC_Tmax_thres <-array(0,dim=c(lon_num,lat_num,Num_thre,model_num))
AC_thres_ctd<-array(0,dim=c(lon_num,lat_num,Num_thre,model_num))
AC_HDD<-array(0,dim=c(lon_num,lat_num,model_num))
AC_CDD<-array(0,dim=c(lon_num,lat_num,model_num))
AC_drange<-array(0,dim=c(lon_num,lat_num,model_num))
AC_frethaw<-array(0,dim=c(lon_num,lat_num,model_num))

# composites of min/max change of the variables
CMIN_Tmax_ann<-array(0,dim=c(lon_num,lat_num))
CMAX_Tmax_ann<-array(0,dim=c(lon_num,lat_num))
CMIN_Tmin_ann<-array(0,dim=c(lon_num,lat_num))
CMAX_Tmin_ann<-array(0,dim=c(lon_num,lat_num))
CMIN_Tavg_ann<-array(0,dim=c(lon_num,lat_num))
CMAX_Tavg_ann<-array(0,dim=c(lon_num,lat_num))

#-- Percent change of the variables
PC_Tmax_ann <-array(0,dim=c(lon_num,lat_num,model_num))
PC_Tmin_ann <-array(0,dim=c(lon_num,lat_num,model_num))
PC_Tavg_ann <-array(0,dim=c(lon_num,lat_num,model_num))
PC_Tmax_mon <-array(0,dim=c(lon_num,lat_num,12,model_num))  
PC_Tmin_mon <-array(0,dim=c(lon_num,lat_num,12,model_num))  
PC_Tavg_mon <-array(0,dim=c(lon_num,lat_num,12,model_num)) 

PC_Tmax_thres <-array(0,dim=c(lon_num,lat_num,Num_thre,model_num))
PC_thres_ctd<-array(0,dim=c(lon_num,lat_num,Num_thre,model_num))
PC_HDD<-array(0,dim=c(lon_num,lat_num,model_num))
PC_CDD<-array(0,dim=c(lon_num,lat_num,model_num))
PC_drange<-array(0,dim=c(lon_num,lat_num,model_num))
PC_frethaw<-array(0,dim=c(lon_num,lat_num,model_num))

# ensemble -- Absolute change of the variables
ES_AC_Tmax_ann <-array(0,dim=c(lon_num,lat_num))
ES_AC_Tmin_ann <-array(0,dim=c(lon_num,lat_num))
ES_AC_Tavg_ann <-array(0,dim=c(lon_num,lat_num))
ES_AC_Tmax_mon <-array(0,dim=c(lon_num,lat_num,12))  
ES_AC_Tmin_mon <-array(0,dim=c(lon_num,lat_num,12))  
ES_AC_Tavg_mon <-array(0,dim=c(lon_num,lat_num,12))  
ES_AC_Tmax_thres <-array(0,dim=c(lon_num,lat_num,Num_thre))
ES_AC_thres_ctd <-array(0,dim=c(lon_num,lat_num,Num_thre))
ES_AC_HDD<-array(0,dim=c(lon_num,lat_num))
ES_AC_CDD<-array(0,dim=c(lon_num,lat_num))
ES_AC_drange<-array(0,dim=c(lon_num,lat_num))
ES_AC_frethaw<-array(0,dim=c(lon_num,lat_num))

#ensemble -- Percent change of the variables
ES_PC_Tmax_ann <-array(0,dim=c(lon_num,lat_num))
ES_PC_Tmin_ann <-array(0,dim=c(lon_num,lat_num))
ES_PC_Tavg_ann <-array(0,dim=c(lon_num,lat_num))
ES_PC_Tmax_mon <-array(0,dim=c(lon_num,lat_num,12))  
ES_PC_Tmin_mon <-array(0,dim=c(lon_num,lat_num,12)) 
ES_PC_Tavg_mon <-array(0,dim=c(lon_num,lat_num,12)) 
ES_PC_Tmax_thres <-array(0,dim=c(lon_num,lat_num,Num_thre))
ES_PC_thres_ctd<-array(0,dim=c(lon_num,lat_num,Num_thre))
ES_PC_HDD<-array(0,dim=c(lon_num,lat_num))
ES_PC_CDD<-array(0,dim=c(lon_num,lat_num))
ES_PC_drange<-array(0,dim=c(lon_num,lat_num))
ES_PC_frethaw<-array(0,dim=c(lon_num,lat_num))

# working variables 
Temp_val <-array(0,dim=c(lon_num,lat_num,day_num,model_num))  # store data
Tmax_val <-array(0,dim=c(lon_num,lat_num,day_num,model_num))  # store data
Tmin_val <-array(0,dim=c(lon_num,lat_num,day_num,model_num))  # store data
ann_temp <-array(0,dim=c(lon_num,lat_num,model_num))
temp_mon <-array(0,dim=c(lon_num,lat_num,12,model_num))  # stores monthly (12) average precip
dum <- array(0,dim=c(lon_num,lat_num))  # dummy array for monthly tmax
dum2 <-array(0,dim=c(lon_num,lat_num))
dumPr<-array(0,dim=c(lon_num,lat_num,model_num))
dumPr2<-array(0,dim=c(lon_num,lat_num,model_num))
dum_thres<-array(0,dim=c(lon_num,lat_num,Num_thre,model_num)) # dummy for thres values

#
#****  MODEL SIMULATION ANALYSIS

flag_obs=0
testing = 0

print("************* Begin Analysis of Model Simulation")
for (i in 1:model_num) {
  print_model<-c(paste(sep="","Analysis of model number :",i))
  print(print_model)
  
  #                         Read projection files
  tag <- 1   # for projections
  years_tot <- Proj_yr_f - Proj_yr_s +1
  temp_val_in <-array(0,dim=c(lon_num,lat_num,day_num,model_num))
  temp<-"Tmax"
  source(c(paste(sep="",workdirUS,"\\Temp\\ReadInput\\readtemp.R")))
  
  if (Units == 2) {temp_val_in[,,,i]<-temp_val_in[,,,i]*9/5 + 32}
  temp_crop<-array(0,dim=c(lon_num,lat_num,day_num))
  temp_crop<-temp_val_in[,,,i]  
  Pr_Tmax_val[,,,i]<-temp_crop
  #
  temp<-"Tmin"
  temp_val_in <-array(0,dim=c(lon_num,lat_num,day_num,model_num))
  source(c(paste(sep="",workdirUS,"\\Temp\\ReadInput\\readtemp.R")))
  
  if (Units == 2) {temp_val_in[,,,i]<-temp_val_in[,,,i]*9/5 + 32}
  temp_crop<-array(0,dim=c(lon_num,lat_num,day_num))
  temp_crop<-temp_val_in[,,,i] 
  Pr_Tmin_val[,,,i]<-temp_crop
  #
  #                         Choose algorithms
  
  fundir<-c(paste(sep="",workdirUS,"\\Temp\\Functions"))
  setwd(fundir)
  
  if (Montemp == 1){
    temp<-"Tmax"
    Temp_val<-Pr_Tmax_val
    source("monthlytemp.R")
    Pr_Tmax_mon[,,,i] <- temp_mon[,,,i]
    temp<-"Tmin"
    Temp_val<-Pr_Tmin_val
    source("monthlytemp.R")
    Pr_Tmin_mon[,,,i] <- temp_mon[,,,i]
    Pr_Tavg_mon<-(Pr_Tmax_mon+Pr_Tmin_mon)/2 }
  
  
  if (Anntemp == 1){
    temp<-"Tmax"
    Temp_val<-Pr_Tmax_val
    source("annualtemp.R")
    Pr_Tmax_ann[,,i] = ann_temp[,,i]
    temp<-"Tmin"
    Temp_val<-Pr_Tmin_val
    source("annualtemp.R")
    Pr_Tmin_ann[,,i] = ann_temp[,,i]
    Pr_Tavg_ann<-(Pr_Tmax_ann+Pr_Tmin_ann)/2 }
  
  if (Tmaxthres ==1){
    Temp_val<-Pr_Tmax_val
    source("TempThresholds.R") 
    Pr_Tmax_thres[,,,i]=dum_thres[,,,i]}
  
  if (CDDHDD == 1){
    print("Calculate HDDCDD")
    Temp_val<-(Pr_Tmin_val+Pr_Tmax_val)/2  # daily averages
    source("CDDHDD.R")
    Pr_CDD[,,i]<-dumPr2[,,i]
    Pr_HDD[,,i]<-dumPr[,,i]
  }
  
  if (drange==1){
    print("Calculate diurnal range")
    Temp_val<-Pr_Tmax_val-Pr_Tmin_val
    source("Diurnal.R")
  }
  
  if (frethaw==1){
    print("Calculate freeze thaw")
    source("FreezeThaw.R")
    Pr_frethaw[,,i]<-fredum}
  
  if (ctd==1){
    print("Calculate consecutive threshold days")
    Tmax_val<-Pr_Tmax_val
    source("ConsThresDays.R")
    Pr_thres_ctd<-dum_thres
  }
  
  print("finish algorithms - projection")
  #                         Read baseline simulations
  tag <-  0 # for baseline
  temp<-"Tmax"
  years_tot <- Base_yr_f - Base_yr_s +1
  temp_val_in <-array(0,dim=c(lon_num,lat_num,day_num,model_num))
  source(c(paste(sep="",workdirUS,"\\Temp\\ReadInput\\readtemp.R")))
  
  if (Units == 2) {temp_val_in[,,,i]<-temp_val_in[,,,i]*9/5 + 32}
  temp_crop<-array(0,dim=c(lon_num,lat_num,day_num))
  temp_crop<-temp_val_in[,,,i]  # why the i?
  Tmax_val[,,,i]<-temp_crop
  
  temp<-"Tmin"
  temp_val_in <-array(0,dim=c(lon_num,lat_num,day_num,model_num))
  source(c(paste(sep="",workdirUS,"\\Temp\\ReadInput\\readtemp.R")))
  
  if (Units == 2) {temp_val_in[,,,i]<-temp_val_in[,,,i]*9/5 + 32}
  temp_crop<-array(0,dim=c(lon_num,lat_num,day_num))
  temp_crop<-temp_val_in[,,,i]  # why the i?
  Tmin_val[,,,i]<-temp_crop
  
  setwd(c(paste(sep="",workdirUS,"\\Temp\\Functions")))
  
  if (Montemp ==1){
    temp<-"Tmax"
    Temp_val<-Tmax_val
    source("monthlytemp.R")
    Bl_mon_Tmax[,,,i] <- temp_mon[,,,i]
    temp<-"Tmin"
    Temp_val<-Tmin_val
    source("monthlytemp.R")
    Bl_mon_Tmin[,,,i] <- temp_mon[,,,i]
    Bl_mon_Tavg<-(Bl_mon_Tmax+Bl_mon_Tmin)/2 }
  
  if (Anntemp == 1){
    temp<-"Tmax"
    Temp_val<-Tmax_val
    source("annualtemp.R")
    Bl_Tmax_ann[,,i] = ann_temp[,,i]
    temp<-"Tmin"
    Temp_val<-Tmin_val
    source("annualtemp.R")
    Bl_Tmin_ann[,,i] = ann_temp[,,i]
    Bl_Tavg_ann<-(Bl_Tmax_ann+Bl_Tmin_ann)/2 }
  
  if (Tmaxthres ==1){
    Temp_val<-Tmax_val
    source("TempThresholds.R") 
    Bl_Tmax_thres[,,,i]=dum_thres[,,,i]}
  
  if (CDDHDD == 1){
    print("Calculate HDDCDD")
    Temp_val<-(Tmin_val+Tmax_val)/2  # daily averages
    source("CDDHDD.R")
    Bl_CDD<-dumPr2
    Bl_HDD<-dumPr
  }
  
  if (drange==1){
    print("Calculate diurnal range")
    Temp_val<-Tmax_val-Tmin_val
    source("Diurnal.R")
  }
  
  if (frethaw==1){
    print("Calculate freeze thaw")
    source("FreezeThaw.R")
    Bl_frethaw[,,i]<-fredum}
  
  if (ctd==1){
    print("Calculate consecutive threshold days")
    source("ConsThresDays.R")
    #Tmax_val_in<-Tmax_val
    Bl_thres_ctd<-dum_thres
  }
  
  print("finish algorithms - baseline simulations")
  #                      Absolute change (future - baseline) 
  
  if(Montemp==1){ AC_Tmax_mon <- Pr_Tmax_mon - Bl_mon_Tmax
  AC_Tmin_mon <- Pr_Tmin_mon - Bl_mon_Tmin
  AC_Tavg_mon <- Pr_Tavg_mon - Bl_mon_Tavg}
  
  if(Anntemp==1){ AC_Tmax_ann <- Pr_Tmax_ann - Bl_Tmax_ann
  AC_Tmin_ann <- Pr_Tmin_ann - Bl_Tmin_ann
  AC_Tavg_ann <- Pr_Tavg_ann - Bl_Tavg_ann}
  
  if(Tmaxthres==1) {AC_Tmax_thres<- Pr_Tmax_thres - Bl_Tmax_thres}
  if(ctd==1) {AC_thres_ctd<-Pr_thres_ctd - Bl_thres_ctd}
  if(CDDHDD==1) {AC_HDD<- Pr_HDD-Bl_HDD
  AC_CDD<- Pr_CDD-Bl_CDD}
  if(drange==1){AC_drange<- Pr_drange-Bl_drange}
  if(frethaw==1){AC_frethaw<- Pr_frethaw-Bl_frethaw}
  
  #  Composite of min/max values for each model for each grid cell
  # probably a faster way to optimize this!
  if (Anntemp==1){
    for (ii in 1:lon_num) {
      for (jj in 1:lat_num) {
        CMIN_Tmax_ann[ii,jj]<-min(AC_Tmax_ann[ii,jj,])
        CMAX_Tmax_ann[ii,jj]<-max(AC_Tmax_ann[ii,jj,])
        CMIN_Tmin_ann[ii,jj]<-min(AC_Tmin_ann[ii,jj,])
        CMAX_Tmin_ann[ii,jj]<-max(AC_Tmin_ann[ii,jj,])
        CMIN_Tavg_ann[ii,jj]<-min(AC_Tavg_ann[ii,jj,])
        CMAX_Tavg_ann[ii,jj]<-max(AC_Tavg_ann[ii,jj,])
      }
    }
  }
  #    Percent difference  (future - baseline) / baseline
  if(Montemp==1){
    for(imon in 1:12) {
      PC_Tmax_mon[,,imon,] <- AC_Tmax_mon[,,imon,] / Bl_mon_Tmax[,,imon,] 
      PC_Tmin_mon[,,imon,] <- AC_Tmin_mon[,,imon,] / Bl_mon_Tmin[,,imon,] 
      PC_Tavg_mon[,,imon,] <- AC_Tavg_mon[,,imon,] / Bl_mon_Tavg[,,imon,] 
    }
  }
  if(Anntemp==1){
    PC_Tmax_ann <- AC_Tmax_ann / Bl_Tmax_ann
    PC_Tmin_ann <- AC_Tmin_ann / Bl_Tmin_ann
    PC_Tavg_ann <- AC_Tavg_ann / Bl_Tavg_ann}
  
  if(CDDHDD==1){PC_HDD <- AC_HDD/Bl_HDD
  PC_CDD <- AC_CDD/Bl_CDD}
  if(drange==1){PC_drange<-AC_drange/Bl_drange}
  
}  # i loop - model by model loop

# ensemble average - absolute  

if(Montemp==1){
  for (imon in 1:12) {
    ES_AC_Tmax_mon[,,imon] <- apply(AC_Tmax_mon[,,imon,],c(1,2),mean) 
    ES_AC_Tmin_mon[,,imon] <- apply(AC_Tmin_mon[,,imon,],c(1,2),mean) 
    ES_AC_Tavg_mon[,,imon] <- apply(AC_Tavg_mon[,,imon,],c(1,2),mean) 
  }}
if(Anntemp==1){
  ES_AC_Tmax_ann <- apply(AC_Tmax_ann,c(1,2),mean)
  ES_AC_Tmin_ann <- apply(AC_Tmin_ann,c(1,2),mean)
  ES_AC_Tavg_ann <- apply(AC_Tavg_ann,c(1,2),mean)}

if (Tmaxthres==1){
  for (ithres in 1:Num_thre){
    ES_AC_Tmax_thres[,,ithres] <- apply(AC_Tmax_thres[,,ithres,],c(1,2),mean) 
    if (ctd==1){
      ES_AC_thres_ctd[,,ithres]<-apply(AC_thres_ctd[,,ithres,],c(1,2),mean)}}
}

if(CDDHDD==1){ES_AC_HDD <- apply(AC_HDD,c(1,2),mean)
ES_AC_CDD <- apply(AC_CDD,c(1,2),mean)}
if(drange==1){ES_AC_drange<-apply(AC_drange,c(1,2),mean)}
if(frethaw==1){ES_AC_frethaw<-apply(AC_frethaw,c(1,2),mean)}

# ensemble average - percent diff
if (Montemp==1){
  for (imon in 1:12) {
    ES_PC_Tmax_mon[,,imon] <- apply(PC_Tmax_mon[,,imon,],c(1,2),mean) 
    ES_PC_Tmin_mon[,,imon] <- apply(PC_Tmin_mon[,,imon,],c(1,2),mean) 
    ES_PC_Tavg_mon[,,imon] <- apply(PC_Tavg_mon[,,imon,],c(1,2),mean) 
  }}
if (Anntemp==1){
  ES_PC_Tmax_ann <- apply(PC_Tmax_ann,c(1,2),mean)
  ES_PC_Tmin_ann <- apply(PC_Tmin_ann,c(1,2),mean)
  ES_PC_Tavg_ann <- apply(PC_Tavg_ann,c(1,2),mean)}

if (CDDHDD==1){ES_PC_HDD <- apply(PC_HDD,c(1,2),mean)
ES_PC_CDD <- apply(PC_CDD,c(1,2),mean)}
if(drange==1){ES_PC_drange<-apply(PC_drange,c(1,2),mean)}
if(frethaw==1){ES_PC_frethaw<-apply(PC_frethaw,c(1,2),mean)}

print("Finished ensemble calculations")