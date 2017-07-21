#  ClimDA
#  ICF International
# For reading in precipitation climate model data that has been regridded for
# 1 degree resolution 
#  May 21, 2015 (v1); July 15, 2015 (v2);  October (v5)
#  Rawlings Miller
#  Innovative Fund
#-----------------------------------
 
print("Starting processing - 1 degree")

#setwd('F:\\ClimDA\\Scripts\\US_only\\Precip\\Functions')
setwd(c(paste(sep="",workdirUS,"\\Precip\\Functions")))

#source("Precip_US_1degree_Main_v5.R")

#setwd('F:\\ClimDA\\US\\RCP85\\ccsm4\\pr')

#library(ncdf)  # for newer netcdf versions  <-- THESE ARE LIBRARIES
library(ncdf4)  #for older netcdf versions, seems consistent with 1 deg files
library(maptools)
library(raster)

#--------------------
leap<-c(seq(from=1940,to=2100,by=4))  #includes leap year for extra day

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
Ob_pr_val<-array(0,dim=c(ob_lon_num,ob_lat_num,bl_day_num))
Num_thres = Num_thre+1
# read obs file
print("Read data from observation precipitation file")
source(c(paste(sep="",workdirUS,"\\Precip\\ReadInput\\readprecip_obs.R")))  # read gridded obs file - 1 deg

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
# Get the x/y grids for the regional analysis & set up variables as such - portion of the obs 
dat1o=list()
dat1o$x=seq(-125,by=1,len=58)
dat1o$y=seq(25,by=1,len=28)
dat1o$z=Ob_pr_val[,,1]
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
lat2_ind<-lat1_ind+ob_lat_num ## REMOVED A MINUS ONE - ALEX LATAILLE

# create new obs precip matrix
obs_precip_crop<-array(0,dim=c(ob_lon_num,ob_lat_num,bl_day_num))
obs_precip_crop<-Ob_pr_val[lon1_ind:lon2_ind,lat1_ind:lat2_ind,]
remove(Ob_pr_val)
Ob_pr_val<-obs_precip_crop  # switch it back

##--- obs variables
Ob_avg_daily_pr <-array(0,dim=c(ob_lon_num,ob_lat_num))  # stores annual average precip for each grid
Ob_ann_pr_tot <-array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_mon_avg_pr <-array(0,dim=c(ob_lon_num,ob_lat_num,12))  # stores monthly (12) average precip
rpBase<-array(0,dim=c(ob_lon_num,ob_lat_num,Num_yrs))  # return periods
Ob_max_pr<-array(0,dim=c(ob_lon_num,ob_lat_num,bl_years_tot))  # maximum precip per year
dum <- array(0,dim=c(ob_lon_num,ob_lat_num))  # dummy array for monthly precp
Ob_thres_pr<-array(0,dim=c(ob_lon_num,ob_lat_num,Num_thres))
Ob_5day<-array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_consdd<-array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_sdii<-array(0,dim=c(ob_lon_num,ob_lat_num))
if(almonindex==1){Ob_al1<-array(0,dim=c(ob_lon_num,ob_lat_num))}
if(almonindex==1){Ob_al2<-array(0,dim=c(ob_lon_num,ob_lat_num))}
if(almonindex==1){Ob_al3<-array(0,dim=c(ob_lon_num,ob_lat_num))}
#days in each month
days_mon <-  c(31,28,31,30,31,30,31,31,30,31,30,31)
days_monlp <-c(31,29,31,30,31,30,31,31,30,31,30,31)

month_st <-c(1,32,60,91,121,152,182,213,244,274,305,335)
month_st_lp <-c(1,32,61,92,122,153,183,214,245,275,306,336)

#**** OBSERVATION SIMULATION ANALYSIS
flag_obs=1
lat_num=ob_lat_num    # number of latitude specific to input file
lon_num=ob_lon_num

print('******Begin Observation Analysis')

if (Dailyprecip ==1){
print("Calculate average daily precip")
source("avgdailyprecip.R")}  # average daily precip

if (Monprecip==1){
print("Calculate average monthly precip")
source("monthlyprecip.R")}

if (Annprecip==1){
print("Calculate average total annual precip")
source("annualprecip.R")}

if (returnper ==1){
print("Calculate return periods")
source("Returnperiods.R")}

if (thresprecip ==1){
print("Calculate thresholds")
source("PrecipThresholds.R") }

if (max5day == 1){
source("Max5precip.R")}

if (maxCDD == 1){
source("ConsDryDays.R")}

if (dailyindex ==1) {
source("Sdii.R")}

if (almonindex ==1){
almon=1
source("almond_ind.R")
Ob_al1<-dum2/tot_yrs
almon=2
source("almond_ind.R")
Ob_al2<-dum2/tot_yrs
almon=3
source("almond_ind.R")
Ob_al3<-dum2/tot_yrs
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
# intialize model storage matrix for each climate model for the future time period
years_tot <- Proj_yr_f - Proj_yr_s +1
day_num <- (Proj_yr_f - Proj_yr_s + 1) *366   # total number of days across the year range - assumes LEAP YEARS (more than needed)

pr_val_in <-array(0,dim=c(lon_num,lat_num,day_num,model_num))  # store precip data
#source("F:\\ClimDA\\Scripts\\US_only\\Precip\\ReadInput\\readprecip.R")  # read gridded obs file - 1 deg
source(c(paste(sep="",workdirUS,"\\Precip\\ReadInput\\readprecip_obs.R")))  # read gridded obs file - 1 deg
# output pr_val
# Get the x/y grids for the regional analysis & set up variables as such - portion of the obs 
dat1=list()
dat1$x=seq(-125.5,by=1,len=60)
dat1$y=seq(24.5,by=1,len=30)
dat1$z=pr_val_in[,,1,1]
projprrast<-raster(dat1)
reg_projsp<-crop(projprrast,reg_cutout)   # crop does a rectangle so includes more than just the grids in region
getlatlon<-extent(reg_projsp)
lon1<-getlatlon[1]  # farthest west
lon2<-getlatlon[2]  # farthest east
lat1<-getlatlon[3]  # most southern
lat2<-getlatlon[4]  # most northern
### TOOK OUT: COMMENTED OUT ABOVE SO FORCES IT TO USE SAME LAT/LON AS OBSERVED
lon_num=abs(lon1)-abs(lon2)+1  #1 since 1 degree,add another +1 to get it aligned with gridded obs
lat_num=lat2-lat1+1  # put back in +1
# convert to indices for pulling out, knowing grid size and 1 deg grids
lon1_ind<-abs(-125.5)-abs(lon1)+1  #so if lon1=-125, then first grid, add 2 to get it aligned with gridded obs, seems 1 grid cell out of whack
lon2_ind<-lon1_ind+lon_num-1
lat1_ind<-lat1-24.5+1  # so if lat1=25, then first grid; proj start at lat1=24, but we want 25, so add one more
lat2_ind<-lat1_ind+lat_num-1   # took out -1
print(c(paste(getlatlon[1]," ",getlatlon[2]," ",getlatlon[3]," ",getlatlon[4])))
print(c(paste("Projected lon1, number :",lon1," ",lon_num," ",lon2)))
print(c(paste("         lat1, number :",lat1," ",lat_num," ",lat2)))

#-- proj variables
Pr_pr_val <-array(0,dim=c(lon_num,lat_num,day_num,model_num))  # store precip data
Pr_avg_daily_pr <-array(0,dim=c(lon_num,lat_num,model_num))  # stores annual average precip for each grid
Pr_ann_pr_tot <-array(0,dim=c(lon_num,lat_num,model_num))
Pr_mon_avg_pr <-array(0,dim=c(lon_num,lat_num,12,model_num))
Pr_thres_pr<-array(0,dim=c(lon_num,lat_num,Num_thres,model_num))
Pr_5day<-array(0,dim=c(lon_num,lat_num,model_num))
Pr_consdd<-array(0,dim=c(lon_num,lat_num,model_num))
Pr_sdii<-array(0,dim=c(lon_num,lat_num,model_num))
if(almonindex==1){Pr_al1<-array(0,dim=c(lon_num,lat_num,model_num))}
if(almonindex==1){Pr_al2<-array(0,dim=c(lon_num,lat_num,model_num))}
if(almonindex==1){Pr_al3<-array(0,dim=c(lon_num,lat_num,model_num))}

#-- baseline variables  [should be Bl not B1 - need to check!!!!]
Bl_pr_val <-array(0,dim=c(lon_num,lat_num,bl_day_num,model_num))  # store precip data
Bl_avg_daily_pr <-array(0,dim=c(lon_num,lat_num,model_num))  # stores annual average precip for each grid
Bl_ann_pr_tot <-array(0,dim=c(lon_num,lat_num,model_num))
Bl_mon_avg_pr <-array(0,dim=c(lon_num,lat_num,12,model_num))  # stores monthly (12) average precip
Bl_rp<-array(0,dim=c(lon_num,lat_num,model_num,Num_yrs))  # return periods
Bl_thres_pr<-array(0,dim=c(lon_num,lat_num,Num_thres,model_num))
Bl_5day<-array(0,dim=c(lon_num,lat_num,model_num))
Bl_consdd<-array(0,dim=c(lon_num,lat_num,model_num))
Bl_sdii<-array(0,dim=c(lon_num,lat_num,model_num))
if(almonindex==1){Bl_al1<-array(0,dim=c(lon_num,lat_num,model_num))}
if(almonindex==1){Bl_al2<-array(0,dim=c(lon_num,lat_num,model_num))}
if(almonindex==1){Bl_al3<-array(0,dim=c(lon_num,lat_num,model_num))}
#-- Absolute change of the variables
AC_avg_daily_pr <-array(0,dim=c(lon_num,lat_num,model_num))  # stores annual average precip for each grid
AC_ann_pr_tot <-array(0,dim=c(lon_num,lat_num,model_num))
AC_mon_avg_pr <-array(0,dim=c(lon_num,lat_num,12,model_num))  # stores monthly (12) average precip
AC_thres_pr <-array(0,dim=c(lon_num,lat_num,Num_thres,model_num))
AC_5day<-array(0,dim=c(lon_num,lat_num,model_num))
AC_consdd<-array(0,dim=c(lon_num,lat_num,model_num))
AC_sdii<-array(0,dim=c(lon_num,lat_num,model_num))
if(almonindex==1){AC_al1<-array(0,dim=c(lon_num,lat_num,model_num))}
if(almonindex==1){AC_al2<-array(0,dim=c(lon_num,lat_num,model_num))}
if(almonindex==1){AC_al3<-array(0,dim=c(lon_num,lat_num,model_num))}
# composites of min/max change of the variables
CMIN_avg_daily_pr<-array(0,dim=c(lon_num,lat_num))
CMAX_avg_daily_pr<-array(0,dim=c(lon_num,lat_num))
CMIN_ann_pr_tot<-array(0,dim=c(lon_num,lat_num))
CMAX_ann_pr_tot<-array(0,dim=c(lon_num,lat_num))

#-- Percent change of the variables
PC_avg_daily_pr <-array(0,dim=c(lon_num,lat_num,model_num))  # stores annual average precip for each grid
PC_ann_pr_tot <-array(0,dim=c(lon_num,lat_num,model_num))
PC_mon_avg_pr <-array(0,dim=c(lon_num,lat_num,12,model_num))  # stores monthly (12) average precip
PC_thres_pr <-array(0,dim=c(lon_num,lat_num,Num_thres,model_num))
PC_5day<-array(0,dim=c(lon_num,lat_num,model_num))
PC_consdd<-array(0,dim=c(lon_num,lat_num,model_num))
PC_sdii<-array(0,dim=c(lon_num,lat_num,model_num))
if(almonindex==1){PC_al1<-array(0,dim=c(lon_num,lat_num,model_num))}
if(almonindex==1){PC_al2<-array(0,dim=c(lon_num,lat_num,model_num))}
if(almonindex==1){PC_al3<-array(0,dim=c(lon_num,lat_num,model_num))}
# ensemble -- Absolute change of the variables
ES_AC_avg_daily_pr <-array(0,dim=c(lon_num,lat_num))  # stores annual average precip for each grid
ES_AC_ann_pr_tot <-array(0,dim=c(lon_num,lat_num))
ES_AC_mon_avg_pr <-array(0,dim=c(lon_num,lat_num,12))  # stores monthly (12) average precip
ES_AC_retperiods <-array(0,dim=c(lon_num,lat_num,Num_yrs))
ES_AC_threvalue <-array(0,dim=c(lon_num,lat_num,Num_thres))
ES_AC_5day<-array(0,dim=c(lon_num,lat_num))
ES_AC_consdd<-array(0,dim=c(lon_num,lat_num))
ES_AC_sdii<-array(0,dim=c(lon_num,lat_num))
if(almonindex==1){ES_AC_al1<-array(0,dim=c(lon_num,lat_num))}
if(almonindex==1){ES_AC_al2<-array(0,dim=c(lon_num,lat_num))}
if(almonindex==1){ES_AC_al3<-array(0,dim=c(lon_num,lat_num))}
#ensemble -- Percent change of the variables
ES_PC_avg_daily_pr <-array(0,dim=c(lon_num,lat_num))  # stores annual average precip for each grid
ES_PC_ann_pr_tot <-array(0,dim=c(lon_num,lat_num))
ES_PC_mon_avg_pr <-array(0,dim=c(lon_num,lat_num,12))  # stores monthly (12) average precip
ES_PC_threvalue <-array(0,dim=c(lon_num,lat_num,Num_thres))
ES_PC_5day<-array(0,dim=c(lon_num,lat_num))
ES_PC_consdd<-array(0,dim=c(lon_num,lat_num))
ES_PC_sdii<-array(0,dim=c(lon_num,lat_num))
if(almonindex==1){ES_PC_al1<-array(0,dim=c(lon_num,lat_num))}
if(almonindex==1){ES_PC_al2<-array(0,dim=c(lon_num,lat_num))}
if(almonindex==1){ES_PC_al3<-array(0,dim=c(lon_num,lat_num))}

ES_OBS_mon_avg_pr <-array(0,dim=c(ob_lon_num,ob_lat_num,12))
ES_OBS_retperiod <-array(0,dim=c(ob_lon_num,ob_lat_num,Num_yrs))

#------return periods
rpFut1<-array(0,dim=c(lon_num,lat_num,model_num,Num_yrs))  # return periods
rpFut_sim<-array(0,dim=c(lon_num,lat_num,model_num,Num_yrs))
rpBase_sim<-array(0,dim=c(lon_num,lat_num,model_num,Num_yrs))

rpFut1_ch<-array(0,dim=c(lon_num,lat_num,model_num,Num_yrs))
Pr_max<-array(0,dim=c(lon_num,lat_num,years_tot,model_num))  # max precip

# working variables 
pr_val <-array(0,dim=c(lon_num,lat_num,day_num,model_num))  # store precip data
avg_daily_pr <-array(0,dim=c(lon_num,lat_num,model_num))  # stores annual average precip for each grid
ann_pr_tot <-array(0,dim=c(lon_num,lat_num,model_num))
mon_avg_pr <-array(0,dim=c(lon_num,lat_num,12,model_num))  # stores monthly (12) average precip
dum <- array(0,dim=c(lon_num,lat_num))  # dummy array for monthly precp
dum_thres<-array(0,dim=c(lon_num,lat_num,Num_thres,model_num)) # dummy for thres values
pr_5day_dum<-array(0,dim=c(lon_num,lat_num,model_num))
pr_consdd_dum<-array(0,dim=c(lon_num,lat_num,model_num))
pr_sdii_dum<-array(0,dim=c(lon_num,lat_num,model_num))
#
#****  MODEL SIMULATION ANALYSIS

flag_obs=0

print("************* Begin Analysis of Model Simulation")
for (i in 1:model_num) {
print_model<-c(paste(sep="","Analysis of model number :",i))
#print("  Analysis of model number:")
print(print_model)

#                         Read projection files
tag <- 1   # for projections

source(c(paste(sep="",workdirUS,"\\Precip\\ReadInput\\readprecip.R")))  
# create new obs precip matrix - regional cropping
precip_crop<-array(0,dim=c(lon_num,lat_num,day_num,model_num))
precip_crop<-pr_val_in[lon1_ind:lon2_ind,lat1_ind:lat2_ind,,i]
#remove(pr_val)
pr_val[,,,i]<-precip_crop  # switch it back

#                         Choose algorithms

if (Dailyprecip == 1){
source("avgdailyprecip.R")
Pr_avg_daily_pr[,,i] = avg_daily_pr[,,i]}

if (Monprecip == 1){
source("monthlyprecip.R")
Pr_mon_avg_pr[,,,i] <- mon_avg_pr[,,,i]}

if (Annprecip == 1){
source("annualprecip.R")
Pr_ann_pr_tot[,,i] = ann_pr_tot[,,i]}

if (returnper == 1) {
source("Returnperiods.R")
rpFut_sim[,,i,] = rpFut1[,,i,]}

if (thresprecip ==1){
source("PrecipThresholds.R") 
Pr_thres_pr[,,,i]=dum_thres[,,,i]}

if (max5day == 1){
source("Max5precip.R") 
Pr_5day[,,i] <-pr_5day_dum[,,i]}

if (maxCDD == 1){
source("ConsDryDays.R")
Pr_consdd[,,i]<-pr_consdd_dum[,,i]}

if (dailyindex ==1) {
source("Sdii.R")
Pr_sdii[,,i]<-pr_sdii_dum[,,i]}

if (almonindex ==1){
almon=1
source("almond_ind.R")
Pr_al1[,,i]<-dum2/tot_yrs
almon=2
source("almond_ind.R")
Pr_al2[,,i]<-dum2/tot_yrs
almon=3
source("almond_ind.R")
Pr_al3[,,i]<-dum2/tot_yrs
}

print("finish algorithms - projection")
#                         Read baseline simulations
tag <-  0 # for baseline
source(c(paste(sep="",workdirUS,"\\Precip\\ReadInput\\readprecip.R")))  
# create new obs precip matrix - regional cropping
precip_crop<-array(0,dim=c(lon_num,lat_num,day_num,model_num))
precip_crop<-pr_val_in[lon1_ind:lon2_ind,lat1_ind:lat2_ind,,i]
#remove(pr_val)
pr_val[,,,i]<-precip_crop  # switch it back

if (Dailyprecip ==1){
source("avgdailyprecip.R")
Bl_avg_daily_pr[,,i] = avg_daily_pr[,,i]}

if (Monprecip ==1){
source("monthlyprecip.R")
Bl_mon_avg_pr[,,,i] <- mon_avg_pr[,,,i]}

if (Annprecip ==1){
source("annualprecip.R")
Bl_ann_pr_tot[,,i] = ann_pr_tot[,,i]}

if (returnper ==1){
source("Returnperiods.R")
rpBase_sim[,,i,]=rpFut1[,,i,]}

if (thresprecip ==1){
source("PrecipThresholds.R") 
Bl_thres_pr[,,,i]=dum_thres[,,,i]}

if (max5day == 1){
source("Max5precip.R") 
Bl_5day[,,i] <-pr_5day_dum[,,i]}

if (maxCDD == 1){
source("ConsDryDays.R")
Bl_consdd[,,i]<-pr_consdd_dum[,,i]}

if (dailyindex ==1) {
source("Sdii.R")
Bl_sdii[,,i]<-pr_sdii_dum[,,i]}

if (almonindex ==1){
almon=1
source("almond_ind.R")
Bl_al1[,,i]<-dum2/tot_yrs
almon=2
source("almond_ind.R")
Bl_al2[,,i]<-dum2/tot_yrs
almon=3
source("almond_ind.R")
Bl_al3[,,i]<-dum2/tot_yrs
}

print("finish algorithms - baseline simulations")

#                      Absolute change (future - baseline) 
if (Dailyprecip ==1) {AC_avg_daily_pr <- Pr_avg_daily_pr - Bl_avg_daily_pr}
if (Monprecip == 1) {AC_mon_avg_pr <- Pr_mon_avg_pr - Bl_mon_avg_pr}
if (Annprecip ==1) {AC_ann_pr_tot <- Pr_ann_pr_tot - Bl_ann_pr_tot}
if (returnper==1) {rpFut1_ch <- rpFut_sim - rpBase_sim}
if (thresprecip ==1) {AC_thres_pr <- Pr_thres_pr - Bl_thres_pr}
if (max5day ==1) {AC_5day <- Pr_5day - Bl_5day}
if (maxCDD == 1) {AC_consdd <- Pr_5day - Bl_5day}
if (dailyindex==1) {AC_sdii <- Pr_sdii - Bl_sdii}
if (almonindex==1) {
AC_al1<-Pr_al1-Bl_al1
AC_al2<-Pr_al2-Bl_al2
AC_al3<-Pr_al3-Bl_al3}
#  Composite of min/max values for each model for each grid cell
# probably a faster way to optimize this!
if (Dailyprecip ==1) {
for (ii in 1:lon_num) {
for (jj in 1:lat_num) {
CMIN_avg_daily_pr[ii,jj]<-min(AC_avg_daily_pr[ii,jj,])
CMAX_avg_daily_pr[ii,jj]<-max(AC_avg_daily_pr[ii,jj,])
CMIN_ann_pr_tot[ii,jj]<-min(AC_ann_pr_tot[ii,jj,])
CMAX_ann_pr_tot[ii,jj]<-max(AC_ann_pr_tot[ii,jj,])
}
}
}

#    Percent difference  (future - baseline) / baseline
if (Dailyprecip ==1) {PC_avg_daily_pr <- AC_avg_daily_pr / Bl_avg_daily_pr}
if (max5day ==1) {PC_5day <-AC_5day / Bl_5day}
if (maxCDD==1) {PC_consdd <- AC_consdd / Bl_consdd}
if (dailyindex==1) {PC_sdii <- AC_sdii / Bl_sdii}
if (almonindex==1) {
PC_al1 <- AC_al1/Bl_al1
PC_al2 <- AC_al2/Bl_al2
PC_al3 <- AC_al3/Bl_al3}

if (Monprecip==1) {
for(imon in 1:12) {
PC_mon_avg_pr[,,imon,] <- AC_mon_avg_pr[,,imon,] / Bl_mon_avg_pr[,,imon,] }
}

if (Annprecip==1) {
PC_ann_pr_tot <- AC_ann_pr_tot / Bl_ann_pr_tot}

# remove below - may run the chance that baseline is zero for a grid cell
#for(ithres in 1:Num_thres) {
#PC_thres_pr[,,ithres,] <- AC_thres_pr[,,ithres,] / Bl_thres_pr[,,ithres,] }

 }  # i loop - model by model loop

# ensemble average - absolute  
if (Dailyprecip ==1) {ES_AC_avg_daily_pr <- apply(AC_avg_daily_pr,c(1,2),mean)}

if (Monprecip==1) {
for (imon in 1:12) {
ES_AC_mon_avg_pr[,,imon] <- apply(AC_mon_avg_pr[,,imon,],c(1,2),mean) }
}

if (Annprecip==1) {ES_AC_ann_pr_tot <- apply(AC_ann_pr_tot,c(1,2),mean)}

if (almonindex ==1){
ES_AC_al1<-apply(AC_al1,c(1,2),mean)
ES_AC_al2<-apply(AC_al2,c(1,2),mean)
ES_AC_al3<-apply(AC_al3,c(1,2),mean)}

if (thresprecip==1) {
for (ithres in 1:Num_thres){
ES_AC_threvalue[,,ithres] <- apply(AC_thres_pr[,,ithres,],c(1,2),mean) }}

if (max5day==1) {ES_AC_5day <-apply(AC_5day,c(1,2),mean)}
if (maxCDD==1) {ES_AC_consdd<-apply(AC_consdd,c(1,2),mean)}
if (dailyindex==1) {ES_AC_sdii<-apply(AC_sdii,c(1,2),mean)}

if (returnper == 1) {
dum1 <-array(0,dim=c(lon_num,lat_num,model_num))
dum1<-rpFut1_ch[,,,1]
ES_AC_retperiods[,,1] <- apply(dum1,c(1,2),mean)   # do i have to break out ret periods into each interval?
dum1 <-array(0,dim=c(lon_num,lat_num,model_num))
dum1<-rpFut1_ch[,,,2]
ES_AC_retperiods[,,2] <- apply(dum1,c(1,2),mean) 
dum1 <-array(0,dim=c(lon_num,lat_num,model_num))
dum1<-rpFut1_ch[,,,3]
ES_AC_retperiods[,,3] <- apply(dum1,c(1,2),mean) }

# ensemble average - percent diff
if (Dailyprecip==1) {ES_PC_avg_daily_pr <- apply(PC_avg_daily_pr,c(1,2),mean)}

if (Monprecip==1) {
for (imon in 1:12) {
ES_PC_mon_avg_pr[,,imon] <- apply(PC_mon_avg_pr[,,imon,],c(1,2),mean) }
}

if (Annprecip==1) {ES_PC_ann_pr_tot <- apply(PC_ann_pr_tot,c(1,2),mean)}

if (almonindex ==1){
ES_PC_al1<-apply(PC_al1,c(1,2),mean)
ES_PC_al2<-apply(PC_al2,c(1,2),mean)
ES_PC_al3<-apply(PC_al3,c(1,2),mean)}

#for (ithres in 1:Num_thres) {
#ES_PC_threvalue[,,ithres] <- apply(PC_thres_pr[,,ithres,],c(1,2),mean) }

if (max5day==1) {ES_PC_5day<-apply(PC_5day,c(1,2),mean)}
if (maxCDD==1) {ES_PC_consdd<-apply(PC_consdd,c(1,2),mean)}
if (dailyindex==1) {ES_PC_sdii<-apply(PC_sdii,c(1,2),mean)}

