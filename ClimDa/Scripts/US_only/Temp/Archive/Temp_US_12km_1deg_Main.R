#  ClimDA
#  ICF International
# For reading in precipitation climate model data that has been regridded for
# 1 degree resolution 
#  May 21, 2015 (v1); July 15, 2015 (v2);  October (v5)
#  Rawlings Miller
#  Innovative Fund
#-----------------------------------

setwd('F:\\ClimDA\\Scripts\\US_only\\Temp\\Functions')

#source("Temp_US_12km_Main.R")

#library(ncdf)  # for newer netcdf versions  <-- THESE ARE LIBRARIES
library(ncdf4)  #for older netcdf versions, seems consistent with 1 deg files
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
source("F:\\ClimDA\\Scripts\\US_only\\CommonFunctions\\get_12km_grid_boundary.R")

#-------------------------------------------------------
# OBSERVATIONS
#--------------------------------
# read in & set up precip
ob_lon_num=lon_num
ob_lat_num=lat_num
ob_lon1 = lon1
ob_lat1 = lat1

Ob_tp_val<-array(0,dim=c(ob_lon_num,ob_lat_num,bl_day_num))

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

# read obs file
print("Read data from observation temperature files")
temp<-"Tmax"
source("F:\\ClimDA\\Scripts\\US_only\\CommonFunctions\\readprecip_obs_12km.R")  # read gridded obs file - 1 deg
if (Units == 2) {Ob_tp_val<-Ob_tp_val*9/5 + 32}
Ob_tmax<-Ob_tp_val

setwd('F:\\ClimDA\\Scripts\\US_only\\Temp\\Functions')

if (Montemp==1){
print("Calculate average Tmax temp")
source("monthlytemp.R")}
Ob_mon_tmax<-Ob_mon_temp

if (Anntemp==1){
print("Calculate average annual temperatures")
source("annualtemp.R")}
Ob_ann_tmax_tot<-Ob_ann_temp

if (Tmaxthres ==1){
print("Calculate maximum temperature thresholds")
source("TempThresholds.R") }

temp<-"Tmin"
source("F:\\ClimDA\\Scripts\\US_only\\CommonFunctions\\readprecip_obs_12km.R")  # read gridded obs file - 1 deg
if (Units == 2) {Ob_tp_val<-Ob_tp_val*9/5 + 32}
Ob_tmin<-Ob_tp_val

if (Montemp==1){
print("Calculate average Tmin temp")
source("monthlytemp.R")
Ob_mon_tmin<-Ob_mon_temp
Ob_mon_tavg<-(Ob_mon_tmax+Ob_mon_tmin)/2}

if (Anntemp==1){
print("Calculate average annual temperatures")
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
years_tot <- Proj_yr_f - Proj_yr_s +1
#day_num <- (Proj_yr_f - Proj_yr_s + 1) *366

day_num = 0
for (yr_file in Proj_yr_s:Proj_yr_f) {
 ifelse((yr_file) %in% leap, daysinyr <- 366, daysinyr<- 365) 
 day_num <-day_num+daysinyr}

i=1

tag<-1
flag_obs<-0
if (inp==1) {
source("F:\\ClimDA\\Scripts\\US_only\\CommonFunctions\\get_12km_grid_boundary.R")}

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
 
#PC_Tmax_thres <-array(0,dim=c(lon_num,lat_num,Num_thre,model_num))
#PC_thres_ctd<-array(0,dim=c(lon_num,lat_num,Num_thre,model_num))
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
#ES_PC_Tmax_thres <-array(0,dim=c(lon_num,lat_num,Num_thre))
#ES_PC_thres_ctd<-array(0,dim=c(lon_num,lat_num,Num_thre))
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

print("************* Begin Analysis of Model Simulation")
for (i in 1:model_num) {
print_model<-c(paste(sep="","Analysis of model number :",i))
#print("  Analysis of model number:")
print(print_model)

#                         Read projection files
tag <- 1   # for projections

temp_val_in <-array(0,dim=c(lon_num,lat_num,day_num,model_num))
temp<-"Tmax"
source("F:\\ClimDA\\Scripts\\US_only\\CommonFunctions\\readprecip_12km.R")
if (Units == 2) {temp_val_in[,,,i]<-temp_val_in[,,,i]*9/5 + 32}
Pr_Tmax_val[,,,i]<-temp_val_in[,,,i]
#
temp<-"Tmin"
source("F:\\ClimDA\\Scripts\\US_only\\CommonFunctions\\readprecip_12km.R")
if (Units == 2) {temp_val_in[,,,i]<-temp_val_in[,,,i]*9/5 + 32}
Pr_Tmin_val[,,,i]<-temp_val_in[,,,i]
#
#                         Choose algorithms

setwd('F:\\ClimDA\\Scripts\\US_only\\Temp\\Functions')

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
temp_val_in <-array(0,dim=c(lon_num,lat_num,day_num,model_num))
source("F:\\ClimDA\\Scripts\\US_only\\CommonFunctions\\readprecip_12km.R")
if (Units == 2) {temp_val_in[,,,i]<-temp_val_in[,,,i]*9/5 + 32}
Tmax_val[,,,i]<-temp_val_in[,,,i]
temp<-"Tmin"
temp_val_in <-array(0,dim=c(lon_num,lat_num,day_num,model_num))
source("F:\\ClimDA\\Scripts\\US_only\\CommonFunctions\\readprecip_12km.R")
if (Units == 2) {temp_val_in[,,,i]<-temp_val_in[,,,i]*9/5 + 32}
Tmin_val[,,,i]<-temp_val_in[,,,i]

setwd('F:\\ClimDA\\Scripts\\US_only\\Temp\\Functions')

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
Bl_thres_ctd<-dum_thres
}

print("finish algorithms - baseline simulations")
#                      Absolute change (future - baseline) 

AC_Tmax_mon <- Pr_Tmax_mon - Bl_mon_Tmax
AC_Tmin_mon <- Pr_Tmin_mon - Bl_mon_Tmin
AC_Tavg_mon <- Pr_Tavg_mon - Bl_mon_Tavg

AC_Tmax_ann <- Pr_Tmax_ann - Bl_Tmax_ann
AC_Tmin_ann <- Pr_Tmin_ann - Bl_Tmin_ann
AC_Tavg_ann <- Pr_Tavg_ann - Bl_Tavg_ann

AC_Tmax_thres<- Pr_Tmax_thres - Bl_Tmax_thres
AC_thres_ctd<-Pr_thres_ctd - Bl_thres_ctd
AC_HDD<- Pr_HDD-Bl_HDD
AC_CDD<- Pr_CDD-Bl_CDD
AC_drange<- Pr_drange-Bl_drange
AC_frethaw<- Pr_frethaw-Bl_frethaw

#  Composite of min/max values for each model for each grid cell
# probably a faster way to optimize this!
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

#    Percent difference  (future - baseline) / baseline

for(imon in 1:12) {
PC_Tmax_mon[,,imon,] <- AC_Tmax_mon[,,imon,] / Bl_mon_Tmax[,,imon,] 
PC_Tmin_mon[,,imon,] <- AC_Tmin_mon[,,imon,] / Bl_mon_Tmin[,,imon,] 
PC_Tavg_mon[,,imon,] <- AC_Tavg_mon[,,imon,] / Bl_mon_Tavg[,,imon,] 
}

PC_Tmax_ann <- AC_Tmax_ann / Bl_Tmax_ann
PC_Tmin_ann <- AC_Tmin_ann / Bl_Tmin_ann
PC_Tavg_ann <- AC_Tavg_ann / Bl_Tavg_ann

#for(ithres in 1:Num_thre) {
#ifelse (Bl_Tmax_thres[,,ithres,]==0,PC_Tmax_thres[,,ithres,] <- AC_Tmax_thres[,,ithres,],
#PC_Tmax_thres[,,ithres,] <- AC_Tmax_thres[,,ithres,] / Bl_Tmax_thres[,,ithres,] )
#ifelse (Bl_thres_ctd[,,ithres,]==0,PC_thres_ctd[,,ithres,]<-AC_thres_ctd[,,ithres,],
#PC_thres_ctd[,,ithres,]<-AC_thres_ctd[,,ithres,] / Bl_thres_ctd[,,ithres,] )  }

PC_HDD  <- AC_HDD/Bl_HDD
PC_CDD <- AC_CDD/Bl_CDD
PC_drange<-AC_drange/Bl_drange
ifelse (Bl_frethaw==0,PC_frethaw<-AC_frethaw*100,PC_frethaw<-AC_frethaw/Bl_frethaw)

 }  # i loop - model by model loop

# ensemble average - absolute  

for (imon in 1:12) {
ES_AC_Tmax_mon[,,imon] <- apply(AC_Tmax_mon[,,imon,],c(1,2),mean) 
ES_AC_Tmin_mon[,,imon] <- apply(AC_Tmin_mon[,,imon,],c(1,2),mean) 
ES_AC_Tavg_mon[,,imon] <- apply(AC_Tavg_mon[,,imon,],c(1,2),mean) 
}

ES_AC_Tmax_ann <- apply(AC_Tmax_ann,c(1,2),mean)
ES_AC_Tmin_ann <- apply(AC_Tmin_ann,c(1,2),mean)
ES_AC_Tavg_ann <- apply(AC_Tavg_ann,c(1,2),mean)

for (ithres in 1:Num_thre){
ES_AC_Tmax_thres[,,ithres] <- apply(AC_Tmax_thres[,,ithres,],c(1,2),mean) 
ES_AC_thres_ctd[,,ithres]<-apply(AC_thres_ctd[,,ithres,],c(1,2),mean)}

ES_AC_HDD <- apply(AC_HDD,c(1,2),mean)
ES_AC_CDD <- apply(AC_CDD,c(1,2),mean)
ES_AC_drange<-apply(AC_drange,c(1,2),mean)
ES_AC_frethaw<-apply(AC_frethaw,c(1,2),mean)

# ensemble average - percent diff

for (imon in 1:12) {
ES_PC_Tmax_mon[,,imon] <- apply(PC_Tmax_mon[,,imon,],c(1,2),mean) 
ES_PC_Tmin_mon[,,imon] <- apply(PC_Tmin_mon[,,imon,],c(1,2),mean) 
ES_PC_Tavg_mon[,,imon] <- apply(PC_Tavg_mon[,,imon,],c(1,2),mean) 
}

ES_PC_Tmax_ann <- apply(PC_Tmax_ann,c(1,2),mean)
ES_PC_Tmin_ann <- apply(PC_Tmin_ann,c(1,2),mean)
ES_PC_Tavg_ann <- apply(PC_Tavg_ann,c(1,2),mean)

#for (ithres in 1:Num_thre) {
#ES_PC_Tmax_thres[,,ithres] <- apply(PC_Tmax_thres[,,ithres,],c(1,2),mean) 
#ES_PC_thres_ctd[,,ithres]<-apply(PC_thres_ctd[,,ithres,],c(1,2),mean)}
ES_PC_HDD <- apply(PC_HDD,c(1,2),mean)
ES_PC_CDD <- apply(PC_CDD,c(1,2),mean)
ES_PC_drange<-apply(PC_drange,c(1,2),mean)
ES_PC_frethaw<-apply(PC_frethaw,c(1,2),mean)

print("Finished ensemble calculations")