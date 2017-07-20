#  ClimDA
#  ICF International
# For reading in precipitation climate model data that has been regridded for
# 1 degree resolution 
#  May 21, 2015 (v1); July 15, 2015 (v2);  October (v5)
#  Rawlings Miller
#  Innovative Fund
#-----------------------------------

#setwd('F:\\ClimDA\\Scripts\\US_only\\Precip\\Functions')
setwd(c(paste(sep="",workdirUS,"\\Precip\\Functions")))
#source("Precip_US_1degree_Main_v5.R")

#setwd('F:\\US\\RCP85\\ccsm4\\pr')

#library(ncdf)  # for newer netcdf versions  <-- THESE ARE LIBRARIES
library(ncdf4)  #for older netcdf versions, seems consistent with 1 deg files
library(maptools)
library(raster)

skippingahead=1
if (skippingahead==1){

#--------------------
leap<-c(seq(from=1940,to=2100,by=4))  #includes leap year for extra day
bl_years_tot <- Base_yr_f - Base_yr_s + 1
bl_day_num <- (Base_yr_f - Base_yr_s + 1) *366
Num_thres = Num_thre+1
#days in each month
days_mon <-  c(31,28,31,30,31,30,31,31,30,31,30,31)
days_monlp <-c(31,29,31,30,31,30,31,31,30,31,30,31)

month_st <-c(1,32,60,91,121,152,182,213,244,274,305,335)
month_st_lp <-c(1,32,61,92,122,153,183,214,245,275,306,336)

#source("F:\\ClimDA\\Scripts\\US_only\\Precip\\ReadInput\\get_12km_grid_boundary.R")
source(c(paste(sep="",workdirUS,"\\Precip\\ReadInput\\get_12km_grid_boundary.R")))

#-------------------------------------------------------
# OBSERVATIONS
#--------------------------------
# read in & set up precip
ob_lon_num=lon_num
ob_lat_num=lat_num

Ob_pr_val<-array(0,dim=c(ob_lon_num,ob_lat_num,bl_day_num))
Num_thres = Num_thre+1

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


#**** OBSERVATION SIMULATION ANALYSIS
flag_obs=1
print('******Begin Observation Analysis')

# read obs file
print("Read data from observation precipitation file")
#source("F:\\ClimDA\\Scripts\\US_only\\Precip\\ReadInput\\readprecip_obs_12km.R")  # read gridded obs file - 1 deg
source(c(paste(sep="",workdirUS,"\\Precip\\ReadInput\\readprecip_obs_12km.R")))

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
years_tot <- Proj_yr_f - Proj_yr_s +1
day_num <- (Proj_yr_f - Proj_yr_s + 1) *366

i=1

tag<-1

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
}  # skipping ahead
flag_obs=0

print("************* Begin Analysis of Model Simulation")
for (i in 1:model_num) {
#for (i in 11:11){
print_model<-c(paste(sep="","Analysis of model number :",i))
#print("  Analysis of model number:")
print(print_model)

#                         Read projection files
tag <- 1   # for projections

pr_val_in <-array(0,dim=c(lon_num,lat_num,day_num,model_num))
#source("F:\\ClimDA\\Scripts\\US_only\\Precip\\ReadInput\\readprecip_12km.R")
source(c(paste(sep="",workdirUS,"\\Precip\\ReadInput\\readprecip_12km.R")))
pr_val[,,,i]<-pr_val_in[,,,i]
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
pr_val_in <-array(0,dim=c(lon_num,lat_num,day_num,model_num))
#source("F:\\ClimDA\\Scripts\\US_only\\Precip\\ReadInput\\readprecip_12km.R")
source(c(paste(sep="",workdirUS,"\\Precip\\ReadInput\\readprecip_12km.R")))
# create new obs precip matrix - regional cropping
#precip_crop<-array(0,dim=c(lon_num,lat_num,day_num,model_num))
#precip_crop<-pr_val_in[lon1_ind:lon2_ind,lat1_ind:lat2_ind,,i]
#remove(pr_val)
#pr_val[,,,i]<-precip_crop  # switch it back
pr_val[,,,i]<-pr_val_in[,,,i]

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
Pr_al1[,,i]<-dum2/tot_yrs
almon=2
source("almond_ind.R")
Pr_al2[,,i]<-dum2/tot_yrs
almon=3
source("almond_ind.R")
Pr_al3[,,i]<-dum2/tot_yrs
}


print("finish algorithms - baseline simulations")
#                      Absolute change (future - baseline) 

if (Dailyprecip==1) {AC_avg_daily_pr <- Pr_avg_daily_pr - Bl_avg_daily_pr}
if (Monprecip ==1) {AC_mon_avg_pr <- Pr_mon_avg_pr - Bl_mon_avg_pr}
if (Annprecip ==1) {AC_ann_pr_tot <- Pr_ann_pr_tot - Bl_ann_pr_tot}
if (returnper==1) {rpFut1_ch <- rpFut_sim - rpBase_sim}
if (thresprecip==1) {AC_thres_pr <- Pr_thres_pr - Bl_thres_pr}
if (max5day==1) {AC_5day <- Pr_5day - Bl_5day}
if (maxCDD==1) {AC_consdd <- Pr_consdd - Bl_consdd}
if (dailyindex ==1) {AC_sdii <- Pr_sdii - Bl_sdii}
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
}
}
}
if (Annprecip ==1) {
for (ii in 1:lon_num) {
for (jj in 1:lat_num) {
CMIN_ann_pr_tot[ii,jj]<-min(AC_ann_pr_tot[ii,jj,])
CMAX_ann_pr_tot[ii,jj]<-max(AC_ann_pr_tot[ii,jj,])
}
}
}

#    Percent difference  (future - baseline) / baseline
if (Dailyprecip==1) {PC_avg_daily_pr <- AC_avg_daily_pr / Bl_avg_daily_pr}
if (max5day==1) {PC_5day <-AC_5day / Bl_5day}
if (maxCDD==1) {PC_consdd <- AC_consdd / Bl_consdd}
if (dailyindex ==1) {PC_sdii <- AC_sdii / Bl_sdii}
if (almonindex==1) {
PC_al1 <- AC_al1/Bl_al1
PC_al2 <- AC_al2/Bl_al2
PC_al3 <- AC_al3/Bl_al3}

if (Monprecip ==1) {
for(imon in 1:12) {
PC_mon_avg_pr[,,imon,] <- AC_mon_avg_pr[,,imon,] / Bl_mon_avg_pr[,,imon,] }
}

if (Annprecip==1) {PC_ann_pr_tot <- AC_ann_pr_tot / Bl_ann_pr_tot}

# removed - Bl could be zero for a grid cell
#for(ithres in 1:Num_thres) {
#PC_thres_pr[,,ithres,] <- AC_thres_pr[,,ithres,] / Bl_thres_pr[,,ithres,] }

 }  # i loop - model by model loop

# ensemble average - absolute  
if (Dailyprecip==1) {ES_AC_avg_daily_pr <- apply(AC_avg_daily_pr,c(1,2),mean)}

if (Monprecip ==1) {
for (imon in 1:12) {
ES_AC_mon_avg_pr[,,imon] <- apply(AC_mon_avg_pr[,,imon,],c(1,2),mean) }
}

#apply(AC_avg_daily_pr,c(1,2),function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})

if (Annprecip ==1) {ES_AC_ann_pr_tot <- apply(AC_ann_pr_tot,c(1,2),mean)}

if (almonindex ==1){
ES_AC_al1<-apply(AC_al1,c(1,2),mean)
ES_AC_al2<-apply(AC_al2,c(1,2),mean)
ES_AC_al3<-apply(AC_al3,c(1,2),mean)}

if (thresprecip ==1) {
for (ithres in 1:Num_thres){
ES_AC_threvalue[,,ithres] <- apply(AC_thres_pr[,,ithres,],c(1,2),mean) }
}

if (max5day==1) {ES_AC_5day <-apply(AC_5day,c(1,2),mean)}
if (maxCDD==1) {ES_AC_consdd<-apply(AC_consdd,c(1,2),mean)}
if (dailyindex==1) {ES_AC_sdii<-apply(AC_sdii,c(1,2),mean)}

if (returnper==1) {
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
if (dailyindex ==1) {ES_PC_avg_daily_pr <- apply(PC_avg_daily_pr,c(1,2),mean)}

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
if (dailyindex ==1) {ES_PC_sdii<-apply(PC_sdii,c(1,2),mean)}

print("Finished ensemble calculations")