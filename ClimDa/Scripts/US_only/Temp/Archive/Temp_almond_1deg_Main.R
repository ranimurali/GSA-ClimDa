#  ClimDA:  ALMOND CALIFORNIA RUN
#  1/17/2016
# 1 degree resolution 
#  Rawlings Miller
#-----------------------------------

setwd('F:\\ClimDA\\Scripts\\US_only\\Temp\\Functions')

Al2_Num_thre=8  # Blossom Tmin (oF)
Al2_Thres_val<-c(22,23,24,26,27,28,29,32)

Al3_Num_thre=5  # Maturing nuts Tmin (oF)
Al3_Thres_val<-c(22,23,24,27,29)

Al7_Num_thre=8  # Maturing nuts Tmax (oF)
Al7_Thres_val<-c(72,75,77,82,88,90,95,100)

computecode = 1 # =1 computations
makefigtab = 1	 # make figures in PDF and table in csv
makeGISdatafile = 0  # make GIS data file

if (computecode == 1) {
print("Start processing for 1 degree Temp for Almonds")
#source("Temp_US_1deg_Main.R")
flag_obs=1
#-------------------------------------------------------
# OBSERVATIONS
#--------------------------------
# read in & set up precip
##--- obs variables
Ob_almon1<- array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_almon2<- array(0,dim=c(ob_lon_num,ob_lat_num,Al2_Num_thre))
Ob_almon3<- array(0,dim=c(ob_lon_num,ob_lat_num,Al3_Num_thre))
Ob_almon4<- array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_almon5<- array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_almon6<- array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_almon7<- array(0,dim=c(ob_lon_num,ob_lat_num,Al7_Num_thre))
Ob_almon8<- array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_almon9<- array(0,dim=c(ob_lon_num,ob_lat_num))
Ob_almon10<- array(0,dim=c(ob_lon_num,ob_lat_num))

# working variables
Ob_dum <- array(0,dim=c(ob_lon_num,ob_lat_num)) 
Ob_dum2<- array(0,dim=c(ob_lon_num,ob_lat_num)) 
Ob_dum_t <- array(0,dim=c(ob_lon_num,ob_lat_num,Al2_Num_thre)) 
Ob_dum2_t<- array(0,dim=c(ob_lon_num,ob_lat_num,Al3_Num_thre))
Ob_dum3_t <- array(0,dim=c(ob_lon_num,ob_lat_num,Al7_Num_thre))

#**** OBSERVATION SIMULATION ANALYSIS
flag_obs=1
print('******Begin Observation Analysis')

lon_num<-ob_lon_num
lat_num<-ob_lat_num

setwd('F:\\ClimDA\\Scripts\\US_only\\Temp\\Functions')
#------- Tmax
almonT = 6
Ob_temp<-Ob_tmax
source('almond_temp_indicators.R')
Ob_almon6<-Ob_dum

almonT = 7
source('almond_temp_indicators.R')
Ob_almon7<-Ob_dum3_t

almonT =8
source('almond_temp_indicators.R')
Ob_almon8<-Ob_dum

#-----------Tmin
Ob_temp<-Ob_tmin

almonT = 1
source('almond_temp_indicators.R')
Ob_almon1<-dum2/tot_yrs

almonT = 2
source('almond_temp_indicators.R')
Ob_almon2<-Ob_dum_t/tot_yrs

almonT = 3
source('almond_temp_indicators.R')
Ob_almon3<-Ob_dum2_t/tot_yrs

almonT = 4
source('almond_temp_indicators.R')
Ob_almon4<-Ob_dum/tot_yrs

almonT = 5
source('almond_temp_indicators.R')
Ob_almon5<-Ob_dum/tot_yrs

#---------Tavg
Ob_temp<- Ob_tavg  #Ob_tavg<-(Ob_tmax+Ob_tmin)/2

almonT = 9
source('almond_temp_indicators.R')
Ob_almon9<-Ob_dum/tot_yrs

almonT = 10
source('almond_temp_indicators.R')
Ob_almon10<-Ob_dum/tot_yrs

print("End Observation Analysis")

if (J_obs ==1) {stop("only obs analysis selected")}  # stop computing if only want obs analysis

#-------------------------------------------------------
# PROJECTIONS
#--------------------------------
# set up matrix with lat/lon and indices for the region for the 1deg projections
i=1
lon_num=11
lat_num=10
#-- proj variables

#Pr_Tmax_val <-array(0,dim=c(lon_num,lat_num,day_num,model_num))  # store Tmax data
#Pr_Tmin_val <-array(0,dim=c(lon_num,lat_num,day_num,model_num))  # store Tmin data
Pr_almon1<- array(0,dim=c(lon_num,lat_num,model_num))
Pr_almon2<- array(0,dim=c(lon_num,lat_num,Al2_Num_thre,model_num))
Pr_almon3<- array(0,dim=c(lon_num,lat_num,Al3_Num_thre,model_num))
Pr_almon4<- array(0,dim=c(lon_num,lat_num,model_num))
Pr_almon5<- array(0,dim=c(lon_num,lat_num,model_num))
Pr_almon6<- array(0,dim=c(lon_num,lat_num,model_num))
Pr_almon7<- array(0,dim=c(lon_num,lat_num,Al7_Num_thre,model_num))
Pr_almon8<- array(0,dim=c(lon_num,lat_num,model_num))
Pr_almon9<- array(0,dim=c(lon_num,lat_num,model_num))
Pr_almon10<- array(0,dim=c(lon_num,lat_num,model_num))

#-- baseline variables  [should be Bl not B1 - need to check!!!!]
#Bl_Tmax_val <-array(0,dim=c(lon_num,lat_num,bl_day_num,model_num))  # store Tmax data
#Bl_Tmin_val <-array(0,dim=c(lon_num,lat_num,bl_day_num,model_num)) 
Bl_almon1<- array(0,dim=c(lon_num,lat_num,model_num))
Bl_almon2<- array(0,dim=c(lon_num,lat_num,Al2_Num_thre,model_num))
Bl_almon3<- array(0,dim=c(lon_num,lat_num,Al3_Num_thre,model_num))
Bl_almon4<- array(0,dim=c(lon_num,lat_num,model_num))
Bl_almon5<- array(0,dim=c(lon_num,lat_num,model_num))
Bl_almon6<- array(0,dim=c(lon_num,lat_num,model_num))
Bl_almon7<- array(0,dim=c(lon_num,lat_num,Al7_Num_thre,model_num))
Bl_almon8<- array(0,dim=c(lon_num,lat_num,model_num))
Bl_almon9<- array(0,dim=c(lon_num,lat_num,model_num))
Bl_almon10<- array(0,dim=c(lon_num,lat_num,model_num))

#-- Absolute change of the variables
AC_almon1<- array(0,dim=c(lon_num,lat_num,model_num))
AC_almon2<- array(0,dim=c(lon_num,lat_num,Al2_Num_thre,model_num))
AC_almon3<- array(0,dim=c(lon_num,lat_num,Al3_Num_thre,model_num))
AC_almon4<- array(0,dim=c(lon_num,lat_num,model_num))
AC_almon5<- array(0,dim=c(lon_num,lat_num,model_num))
AC_almon6<- array(0,dim=c(lon_num,lat_num,model_num))
AC_almon7<- array(0,dim=c(lon_num,lat_num,Al7_Num_thre,model_num))
AC_almon8<- array(0,dim=c(lon_num,lat_num,model_num))
AC_almon9<- array(0,dim=c(lon_num,lat_num,model_num))
AC_almon10<- array(0,dim=c(lon_num,lat_num,model_num))

#-- Percent change of the variables
PC_almon1<- array(0,dim=c(lon_num,lat_num,model_num))
PC_almon2<- array(0,dim=c(lon_num,lat_num,Al2_Num_thre,model_num))
PC_almon3<- array(0,dim=c(lon_num,lat_num,Al3_Num_thre,model_num))
PC_almon4<- array(0,dim=c(lon_num,lat_num,model_num))
PC_almon5<- array(0,dim=c(lon_num,lat_num,model_num))
PC_almon6<- array(0,dim=c(lon_num,lat_num,model_num))
PC_almon7<- array(0,dim=c(lon_num,lat_num,Al7_Num_thre,model_num))
PC_almon8<- array(0,dim=c(lon_num,lat_num,model_num))
PC_almon9<- array(0,dim=c(lon_num,lat_num,model_num))
PC_almon10<- array(0,dim=c(lon_num,lat_num,model_num))

# ensemble -- Absolute change of the variables
ES_AC_almon1<- array(0,dim=c(lon_num,lat_num))
ES_AC_almon2<- array(0,dim=c(lon_num,lat_num,Al2_Num_thre))
ES_AC_almon3<- array(0,dim=c(lon_num,lat_num,Al3_Num_thre))
ES_AC_almon4<- array(0,dim=c(lon_num,lat_num))
ES_AC_almon5<- array(0,dim=c(lon_num,lat_num))
ES_AC_almon6<- array(0,dim=c(lon_num,lat_num))
ES_AC_almon7<- array(0,dim=c(lon_num,lat_num,Al7_Num_thre))
ES_AC_almon8<- array(0,dim=c(lon_num,lat_num))
ES_AC_almon9<- array(0,dim=c(lon_num,lat_num))
ES_AC_almon10<- array(0,dim=c(lon_num,lat_num))

#ensemble -- Percent change of the variables
ES_PC_almon1<- array(0,dim=c(lon_num,lat_num))
ES_PC_almon2<- array(0,dim=c(lon_num,lat_num,Al2_Num_thre))
ES_PC_almon3<- array(0,dim=c(lon_num,lat_num,Al3_Num_thre))
ES_PC_almon4<- array(0,dim=c(lon_num,lat_num))
ES_PC_almon5<- array(0,dim=c(lon_num,lat_num))
ES_PC_almon6<- array(0,dim=c(lon_num,lat_num))
ES_PC_almon7<- array(0,dim=c(lon_num,lat_num,Al7_Num_thre))
ES_PC_almon8<- array(0,dim=c(lon_num,lat_num))
ES_PC_almon9<- array(0,dim=c(lon_num,lat_num))
ES_PC_almon10<- array(0,dim=c(lon_num,lat_num))

# working variables 
Temp_val <-array(0,dim=c(lon_num,lat_num,day_num))  # store data
mod_dum_t <- array(0,dim=c(lon_num,lat_num,Al2_Num_thre)) 
mod_dum2_t<- array(0,dim=c(lon_num,lat_num,Al3_Num_thre))
mod_dum3_t <- array(0,dim=c(lon_num,lat_num,Al7_Num_thre))

dum <- array(0,dim=c(lon_num,lat_num))  # dummy array for monthly tmax
dum2 <-array(0,dim=c(lon_num,lat_num))

#
#****  MODEL SIMULATION ANALYSIS
flag_obs=0

print("************* Begin Analysis of Model Simulation")
for (i in 1:model_num) {
print_model<-c(paste(sep="","Analysis of model number :",i))
print(print_model)

#                         Read projection files
tag <- 1   # for projections
#                         Choose algorithms

setwd('F:\\ClimDA\\Scripts\\US_only\\Temp\\Functions')

#------- Tmax
almonT = 6
Temp_val<-Pr_Tmax_val[,,,i]
source('almond_temp_indicators.R')
Pr_almon6[,,i]<-mod_dum/tot_yrs

almonT = 7
source('almond_temp_indicators.R')
Pr_almon7[,,,i]<-mod_dum3_t/tot_yrs

almonT =8
source('almond_temp_indicators.R')
Pr_almon8[,,i]<-mod_dum/tot_yrs

#-----------Tmin
Temp_val<-Pr_Tmin_val[,,,i]

almonT = 1
source('almond_temp_indicators.R')
Pr_almon1[,,i]<-dum2/tot_yrs

almonT = 2
source('almond_temp_indicators.R')
Pr_almon2[,,,i]<-mod_dum_t/tot_yrs

almonT = 3
source('almond_temp_indicators.R')
Pr_almon3[,,,i]<-mod_dum2_t/tot_yrs

almonT = 4
source('almond_temp_indicators.R')
Pr_almon4[,,i]<-dum/tot_yrs

almonT = 5
source('almond_temp_indicators.R')
Pr_almon5[,,i]<-dum/tot_yrs

#---------Tavg
Temp_val<- (Pr_Tmin_val[,,,i]+Pr_Tmax_val[,,,i])/2  #Ob_tavg<-(Ob_tmax+Ob_tmin)/2

almonT = 9
source('almond_temp_indicators.R')
Pr_almon9[,,i]<-dum/tot_yrs

almonT = 10
source('almond_temp_indicators.R')
Pr_almon10[,,i]<-dum/tot_yrs

print("finish algorithms - projection")
#                         Read baseline simulations
setwd('F:\\ClimDA\\Scripts\\US_only\\Temp\\Functions')
# working variables 
Temp_val <-array(0,dim=c(lon_num,lat_num,day_num))  # store data
mod_dum_t <- array(0,dim=c(lon_num,lat_num,Al2_Num_thre)) 
mod_dum2_t<- array(0,dim=c(lon_num,lat_num,Al3_Num_thre))
mod_dum3_t <- array(0,dim=c(lon_num,lat_num,Al7_Num_thre))

dum <- array(0,dim=c(lon_num,lat_num))  # dummy array for monthly tmax
dum2 <-array(0,dim=c(lon_num,lat_num))

#------- Tmax
almonT = 6
Temp_val<-Pr_Tmax_val[,,,i]
source('almond_temp_indicators.R')
Bl_almon6[,,i]<-mod_dum/tot_yrs

almonT = 7
source('almond_temp_indicators.R')
Bl_almon7[,,,i]<-mod_dum3_t/tot_yrs

almonT =8
source('almond_temp_indicators.R')
Bl_almon8[,,i]<-mod_dum/tot_yrs

#-----------Tmin
Temp_val<-Bl_Tmin_val[,,,i]

almonT = 1
source('almond_temp_indicators.R')
Bl_almon1[,,i]<-dum2/tot_yrs

almonT = 2
source('almond_temp_indicators.R')
Bl_almon2[,,,i]<-mod_dum_t/tot_yrs

almonT = 3
source('almond_temp_indicators.R')
Bl_almon3[,,,i]<-mod_dum2_t/tot_yrs

almonT = 4
source('almond_temp_indicators.R')
Bl_almon4[,,i]<-dum/tot_yrs

almonT = 5
source('almond_temp_indicators.R')
Bl_almon5[,,i]<-dum/tot_yrs

#---------Tavg
Temp_val<- (Bl_Tmin_val[,,,i]+Bl_Tmax_val[,,,i])/2  #Ob_tavg<-(Ob_tmax+Ob_tmin)/2

almonT = 9
source('almond_temp_indicators.R')
Bl_almon9[,,i]<-dum/tot_yrs

almonT = 10
source('almond_temp_indicators.R')
Bl_almon10[,,i]<-dum/tot_yrs

 }  # i loop - model by model loop

print("finish algorithms - baseline simulations")
#                      Absolute change (future - baseline) 
AC_almon1 <- Pr_almon1 - Bl_almon1
AC_almon2 <- Pr_almon2 - Bl_almon2
AC_almon3 <- Pr_almon3 - Bl_almon3
AC_almon4 <- Pr_almon4 - Bl_almon4
AC_almon5 <- Pr_almon5 - Bl_almon5
AC_almon6 <- Pr_almon6 - Bl_almon6
AC_almon7 <- Pr_almon7 - Bl_almon7
AC_almon8 <- Pr_almon8 - Bl_almon8
AC_almon9 <- Pr_almon9 - Bl_almon9
AC_almon10 <- Pr_almon10 - Bl_almon10
#    Percent difference  (future - baseline) / baseline
PC_almon1 <- AC_almon1 / Bl_almon1
PC_almon2 <- AC_almon2 / Bl_almon2
PC_almon3 <- AC_almon3 / Bl_almon3
PC_almon4 <- AC_almon4 / Bl_almon4
PC_almon5 <- AC_almon5 / Bl_almon5
PC_almon6 <- AC_almon6 / Bl_almon6
PC_almon7 <- AC_almon7 / Bl_almon7
PC_almon8 <- AC_almon8 / Bl_almon8
PC_almon9 <- AC_almon9 / Bl_almon9
PC_almon10 <- AC_almon10 / Bl_almon10

# ensemble average - absolute  
ES_AC_almon1 <- apply(AC_almon1,c(1,2),mean)
ES_AC_almon4 <- apply(AC_almon4,c(1,2),mean)
ES_AC_almon5 <- apply(AC_almon5,c(1,2),mean)
ES_AC_almon6 <- apply(AC_almon6,c(1,2),mean)
ES_AC_almon8 <- apply(AC_almon8,c(1,2),mean)
ES_AC_almon9 <- apply(AC_almon9,c(1,2),mean)
ES_AC_almon10 <- apply(AC_almon10,c(1,2),mean)

for (ithres in 1:Al2_Num_thre) {
ES_AC_almon2[,,ithres] <- apply(AC_almon2[,,ithres,],c(1,2),mean)
}
for (ithres in 1:Al3_Num_thre) {
ES_AC_almon3[,,ithres] <- apply(AC_almon3[,,ithres,],c(1,2),mean)
}
for (ithres in 1:Al7_Num_thre) {
ES_AC_almon7[,,ithres] <- apply(AC_almon7[,,ithres,],c(1,2),mean)
}

# ensemble average - percent diff
ES_PC_almon1 <- apply(PC_almon1,c(1,2),mean)
ES_PC_almon4 <- apply(PC_almon4,c(1,2),mean)
ES_PC_almon5 <- apply(PC_almon5,c(1,2),mean)
ES_PC_almon6 <- apply(PC_almon6,c(1,2),mean)
ES_PC_almon8 <- apply(PC_almon8,c(1,2),mean)
ES_PC_almon9 <- apply(PC_almon9,c(1,2),mean)
ES_PC_almon10 <- apply(PC_almon10,c(1,2),mean)

for (ithres in 1:Al2_Num_thre) {
ES_PC_almon2[,,ithres] <- apply(PC_almon2[,,ithres,],c(1,2),mean)
}
for (ithres in 1:Al3_Num_thre) {
ES_PC_almon3[,,ithres] <- apply(PC_almon3[,,ithres,],c(1,2),mean)
}
for (ithres in 1:Al7_Num_thre) {
ES_PC_almon7[,,ithres] <- apply(PC_almon7[,,ithres,],c(1,2),mean)
}
print("Finished ensemble calculations")
}

if (makefigtab ==1) {

#------------------------------------------------ MAKE TABLE
dat1=list()
# 1 degree grid or 1/8 degree
if (inp==1) {degree_i = 1}
if (inp>1) {degree_i = 0.125}

dat1$x=seq(lon1,by=degree_i,len=lon_num)
dat1$y=seq(lat1,by=degree_i,len=lat_num)

dat1o=list()
# do we need a different array set for obs than proj?
if (inp>1) {
ob_lon1 = lon1
ob_lat1 = lat1}
#  below - no reason it call if run after ClimDA main as these are already set
#if (inp==1){
#ob_lon1 = ob_lon1
#ob_lat1 = ob_lat1
#ob_lat_num = ob_lat_num
#ob_lon_num =  ob_lon_num}
# CA runs
#ob_lon1 = -124
#ob_lat1 = 32
#ob_lat_num<-11
#ob_lon_num<-11}

dat1o$x=seq(ob_lon1,by=degree_i,len=ob_lon_num)
dat1o$y=seq(ob_lat1,by=degree_i,len=ob_lat_num)

dir_filname<-c(paste(output_dir,"Temp_Figures_Almond_",US_region,"_",Scen,".pdf",sep=""))
pdf(dir_filname)

ob_lab<-c(paste(sep="","Obs:",Base_yr_s," to ",Base_yr_f))
proj_lab<-c(paste(sep="","Proj:",Proj_yr_s," to ",Proj_yr_f))
cnames<-c(ob_lab,"Modeled Baseline",proj_lab,"Abs Change","Percent Change","95%-low","95%-high")

thresnames2<-rep(0,Al2_Num_thre)
thresnames3<-rep(0,Al3_Num_thre)
thresnames7<-rep(0,Al7_Num_thre)
for (numthre  in 1:Al2_Num_thre) {
  thresnames2[numthre]<-(paste("BloomDayMinTemp < ",Al2_Thres_val[numthre]))}
for (numthre  in 1:Al3_Num_thre) {
  thresnames3[numthre]<-(paste("NutDayMinTemp< ",Al3_Thres_val[numthre]))}
for (numthre  in 1:Al7_Num_thre) {
  thresnames7[numthre]<-(paste("Blooms Days Tmax > ",Al7_Thres_val[numthre]))}

rnames<-c("FebDays<39.2",thresnames2,thresnames3,"DormantDays<29","DormantDaysTmin<45","DormantDaysTmax<45",thresnames7,"NutTmaxDays>75","DormantDTavg<45","BloomTavgD>=55")

bigtable<-array(0,dim=c(length(rnames),7))
# ---- fill in first column with obs bigtable[1, ]; second column with proj bigtable[,2]
# make figures as we go
#
cntyrs=1
print("indicator 1")
dat1o$z=Ob_almon1
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_AC_almon1
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rrp,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

par(mfrow=c(1,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(mask(rr,reg_cutout),main="Obs Feb Days Tmin<39.2F")
plot(reg_cutout,add=TRUE)

plot(mask(rrp,reg_cutout),main="Ens Change in 2030")
plot(reg_cutout,add=TRUE)

dat1$z<- apply(Bl_almon1,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(Pr_almon1,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_almon1
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_almon1,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

cntyrs = cntyrs + 1

#  almond indicator 2
print("indicator 2")
par(mfrow=c(Al3_Num_thre,2))
#cntloc = inmonth-1
for (ithres in 1:Al3_Num_thre) {
#cntloc = cntloc + 1

#print(cntloc)

dat1o$z=Ob_almon2[,,ithres]
rr=raster(dat1o)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("BloomDayMinTemp (F) < ",Al2_Thres_val[ithres],sep="")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_almon2[,,ithres]
rr=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Proj Change in Days ",sep="")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_almon2[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_almon2[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_almon2[,,ithres]
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
#bigtable[cntyrs,5]<-"n/a"

dat1$z=apply(AC_almon2[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
cntyrs = cntyrs + 1
}
#  almond indicator 3
print("indicator 3")
par(mfrow=c(Al3_Num_thre,2))
#cntloc = inmonth-1
for (ithres in 1:Al3_Num_thre) {
#cntloc = cntloc + 1

#print(cntloc)

dat1o$z=Ob_almon3[,,ithres]
rr=raster(dat1o)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("NutDayMinTemp(F) < ",Al3_Thres_val[ithres],sep="")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_almon3[,,ithres]
rr=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Proj Change in Days ",sep="")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_almon3[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_almon3[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_almon3[,,ithres]
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
#bigtable[cntyrs,5]<-"n/a"

dat1$z=apply(AC_almon3[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
cntyrs = cntyrs + 1
}
# almond indicator 4
print("indicator 4")
dat1o$z=Ob_almon4
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_AC_almon4
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rrp,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

par(mfrow=c(1,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(mask(rr,reg_cutout),main="Obs DormantDays Tmin<29F")
plot(reg_cutout,add=TRUE)

plot(mask(rrp,reg_cutout),main="Ens Change in 2030")
plot(reg_cutout,add=TRUE)

dat1$z<- apply(Bl_almon4,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(Pr_almon4,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_almon4
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_almon4,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

cntyrs = cntyrs + 1

# almond indicator 5
print("indicator 5")
dat1o$z=Ob_almon5
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_AC_almon5
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rrp,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

par(mfrow=c(1,1))  # two plots side-by-side (rows = 2, columns = 2)
plot(mask(rr,reg_cutout),main="Obs DormantDaysTmin < 45F")
plot(reg_cutout,add=TRUE)

plot(mask(rrp,reg_cutout),main="Ens Change in 2030")
plot(reg_cutout,add=TRUE)

dat1$z<- apply(Bl_almon5,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(Pr_almon5,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_almon5
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_almon5,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

cntyrs = cntyrs + 1

# almond indicator 6
print("indicator 6")
dat1o$z=Ob_almon6
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_AC_almon6
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rrp,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

par(mfrow=c(1,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(mask(rr,reg_cutout),main="Obs DormantDaysTmax < 45F")
plot(reg_cutout,add=TRUE)

plot(mask(rrp,reg_cutout),main="Ens Change in 2030")
plot(reg_cutout,add=TRUE)

dat1$z<- apply(Bl_almon6,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(Pr_almon6,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_almon6
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_almon6,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

cntyrs = cntyrs + 1

#  almond indicator 7
print("indicator 7")
par(mfrow=c(Al7_Num_thre,2))
#cntloc = inmonth-1
for (ithres in 1:Al7_Num_thre) {

dat1o$z=Ob_almon7[,,ithres]
rr=raster(dat1o)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Blooms Days Tmax (F) > < ",Al2_Thres_val[ithres],sep="")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_almon7[,,ithres]
rr=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Proj Change in Days ",sep="")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_almon7[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_almon7[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_almon7[,,ithres]
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
#bigtable[cntyrs,5]<-"n/a"

dat1$z=apply(AC_almon7[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
cntyrs <-cntyrs+1
}

# almond indicator 8
print("indicator 8")
dat1o$z=Ob_almon8
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_AC_almon8
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rrp,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

par(mfrow=c(1,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(mask(rr,reg_cutout),main="Obs NutTmaxDays > 75F")
plot(reg_cutout,add=TRUE)

plot(mask(rrp,reg_cutout),main="Ens Change in 2030")
plot(reg_cutout,add=TRUE)

dat1$z<- apply(Bl_almon8,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(Pr_almon8,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_almon8
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_almon8,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

cntyrs = cntyrs + 1

# almond indicator 9
print("indicator 9")
dat1o$z=Ob_almon9
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_AC_almon9
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rrp,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

par(mfrow=c(1,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(mask(rr,reg_cutout),main="Obs Dormant Days Tavg<45")
plot(reg_cutout,add=TRUE)

plot(mask(rrp,reg_cutout),main="Ens Change in 2030")
plot(reg_cutout,add=TRUE)

dat1$z<- apply(Bl_almon9,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(Pr_almon9,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_almon9
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_almon9,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

cntyrs = cntyrs + 1

# almond indicator 10
print("indicator 10")
dat1o$z=Ob_almon10
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_AC_almon10
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rrp,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

par(mfrow=c(1,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(mask(rr,reg_cutout),main="Obs Bloom Days Tavg >=55")
plot(reg_cutout,add=TRUE)

plot(mask(rrp,reg_cutout),main="Ens Change in 2030")
plot(reg_cutout,add=TRUE)

dat1$z<- apply(Bl_almon10,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(Pr_almon10,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_almon10
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_almon10,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

#cntyrs = cntyrs + 1

dev.off()

# table to console
write.table(format(bigtable, justify="right"),  row.names=rnames, col.names=cnames, quote=F)
# output to csv
out_tab_fil<-c(paste(sep="",output_dir,US_region,"_Temp_Almond",Scen,".csv"))
write.table(format(bigtable, justify="right"), file=out_tab_fil, row.names=rnames, col.names=cnames, quote=F,sep=",")
} # make figures and tables