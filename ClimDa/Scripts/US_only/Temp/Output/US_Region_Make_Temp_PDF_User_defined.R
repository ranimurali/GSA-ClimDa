#
# Using USGCRP 2014 Climate assessment for U.S. continental regions;  except SE <> Puerto Rico, U.S. Virgin Islands
library(maptools)
library(raster)

print("Developing Figures")

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

dat1o$x=seq(ob_lon1,by=degree_i,len=ob_lon_num)
dat1o$y=seq(ob_lat1,by=degree_i,len=ob_lat_num)

dir_filname<-c(paste(output_dir,"Temp_Figures_",US_region,"_",Scen,".pdf",sep=""))
pdf(dir_filname)

ob_lab<-c(paste(sep="","Obs:",Base_yr_s," to ",Base_yr_f))
proj_lab<-c(paste(sep="","Proj:",Proj_yr_s," to ",Proj_yr_f))
cnames<-c(ob_lab,"Modeled Baseline",proj_lab,"Abs Change","Percent Change","95%-low","95%-high")

thresnames<-rep(0,Num_thre)
consnames<-rep(0,Num_thre)
for (numthre  in 1:Num_thre) {
  thresnames[numthre]<-c(paste("Threshold > ",Thres_val[numthre]))
  consnames[numthre]<-c(paste("Consecutive Days of ",Thres_val[numthre]))
}

rnames<-c("annualTavg","annualTmax","annualTmin",
"JanTavg","FebTavg","MarTavg","AprTavg","MayTavg","JuneTavg","JulTavg","AugTavg","SepTavg","OctTavg","NovTavg","DecTavg",
"JanTmax","FebTmax","MarTmax","AprTmax","MayTmax","JuneTmax","JulTmax","AugTmax","SepTmax","OctTmax","NovTmax","DecTmax",
"JanTmin","FebTmin","MarTmin","AprTmin","MayTmin","JuneTmin","JulTmin","AugTmin","SepTmin","OctTmin","NovTmin","DecTmin",
thresnames,consnames,"HDD","CDD","Diurnal","Daily FreezeThaw") 

bigtable<-array(0,dim=c(length(rnames),7))
# ---- fill in first column with obs bigtable[1, ]; second column with proj bigtable[,2]
# make figures as we go
#
#-- annual temps
#1.  Average Temp
cntyrs=1
dat1o$z=Ob_ann_tavg_tot
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(dat1o$z,na.rm=TRUE)

dat1$z=ES_AC_Tavg_ann
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(dat1$z,na.rm=TRUE)

par(mfrow=c(2,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(rr,main="Obs Annual Temp")

plot(rrp,main="Ens Change in Annual Temp")

dat1$z=CMIN_Tavg_ann
rr=raster(dat1)
plot(rr,main="Minimum Composite")

dat1$z=CMAX_Tavg_ann
rr=raster(dat1)
plot(rr,main="Maximum Composite")

dat1$z<- apply(Bl_Tavg_ann,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(Pr_Tavg_ann,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(dat1$z,na.rm=TRUE)

dat1$z=ES_PC_Tavg_ann
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(AC_Tavg_ann,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[cntyrs,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))

cntyrs = cntyrs + 1

# 2.  TMAX

dat1o$z=Ob_ann_tmax_tot
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(dat1o$z,na.rm=TRUE)

dat1$z=ES_AC_Tmax_ann
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(dat1$z,na.rm=TRUE)

par(mfrow=c(2,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(rr,main="Obs Annual Maximum Temp")

plot(rrp,main="Ens Change in Annual Max Temp")

dat1$z=CMIN_Tmax_ann
rr=raster(dat1)
plot(rr,main="Minimum Composite")

dat1$z=CMAX_Tmax_ann
rr=raster(dat1)
plot(rr,main="Maximum Composite")

dat1$z<- apply(Bl_Tmax_ann,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(Pr_Tmax_ann,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(dat1$z,na.rm=TRUE)

dat1$z=ES_PC_Tmax_ann
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(AC_Tmax_ann,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[cntyrs,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))

cntyrs = cntyrs + 1

#- 3.  TMIN
dat1o$z=Ob_ann_tmin_tot
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
bigtable[cntyrs,1]<-mean(dat1o$z,na.rm=TRUE)

dat1$z=ES_AC_Tmin_ann
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(dat1$z,na.rm=TRUE)

par(mfrow=c(2,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(rr,main="Obs Annual Minimum Temp")

plot(rrp,main="Ens Change in Annual Minimum Temp")

dat1$z=CMIN_Tmin_ann
rr=raster(dat1)
plot(rr,main="Minimum Composite")

dat1$z=CMAX_Tmin_ann
rr=raster(dat1)
plot(rr,main="Maximum Composite")

dat1$z<- apply(Bl_Tmin_ann,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(Pr_Tmin_ann,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(dat1$z,na.rm=TRUE)

dat1$z=ES_PC_Tmin_ann
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(AC_Tmin_ann,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[cntyrs,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))

cntyrs = cntyrs + 1


#-- monthly---------------------------------------------
# 1. Avg Temp
inmonth=cntyrs
#par(mfrow=c(12,2))  
for (imtab in 1:12) {
dat1o$z=Ob_mon_tavg[,,imtab]
rr=raster(dat1o)
#inmonth=inmonth+1
bigtable[inmonth,1]<-mean(dat1o$z,na.rm=TRUE)

if ((imtab == 1) | (imtab == 5) | (imtab == 9)) { par(mfrow=c(2,2)) }  # four plots side-by-side (rows = 2, columns = 2)
plot(rr,main=c(paste("Obs Monthly Tavg: ",imtab,sep="")))

dat1$z=ES_AC_Tavg_mon[,,imtab]
rr=raster(dat1)
bigtable[inmonth,4]<-mean(dat1$z,na.rm=TRUE)
plot(rr,main=c(paste("Change in Monthly Tavg",imtab,sep="")))

dat1$z<-apply(Bl_mon_Tavg[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z<-apply(Pr_Tavg_mon[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,3]<-mean(dat1$z,na.rm=TRUE)

dat1$z<-ES_PC_Tavg_mon[,,imtab]
rr=raster(dat1)
bigtable[inmonth,5]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(AC_Tavg_mon[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[inmonth,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))

inmonth = inmonth + 1
}
# 2. Max Temp
#par(mfrow=c(12,2))  
for (imtab in 1:12) {
dat1o$z=Ob_mon_tmax[,,imtab]
rr=raster(dat1o)

bigtable[inmonth,1]<-mean(dat1o$z,na.rm=TRUE)

if ((imtab == 1) | (imtab == 5) | (imtab == 9)) { par(mfrow=c(2,2)) }  # four plots side-by-side (rows = 2, columns = 2)
#par(mfrow=c(2,2))  # two plots side-by-side (rows = 1, columns = 2)
plot(rr,main=c(paste("Obs Monthly Tmax: ",imtab,sep="")))

dat1$z=ES_AC_Tmax_mon[,,imtab]
rr=raster(dat1)
bigtable[inmonth,4]<-mean(dat1$z,na.rm=TRUE)
plot(rr,main=c(paste("Change in Monthly Tmax",imtab,sep="")))

dat1$z<-apply(Bl_mon_Tmax[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z<-apply(Pr_Tmax_mon[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,3]<-mean(dat1$z,na.rm=TRUE)

dat1$z<-ES_PC_Tmax_mon[,,imtab]
rr=raster(dat1)
bigtable[inmonth,5]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(AC_Tmax_mon[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[inmonth,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
inmonth = inmonth + 1
}

# 3. Min Temp

#par(mfrow=c(12,2))  
for (imtab in 1:12) {
dat1o$z=Ob_mon_tmin[,,imtab]
rr=raster(dat1o)

bigtable[inmonth,1]<-mean(dat1o$z,na.rm=TRUE)

if ((imtab == 1) | (imtab == 5) | (imtab == 9)) { par(mfrow=c(2,2)) }  # four plots side-by-side (rows = 2, columns = 2)
#par(mfrow=c(2,2))  # two plots side-by-side (rows = 1, columns = 2)
plot(rr,main=c(paste("Obs Monthly Tmin:",imtab,sep="")))

dat1$z=ES_AC_Tmin_mon[,,imtab]
rr=raster(dat1)
bigtable[inmonth,4]<-mean(dat1$z,na.rm=TRUE)
plot(rr,main=c(paste("Change in Monthly Tmin",imtab,sep="")))

dat1$z<-apply(Bl_mon_Tmin[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z<-apply(Pr_Tmin_mon[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,3]<-mean(dat1$z,na.rm=TRUE)

dat1$z<-ES_PC_Tmin_mon[,,imtab]
rr=raster(dat1)
bigtable[inmonth,5]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(AC_Tmin_mon[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[inmonth,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))

inmonth = inmonth + 1
}   # 

print("plotting Tmax thresholds")

#  plot Tmax thresholds
par(mfrow=c(Num_thre,2))
cntloc = inmonth-1
for (ithres in 1:Num_thre) {
cntloc = cntloc + 1

print(cntloc)

dat1o$z=Ob_thres_tmax[,,ithres]
rr=raster(dat1o)
bigtable[cntloc,1]<-mean(dat1o$z,na.rm=TRUE)
plot(rr,main=c(paste("Obs Days above Tmax threshold:",Thres_val[ithres],sep="")))

dat1$z=ES_AC_Tmax_thres[,,ithres]
rr=raster(dat1)
bigtable[cntloc,4]<-mean(dat1$z,na.rm=TRUE)
plot(rr,main=c(paste("Proj Days above Tmax threshold:",Thres_val[ithres],sep="")))

dat1$z<-apply(Bl_Tmax_thres[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z<-apply(Pr_Tmax_thres[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,3]<-mean(dat1$z,na.rm=TRUE)

#dat1$z=ES_PC_Tmax_thres[,,ithres]
#rr=raster(dat1)
#bigtable[cntloc,5]<-mean(dat1$z,na.rm=TRUE)
bigtable[cntloc,5]<-"n/a"

dat1$z=apply(AC_Tmax_thres[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[cntloc,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
}

print("consecutive days for each thresholds")
print(cntloc+1)

#  plot Tmax thresholds
par(mfrow=c(Num_thre,2))

for (ithres in 1:Num_thre) {
cntloc = cntloc + 1
dat1o$z=Ob_thres_ctd[,,ithres]
rr=raster(dat1o)
bigtable[cntloc,1]<-mean(dat1o$z,na.rm=TRUE)
plot(rr,main=c(paste("Obs Cons Days above Tmax threshold:",Thres_val[ithres],sep="")))

dat1$z=ES_AC_thres_ctd[,,ithres]
rr=raster(dat1)
bigtable[cntloc,4]<-mean(dat1$z,na.rm=TRUE)
plot(rr,main=c(paste("Proj Cons Days above Tmax threshold:",Thres_val[ithres],sep="")))

dat1$z<-apply(Bl_thres_ctd[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z<-apply(Pr_thres_ctd[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,3]<-mean(dat1$z,na.rm=TRUE)

#dat1$z=ES_PC_thres_ctd[,,ithres]
#rr=raster(dat1)
#bigtable[cntloc,5]<-mean(dat1$z,na.rm=TRUE)
bigtable[cntloc,5]<-"n/a"

dat1$z=apply(AC_thres_ctd[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[cntloc,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
}

cntyrs<-cntloc+1

print("plotting HDD")
print(cntyrs)

dat1o$z=Ob_HDD
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(dat1o$z,na.rm=TRUE)

dat1$z=ES_AC_HDD
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(dat1$z,na.rm=TRUE)

par(mfrow=c(2,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(rr,main="Obs Annual HDD")

plot(rrp,main="Ens Change in HDD")

dat1$z<- apply(Bl_HDD,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(Pr_HDD,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(dat1$z,na.rm=TRUE)

dat1$z=ES_PC_HDD
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(AC_HDD,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[cntyrs,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))

cntyrs = cntyrs + 1

print("plotting CDD")
print(cntyrs)

dat1o$z=Ob_CDD
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(dat1o$z,na.rm=TRUE)

dat1$z=ES_AC_CDD
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(dat1$z,na.rm=TRUE)

par(mfrow=c(2,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(rr,main="Obs Annual CDD")

plot(rrp,main="Ens Change in CDD")

dat1$z<- apply(Bl_CDD,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(Pr_CDD,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(dat1$z,na.rm=TRUE)

dat1$z=ES_PC_CDD
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(AC_CDD,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[cntyrs,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))

cntyrs = cntyrs + 1

print("plotting diurnal range")
print(cntyrs)

dat1o$z=Ob_drange
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(dat1o$z,na.rm=TRUE)

dat1$z=ES_AC_drange
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(dat1$z,na.rm=TRUE)

par(mfrow=c(2,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(rr,main="Obs Avg Annual Diurnal Range")

plot(rrp,main="Ens Change in Diurnal Range")

dat1$z<- apply(Bl_drange,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(Pr_drange,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(dat1$z,na.rm=TRUE)

dat1$z=ES_PC_drange
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(AC_drange,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[cntyrs,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))

cntyrs = cntyrs + 1

print("plotting freeze/thaw")
print(cntyrs)

dat1o$z=Ob_frethaw
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[cntyrs,1]<-mean(dat1o$z,na.rm=TRUE)

dat1$z=ES_AC_frethaw
rrp=raster(dat1)
bigtable[cntyrs,4]<-mean(dat1$z,na.rm=TRUE)

par(mfrow=c(2,2))  # two plots side-by-side (rows = 2, columns = 2)
plot(rr,main="Obs Avg Freeze Thaw Days per Year")

plot(rrp,main="Ens Change in Freeze Thaw")

dat1$z<- apply(Bl_frethaw,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(Pr_frethaw,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(dat1$z,na.rm=TRUE)

dat1$z=ES_PC_frethaw
rr=raster(dat1)
bigtable[cntyrs,5]<-mean(dat1$z,na.rm=TRUE)

dat1$z=apply(AC_frethaw,c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(dat1$z,na.rm=TRUE)-1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))
bigtable[cntyrs,7]<-mean(dat1$z,na.rm=TRUE)+1.96*sd(dat1$z,na.rm=TRUE)/sqrt(length(dat1$z))

cntyrs = cntyrs + 1

dev.off()

# table to console
write.table(format(bigtable, justify="right"),  row.names=rnames, col.names=cnames, quote=F)
# output to csv
out_tab_fil<-c(paste(sep="",output_dir,US_region,"_Temp_",Scen,".csv"))
write.table(format(bigtable, justify="right"), file=out_tab_fil, row.names=rnames, col.names=cnames, quote=F,sep=",")

