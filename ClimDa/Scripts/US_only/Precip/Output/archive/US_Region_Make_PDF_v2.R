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

dir_filname<-c(paste(output_dir,"Figures_",US_region,"_",Scen,".pdf",sep=""))
pdf(dir_filname)

ob_lab<-c(paste(sep="","Obs:",Base_yr_s," to ",Base_yr_f))
proj_lab<-c(paste(sep="","Proj:",Proj_yr_s," to ",Proj_yr_f))
cnames<-c(ob_lab,"Modeled Baseline",proj_lab,"Abs Change","Percent Change","95%-low","95%-high")
#cnames<-c(Scen,ob_lab,proj_lab)

retnames<-rep(0,Num_yrs)
for (rps in 1:Num_yrs) {
 retnames[rps]<-c(paste("Ret ",Ret_yrs[rps]))
}

thresnames<-rep(0,Num_thres)
for (numthre  in 1:Num_thre) {
  thresnames[numthre]<-c(paste("Threshold < ",Thres_val[numthre]))
}
thresnames[Num_thres]<-c(paste("Threshold > ",Thres_val[numthre]))

if (returnper == 1) {
rnames<-c("annual","avgdaily","Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sep","Oct","Nov","Dec",retnames,
thresnames,"max5day","maxCDD","dailyindex","almonYRprecip","almonJanbad","almonFebbad") }
if (returnper !=1) {
rnames<-c("annual","avgdaily","Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sep","Oct","Nov","Dec",
thresnames,"max5day","maxCDD","dailyindex","almonYRprecip","almonJanbad","almonFebbad") }


bigtable<-array(0,dim=c(length(rnames),7))
# ---- fill in first column with obs bigtable[1, ]; second column with proj bigtable[,2]
# make figures as we go
#
#-- annual precip
if (Annprecip ==1) {
dat1o$z=Ob_ann_pr_tot
rr=raster(dat1o)
# this will extract by state - but our grid size is too large for that - need regional averages, hence average of this
# na.rm=TRUE since some states have no grid cells that work for them - so the values are NA
#bigtable[1,1]<-extract(rr,reg_cutout,weights=TRUE,fun=mean)
bigtable[1,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_AC_ann_pr_tot
rrp=raster(dat1)
bigtable[1,4]<-mean(extract(rrp,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

par(mfrow=c(2,2))  # two plots side-by-side (rows = 2, columns = 2)
#brk<- c(0,400,800,1200,1600)
#arg <- list(at=c(200,600,1000,1400),labels=c("200","600","1000","1400"))
#plot(mask(rr,reg_cutout),main="Obs Annual Precip")
plot(crop(rr,reg_cutout),main="Obs Annual Precip")
#plot(crop(rr,reg_cutout),col=terrain.colors(3),breaks=brk,axis.args=arg)
plot(reg_cutout,add=TRUE)

plot(crop(rrp,reg_cutout),main="Change in Ens Avg Annual Precip")
#plot(mask(rrp,reg_cutout),main="Change in Ens Avg Annual Precip")
plot(reg_cutout,add=TRUE)

dat1$z=CMIN_ann_pr_tot
rr=raster(dat1)
plot(crop(rr,reg_cutout),main="Minimum Composite")
plot(reg_cutout,add=TRUE)

dat1$z=CMAX_ann_pr_tot
rr=raster(dat1)
plot(crop(rr,reg_cutout),main="Maximum Composite")
plot(reg_cutout,add=TRUE)

dat1$z<- apply(Bl_ann_pr_tot,c(1,2),mean)
rr=raster(dat1)
bigtable[1,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(Pr_ann_pr_tot,c(1,2),mean)
rr=raster(dat1)
bigtable[1,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_ann_pr_tot
rr=raster(dat1)
bigtable[1,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_ann_pr_tot,c(1,2),mean)
rr=raster(dat1)
bigtable[1,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[1,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

#bigtable[1,6]<-mean(apply(AC_ann_tot_pr,c(1,2),function(x){mean(x)-1.96*sd(x)/sqrt(length(x))}))
#bigtable[1,7]<-mean(apply(AC_ann_tot_pr,c(1,2),function(x){mean(x)+1.96*sd(x)/sqrt(length(x))}))
}

#--- avg daily
if (Dailyprecip ==1) {
dat1o$z=Ob_avg_daily_pr
rr=raster(dat1o)
bigtable[2,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

par(mfrow=c(2,2))  # two plots side-by-side (rows = 1, columns = 2)
plot(crop(rr,reg_cutout),main="Obs Avg Daily Precip")
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_avg_daily_pr
rr=raster(dat1)
bigtable[2,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main="Change in Ens Avg Daily Precip")
plot(reg_cutout,add=TRUE)

dat1$z=CMIN_avg_daily_pr
rr=raster(dat1)
plot(crop(rr,reg_cutout),main="Minimum Composite")
plot(reg_cutout,add=TRUE)

dat1$z=CMAX_avg_daily_pr
rr=raster(dat1)
plot(crop(rr,reg_cutout),main="Maximum Composite")
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_avg_daily_pr,c(1,2),mean)
rr=raster(dat1)
bigtable[2,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_avg_daily_pr,c(1,2),mean)
rr=raster(dat1)
bigtable[2,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_avg_daily_pr
rr=raster(dat1)
bigtable[2,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_avg_daily_pr,c(1,2),mean)
rr=raster(dat1)
bigtable[2,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[2,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
#bigtable[2,6]<-mean(apply(AC_avg_daily_pr,c(1,2),function(x){mean(x)-1.96*sd(x)/sqrt(length(x))}))
#bigtable[2,7]<-mean(apply(AC_avg_daily_pr,c(1,2),function(x){mean(x)+1.96*sd(x)/sqrt(length(x))}))
}

#-- monthly
if (Monprecip ==1) {
inmonth=2
#par(mfrow=c(12,2))  
for (imtab in 1:12) {
dat1o$z=Ob_mon_avg_pr[,,imtab]
rr=raster(dat1o)
inmonth=inmonth+1
bigtable[inmonth,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

par(mfrow=c(2,2))  # two plots side-by-side (rows = 1, columns = 2)
plot(crop(rr,reg_cutout),main=c(paste("Obs Monthly Precip:",imtab,sep="")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_mon_avg_pr[,,imtab]
rr=raster(dat1)
bigtable[inmonth,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Change in Monthly Precip",imtab,sep="")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_mon_avg_pr[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_mon_avg_pr[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-ES_PC_mon_avg_pr[,,imtab]
rr=raster(dat1)
bigtable[inmonth,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_mon_avg_pr[,,imtab,],c(1,2),mean)
rr=raster(dat1)
bigtable[inmonth,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[inmonth,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

#bigtable[inmonth,6]<-mean(apply(AC_mon_avg_pr,c(1,2),function(x){mean(x)-1.96*sd(x)/sqrt(length(x))}))
#bigtable[inmonth,7]<-mean(apply(AC_mon_avg_pr,c(1,2),function(x){mean(x)+1.96*sd(x)/sqrt(length(x))}))
}
}   # come out with index of 15

#------------ return periods
if (returnper == 1){
# return periods
par(mfrow=c(Num_yrs,2))
for (retyrs in 1:Num_yrs) {
dat1o$z=rpBase[,,retyrs]
cntyrs = retyrs + 14
rr=raster(dat1o)
bigtable[cntyrs,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

#par(mfrow=c(1,2))  # two plots side-by-side (rows = 1, columns = 2)
plot(crop(rr,reg_cutout),main=c(paste("Obs Return Periods:",Ret_yrs[retyrs],sep="")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_retperiods[,,retyrs]
rr=raster(dat1)
bigtable[cntyrs,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Change in Return Periods:",Ret_yrs[retyrs],sep="")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(rpBase_sim[,,retyrs,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(rpFut_sim[,,retyrs,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

#dat1$z<-ES_PC_
#rr=raster(dat1)
#bigtable[cntyrs,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
bigtable[cntyrs,5]<-0

dat1$z=apply(rpFut1_ch[,,retyrs,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntyrs,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntyrs,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

#bigtable[cntyrs,6]<-mean(apply(rpFut1_ch[,,retyrs,],c(1,2),function(x){mean(x)-1.96*sd(x)/sqrt(length(x))}))
#bigtable[cntyrs,7]<-mean(apply(rpFut1_ch[,,retyrs,],c(1,2),function(x){mean(x)+1.96*sd(x)/sqrt(length(x))}))

}
}  # return period 

#----  plot precip thresholds
if (thresprecip ==1) {
par(mfrow=c(Num_thres,2))
cntloc = Num_yrs+14
for (ithres in 1:Num_thre) {
cntloc = cntloc + 1
dat1o$z=Ob_thres_pr[,,ithres]
rr=raster(dat1o)
bigtable[cntloc,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Obs Days below threshold:",Thres_val[ithres],sep="")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_threvalue[,,ithres]
rr=raster(dat1)
bigtable[cntloc,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Proj Days below threshold:",Thres_val[ithres],sep="")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_thres_pr[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_thres_pr[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_threvalue[,,ithres]
rr=raster(dat1)
bigtable[cntloc,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_thres_pr[,,ithres,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntloc,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
#bigtable[inmonth,6]<-mean(apply(AC_thres_pr[,,ithres,],c(1,2),function(x){mean(x)-1.96*sd(x)/sqrt(length(x))}))
#bigtable[inmonth,7]<-mean(apply(AC_thres_pr[,,ithres,],c(1,2),function(x){mean(x)+1.96*sd(x)/sqrt(length(x))}))

}

cntloc = cntloc + 1
dat1o$z=Ob_thres_pr[,,ithres+1]
rr=raster(dat1o)
bigtable[cntloc,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Obs Days above threshold:",Thres_val[ithres],sep="")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_threvalue[,,ithres+1]
rr=raster(dat1)
bigtable[cntloc,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

plot(crop(rr,reg_cutout),main=c(paste("Proj Days above threshold:",Thres_val[ithres],sep="")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_thres_pr[,,ithres+1,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_thres_pr[,,ithres+1,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_threvalue[,,ithres+1]
rr=raster(dat1)
bigtable[cntloc,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_thres_pr[,,ithres+1,],c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntloc,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

#bigtable[cntloc,6]<-mean(apply(AC_thres_pr[,,ithres+1,],c(1,2),function(x){mean(x)-1.96*sd(x)/sqrt(length(x))}))
#bigtable[cntloc,7]<-mean(apply(AC_thres_pr[,,ithres+1,],c(1,2),function(x){mean(x)+1.96*sd(x)/sqrt(length(x))}))
}

#--- 5 day maximum
if (max5day ==1 ){
cntloc = cntloc + 1
dat1o$z=Ob_5day
rr=raster(dat1o)
bigtable[cntloc,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Obs Maximum 5 Day Precip")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_5day
rr=raster(dat1)
bigtable[cntloc,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

plot(crop(rr,reg_cutout),main=c(paste("Proj Change in Maximum 5 Day Precip")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_5day,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_5day,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_5day
rr=raster(dat1)
bigtable[cntloc,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_5day,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntloc,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

#bigtable[cntloc,6]<-mean(apply(AC_5day,c(1,2),function(x){mean(x)-1.96*sd(x)/sqrt(length(x))}))
#bigtable[cntloc,7]<-mean(apply(AC_5day,c(1,2),function(x){mean(x)+1.96*sd(x)/sqrt(length(x))}))
}

# consecutive dry days
if (maxCDD == 1) {
cntloc = cntloc + 1
dat1o$z=Ob_consdd
rr=raster(dat1o)
bigtable[cntloc,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Obs Maximum Consecutive Dry Days Per Year")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_consdd
rr=raster(dat1)
bigtable[cntloc,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

plot(crop(rr,reg_cutout),main=c(paste("Proj Change in Cons Dry Days Per Year")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_consdd,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_consdd,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_consdd
rr=raster(dat1)
bigtable[cntloc,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_consdd,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntloc,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

#bigtable[cntloc,6]<-mean(apply(AC_consdd,c(1,2),function(x){mean(x)-1.96*sd(x)/sqrt(length(x))}))
#bigtable[cntloc,7]<-mean(apply(AC_consdd,c(1,2),function(x){mean(x)+1.96*sd(x)/sqrt(length(x))}))
}

if (dailyindex==1) {
cntloc = cntloc + 1
dat1o$z=Ob_sdii
rr=raster(dat1o)
bigtable[cntloc,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Obs Simply Daily Intensity Index")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_sdii
rr=raster(dat1)
bigtable[cntloc,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

plot(crop(rr,reg_cutout),main=c(paste("Proj Change in Simple Daily Intensity Index")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_sdii,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_sdii,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_sdii
rr=raster(dat1)
bigtable[cntloc,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_sdii,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntloc,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

#bigtable[cntloc,6]<-mean(apply(AC_sdii,c(1,2),function(x){mean(x)-1.96*sd(x)/sqrt(length(x))}))
#bigtable[cntloc,7]<-mean(apply(AC_sdii,c(1,2),function(x){mean(x)+1.96*sd(x)/sqrt(length(x))}))
}

# ALMOND INDICATORS
if (almonindex ==1){
# indicator 1
cntloc = cntloc + 1
dat1o$z=Ob_al1
rr=raster(dat1o)
bigtable[cntloc,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Almond: % of years between 960 and 1270mm")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_al1
rr=raster(dat1)
bigtable[cntloc,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

plot(crop(rr,reg_cutout),main=c(paste("Proj Change in % of years between 960 and 1270mm")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_al1,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_al1,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_al1
rr=raster(dat1)
bigtable[cntloc,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_al1,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntloc,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

# -- second indicator

cntloc = cntloc + 1
dat1o$z=Ob_al2
rr=raster(dat1o)
bigtable[cntloc,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Almond: % of yrs Jan is to wet")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_al2
rr=raster(dat1)
bigtable[cntloc,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

plot(crop(rr,reg_cutout),main=c(paste("Proj Change in % of yrs Jan is to wet")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_al2,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_al2,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_al2
rr=raster(dat1)
bigtable[cntloc,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_al2,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntloc,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

# -- third indicator

cntloc = cntloc + 1
dat1o$z=Ob_al3
rr=raster(dat1o)
bigtable[cntloc,1]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))
plot(crop(rr,reg_cutout),main=c(paste("Almond: Feb days of > trace rain")))
plot(reg_cutout,add=TRUE)

dat1$z=ES_AC_al3
rr=raster(dat1)
bigtable[cntloc,4]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

plot(crop(rr,reg_cutout),main=c(paste("Proj Change in Feb days of > trace rain")))
plot(reg_cutout,add=TRUE)

dat1$z<-apply(Bl_al3,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,2]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z<-apply(Pr_al3,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,3]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=ES_PC_al3
rr=raster(dat1)
bigtable[cntloc,5]<-mean(extract(rr,reg_cutout,weights=TRUE,fun=mean,na.rm=TRUE))

dat1$z=apply(AC_al3,c(1,2),mean)
rr=raster(dat1)
bigtable[cntloc,6]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)-1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))
bigtable[cntloc,7]<-mean(extract(rr,reg_cutout,function(x){mean(x,na.rm=TRUE)+1.96*sd(x,na.rm=TRUE)/sqrt(length(x))}))

}  # almond indicator

dev.off()

# table to console
write.table(format(bigtable, justify="right"),  row.names=rnames, col.names=cnames, quote=F)
# output to csv
out_tab_fil<-c(paste(sep="",output_dir,US_region,"_",Scen,".csv"))
write.table(format(bigtable, justify="right"), file=out_tab_fil, row.names=rnames, col.names=cnames, quote=F,sep=",")

