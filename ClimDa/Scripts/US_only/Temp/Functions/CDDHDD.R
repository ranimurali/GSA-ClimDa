# calculate cooling degree days (CDD) & heating degree days (HDD)
# CDD = Tavg - 18.3oC
# HDD = 18.3oC - Tavg
# sum for each day in the year, average across all years in the time period
#-----

ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)
if(flag_obs==0 && tag==0) {tot_yrs<-bl_years_tot}
if (flag_obs==1) {bl_day_num<-dim(Ob_tavg)[3]}

ifelse (Units==1,DDthe<-18,DDthe<-65)

print("start CDD/HDD")

 if(flag_obs==0) {
  ifelse(tag==0,dend<-bl_day_num,dend<-day_num)
  for (ilat2 in 1:lat_num) {
  for (ilon2 in 1:lon_num) {
  for (iday in 1:dend) {
    ifelse (Temp_val[ilon2,ilat2,iday,i] < DDthe,
         dumPr[ilon2,ilat2,i] <- dumPr[ilon2,ilat2,i] + (DDthe-Temp_val[ilon2,ilat2,iday,i]),
         dumPr2[ilon2,ilat2,i]<-dumPr2[ilon2,ilat2,i] + (Temp_val[ilon2,ilat2,iday,i]-DDthe))
  }
  }
  } 
  dumPr[,,i] <- dumPr[,,i] / tot_yrs
  dumPr2[,,i] <- dumPr2[,,i] / tot_yrs

#    dumPr[,,i]<-apply(Temp_val[,,1:iend,i],c(1,2),function(x){if(x<D) {sum(18-x)}} )/tot_yrs
#   dumPr2[,,i]<-apply(Temp_val[,,1:iend,i],c(1,2),function(x){if(x>18) {sum(x-18)}} )/tot_yrs
  }   

 if (flag_obs==1) {

 for (ilat2 in 1:lat_num) {
  for (ilon2 in 1:lon_num) {
  for (iday in 1:bl_day_num) {
    ifelse (Ob_tavg[ilon2,ilat2,iday] < DDthe,
         Ob_HDD[ilon2,ilat2] <- Ob_HDD[ilon2,ilat2] + (DDthe-Ob_tavg[ilon2,ilat2,iday]),
         Ob_CDD[ilon2,ilat2]<-Ob_CDD[ilon2,ilat2] + (Ob_tavg[ilon2,ilat2,iday]-DDthe))
  }
  }
  } 
  Ob_CDD <- Ob_CDD / tot_yrs
  Ob_HDD <- Ob_HDD / tot_yrs

#   Ob_CDD=apply(dum[,,],c(1,2),function(x){if(x<18) {sum(18-x)}}) /tot_yrs
#   Ob_HDD=apply(dum[,,],c(1,2),function(x){if(x>18) {sum(x-18)}}) /tot_yrs
 }
  
print("finish Tmax thresholds")

