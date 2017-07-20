# Average max number of consecutive dry days per year
# defined as below 1mm

print("start consecutive threshold days")

# get the number of years
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

# each grid cell/ each year find max thres & then average for time period

for (ithres in 1:Num_thre){
for (ilat2 in 1:lat_num) {
for (ilon2 in 1:lon_num) {

indexDY=0
stct=0
daysinyr=0
condd = array(0,dim=c(tot_yrs))

for (yrc in 1:tot_yrs) {
  daysinyr = 365+ind_yrs[yrc]
  stct = 0
  # for through "day by day"
  for (dbd in 1:daysinyr) {
   indexDY = indexDY + 1  # index in array
  # 
# Observed 
   if ((flag_obs==1) && (sum(is.na(Ob_tmax[ilon2,ilat2,indexDY]))==0) && (stct>=1)) {

      if (Ob_tmax[ilon2,ilat2,indexDY] >=Thres_val[ithres]  ) {
       stct=stct+1}

      if (Ob_tmax[ilon2,ilat2,indexDY] < Thres_val[ithres] ) {
        if(stct>condd[yrc]) {
            condd[yrc]=stct
            stct=0} 
          if(stct<condd[yrc]){
            stct=0} }
    }

    if ((flag_obs==1) && (sum(is.na(Ob_tmax[ilon2,ilat2,indexDY]))==0) &&
          (Ob_tmax[ilon2,ilat2,indexDY] >=Thres_val[ithres] ) && (stct==0)) {
    stct=1   }

# Model Simulations
   if ((flag_obs==0) && (sum(is.na(Tmax_val[ilon2,ilat2,indexDY,i]))==0) && (stct>=1)) {

      if (Tmax_val[ilon2,ilat2,indexDY,i] >=Thres_val[ithres] ) {
         stct=stct+1}

      if (Tmax_val[ilon2,ilat2,indexDY,i] <Thres_val[ithres] ) {
        if(stct>condd[yrc]) {
            condd[yrc]=stct
            stct=0}
        if(stct<condd[yrc]){
            stct=0} }
    }

   if ((flag_obs==0) && (sum(is.na(Tmax_val[ilon2,ilat2,indexDY,i]))==0) &&
       (Tmax_val[ilon2,ilat2,indexDY,i] >=Thres_val[ithres] ) && (stct==0)) {
    stct=1   }

  }  # for dbd

} # for yrc
ifelse(flag_obs==1,Ob_thres_ctd[ilon2,ilat2,ithres]<-mean(condd),
dum_thres[ilon2,ilat2,ithres,i]<-mean(condd))
} # ilon2
} # ilat2
} # thresholds

print("finish consecutive threshold days")

