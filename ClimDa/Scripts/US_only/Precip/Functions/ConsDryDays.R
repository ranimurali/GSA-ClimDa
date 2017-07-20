# Average max number of consecutive dry days per year
# can have different definitions (e.g., as below 1mm) - change dryt accordingly

print("start consecutive dry days")

dryt = 0.254  # dry day threshold of 0.254mm (~ 0.01 inches, used by USGCRP in NCA 2014)

# get the number of years
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

#condd = array(0,dim=c(tot_yrs))

# each grid cell/ each year find max cdd & then average for time period

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
   indexDY = indexDY + 1  # index in precip array
  # 
   if ((flag_obs==1) && (sum(is.na(Ob_pr_val[ilon2,ilat2,indexDY]))==0) && (stct>=1)) {

      if (Ob_pr_val[ilon2,ilat2,indexDY] <dryt ) {
       stct=stct+1}

      if (Ob_pr_val[ilon2,ilat2,indexDY] >dryt ) {
        if(stct>condd[yrc]) {
            condd[yrc]=stct
            stct=0} 
          if(stct<condd[yrc]){
            stct=0} }
    }

    if ((flag_obs==1) && (sum(is.na(Ob_pr_val[ilon2,ilat2,indexDY]))==0) &&
          (Ob_pr_val[ilon2,ilat2,indexDY] <dryt ) && (stct==0)) {
    stct=1   }

   if ((flag_obs==0) && (sum(is.na(pr_val[ilon2,ilat2,indexDY,i]))==0) && (stct>=1)) {

      if (pr_val[ilon2,ilat2,indexDY,i] <dryt ) {
         stct=stct+1}

      if (pr_val[ilon2,ilat2,indexDY,i] >dryt ) {
        if(stct>condd[yrc]) {
            condd[yrc]=stct
            stct=0}
        if(stct<condd[yrc]){
            stct=0} }
    }

   if ((flag_obs==0) && (sum(is.na(pr_val[ilon2,ilat2,indexDY,i]))==0) &&
       (pr_val[ilon2,ilat2,indexDY,i] <1 ) && (stct==0)) {
    stct=1   }

  }  # for dbd

} # for yrc
ifelse(flag_obs==1,Ob_consdd[ilon2,ilat2]<-mean(condd),
pr_consdd_dum[ilon2,ilat2,i]<-mean(condd))
} # ilon2
} # ilat2


print("finish consecutive dry days")

