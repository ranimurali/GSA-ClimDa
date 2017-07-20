im=2
  inS = 0
  inE = 0

  ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

  # calculate total monthly precipitation for each year in the period
  for (iyr in 1:tot_yrs) {

    # set start(inS) and end(inE) indices to the right array index
    # depending if it's a leap year (ind_yrs)
    if (iyr == 1) {
    ifelse(ind_yrs[iyr]>0,inS<-month_st_lp[im],inS<-month_st[im])     
    ifelse(ind_yrs[iyr]>0,inE<-inS+days_monlp[im]+days_monlp[im+1]-1,inE<-inS+days_mon[im]+days_mon[im+1]-1) 
    }
   # sum up the number of days at or below the threshold and average over tot_yrs
    if(flag_obs==0) {
      for (ithres in 1:Al2_Num_thre) {
       mod_dum_t[,,ithres]<-apply(Temp_val[,,inS:inE],c(1,2),function(x){sum(x<Al2_Thres_val[ithres])} )+ mod_dum_t[,,ithres]
      }  # for loop
    }   # flag_obs ==0

   if (flag_obs==1) {
      for (ithres in 1:Al2_Num_thre) {
       Ob_dum_t[,,ithres]<-apply(Ob_temp[,,inS:inE],c(1,2),function(x){sum(x<Al2_Thres_val[ithres])} )+Ob_dum_t[,,ithres]
       }  # for loop
    } # flag_obs == 1  
     # to go to the first and last day of the month for the next year
     # the index above has already taken leap year into account for first year
     if (iyr == 1) {
     inS <- inS + 365
     inE <- inE + 365     
     }
     else {
     inS <- inS + 365 + ind_yrs[iyr]
     inE <- inE + 365 + ind_yrs[iyr]  
     }
} #years

print("finish Tmin Blossom thresholds")
#} # almon 2


