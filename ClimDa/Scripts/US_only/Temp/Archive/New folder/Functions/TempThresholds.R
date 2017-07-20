# Temperature Thresholds
#
#

ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start temp thresholds")
  
   # sum up the number of days at or below the threshold and average over tot_yrs
 
   if(flag_obs==0) {
      for (ithres in 1:Num_thre) {
       dum_thres[,,ithres,i]<-apply(Temp_val[,,,i],c(1,2),function(x){sum(x>Thres_val[ithres])} )/tot_yrs
      }  # for loop
    }   

   if (flag_obs==1) {
      for (ithres in 1:Num_thre) {
       Ob_thres_tmax[,,ithres]<-apply(Ob_temp[,,],c(1,2),function(x){sum(x>Thres_val[ithres])} )/tot_yrs
       }  # for loop
    }
  
print("finish Tmax thresholds")

