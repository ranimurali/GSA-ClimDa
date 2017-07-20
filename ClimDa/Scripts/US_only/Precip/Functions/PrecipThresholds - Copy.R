# Precipitation Thresholds
#
#

ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start precip thresholds")
  
   # sum up the number of days at or below the threshold and average over tot_yrs
   sum_days_dum <-array(0,dim=c(lon_num,lat_num))
   if(flag_obs==0) {
      for (ithres in 1:Num_thre) {
       dum_thres[,,ithres,i]<-apply(pr_val[,,,i],c(1,2),function(x){sum(x<=Thres_val[ithres])} )

      if (ithres>1) {
        dum_thres[,,ithres,i]=dum_thres[,,ithres,i]-sum_days_dum }

      sum_days_dum <-sum_days_dum + dum_thres[,,ithres,i]
      }  # for loop
     dum_thres[,,ithres+1,i]<-apply(pr_val[,,,i],c(1,2),function(x){sum(x>Thres_val[ithres])})
     dum_thres=dum_thres/tot_yrs
    }   

   sum_days_dum <-array(0,dim=c(lon_num,lat_num))
   if (flag_obs==1) {
      for (ithres in 1:Num_thre) {
       Ob_thres_pr[,,ithres]<-apply(Ob_pr_val[,,],c(1,2),function(x){sum(x<=Thres_val[ithres])} )

       if (ithres>1) {
        Ob_thres_pr[,,ithres]=Ob_thres_pr[,,ithres]- sum_days_dum }

       sum_days_dum <-sum_days_dum + Ob_thres_pr[,,ithres]
       }  # for loop
      Ob_thres_pr[,,ithres+1]<-apply(Ob_pr_val[,,],c(1,2),function(x){sum(x>Thres_val[ithres])})
      Ob_thres_pr=Ob_thres_pr/tot_yrs
   }
  
print("finish precip thresholds")

