# calculate average diurnal range
#-----

#ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start diurnal range")

 if(flag_obs==0) {
   ifelse(tag==1,Pr_drange[,,i] <-apply(Temp_val[,,,i],c(1,2),mean),
     Bl_drange[,,i] <-apply(Temp_val[,,,i],c(1,2),mean))
  }   

 if (flag_obs==1) {
     Ob_drange<-apply(Ob_di,c(1,2),mean)
 } 
print("finish Diurnal Range")
