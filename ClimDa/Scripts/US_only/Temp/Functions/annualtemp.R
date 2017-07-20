## Annual Average Temperatures
# just sum up entire matrix & divide by years

 if (flag_obs ==0) {
   dummy_dim <- dim(Temp_val)
   iend <- dummy_dim[3] }

 if (flag_obs==1) {
   dummy_dim <-dim(Ob_tp_val)
   iend <- dummy_dim[3] }

 ifelse(flag_obs==0,ann_temp[,,i]<-apply(Temp_val[,,1:iend,i],c(1,2),mean),
     Ob_ann_temp<-apply(Ob_temp[,,1:iend],c(1,2),mean) ) 

print("finish annual temp avg")
