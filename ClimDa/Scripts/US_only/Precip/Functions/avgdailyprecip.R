## Average of Daily Precip 

if (flag_obs == 0) {
# seperate out the precip that is above trace 

#avg_daily_pr[,,i]<-apply(pr_val[,,1:mat_end,i],c(1,2),mean)/sum(pr_val[,,1:mat_end,i]>0.99)  # apply the mean function to col/rows (c(1,2)) for the time period
avg_daily_pr[,,i]<-apply(pr_val[,,1:mat_end,i],c(1,2),mean)
print("finish avg daily precip") }

if (flag_obs == 1) {
Ob_avg_daily_pr<-apply(Ob_pr_val[,,1:mat_end],c(1,2),mean)  # apply the mean function to col/rows (c(1,2)) for the time period
print("finish avg daily precip") }

 
