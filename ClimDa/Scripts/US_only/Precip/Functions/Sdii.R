## Simple Daily Intensity Index
# The simple precipitation intensity index is computed by taking the sum of precipitation 
# and dividing that by the number of wet days in the period. This gives the mean precipitation in wet days. 

ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start simple daily intensity index")
         
 if (flag_obs ==0) {
   dummy_dim <- dim(pr_val)
   iend <- dummy_dim[3] }

 if (flag_obs==1) {
   dummy_dim <-dim(Ob_pr_val)
   iend <- dummy_dim[3] }


for (ilat2 in 1:lat_num) {
for (ilon2 in 1:lon_num) {

# count the number of days in the time period where precip > 1mm
 ifelse(flag_obs==0,dumct<-length(which(pr_val[ilon2,ilat2,1:iend,i]>1)),
     dumct<-length(which(Ob_pr_val[ilon2,ilat2,1:iend]>1)) ) 

# get total precip for the period(assuming <1mm does not really contribute to total annual precip
 ifelse(flag_obs==0,totprecip<-sum(pr_val[ilon2,ilat2,1:iend,i]),
    totprecip<-sum(Ob_pr_val[ilon2,ilat2,1:iend]))

# divide total precip for period/ number of days with rain 
 ifelse(flag_obs==0,pr_sdii_dum[ilon2,ilat2,i]<-totprecip/dumct,
    Ob_sdii[ilon2,ilat2]<-totprecip/dumct)

} # ilon2
} # ilat2
print("finish simple daily intensity index")
