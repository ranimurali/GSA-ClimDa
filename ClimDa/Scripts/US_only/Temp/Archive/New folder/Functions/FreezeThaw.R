# calculate freeze thaw
#-----

ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

 if (flag_obs ==0) {
   dummy_dim <- dim(Temp_val)
   iend <- dummy_dim[3] }

 if (flag_obs==1) {
   dummy_dim <-dim(Ob_tp_val)
   iend <- dummy_dim[3] }

print("start freeze thaw")
fredum<-array(0,dim=c(lon_num,lat_num))
ifelse (Units==1,trig<-0,trig<-32)  # if units are celius, freeze/thaw is 0

 if(flag_obs==0) {
   for (lonlon in 1:lon_num) {
   for (latlat in 1:lat_num) {
   for (jj in 1:iend){ 
# projected simulations
   if (tag==1) {
   if ( (sum(is.na(Pr_Tmin_val[lonlon,latlat,jj,i]))==0) &&
       (sum(is.na(Pr_Tmax_val[lonlon,latlat,jj,i]))==0)   )    {
   if ((Pr_Tmin_val[lonlon,latlat,jj,i]<trig)&&(Pr_Tmax_val[lonlon,latlat,jj,i]>trig)) {
    fredum[lonlon,latlat]<-fredum[lonlon,latlat] + 1}}
   }  # if tag 1
# baseline simulations  
   if (tag==0) {
   if ( (sum(is.na(Tmin_val[lonlon,latlat,jj,i]))==0) &&
       (sum(is.na(Tmax_val[lonlon,latlat,jj,i]))==0)    )    {
   if ((Tmin_val[lonlon,latlat,jj,i]<trig)&&(Tmax_val[lonlon,latlat,jj,i]>trig)) {
    fredum[lonlon,latlat]<-fredum[lonlon,latlat] + 1}}
    } # if Tag 0
    }   #jj
    } # latlat
    } # lonlon
fredum<-fredum/tot_yrs
}  # flag 0

 if (flag_obs==1) {
   for (lonlon in 1:ob_lon_num) {
   for (latlat in 1:ob_lat_num) {
   for (jj in 1:iend){
   if (  (sum(is.na(Ob_tmin[lonlon,latlat,jj]))==0) &&
       (sum(is.na(Ob_tmax[lonlon,latlat,jj]))==0)    )    {
    if ((Ob_tmin[lonlon,latlat,jj]<trig)&&(Ob_tmax[lonlon,latlat,jj]>trig)) {
      fredum[lonlon,latlat]<-fredum[lonlon,latlat] + 1}
    } # if number
    }   # jj
    } # latlat
    } # lonlon
fredum<-fredum/tot_yrs
}  # flag

print("finish freeze thaw")
