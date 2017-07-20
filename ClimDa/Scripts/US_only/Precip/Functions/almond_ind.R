##  Almond indicators
#
if(flag_obs==1) {
lon_num=ob_lon_num
lat_num=ob_lat_num}
#
dum2 <- array(0,dim=c(lon_num,lat_num))
dum <-array(0,dim=c(lon_num,lat_num))
#
if (almon==1){
for (iyr in 1:tot_yrs) {
  # for the first year, set up indices at the start of the selected time period
  # and end of year of the first year selected (add 1 if leap year)
  if (iyr == 1) {
    inS <-1
    inE<- 365 + as.numeric(ind_yrs[iyr])
  }
  if (iyr != 1) {
    inS <- inS + 365 + ind_yrs[iyr]
    inE <- inE + 365 + ind_yrs[iyr]
  }
 
   # check if model simulation analysis or obs analysis and sum year
   ifelse(flag_obs==0,dum<-apply(pr_val[,,inS:inE,i],c(1,2),sum),
     dum<-apply(Ob_pr_val[,,inS:inE],c(1,2),sum) ) 

     # count number of years when precip
    for (i_lon in 1:lon_num) {
     for (i_lat in 1:lat_num) {
        if (is.na(dum[i_lon,i_lat])=="FALSE") {
        if ((dum[i_lon,i_lat] > 960) && (dum[i_lon,i_lat] < 1270)) {
           dum2[i_lon,i_lat]<- dum2[i_lon,i_lat]+1}
      }
      }
      }

}  # iyr
} # almon

if (almon==2){
for (im in 1:12) {
  inS = 0
  inE = 0

   ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

  # calculate total monthly precipitation for each year in the period
  for (iyr in 1:tot_yrs) {

    # set start(inS) and end(inE) indices to the right array index
    # depending if it's a leap year (ind_yrs)
    if (iyr == 1) {
    ifelse(ind_yrs[iyr]>0,inS<-month_st_lp[im],inS<-month_st[im])     
    ifelse(ind_yrs[iyr]>0,inE<-inS+days_monlp[im]-1,inE<-inS+days_mon[im]-1) 
    }
    # if model simulation then model matrix, if obs then use obs matrix
    ifelse(flag_obs == 0,dum <-  apply(pr_val[,,inS:inE,i],c(1,2),sum),
     dum <-  apply(Ob_pr_val[,,inS:inE],c(1,2),sum))
# checking if January total precip > 3cm (that's bad for almonds)
     if (im==1) {
       for (i_lon in 1:lon_num) {
       for (i_lat in 1:lat_num) {
        if (is.na(dum[i_lon,i_lat])=="FALSE") {
        if (dum[i_lon,i_lat] > 30) {
           dum2[i_lon,i_lat]<- dum2[i_lon,i_lat]+1}
      }
      }
      }
      }       
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
   }  # for years
 }  # for months
} # almond

if (almon==3){
for (im in 1:12) {
  inS = 0
  inE = 0
  ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

  # calculate total monthly precipitation for each year in the period
  for (iyr in 1:tot_yrs) {

    # set start(inS) and end(inE) indices to the right array index
    # depending if it's a leap year (ind_yrs)
    if (iyr == 1) {
    ifelse(ind_yrs[iyr]>0,inS<-month_st_lp[im],inS<-month_st[im])     
    ifelse(ind_yrs[iyr]>0,inE<-inS+days_monlp[im]-1,inE<-inS+days_mon[im]-1) 
    }
    # how many days in Feb are above trace levels in precip (bees hate precip)
    if (im==2){
      ifelse(flag_obs==1,dum<-apply(Ob_pr_val[,,inS:inE],c(1,2),function(x){sum(x>0.254)}),
       dum <-apply(pr_val[,,inS:inE,i],c(1,2),function(x){sum(x>0.254)})) 
       dum2 <- dum2 + dum }

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
   }  # for years
 }  # for months
}  # for almonds


print("finish 1 almond indicator")
