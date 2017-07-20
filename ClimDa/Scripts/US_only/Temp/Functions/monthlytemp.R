## Monthly Temp Average
for (im in 1:12) {
  inS = 0
  inE = 0

  dum2 <- array(0,dim=c(lon_num,lat_num))

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
    ifelse(flag_obs == 0,dum <-  apply(Temp_val[,,inS:inE,i],c(1,2),mean),
     dum <-  apply(Ob_temp[,,inS:inE],c(1,2),mean))

     dum2 <- dum2 + dum

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

  # determine average monthly precipitation for the entire period
  ifelse(flag_obs==0,temp_mon[,,im,i] <- dum2/years_tot,
    Ob_mon_temp[,,im]<-dum2/bl_years_tot)
 
 }  # for months

print("finish monthly analysis")

