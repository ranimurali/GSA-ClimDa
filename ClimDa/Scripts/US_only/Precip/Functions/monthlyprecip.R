## Monthly Precip Average
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
    ifelse(flag_obs == 0,dum <-  apply(pr_val[,,inS:inE,i],c(1,2),sum),
     dum <-  apply(Ob_pr_val[,,inS:inE],c(1,2),sum))

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
  ifelse(flag_obs==0,mon_avg_pr[,,im,i] <- dum2/years_tot,
    Ob_mon_avg_pr[,,im]<-dum2/bl_years_tot)
 
# plot month by month in pdf
#  name_file<-c(paste(sep="","F:\\Output\\Test1\\Obs_",im,".pdf"))
#  pdf(name_file,width=7,height=5)
#  title_pl<-c(paste(sep="","Average Observed Monthly Rainfall (mm) for ",im,
#    Base_yr_s," to ",Base_yr_f))
#  filled.contour(y=seq(25.5,52.5,by=1),x=seq(235.5,292.5,by=1),
#       Ob_mon_avg_pr[,,im],main=title_pl)
#  dev.off()

 }  # for months



print("finish monthly precip")

