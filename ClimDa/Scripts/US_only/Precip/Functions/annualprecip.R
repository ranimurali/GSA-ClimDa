## Annual Precip Total Average
# could simplify and just sum up entire matrix & then divide by years
method = 1

# method 1
if (method==1) {
#  print("method 1")
 if (flag_obs ==0) {
   dummy_dim <- dim(pr_val)
   iend <- dummy_dim[3] }

 if (flag_obs==1) {
   dummy_dim <-dim(Ob_pr_val)
   iend <- dummy_dim[3] }
#   print(iend) 

 ifelse(flag_obs==0,dum<-apply(pr_val[,,1:iend,i],c(1,2),sum),
     dum<-apply(Ob_pr_val[,,1:iend],c(1,2),sum) ) 

 ifelse(flag_obs==0,ann_pr_tot[,,i]<-dum/years_tot,
    Ob_ann_pr_tot<-dum/bl_years_tot)

# plotting
#  name_file<-c(paste(sep="","F:\\Output\\Test1\\Obs_Annual_Precip.pdf"))
#  pdf(name_file,width=7,height=5)
# title_pl<-c(paste(sep="","Average Observed Annual Precip (mm)",
#      Base_yr_s," to ",Base_yr_f))
#  filled.contour(y=seq(25.5,52.5,by=1),x=seq(235.5,292.5,by=1),
#       Ob_ann_pr_tot,main=title_pl)
#  dev.off()

}

# method 2
if (method == 2) {
dum2 <- array(0,dim=c(lon_num,lat_num))

for (iyr in 1:years_tot) {
  # for the first year, set up indices at the start of the selected time period
  # and end of year of the first year selected (add 1 if leap year)
  if (iyr == 1) {
    inS <-1
    inE<- 365 + as.numeric(ind_yrs[iyr])
  }
 
   # check if model simulation analysis or obs analysis and sum year
   ifelse(flag_obs==0,dum<-apply(pr_val[,,inS:inE,i],c(1,2),sum),
     dum<-apply(Ob_pr_val[,,inS:inE],c(1,2),sum) ) 

    # add the sum of precip for the year to past year sums
    dum2 <- dum2 + dum  

    inS <- inS + 365 + ind_yrs[iyr]
    inE <- inE + 365 + ind_yrs[iyr]

  }  # for annual precip tot
  # check if model simulation analysis or obs analysis
  ifelse(flag_obs==0,ann_pr_tot[,,i] <- dum2 /years_tot,
   Ob_ann_pr_tot<-dum2/years_tot)

# plotting
#  name_file<-c(paste(sep="","F:\\Output\\Test1\\Obs_Annual_Precip.pdf"))
#  pdf(name_file,width=7,height=5)
#  title_pl<-c(paste(sep="","Average Observed Annual Precip (mm)"))
#  filled.contour(y=seq(25.5,52.5,by=1),x=seq(235.5,292.5,by=1),
#       Ob_ann_pr_tot,main=title_pl)
#  dev.off()

}
print("finish annual precip total avg")
