#  Temperature Almond Indicators in California
#  1/2016
#

ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

#---------------------- TMIN INDICATORS
# avg monthly Tmin for Feb < 4C
if (almonT == 1){
  dum <- array(0,dim=c(lon_num,lat_num))
  dum2 <- array(0,dim=c(lon_num,lat_num))
  dum3 <- array(0,dim=c(ob_lon_num,ob_lat_num))
  dum4 <- array(0,dim=c(ob_lon_num,ob_lat_num))

  im = 2  # month is Feb
  inS = 0
  inE = 0
  ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

  # calculate total monthly precipitation for each year in the period
  for (iyr in 1:tot_yrs) {

    # set start(inS) and end(inE) indices to the right array index
    # depending if it's a leap year (ind_yrs)
    if (iyr == 1) {
    ifelse(ind_yrs[iyr]>0,inS<-month_st_lp[im],inS<-month_st[im])     
    ifelse(ind_yrs[iyr]>0,inE<-inS+days_monlp[im]-1,inE<-inS+days_mon[im]-1) }

    # if model simulation then model matrix, if obs then use obs matrix
    ifelse(flag_obs == 0,dum <-  apply(Temp_val[,,inS:inE],c(1,2),mean),
     dum3 <-  apply(Ob_temp[,,inS:inE],c(1,2),mean))

     ifelse(flag_obs==0,dum2<-apply(dum,c(1,2),function(x){sum(x<39.2)}) + dum2,
     dum4<-apply(dum3,c(1,2),function(x){sum(x<39.2)}) + dum4)

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

print("finish almon 1")
}
# Minimum Temperature Thresholds - set for the blossom period
#  Temp_val <-- Pr_Tmin_val, etc.
#
if (almonT == 2){
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start Tmin Blossom thresholds")
  
#for (im in 1:12) {
  im=2
  inS = 0
  inE = 0

  ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

  # calculate total monthly precipitation for each year in the period
  for (iyr in 1:tot_yrs) {

    # set start(inS) and end(inE) indices to the right array index
    # depending if it's a leap year (ind_yrs)
    if (iyr == 1) {
    ifelse(ind_yrs[iyr]>0,inS<-month_st_lp[im],inS<-month_st[im])     
    ifelse(ind_yrs[iyr]>0,inE<-inS+days_monlp[im]+days_monlp[im+1]-1,inE<-inS+days_mon[im]+days_mon[im+1]-1) 
    }
   # sum up the number of days at or below the threshold and average over tot_yrs
# 
    if(flag_obs==0) {
      for (ithres in 1:Al2_Num_thre) {
       mod_dum_t[,,ithres]<-apply(Temp_val[,,inS:inE],c(1,2),function(x){sum(x<Al2_Thres_val[ithres])} )+ mod_dum_t[,,ithres]
      }  # for loop
    }   # flag_obs ==0

   if (flag_obs==1) {
      for (ithres in 1:Al2_Num_thre) {
       Ob_dum_t[,,ithres]<-apply(Ob_temp[,,inS:inE],c(1,2),function(x){sum(x<Al2_Thres_val[ithres])} )+Ob_dum_t[,,ithres]
       }  # for loop
    } # flag_obs == 1  
#   
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
} #years
#
print("finish Tmin Blossom thresholds")
} # almon 2

# Minimum Temperature Thresholds - set for the nut period
#  Temp_val <-- Pr_Tmin_val, etc.
#
if (almonT == 3){
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start Tmin Blossom thresholds")
  
#for (im in 1:12) {
  inS = 0
  inE = 0

  im=4
  ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

  # calculate total monthly precipitation for each year in the period
  for (iyr in 1:tot_yrs) {

    # set start(inS) and end(inE) indices to the right array index
    # depending if it's a leap year (ind_yrs)
    if (iyr == 1) {
 #   ifelse(ind_yrs[iyr]>0,inS<-month_st_lp[im],inS<-month_st[im])     
 #   ifelse(ind_yrs[iyr]>0,inE<-inS+days_monlp[im]-1,inE<-inS+days_mon[im]-1) 
    ifelse(ind_yrs[iyr]>0,inS<-month_st_lp[im],inS<-month_st[im])     
    ifelse(ind_yrs[iyr]>0,inE<-inS+152-1,inE<-inS+152-1) 

    }
   # sum up the number of days at or below the threshold and average over tot_yrs
#   if ((im == 4) | (im==5) | (im==6) | (im==7) | (im==8) ) {
   
   if(flag_obs==0) {
      for (ithres in 1:Al3_Num_thre) {
       mod_dum2_t[,,ithres]<-apply(Temp_val[,,inS:inE],c(1,2),function(x){sum(x<Al3_Thres_val[ithres])} )+ mod_dum2_t[,,ithres]
      }  # for loop
    }   # flag_obs ==0

   if (flag_obs==1) {
      for (ithres in 1:Al3_Num_thre) {
       Ob_dum2_t[,,ithres]<-apply(Ob_temp[,,inS:inE],c(1,2),function(x){sum(x<Al3_Thres_val[ithres])} )+Ob_dum2_t[,,ithres]
       }  # for loop
    } # flag_obs == 1  
#    }  # im = bloom months
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
}# years
#} #months
print("finish Tmin Nut thresholds")
} # almon 3

# Minimum Temperature below Tmin for dormant stage
#  Temp_val <-- Pr_Tmin_val, etc.
#
if (almonT == 4){
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start Tmin Blossom thresholds")
  
 Ob_dum <- array(0,dim=c(lon_num,lat_num))
 dum <- array(0,dim=c(lon_num,lat_num))
  dum2 <- array(0,dim=c(lon_num,lat_num))

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
   # sum up the number of days at or below the threshold and average over tot_yrs
   if ((im ==11) | (im==12) | (im==1) ) {
   
   if(flag_obs==0) {
       dum<-apply(Temp_val[,,inS:inE],c(1,2),function(x){sum(x<29)} )+ dum
    }   # flag_obs ==0

   if (flag_obs==1) {
       Ob_dum<-apply(Ob_temp[,,inS:inE],c(1,2),function(x){sum(x<29)} )+Ob_dum
    } # flag_obs == 1  
    }  # im = dormant months
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
} #years
} #months
print("finish Tmin Dormant")
} # almon 4
# Minimum Temperature below Tmin for dormant stage
#  Temp_val <-- Pr_Tmin_val, etc.
#
if (almonT == 5){
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start Tmin Blossom thresholds")
  
 Ob_dum <- array(0,dim=c(ob_lon_num,ob_lat_num))
 dum <- array(0,dim=c(lon_num,lat_num))

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
   # sum up the number of days at or below the threshold and average over tot_yrs
   if ((im ==11) | (im==12) | (im==1) ) {
   
   if(flag_obs==0) {
       dum<-apply(Temp_val[,,inS:inE],c(1,2),function(x){sum(x<45)} )+ dum
    }   # flag_obs ==0

   if (flag_obs==1) {
       Ob_dum<-apply(Ob_temp[,,inS:inE],c(1,2),function(x){sum(x<45)} )+Ob_dum
    } # flag_obs == 1  
    }  # im = dormant months
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
} #years
}#months
print("finish Tmin Dormant")
} # almon 5

#------------------ TMAX INDICATORS

# Maximum Temperature below 45 for dormant stage
#  Temp_val <-- Pr_Tmin_val, etc.
#
if (almonT == 6){
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start Tmin Blossom thresholds")

 Ob_dum <- array(0,dim=c(ob_lon_num,ob_lat_num))
 mod_dum <-array(0,dim=c(lon_num,lat_num))  # store data
  
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
   # sum up the number of days at or below the threshold and average over tot_yrs
   if ((im ==11) | (im==12) | (im==1) ) {
   
   if(flag_obs==0) {
       mod_dum<-apply(Temp_val[,,inS:inE],c(1,2),function(x){sum(x<45)} )+ mod_dum
    }   # flag_obs ==0

   if (flag_obs==1) {
       Ob_dum<-apply(Ob_temp[,,inS:inE],c(1,2),function(x){sum(x<45)} )+Ob_dum
    } # flag_obs == 1  
    }  # im = dormant months
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
} #years
}#months
print("finish Tmax Dormant")
} # almon 6

# Max Temperature Thresholds - set for the blossom period
#  Temp_val <-- Pr_Tmax_val, etc.
#
if (almonT == 7){
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start Tmin Blossom thresholds")
  
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
   # sum up the number of days at or below the threshold and average over tot_yrs
   if ((im == 2) | (im==3)) {
   
   if(flag_obs==0) {
      for (ithres in 1:Al7_Num_thre) {
       mod_dum3_t[,,ithres]<-apply(Temp_val[,,inE:inS],c(1,2),function(x){sum(x>Al7_Thres_val[ithres])} )+mod_dum3_t[,,ithres]
      }  # for loop
    }   # flag_obs ==0

   if (flag_obs==1) {
      for (ithres in 1:Al7_Num_thre) {
       Ob_dum3_t[,,ithres]<-apply(Ob_temp[,,inE:inS],c(1,2),function(x){sum(x>Al7_Thres_val[ithres])} )+Ob_dum3_t[,,ithres]
       }  # for loop
    } # flag_obs == 1  
    }  # im = bloom months
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
} #years
}#months
print("finish Tmax Blossom thresholds")
} # almon 7

# Max Temperature Thresholds - set for the nut period
#  Temp_val <-- Pr_Tmin_val, etc.
#
if (almonT == 8){
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start Tmin Blossom thresholds")
 Ob_dum <- array(0,dim=c(ob_lon_num,ob_lat_num))
 mod_dum <- array(0,dim=c(lon_num,lat_num))
  
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
   # sum up the number of days at or below the threshold and average over tot_yrs
   if ((im == 3) | (im==4) ) {
   
   if(flag_obs==0) {
        mod_dum<-apply(Temp_val[,,inE:inS],c(1,2),function(x){sum(x>75)} )+ mod_dum
     }   # flag_obs ==0

   if (flag_obs==1) {
      Ob_dum<-apply(Ob_temp[,,inE:inS],c(1,2),function(x){sum(x>75)} )+Ob_dum
    } # flag_obs == 1  
    }  # im = bloom months
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
} #years
}#months
print("finish Tmin Nut thresholds")
} # almon 8

#------------------------------------------- Tavg

# Avg Temperature for dormant
#  Temp_val <-- Pr_Tavg_val, etc.
#
if (almonT == 9){
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start Tavg dormant")
 Ob_dum <- array(0,dim=c(ob_lon_num,ob_lat_num))
 dum <- array(0,dim=c(lon_num,lat_num))
  
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
   # sum up the number of days at or below the threshold and average over tot_yrs
   if ((im == 11) | (im==12) | (im==1) ) {
   
   if(flag_obs==0) {
        dum<-apply(Temp_val[,,inS:inE],c(1,2),function(x){sum(x<45)} )+ dum
    }   # flag_obs ==0

   if (flag_obs==1) {
       Ob_dum<-apply(Ob_temp[,,inS:inE],c(1,2),function(x){sum(x<45)} )+Ob_dum
    } # flag_obs == 1  
    }  # im = bloom months
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
} #years
}#months
print("finish Tavg dormant")
} # almon 9
#---------
# Avg Temperature for blossom
#  Temp_val <-- Pr_Tavg_val, etc.
#
if (almonT == 10){
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

print("start Tavg blossom")
 Ob_dum <- array(0,dim=c(ob_lon_num,ob_lat_num))
 dum <- array(0,dim=c(lon_num,lat_num))
  
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
   # sum up the number of days at or below the threshold and average over tot_yrs
   if ((im == 2) | (im==3) ) {
   
   if(flag_obs==0) {
           dum<-apply(Temp_val[,,inS:inE],c(1,2),function(x){sum(x>=55)} )+ dum
    }   # flag_obs ==0

   if (flag_obs==1) {
        Ob_dum<-apply(Ob_temp[,,inS:inE],c(1,2),function(x){sum(x>=55)} )+Ob_dum
    } # flag_obs == 1  
    }  # im = bloom months
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
} #years
}#months
print("finish Tavg blossom")
} # almon 10

