#  Open file and read precipitation data for time periods
#print("tag")
#print(tag)
#
if (Modell[i] == "ccsm4") {
 num_file_pr=3
 pr_yr_s<-c("20360101","20460101","20560101")
 pr_yr_f<-c("20451231","20551231","20651231")
 num_file_bs=2
 bs_yr_s<-c("19800101","19900101")
 bs_yr_f<-c("19891231","19991231") }

if (Modell[i]=="inmcm4") {
 num_file_pr=3
 pr_yr_s<- c("20360101","20460101","20560101")
 pr_yr_f<-c("20451231","20551231","20651231")
 num_file_bs=2
 bs_yr_s<-c("19800101","19900101")
 bs_yr_f<-c("19891231","19991231") }

if (Modell[i]=="GFDLCM3") {
 num_file_pr=4
 pr_yr_s<-c("20410101","20460101","20510101","20560101")
 pr_yr_f<-c("20451231","20501231","20551231","20601231")
 num_file_bs=4
 bs_yr_s<-c("19800101","19850101","19900101","19950101")
 bs_yr_f<-c("19841231","19891231","19941231","19991231") }

# has the start date crossed into the model file
# file_flag <- 0 
 mat_start <- 1
 
 # make an index of those year to track whether to include in these calculations - changed to check for leap years
ifelse(tag==0,years_tot<-bl_years_tot,years_tot<-years_tot)

     ind_yrs <-c(rep(0,years_tot))
     iyr_ind <- 1

ifelse(tag>0,num_file<-num_file_pr,num_file<-num_file_bs)

 for (j in 1:num_file) {

 if (tag == 1) {
  fil_dir<-c(paste(sep="","F:\\ClimDA\\US\\",Scen_l,"\\",Modell[i],"\\pr"))}
 if (tag == 0) {
  fil_dir<-c(paste(sep="","F:\\ClimDA\\US\\HistMod\\",Modell[i],"\\pr"))}
 
   # how many years are in this file that will be used?
     # how many years are in the file
     if (tag == 1) {
       yrs_in_file = as.numeric(substr(pr_yr_f[j],1,4)) - as.numeric(substr(pr_yr_s[j],1,4)) + 1 }

     if (tag == 0) {
       yrs_in_file = as.numeric(substr(bs_yr_f[j],1,4)) - as.numeric(substr(bs_yr_s[j],1,4)) + 1 }
    
 #    print("years in file")
 #    print(yrs_in_file)
    
     # make an index of those year to track whether to include in these calculations - changed to check for leap years
     counter<-0
     startyr <-0
     endyr <- 0
     pnt_ind <- 1
     # This for loop finds out: (1) which years in the file are in the projected request; (2) which years are leap years
     for (k in 1:yrs_in_file) {
       # convert starting year in file from "20100101" to "2010" and in numeric form
       countyear <- as.numeric(substr(pr_yr_s[j],1,4))
       
       if ( ((countyear+k-1) >= Proj_yr_s) && ((countyear+k-1) <= Proj_yr_f) ) {
 #        print("hit year")
 #        print(countyear+k-1)

         if (counter == 0) { 
                startyr <- pnt_ind
                counter <- 1 }
        ifelse((countyear+k-1) %in% leap,ind_yrs[iyr_ind] <- 1,ind_yrs[iyr_ind] <- 0)
        iyr_ind <- iyr_ind+1

        }  # if statement
 
      # check for leap years so the added day gets taken into account when pulling the data
#       ifelse(match(countyear+k-1,leap)>0, pnt_ind <- pnt_ind + 366, pnt_ind<-pnt_ind+365)
        if ((countyear+k-1) <= Proj_yr_f) {
        ifelse((countyear+k-1) %in% leap, pnt_ind <- pnt_ind + 366, pnt_ind<-pnt_ind+365)
      
        if (counter == 1) {
          endyr <- pnt_ind-1 }
        }
  } # for k loop
  
#print("finish k loop")
#  need to pull data into model matrix
  if (counter == 1 ) {

  setwd(fil_dir)

  if (tag > 0) {
  fil_open<-c(paste(sep="","regridded_1deg_pr_day_",Modeln[i],"_",Scen_n,"_r1i1p1_",pr_yr_s[j],"-",pr_yr_f[j],".nc"))}

  if (tag == 0) {
  fil_open<-c(paste(sep="","regridded_1deg_pr_day_",Modeln[i],"_historical_r1i1p1_",bs_yr_s[j],"-",bs_yr_f[j],".nc"))}

  print("opening file")
  print(fil_open)

  pr_1 = nc_open(fil_open) 
 
  mat_end <- endyr-startyr + 1 + mat_start -1
  pr_val_whole <-ncvar_get(pr_1,"pr") 
  pr_val_in[,,mat_start:mat_end,i] <- pr_val_whole[,,startyr:endyr]
  mat_start <- mat_end+1 
  setwd("F:\\ClimDA\\Scripts\\US_only\\Precip\\Functions")
  }

#} # if loop

#print("if loop ends")

} # j loops end - reading file by file and getting data

print("j loops ends")
