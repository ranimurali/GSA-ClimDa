#  Open file and read temperature data for time periods
#
if (Modell[i] == "bcc-csm1-1") {
 num_file_pr=6
 pr_yr_s<-c("20060101","20160101","20260101","20360101","20460101","20560101")
 pr_yr_f<-c("20151231","20251231","20351231","20451231","20561231","20651231")
 num_file_bs=6
 bs_yr_s<-c("19500101","19600101","19700101","19800101","19900101","20000101")
 bs_yr_f<-c("19591231","19691231","19791231","19891231","19991231","20051231")}

if (Modell[i] == "ccsm4") {
 num_file_pr=9
 pr_yr_s<-c("20060101","20160101","20260101","20360101","20460101","20560101","20660101","20760101","20860101")
 pr_yr_f<-c("20151231","20251231","20351231","20451231","20551231","20651231","20751231","20851231","20951231")
 num_file_bs=4
 bs_yr_s<-c("19700101","19800101","19900101","20000101")
 bs_yr_f<-c("19791231","19891231","19991231","20051231") }

if (Modell[i]=="GFDLCM3") {
 num_file_pr=11
 pr_yr_s<-c("20060101","20110101","20160101","20210101","20260101","20310101","20360101","20410101","20460101","20510101","20560101")
 pr_yr_f<-c("20101231","20151231","20201231","20251231","20301231","20351231","20401231","20451231","20501231","20551231","20601231")
 num_file_bs=7
 bs_yr_s<-c("19700101","19750101","19800101","19850101","19900101","19950101","20000101")
 bs_yr_f<-c("19741231","19791231","19841231","19891231","19941231","19991231","20051231") }

if (Modell[i]=="gfdl-esm2g") {
 num_file_pr=8
 pr_yr_s<-c("20060101","20110101","20160101","20210101","20260101","20310101","20360101","20410101")
 pr_yr_f<-c("20101231","20151231","20201231","20251231","20301231","20351231","20401231","20451231")
 num_file_bs=6
 bs_yr_s<-c("19760101","19810101","19860101","19910101","19960101","20010101")
 bs_yr_f<-c("19801231","19851231","19901231","19951231","20001231","20051231")}

# num_file_bs=10
# bs_yr_s<-c("19560101","19610101","19660101","19710101","19760101","19810101","19860101","19910101","19960101","20010101")
# bs_yr_f<-c("19601231","19651231","19701231","19751231","19801231","19851231","19901231","19951231","20001231","20051231")}

if (Modell[i]=="gfdl-esm2m") {
 num_file_pr=8
 pr_yr_s<-c("20060101","20110101","20160101","20210101","20260101","20310101","20360101","20410101")
 pr_yr_f<-c("20101231","20151231","20201231","20251231","20301231","20351231","20401231","20451231")
 num_file_bs=6
 bs_yr_s<-c("19760101","19810101","19860101","19910101","19960101","20010101")
 bs_yr_f<-c("19801231","19851231","19901231","19951231","20001231","20051231")}

if (Modell[i]=="inmcm4") {
 num_file_pr=6
 pr_yr_s<-c("20060101","20160101","20260101","20360101","20460101","20560101")
 pr_yr_f<-c("20151231","20251231","20351231","20451231","20551231","20651231")
 num_file_bs=6
 bs_yr_s<-c("19500101","19600101","19700101","19800101","19900101","20000101")
 bs_yr_f<-c("19591231","19691231","19791231","19891231","19991231","20051231") }

if (Modell[i]=="ipsl-cm5a-lr") {
 num_file_pr=4
 pr_yr_s<-c("20060101","20160101","20260101","20360101")
 pr_yr_f<-c("20151231","20251231","20351231","20451231")
 num_file_bs=3
 bs_yr_s<-c("19800101","19900101","20000101")
 bs_yr_f<-c("19891231","19991231","20051231") }

if (Modell[i]=="ipsl-cm5a-mr") {
 num_file_pr=4
 pr_yr_s<-c("20060101","20160101","20260101","20360101")
 pr_yr_f<-c("20151231","20251231","20351231","20451231")
 num_file_bs=3
 bs_yr_s<-c("19800101","19900101","20000101")
 bs_yr_f<-c("19891231","19991231","20051231") }

if (Modell[i]=="miroc-esm-chem") {
 num_file_pr=4
 pr_yr_s<-c("20060101","20160101","20260101","20360101")
 pr_yr_f<-c("20151231","20251231","20351231","20451231")
 num_file_bs=6
 bs_yr_s<-c("19500101","19600101","19700101","19800101","19900101","20000101")
 bs_yr_f<-c("19591231","19691231","19791231","19891231","19991231","20051231")}

if (Modell[i]=="miroc-esm") {
 num_file_pr=4
 pr_yr_s<-c("20060101","20160101","20260101","20360101")
 pr_yr_f<-c("20151231","20251231","20351231","20451231")
 num_file_bs=4
 bs_yr_s<-c("19700101","19800101","19900101","20000101")
 bs_yr_f<-c("19791231","19891231","19991231","20051231")}

if (Modell[i]=="mri-cgcm3") {
 num_file_pr=3
 pr_yr_s<-c("20060101","20160101","20260101","20360101")
 pr_yr_f<-c("20151231","20251231","20351231","20451231")
 num_file_bs=3
 bs_yr_s<-c("19800101","19900101","20000101")
 bs_yr_f<-c("19891231","19991231","20051231")}


# has the start date crossed into the model file
# file_flag <- 0 
 mat_start <- 1
 
 # make an index of those year to track whether to include in these calculations - changed to check for leap years
ifelse(tag==0,years_tot<-bl_years_tot,years_tot<-years_tot)

     ind_yrs <-c(rep(0,years_tot))
     iyr_ind <- 1

ifelse(tag>0,num_file<-num_file_pr,num_file<-num_file_bs)

 for (j in 1:num_file) {

if (temp=="Tmin") {
 if (tag == 1) {
  fil_dir<-c(paste(sep="",ClimDAloc,"\\US\\",Scen_l,"\\",Modell[i],"\\tmin"))}
 if (tag == 0) {
  fil_dir<-c(paste(sep="",ClimDAloc,"\\US\\HistMod\\",Modell[i],"\\tmin"))}  }
 
if (temp=="Tmax") {
 if (tag == 1) {
  fil_dir<-c(paste(sep="",ClimDAloc,"\\US\\",Scen_l,"\\",Modell[i],"\\tmax"))}
 if (tag == 0) {
  fil_dir<-c(paste(sep="",ClimDAloc,"\\US\\HistMod\\",Modell[i],"\\tmax"))}  }

   # how many years are in this file that will be used?
     # how many years are in the file
     if (tag == 1) {
       yrs_in_file = as.numeric(substr(pr_yr_f[j],1,4)) - as.numeric(substr(pr_yr_s[j],1,4)) + 1 }

     if (tag == 0) {
       yrs_in_file = as.numeric(substr(bs_yr_f[j],1,4)) - as.numeric(substr(bs_yr_s[j],1,4)) + 1 }
    
     # make an index of those year to track whether to include in these calculations - changed to check for leap years
     counter<-0
     startyr <-0
     endyr <- 0
     pnt_ind <- 1
     # This for loop finds out: (1) which years in the file are in the projected request; (2) which years are leap years
     for (k in 1:yrs_in_file) {
       # convert starting year in file from "20100101" to "2010" and in numeric form
      if (tag>0){ 
      countyear <- as.numeric(substr(pr_yr_s[j],1,4))}
      if (tag==0){
      countyear <-as.numeric(substr(bs_yr_s[j],1,4))}
       
	if (tag>0){
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

        } # tag - proj

       if (tag==0){
       if ( ((countyear+k-1) >= Base_yr_s) && ((countyear+k-1) <= Base_yr_f) ) {
         if (counter == 0) { 
                startyr <- pnt_ind
                counter <- 1 }
        ifelse((countyear+k-1) %in% leap,ind_yrs[iyr_ind] <- 1,ind_yrs[iyr_ind] <- 0)
        iyr_ind <- iyr_ind+1

        }  # if statement
 
      # check for leap years so the added day gets taken into account when pulling the data
#       ifelse(match(countyear+k-1,leap)>0, pnt_ind <- pnt_ind + 366, pnt_ind<-pnt_ind+365)
        if ((countyear+k-1) <= Base_yr_f) {
        ifelse((countyear+k-1) %in% leap, pnt_ind <- pnt_ind + 366, pnt_ind<-pnt_ind+365)
      
        if (counter == 1) {
          endyr <- pnt_ind-1 }
        }

        } # tag - baseline

  } # for k loop
  
#print("finish k loop")
#  need to pull data into model matrix
  if (counter == 1 ) {

  setwd(fil_dir)

if (temp=="Tmin") {
  if (tag > 0) {
  fil_open<-c(paste(sep="","regridded_1deg_tasmin_day_",Modeln[i],"_",Scen_n,"_r1i1p1_",pr_yr_s[j],"-",pr_yr_f[j],".nc"))}

  if (tag == 0) {
  fil_open<-c(paste(sep="","regridded_1deg_tasmin_day_",Modeln[i],"_historical_r1i1p1_",bs_yr_s[j],"-",bs_yr_f[j],".nc"))}  }

if (temp=="Tmax") {
  if (tag > 0) {
  fil_open<-c(paste(sep="","regridded_1deg_tasmax_day_",Modeln[i],"_",Scen_n,"_r1i1p1_",pr_yr_s[j],"-",pr_yr_f[j],".nc"))}

  if (tag == 0) {
  fil_open<-c(paste(sep="","regridded_1deg_tasmax_day_",Modeln[i],"_historical_r1i1p1_",bs_yr_s[j],"-",bs_yr_f[j],".nc"))}  }

  print("opening file")
  print(fil_open)

  tp_1 = nc_open(fil_open) 
 
  mat_end <- endyr-startyr + 1 + mat_start -1
#  pr_val_whole <-ncvar_get(pr_1,"pr") 
#  pr_val_in[,,mat_start:mat_end,i] <- pr_val_whole[,,startyr:endyr]
 if (temp=="Tmin") { tp_val_whole <-ncvar_get(tp_1,"tasmin")}
 if (temp=="Tmax") { tp_val_whole <-ncvar_get(tp_1,"tasmax")}
 
  if (testing !=1){
  temp_val_in[,,mat_start:mat_end,i]<-tp_val_whole[lon1_ind:lon2_ind,lat1_ind:lat2_ind,startyr:endyr]
  mat_start <- mat_end+1}
  if (testing == 1){
  temp_val_in[,,mat_start:mat_end,i]<-tp_val_whole[,,startyr:endyr]
  mat_start <- mat_end+1}
  setwd(c(paste(sep="",ClimDAloc,"\\Scripts\\US_only\\Temp\\Functions")))
  }

#} # if loop

#print("if loop ends")

} # j loops end - reading file by file and getting data

print("j loops ends")
