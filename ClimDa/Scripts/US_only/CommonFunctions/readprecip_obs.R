#  Readprecip_obs.R
#  This script reads from 1 degree gridded observed data
#  and placed results into pr_val[# lat, # lon, year range requested]
#
# Read inptut data:
#Product:               Maurer2002 Gridded Observed
#GDT  1.2
# Pr - daily average precipitation (mm/day)
#Period:                1950Jan through 1999Dec (Gregorian calendar / aka, leap years)
#Resolution:            1 degree
#Latitude bounds:       (25.0, 53.0)
#Longitude bounds:      (-125.0, -67.0)
#Area within bounds:    15451105 km^2 (approx)
#Dimensions:         
# Times:                18262
# Latitudes:            28
# Longitudes:           58
# Projections:          1


#  Open file and read precipitation data for time periods

# in observation file - the years it starts/ends with
 obs_start_yr<-c("19500101")
 obs_end_yr <-c("19991231")

# from Precip_US_1degree_Main_v3.R
 bs_yr_s<-Base_yr_s
 bs_yr_f<-Base_yr_f
 
# has the start date crossed into the model file
 
 # make an index of those year to track whether to include in these calculations - changed to check for leap years
ind_yrs <-c(rep(0,bl_years_tot))
iyr_ind <- 1

if (Module_ch =="Precip") {
fil_dir<-c(paste(sep="",workdirUS,"\\US\\obs\\1_deg\\pr"))}
if (Module_ch=="Temp") {
fil_dir_tmax<-c(paste(sep="",workdirUS,"\\US\\obs\\1_deg\\tmax"))
fil_dir_tmin<-c(paste(sep="",workdirUS,"\\US\\obs\\1_deg\\tmin"))}
 
# how many years are in this file that will be used?
# 1.  how many years are in the file
  yrs_in_file = as.numeric(substr(obs_end_yr,1,4)) - as.numeric(substr(obs_start_yr,1,4)) + 1 
    
##     print("years in file")
##     print(yrs_in_file)
    
# 2. make an index of those year to track whether to include in these calculations - changed to check for leap years
     counter<-0
     startyr <-0
     endyr <- 0
     pnt_ind <- 1
    
# 3. This for loop finds out: (1) which years in the file are in the projected request; (2) which years are leap years
     for (k in 1:yrs_in_file) {
       # convert starting year in file from "20100101" to "2010" and in numeric form
       countyear <- as.numeric(substr(obs_start_yr,1,4))
       
       if ( ((countyear+k-1) >= bs_yr_s) && ((countyear+k-1) <= bs_yr_f) ) {
          if (counter == 0) { 
                startyr <- pnt_ind
                counter <- 1 }
        ifelse((countyear+k-1) %in% leap,ind_yrs[iyr_ind] <- 1,ind_yrs[iyr_ind] <- 0)
        iyr_ind <- iyr_ind+1

        }  # if statement
 
      # check for leap years so the added day gets taken into account when pulling the data
        if ((countyear+k-1) <= bs_yr_f) {
        ifelse((countyear+k-1) %in% leap, pnt_ind <- pnt_ind + 366, pnt_ind<-pnt_ind+365)
      
        if (counter == 1) {
          endyr <- pnt_ind-1 }
        }
  } # for k loop
  
##print("finish k loop")
# 4. need to pull data into model matrix
if (Module_ch=="Precip") {
  setwd(fil_dir)

  fil_open<-c("Extraction_pr.nc")

  print("opening file:")
  print(fil_open)

  pr_2 = nc_open(fil_open) 
  pr_val_whole <-ncvar_get(pr_2,"pr") 
  
  Ob_pr_val <-pr_val_whole[,,startyr:endyr]
  mat_end <- endyr-startyr + 1

  rm (pr_val_whole)
  rm (pr_2)

  setwd(c(paste(sep="",workdirUS,"\\Scripts\\US_only\\Precip\\Functions")
}
if (Module_ch=="Temp") {
  setwd(fil_dir_tmax)

  fil_open<-c("Extraction_tasmax.nc")

  print("opening file:")
  print(fil_open)

  tmax_2 = nc_open(fil_open) 
  tmax_val_whole <-ncvar_get(tmax_2,"tasmax") 
  
  Ob_tmax <-tmax_val_whole[,,startyr:endyr]
  mat_end <- endyr-startyr + 1

  setwd(fil_dir_tmin)

  fil_open<-c("Extraction_tasmin.nc")

  print("opening file:")
  print(fil_open)

  tmin_2 = nc_open(fil_open) 
  tmin_val_whole <-ncvar_get(tmin_2,"tasmin") 
  
  Ob_tmax <-tmin_val_whole[,,startyr:endyr]
  mat_end <- endyr-startyr + 1

  rm(tmax_2)
  rm(tmin_2)
  rm(tmin_val_whole)
  rm(tmax_val_whole)

  setwd(c(paste(sep="",workdirUS,"\\Scripts\\US_only\\Temp\\Functions")

