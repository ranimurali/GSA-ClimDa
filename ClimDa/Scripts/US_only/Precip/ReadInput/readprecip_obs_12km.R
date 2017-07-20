#  Readprecip_obs.R
#  This script reads from 1 degree gridded observed data
#  and placed results into pr_val[# lat, # lon, year range requested]
#
#  November 14, 2015
# RLM
#
#  Open file and read precipitation data for time periods

# in observation file - the years it starts/ends with
# obs_start_yr<-c("1980")
# obs_end_yr <-c("1999")
obs_start_yr<-Base_yr_s  # don't think these are needed
obs_end_yr <-Base_yr_f

# 
 bs_yr_s<-Base_yr_s
 bs_yr_f<-Base_yr_f
 bs_yr_num<-bs_yr_f - bs_yr_s + 1

# has the start date crossed into the model file
 
 # make an index of those year to track whether to include in these calculations - changed to check for leap years
 ind_yrs <-c(rep(0,bl_years_tot))
 iyr_ind <- 1

# directory obs file is located
 fil_dir<-c(paste(sep="",ClimDAloc,"\\US\\obs\\1_8deg\\"))
 
 startday <-1
 endday <- 0
 loncount<-ob_lon_num
 latcount<-ob_lat_num

   for (k in 1:bs_yr_num) {
       print("opening file:")
       yr_file<- bs_yr_s + k - 1
       fil_open<-c(paste(sep="",fil_dir,"gridded_obs.pr.OBS_125deg.daily.",yr_file,".nc"))
       print(fil_open)

# take into account leap years   
       ifelse((yr_file) %in% leap, daysinyr <- 366, daysinyr<- 365)     
       endday <- endday + daysinyr

       pr_2 = nc_open(fil_open) 
       Ob_pr_val[,,startday:endday] <-ncvar_get(pr_2,"pr",start=c(lon1_ind,lat1_ind,1),count=c(loncount,latcount,daysinyr)) 

       startday <- endday + 1
    }
  mat_end<-endday
  setwd(c(paste(sep="",ClimDAloc,"\\Scripts\\US_only\\Precip\\Functions")))
  remove(pr_2)