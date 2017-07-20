##  PRECIPITATION MODULE
#
# - user specifies data input, parameters, precip variables, output
#
# Oct 7, 2015 (v1)
# Rawlings Miller
#---------------------------------
# if you need to clear everything - rm(list=ls(all=TRUE))
setwd('F:\\ClimDA\\Scripts\\US_only\\Precip')
#source("Precip_Module.R")

# ****USER INSERT INFORMATION  ****
#spatial scale - U.S. input files and shapefile overlay
inp = 3  # 1=U.S. 1 degree data (region), 2=U.S. 12 km (state), 3=U.S.12km(county)
# extract a portion of the data for analysis 
# for 1degree data - choices are UnitedStates,Northeast,Southeast,Midwest,GreatPlns,Southwest,Northwest
#US_region<-c("Northeast")  
#US_region<-c("Massachusetts")
#US_region<-c("18181")   # used in file name for figures/tables
US_region<-c("20025")
statename<-US_region
# just obs data analysis?
J_obs = 0  # 1 for "yes"
# baseline time period year to year, inclusive
Base_yr_s=1981
Base_yr_f=1999
# output format
output_dir = c(paste("F:\\ClimDA\\Output\\Test5\\"))
figure_pdf = 1   # plot figures into a pdf
table_csv = 1    # plot tables into a pdf

#-----PROJECTION INFO
# projected time period year to year
Proj_yr_s=2041
Proj_yr_f=2059
# number of climate models
model_num= 3     # insert number of climate models
# number of scenarios
Scen_num = 1
# scenario name
Scen<-c("RCP85")
# scenario
Scen_l<-c("RCP85")
Scen_n<-c("rcp85")
#model location for directory of the files
Modell<-c("ccsm4","GFDLCM3","inmcm4")
# model name as provided by 
Modeln<-c("CCSM4","GFDL-CM3","inmcm4")

#---- Precip variables - which scripts to call
# where 1 = "yes", 0 = "no"
Annprecip = 1   # annual precip avg over time period
Dailyprecip = 1 # average daily precip per year, avg over time period
Monprecip = 1   # monthly precip
returnper = 1   # return periods (see below for set up)
thresprecip = 1  # avg number of days per year above threshold
max5day = 1     #maximum precip over 5 days per year
maxCDD = 1      # maximum number of dry days per year
dailyindex = 1  # daily intensity index

#--return periods - recurrence intervals (e.g., "100" is 
# for the 100 year storm or storm with a 1% chance of occurring in any given year
Num_yrs=3
Ret_yrs<-c(10, 50, 100)
#
#--threshold data
#number of thresholds is one more than threshold values,
# <= the first value, between second/third value, etc.
# >last value
Num_thre=3  # Num_thres= Num_thre+1 for bins
Thres_val<-c(1,50,100)

#---------------------------------------------------------------
#---  call precip scripts

if (inp==1) {source("Precip_US_1degree_Main_v5.R")}  
if (inp==2) {source("Precip_US_12km_Main.r")}
if (inp==3) {source("Precip_US_12km_Main.r")}

if ((figure_pdf==1) & (table_csv==1)) {
 source("F:\\ClimDA\\Scripts\\US_only\\Precip\\Output\\US_Region_Make_PDF_v2.R")}
