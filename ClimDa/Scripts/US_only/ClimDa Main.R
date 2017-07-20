## CLIMDA MODULE - here is a test
#
# - user specifies data input, parameters, precip variables, output
#
# April 2016 (First Version)
# Innovation Fund, Principal Investigator: Rawlings Miller, Alex Lataille
# Training on ClimDA: Alex Lataille, John Synder, Angela Wong, Cassie Bhat
#
# Precipitation - millimeters
# Temperature - Celsius or Fahrenheit - user's choice
#
#---------------------------------
# if you need to clear everything from the R workspace (e.g., after ClimDA run and before another): rm(list=ls(all=TRUE))

# directory of U.S. ClimDA tool - change this to the correct drive/etc.
# setwd('C:\\Users\\Rawlings\\Desktop\\PORTABLE\\ClimDA\\Scripts\\US_only')

#ClimDAloc<-c('F:\\ClimDA')
ClimDAloc<-c('F:\\ClimDa')
workdirUS<-c(paste(sep="",ClimDAloc,'\\Scripts\\US_only'))
setwd(workdirUS)

#------ TO RUN THIS PROGRAM, cut/paste the three lines above & line below into the R console - the first being the workdir US folder & second calls/runs the program
#source("ClimDA_Main.R")

# ****USER INSERT INFORMATION  ****
#spatial scale - U.S. input files and shapefile overlay - if using a specified grid, then choose inp=1 for 1 degree climate data, inp=2 for 1/8 degree
inp = 3 # 1=U.S. 1 degree data (region or state), 2=U.S. 12 km (state), 3=U.S. 12km (county) <-- which U.S. dataset to use
       
usergrid=0  # 1= user specify the grid w/ lat and lon -otherwise defaults to shapefile
if (usergrid ==1 ) {  
lat1 = 34.5
lat2 = 35.0
lon1 = -105.0    # (-) value of lon
lon2 = -104.8
US_region<-c("userdef1")  # user defined grid
}
# extract a portion of the data for analysis 
# for 1degree data - choices are UnitedStates,Northeast,Southeast,Midwest,GreatPlns,Southwest,Northwest, State name
# for 12 km data - choices are a statename or county fip


#US_region<-c("California")  # state name  (Note: if chosing a state, will need to turn it on later in the script (do the same thing as was done for California))
US_region <-c("34005")  # BUCKS (county fip)
#US_region<-c("Southwest")   # region

statename<-US_region

# just obs data analysis?
J_obs = 0  # 1 for "yes"

# baseline time period year to year, inclusive
Base_yr_s=1990
Base_yr_f=1999
# output format - You can add new output folder after "\\Output\\FOLDER NAME HERE" below 
output_dir<-c(paste(sep="",ClimDAloc,"\\Output\\Test"))
figure_pdf = 1   # plot figures into a pdf
table_csv = 1    # plot tables into a csv file that can be imported into Excel and formatted - defaults to table_csv=1 if figure_pdf=1
GIS_output = 0   # create obs/proj csv file for GIS insert (value by model/ens grid cell by grid cell)

#-----PROJECTION INFO
# projected time period year to year
Proj_yr_s=2006
Proj_yr_f=2006
# number of climate models
model_num= 1    # insert number of climate models
# number of scenarios
Scen_num = 1
# scenario name  (keep format of these scenarios consistent with what's below - it has to do with directory folder name and then file names...)
Scen<-c("RCP85")
# scenario
Scen_l<-c("RCP85")
Scen_n<-c("rcp85")
#model location for directory of the files
Modell<-c("bcc-csm1-1")
#Modell<-c("bcc-csm1-1","ccsm4","GFDLCM3","gfdl-esm2g","gfdl-esm2m","inmcm4","ipsl-cm5a-lr","ipsl-cm5a-mr","miroc-esm-chem","miroc-esm","mri-cgcm3")
# model name as provided by 
Modeln<-c("bcc-csm1-1")
#Modeln<-c("bcc-csm1-1","CCSM4","GFDL-CM3","GFDL-ESM2G","GFDL-ESM2M","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR","MIROC-ESM-CHEM","MIROC-ESM","MRI-CGCM3")

# ---- Temp or Precip
Module_ch<-"Temp"  # choices: "Temp" for temperature, "Precip" for precipitation

if (Module_ch =="Precip"){
#---- Precip variables - which scripts to call
# where 1 = "yes", 0 = "no"
Annprecip = 0   #1 annual precip avg over time period
Dailyprecip = 0 #1 average daily precip per year, avg over time period
Monprecip = 0   #1 monthly precip
returnper = 0   #0 return periods (see below for set up) [Fyi - I don't like these datasets for return periods]
thresprecip = 1  #1 avg number of days per year above threshold
max5day = 0     #1 maximum precip over 5 days per year
maxCDD = 0      #1 maximum number of dry days per year
dailyindex = 0  #1 daily intensity index
almonindex = 0  #0 indicators tailored for almonds

#--return periods - recurrence intervals (e.g., "100" is 
# for the 100 year storm or storm with a 1% chance of occurring in any given year
Num_yrs=2
Ret_yrs<-c(10,100)
#
#--threshold data
#number of thresholds is one more than threshold values,
# <= the first value, between second/third value, etc.
# >last value
Num_thre=3  # Num_thres= Num_thre+1 for bins
Thres_val<-c(25,50,25)
}

if (Module_ch =="Temp") {
#--------- Temperature variables
Units = 2  # 1=Celsius, 2 = Fahrenheit
Anntemp = 1   #1 annual temp avg over time period
Montemp = 1   #1 monthly temp (Tmax, Tmin, Tavg)
Tmaxthres = 1  #1 avg number of days per year above threshold
ctd = 1  #1 max consecutive days per year for each threshold
CDDHDD = 1  #1 calculate cooling/heating degree days
drange =1  #1 average daily diurnal range
frethaw=1 #1 daily freeze thaw
alomindex = 0 #0 indicators tailored for almonds

#number of thresholds is one more than threshold values,
# < the first value, between second/third value, etc.
Num_thre=1  # Num_thres= Num_thre+1 for bins
#Thres_val<-c(32.2,35,37.8)  # celsius, 90F=32C,95F=35C,100F=37.8C  
Thres_val<-c(70)
}
#---------------------------------------------------------------
# END USER INPUT
#---------------------------------------------------------------
#---  call precip and temp scripts

#---- Precip analysis

if (Module_ch == "Precip") {
workdirPrecip<-c(paste(sep="",workdirUS,"\\Precip\\"))
if (inp==1) {source(c(paste(sep="",workdirPrecip,"Precip_US_1degree_Main_v5.R")))}  
if (inp==2) {source(c(paste(sep="",workdirPrecip,"Precip_US_12km_Main.r")))}
if (inp==3) {source(c(paste(sep="",workdirPrecip,"Precip_US_12km_Main.r")))}

if ((figure_pdf==1) & (table_csv==1)) {
  source(c(paste(sep="",workdirUS,"\\Precip\\Output\\US_Region_Make_PDF_v2.R")))
  source(c(paste(sep="",workdirUS,"\\Precip\\Output\\US_Region_Make_Precip_GIS.R")))}
}  # end Precip analysis

#if (GIS_output ==1 ) {
#  source(c(paste(sep="",workdir,"Output\\US_Region_Make_Temp_GIS.R"))) }

#---- Temp analysis
if (Module_ch == "Temp") {
#if (inp==1) {source("F:\\ClimDA\\Scripts\\US_only\\Temp\\Temp_US_12km_1deg_Main.r")}  
workdirTemp<-c(paste(sep="",workdirUS,"\\Temp\\"))
if (inp==1) {source(c(paste(sep="",workdirTemp,"Temp_US_1deg_Main.R")))}  
if (inp > 1) {source(c(paste(sep="",workdirTemp,"Temp_US_12km_Main.r")))}
#if (inp==3) {source(c(paste(sep="",workdirTemp,"Temp_US_12km_Main.r")))}

#if ((figure_pdf==1) & (table_csv==1) & (usergrid !=1) ) {
# ASSUMING ALWAYS GIVE TABLE IF GIVING FIGURES (table takes no time to produce)
if ((table_csv==1) & (usergrid !=1) ) {
 source(c(paste(sep="",workdirTemp,"Output\\US_Region_Make_Temp_PDF.R")))
 source(c(paste(sep="",workdirTemp,"Output\\US_Region_Make_Temp_GIS.R")))}

if ((figure_pdf==1) & (table_csv==1) & (usergrid ==1) ) {
 source(c(paste(sep="",workdirTemp,"Output\\US_Region_Make_Temp_PDF_User_defined.R")))
 source(c(paste(sep="",workdirTemp,"Output\\US_Region_Make_Temp_GIS.R")))}

if (GIS_output ==1 ) {
  source(c(paste(sep="",workdirTemp,"Output\\US_Region_Make_Temp_GIS.R"))) }
}  # end temp analysis

#proc.time() - ptm
