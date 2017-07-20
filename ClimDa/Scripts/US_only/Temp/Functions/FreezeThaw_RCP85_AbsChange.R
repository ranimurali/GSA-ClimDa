# Explore Maine for spots of freeze/thaw future change
# Assumes day: Tmin<0C, Tmax >=0C
# 

#setwd("F:\\TEACR")
library(ncdf) 

# Set dates
St_T_1<-1980
En_T_1<-1999
#St_T_2<-2025
#En_T_2<-2055
St_T_2<-2080
En_T_2<-2099
num_years<-20

#For model and scenario
filename_tmin <- ("F:\\TEACR\\RCP85\\Extraction_tasmin.nc")
filename_tmax <- ("F:\\TEACR\\RCP85\\Extraction_tasmax.nc")

# Open files
clim_tmin <- open.ncdf(filename_tmin)
clim_tmax <- open.ncdf(filename_tmax)

# Loop Each Model- 5 models

#-- start = lat, lon, time, proj (aka climate model)
#st_tim<- c((St_T_1 - 1980)*365 + 1)
#en_tim<- c((En_T_1 - 1980)*365 + 1)

#start <-c(1,1,st_tim,1)
#count <-c(-1,-1,en_tim,1)

start<-c(1,1,1,1)
count<-c(-1,-1,-1,5)

Tmin <- get.var.ncdf(clim_tmin,varid= "tasmin",start,count)
Tmax <- get.var.ncdf(clim_tmax,varid="tasmax",start,count)

for (m in 1:5) {

num_days_avg <- matrix(data=0,nrow=35,ncol=38)

# FIRST time step

# for years
for (j in 1:19) {    # for each year

for (k in 1:365) {    # every day in the year
  indexDY=(j-1)*365 + k

#  -- Count number of days per year with both Tmin and Tmax
#  -- Tmin < 273 Kelvin and Tmax >= 273 Kelvin - then grid space = 1, else 0
  num_days<-ifelse(Tmin[,,indexDY,m]<0 & Tmax[,,indexDY,m]>=0 & !is.na(Tmin[,,indexDY,m]) &
    !is.na(Tmax[,,indexDY,m]),1,0)

#  -- Average across years
   num_days_avg <- num_days_avg + num_days
}  # end days within a year
}  # end summing year by year

freeze_first <- num_days_avg / num_years   
# above will be a problem if no day across the years met criteria ever
#------------------------------------------------
# SECOND time step

num_days_avg <- matrix(data=0,nrow=35,ncol=38)

# for years
for (j in 1:19) {    # for each year

for (k in 1:365) {    # every day in the year
  indexDY=(j-1)*365 + k + 36498  # adding in 36,498 to be equiv to 2080 starting point

#  -- Count number of days per year with both Tmin and Tmax
#  -- Tmin < 273 Kelvin and Tmax >= 273 Kelvin - then grid space = 1, else 0
  num_days<-ifelse(Tmin[,,indexDY,m]<0 & Tmax[,,indexDY,m]>=0 & !is.na(Tmin[,,indexDY,m]) &
    !is.na(Tmax[,,indexDY,m]),1,0)

#  -- Average across years
   num_days_avg <- num_days_avg + num_days
}  # end days within a year
}  # end summing year by year

freeze_second <- num_days_avg / num_years   
# above will be a problem if no day across the years met criteria ever

# change
if(m==1) {freeze_change_1 <- (freeze_second- freeze_first)}
if (m==2) {freeze_change_2 <- (freeze_second- freeze_first)}
if (m==3) {freeze_change_3 <- (freeze_second- freeze_first)}
if (m==4) {freeze_change_4 <- (freeze_second- freeze_first)}
if (m==5) {freeze_change_5 <- (freeze_second- freeze_first)}
}  # m, cycling through each projection (or climate model)

# Average across the scenario
ens_array<-array(c(freeze_change_1,freeze_change_2,freeze_change_3,freeze_change_4,freeze_change_5),dim=c(35,38,5))
ens_freeze<-apply(ens_array,c(1,2),mean)

# Plot each model/scenario & ensemble
y<- seq(43.125,47.75, by = 0.125)
x<- seq(-71.3125,-67.0625, by =0.125) 
z<- ens_freeze

filled.contour(x,y,z,color.palette=colorRampPalette(c("Brown","ivory2","Blue")),
  plot.title = title(main = "Ensemble Avg, 2080-2099 vs 1980-1999",
   xlab = "Longitude", ylab = "Latitude"),
 zlim=c(-30,30))


# Done