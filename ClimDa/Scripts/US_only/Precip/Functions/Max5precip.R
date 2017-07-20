# 5-day Maximum Precip
#

print("start 5-Day Maximum Precipitation")

# get the number of years
ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

#max5ann = array(0,dim=c(tot_yrs))

# each grid cell/ each year find max 5 day & then average for time period
#if (flag_obs==1) {   #change lat/lon to observed lat/lon; proj automatically are lat/lon
#lat_num=ob_lat_num  
#lon_num=ob_lon_num }

for (ilat2 in 1:lat_num) {
for (ilon2 in 1:lon_num) {

indexDY=0
daysinyr=0
max5ann = array(0,dim=c(tot_yrs))

for (yrc in 1:tot_yrs) {
  daysinyr = 365+ind_yrs[yrc]
  maxtry=0
 
  # for through "day by day"
  for (dbd in 1:daysinyr) {
  indexDY = indexDY + 1  # index in precip array
  # max check  
#  if ((dbd > 4) && (dbd <361)) {
  if ((dbd > 4) && (dbd <= daysinyr)) {

   if ((flag_obs==1) && (sum(is.na(Ob_pr_val[ilon2,ilat2,indexDY])) == 0)) {
   maxtry<-Ob_pr_val[ilon2,ilat2,indexDY]+Ob_pr_val[ilon2,ilat2,indexDY-1]+
    Ob_pr_val[ilon2,ilat2,indexDY-2]+Ob_pr_val[ilon2,ilat2,indexDY-3]+Ob_pr_val[ilon2,ilat2,indexDY-4]}

   if ((flag_obs==0) && (sum(is.na(pr_val[ilon2,ilat2,indexDY,i])) == 0)) {
   maxtry<-pr_val[ilon2,ilat2,indexDY,i]+pr_val[ilon2,ilat2,indexDY-1,i]+
    pr_val[ilon2,ilat2,indexDY-2,i]+pr_val[ilon2,ilat2,indexDY-3,i]+pr_val[ilon2,ilat2,indexDY-4,i]}

   if (maxtry>max5ann[yrc]) {
     max5ann[yrc] = maxtry }

   } # if dbd
   } # for dbd
} # for yrc
ifelse(flag_obs==1,Ob_5day[ilon2,ilat2]<-mean(max5ann),
pr_5day_dum[ilon2,ilat2,i]<-mean(max5ann))
} # ilon2
} # ilat2


print("finish 5-Day Maximum")

