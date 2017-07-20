# following from: http://stackoverflow.com/questions/27524131/calculation-of-return-levels-based-on-a-gpd-in-different-r-packages
# above link is below but also has an interesting exchange - 
# in sum, I read this to mean that extRemes is optimum compared to the other libraries
# MLE Fitting of GPD - package extRemes
# fit gpd
#pot.ext <- fevd(potvalues, method = "MLE", type="GP", threshold=th)
# return levels:
#rl.extremes <-  return.level(pot.ext, conf = 0.05,
#                            return.period= c(2,5,10,20,50,100))
#rl.extremes <- as.numeric(rl.extremes)
library(gdata)
library(extRemes)

#dir_filname<-c(paste(output_dir,"ReturnFits_",US_region,"_",Scen,".pdf",sep=""))
#pdf(dir_filname)

ifelse (flag_obs ==1,tot_yrs <- bl_years_tot, tot_yrs<-years_tot)

#1. identify the day with the maximum precipitation for each year in the period
  for (iyr in 1:tot_yrs) {

  if (iyr == 1) {
    inS <-1
    inE<- 365 + as.numeric(ind_yrs[iyr])}
  if (iyr != 1){
     inS <- inS + 365 + ind_yrs[iyr]
     inE <- inE + 365 + ind_yrs[iyr]}

   # check if model simulation analysis or obs analysis and get max for the year
   ifelse(flag_obs==0,Pr_max[,,iyr,i]<-apply(pr_val[,,inS:inE,i],c(1,2),max),
    Ob_max_pr[,,iyr]<-apply(Ob_pr_val[,,inS:inE],c(1,2),max) ) 

#    inS <- inS + 365 + ind_yrs[iyr]
#    inE <- inE + 365 + ind_yrs[iyr]
   }  # for the iyr for loop

#2. fit distribution and create return periods
ifelse(flag_obs==0,latindx<-lat_num,latindx<-ob_lat_num)
ifelse(flag_obs==0,lonindx<-lon_num,lonindx<-ob_lon_num)

#          determine distribution/return periods for each grid cell
for (ilat in 1:latindx) {
for (ilon in 1:lonindx) {

# if this if for projected data with a model index in the matrices
if (flag_obs==0) {

# check that grid cell has values are are not NA's
if (sum(is.na(Pr_max[ilon,ilat,,i]))==0) {
fit_matrix<-fevd(Pr_max[ilon,ilat,,i],type="GEV")
rpFut1[ilon,ilat,i,]<-return.level(fit_matrix,conf=0.05,return.period<-Ret_yrs)

# not all return periods are going to look right or be right - so as one way to screen out the bad
# if the return periods are crazy high, likely bec/ the fit is very poor (plot(fit_matrix) - you'll
# likely see one fit-modeled data point being something like 1+e24 - so make these crazy grid cells
# be negative in value so we then plot or ignore any negative return periods

# R2 =+(COVAR(O$3:O$83,AE$3:AE$83)/(STDEV(O$3:O$83)*STDEV(AE$3:AE$83)))
# code for seeing the probabilty of a given number - 1.54
# pgev( 1.54, loc=fit$mle[1], scale=fit$mle[2], shape=fit$mle[3], lower.tail=FALSE)
#if(rpFut1[ilon,ilat,i,]>500) {rpFut1[ilon,ilat,i,]<-Ret_yrs*-1 }
# most rainfall ever records for 24 hours is 1,825mm (72in)
# 
for (sweepRP in 1:Num_yrs) {
if( rpFut1[ilon,ilat,i,sweepRP] > 1850 ) {rpFut1[ilon,ilat,i,sweepRP]<- NaN }
else {
if( rpFut1[ilon,ilat,i,sweepRP] < 0 ) {rpFut1[ilon,ilat,i,sweepRP]<- NaN }
}
}

}  #grid cell is not NAs
}

# observed data
if (flag_obs ==1) {

# check that grid cell has values are are not NA's
if (sum(is.na(Ob_max_pr[ilon,ilat,]))==0) {
   fit_matrix<-fevd(Ob_max_pr[ilon,ilat,],type="GEV")
   rpBase[ilon,ilat,]<-return.level(fit_matrix,conf=0.05,return.period<-Ret_yrs) 

# not all return periods are going to look right or be right - so as one way to screen out the bad
# if the return periods are crazy high, likely bec/ the fit is very poor (plot(fit_matrix) - you'll
# likely see one fit-modeled data point being something like 1+e24 - so make these crazy grid cells
# be negative in value so we then plot or ignore any negative return periods
for (sweepRP in 1:Num_yrs) {
if( rpBase[ilon,ilat,sweepRP] > 1850 ) {rpBase[ilon,ilat,sweepRP]<- NaN }
else {
if( rpBase[ilon,ilat,sweepRP] < 0 ) {rpBase[ilon,ilat,sweepRP]<- NaN }
}
}


#if(rpBase[ilon,ilat,]>500) {rpBase[ilon,ilat,]<-500 }
#{rpBase[ilon,ilat,]<-Ret_yrs*-1 }
}  #grid cell is not NAs


} # flag_obs
}  # ilon array
}  # ilat array   


