
#                      Absolute change (future - baseline) 

AC_Tmax_mon <- Pr_Tmax_mon - Bl_mon_Tmax
AC_Tmin_mon <- Pr_Tmin_mon - Bl_mon_Tmin
AC_Tavg_mon <- Pr_Tavg_mon - Bl_mon_Tavg

AC_Tmax_ann <- Pr_Tmax_ann - Bl_Tmax_ann
AC_Tmin_ann <- Pr_Tmin_ann - Bl_Tmin_ann
AC_Tavg_ann <- Pr_Tavg_ann - Bl_Tavg_ann

AC_Tmax_thres<- Pr_Tmax_thres - Bl_Tmax_thres
AC_thres_ctd<-Pr_thres_ctd - Bl_thres_ctd
AC_HDD<- Pr_HDD-Bl_HDD
AC_CDD<- Pr_CDD-Bl_CDD
AC_drange<- Pr_drange-Bl_drange
AC_frethaw<- Pr_frethaw-Bl_frethaw

#  Composite of min/max values for each model for each grid cell
# probably a faster way to optimize this!
for (ii in 1:lon_num) {
for (jj in 1:lat_num) {
CMIN_Tmax_ann[ii,jj]<-min(AC_Tmax_ann[ii,jj,])
CMAX_Tmax_ann[ii,jj]<-max(AC_Tmax_ann[ii,jj,])
CMIN_Tmin_ann[ii,jj]<-min(AC_Tmin_ann[ii,jj,])
CMAX_Tmin_ann[ii,jj]<-max(AC_Tmin_ann[ii,jj,])
CMIN_Tavg_ann[ii,jj]<-min(AC_Tavg_ann[ii,jj,])
CMAX_Tavg_ann[ii,jj]<-max(AC_Tavg_ann[ii,jj,])
}
}

#    Percent difference  (future - baseline) / baseline

for(imon in 1:12) {
PC_Tmax_mon[,,imon,] <- AC_Tmax_mon[,,imon,] / Bl_mon_Tmax[,,imon,] 
PC_Tmin_mon[,,imon,] <- AC_Tmin_mon[,,imon,] / Bl_mon_Tmin[,,imon,] 
PC_Tavg_mon[,,imon,] <- AC_Tavg_mon[,,imon,] / Bl_mon_Tavg[,,imon,] 
}

PC_Tmax_ann <- AC_Tmax_ann / Bl_Tmax_ann
PC_Tmin_ann <- AC_Tmin_ann / Bl_Tmin_ann
PC_Tavg_ann <- AC_Tavg_ann / Bl_Tavg_ann

for(ithres in 1:Num_thre) {
PC_Tmax_thres[,,ithres,] <- AC_Tmax_thres[,,ithres,] / Bl_Tmax_thres[,,ithres,] 
PC_thres_ctd[,,ithres,]<-AC_thres_ctd[,,ithres,] / Bl_thres_ctd[,,ithres,]}
PC_HDD  <- AC_HDD/Bl_HDD
PC_CDD <- AC_CDD/Bl_CDD
PC_drange<-AC_drange/Bl_drange
PC_frethaw<-AC_frethaw/Bl_frethaw

# }  # i loop - model by model loop

# ensemble average - absolute  

for (imon in 1:12) {
ES_AC_Tmax_mon[,,imon] <- apply(AC_Tmax_mon[,,imon,],c(1,2),mean) 
ES_AC_Tmin_mon[,,imon] <- apply(AC_Tmin_mon[,,imon,],c(1,2),mean) 
ES_AC_Tavg_mon[,,imon] <- apply(AC_Tavg_mon[,,imon,],c(1,2),mean) 
}

ES_AC_Tmax_ann <- apply(AC_Tmax_ann,c(1,2),mean)
ES_AC_Tmin_ann <- apply(AC_Tmin_ann,c(1,2),mean)
ES_AC_Tavg_ann <- apply(AC_Tavg_ann,c(1,2),mean)

for (ithres in 1:Num_thre){
ES_AC_Tmax_thres[,,ithres] <- apply(AC_Tmax_thres[,,ithres,],c(1,2),mean) 
ES_AC_thres_ctd[,,ithres]<-apply(AC_thres_ctd[,,ithres,],c(1,2),mean)}

ES_AC_HDD <- apply(AC_HDD,c(1,2),mean)
ES_AC_CDD <- apply(AC_CDD,c(1,2),mean)
ES_AC_drange<-apply(AC_drange,c(1,2),mean)
ES_AC_frethaw<-apply(AC_frethaw,c(1,2),mean)

# ensemble average - percent diff

for (imon in 1:12) {
ES_PC_Tmax_mon[,,imon] <- apply(PC_Tmax_mon[,,imon,],c(1,2),mean) 
ES_PC_Tmin_mon[,,imon] <- apply(PC_Tmin_mon[,,imon,],c(1,2),mean) 
ES_PC_Tavg_mon[,,imon] <- apply(PC_Tavg_mon[,,imon,],c(1,2),mean) 
}

ES_PC_Tmax_ann <- apply(PC_Tmax_ann,c(1,2),mean)
ES_PC_Tmin_ann <- apply(PC_Tmin_ann,c(1,2),mean)
ES_PC_Tavg_ann <- apply(PC_Tavg_ann,c(1,2),mean)

for (ithres in 1:Num_thre) {
ES_PC_Tmax_thres[,,ithres] <- apply(PC_Tmax_thres[,,ithres,],c(1,2),mean) 
ES_PC_thres_ctd[,,ithres]<-apply(PC_thres_ctd[,,ithres,],c(1,2),mean)}
ES_PC_HDD <- apply(PC_HDD,c(1,2),mean)
ES_PC_CDD <- apply(PC_CDD,c(1,2),mean)
ES_PC_drange<-apply(PC_drange,c(1,2),mean)
ES_PC_frethaw<-apply(PC_frethaw,c(1,2),mean)

print("Finished ensemble calculations")