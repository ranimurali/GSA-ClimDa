#
# Using USGCRP 2014 Climate assessment for U.S. continental regions;  except SE <> Puerto Rico, U.S. Virgin Islands
print("For GIS")

thresnames<-rep(0,Num_thre)
consnames<-rep(0,Num_thre)
for (numthre  in 1:Num_thre) {
  thresnames[numthre]<-c(paste("Threshold > ",Thres_val[numthre]))
  consnames[numthre]<-c(paste("Consecutive Days of ",Thres_val[numthre]))
}

cvarnames<-c("annualTavg","annualTmax","annualTmin",
"JanTavg","FebTavg","MarTavg","AprTavg","MayTavg","JuneTavg","JulTavg","AugTavg","SepTavg","OctTavg","NovTavg","DecTavg",
"JanTmax","FebTmax","MarTmax","AprTmax","MayTmax","JuneTmax","JulTmax","AugTmax","SepTmax","OctTmax","NovTmax","DecTmax",
"JanTmin","FebTmin","MarTmin","AprTmin","MayTmin","JuneTmin","JulTmin","AugTmin","SepTmin","OctTmin","NovTmin","DecTmin",
thresnames,consnames,"HDD","CDD","Diurnal","Daily FreezeThaw") 

#------------------------------------------------ MAKE TABLE
#-------- model simulation GIS data file first
#mod_ens<-model_num+1
cnames[1]<-"index"
cnames[2]<-"longitude"
cnames[3]<-"latitude"
colid<-4
for (llk in 1:length(cvarnames)){
for (jkj in 1:model_num) {
cnames[colid]<-c(paste(cvarnames[llk],"_",jkj))
colid<-colid + 1
}
cnames[colid]<-c(paste(cvarnames[llk],"_Ens"))
colid<-colid+1
}
 
table_col <- length(cnames)
table_row <- lon_num * lat_num
bigtable<-array(0,dim=c(table_row, table_col))

cont = 1

#----------- model simulations
for (lonlon in 1:lon_num) {
for (latlat in 1:lat_num) {

ifelse (inp>1,bigtable[cont,1]<-lon1+(lonlon-1)*0.125,bigtable[cont,1]<-lon1+(lonlon-1)*1)
ifelse (inp>1,bigtable[cont,2]<-lat1+(latlat-1)*0.125,bigtable[cont,2]<-lat1+(latlat-1)*1)

colind=3

#if (tag==1) {
mod_num<-model_num

# annual averages
 for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_Tavg_ann[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_Tavg_ann[lonlon,latlat]
  colind<-colind+1

 for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_Tmax_ann[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_Tmax_ann[lonlon,latlat]
  colind<-colind+1

 for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_Tmin_ann[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_Tmin_ann[lonlon,latlat]
  colind<-colind+1

# monthly averages
  for (imonth in 1:12){
   for (im in 1:mod_num) {
      bigtable[cont,colind]<-AC_Tavg_mon[lonlon,latlat,imonth,im]
      colind<-colind + 1
    }
    bigtable[cont,colind]<-ES_AC_Tavg_mon[lonlon,latlat,imonth]
    colind<-colind + 1
   }

 for (imonth in 1:12){
   for (im in 1:mod_num) {
      bigtable[cont,colind]<-AC_Tmax_mon[lonlon,latlat,imonth,im]
      colind<-colind + 1
    }
    bigtable[cont,colind]<-ES_AC_Tmax_mon[lonlon,latlat,imonth]
    colind<-colind + 1
   }

 for (imonth in 1:12){
   for (im in 1:mod_num) {
      bigtable[cont,colind]<-AC_Tmin_mon[lonlon,latlat,imonth,im]
      colind<-colind + 1
    }
    bigtable[cont,colind]<-ES_AC_Tmin_mon[lonlon,latlat,imonth]
    colind<-colind + 1
   }

#thresholds
  for (ithres in 1:Num_thre) {
    for (im in 1:mod_num) {
       bigtable[cont,colind]<-AC_Tmax_thres[lonlon,latlat,ithres,im]
       colind<-colind+1
     }
     bigtable[cont,colind]<-ES_AC_Tmax_thres[lonlon,latlat,ithres]
     colind<-colind+1
   }   
# consecutive days for thresholds
  for (ithres in 1:Num_thre) {
    for (im in 1:mod_num) {
       bigtable[cont,colind]<-AC_thres_ctd[lonlon,latlat,ithres,im]
       colind<-colind+1
     }
     bigtable[cont,colind]<-ES_AC_thres_ctd[lonlon,latlat,ithres]
     colind<-colind+1
   }   

# HDD

for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_HDD[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_HDD[lonlon,latlat]
  colind<-colind+1

# CDD
for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_CDD[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_CDD[lonlon,latlat]
  colind<-colind+1

#Diurnal
for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_drange[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_drange[lonlon,latlat]
  colind<-colind+1

#Daily FreezeThaw
for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_frethaw[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_frethaw[lonlon,latlat]
  colind<-colind+1

  cont <- cont+1
} # latitude
} #longitude

# table to console
write.table(format(bigtable, justify="right"),  col.names=cnames, quote=F)

# output to csv
out_tab_fil<-c(paste(sep="",output_dir,"\\",US_region,"_GIS_data_GCM_Temp_",Scen,".csv"))

write.table(format(bigtable, justify="right"), file=out_tab_fil, col.names=cnames, quote=F,sep=",")

print("obs")

#--- obs
cvarnames<-c("annualTavg","annualTmax","annualTmin",
"JanTavg","FebTavg","MarTavg","AprTavg","MayTavg","JuneTavg","JulTavg","AugTavg","SepTavg","OctTavg","NovTavg","DecTavg",
"JanTmax","FebTmax","MarTmax","AprTmax","MayTmax","JuneTmax","JulTmax","AugTmax","SepTmax","OctTmax","NovTmax","DecTmax",
"JanTmin","FebTmin","MarTmin","AprTmin","MayTmin","JuneTmin","JulTmin","AugTmin","SepTmin","OctTmin","NovTmin","DecTmin",
thresnames,consnames,"HDD","CDD","Diurnal","Daily FreezeThaw")

totc<-length(cvarnames)+3
cnames<-array(0,totc)
cnames[1]<-"index"
cnames[2]<-"longitude"
cnames[3]<-"latitude"
cont=1

icol<-4
for (llk in 1:totc){
cnames[icol]<-cvarnames[llk]
icol<-icol+1
}
table_col<-length(cnames)

table_row <- ob_lon_num*ob_lat_num

bigtable<-array(0,dim=c(table_row, table_col))

for (lonlon in 1:ob_lon_num) {
for (latlat in 1:ob_lat_num) {

ifelse (inp>1,bigtable[cont,1]<-ob_lon1+(lonlon-1)*0.125,bigtable[cont,1]<-ob_lon1+(lonlon-1)*1)
ifelse (inp>1,bigtable[cont,2]<-ob_lat1+(latlat-1)*0.125,bigtable[cont,2]<-ob_lat1+(latlat-1)*1)

colind=3

#if (tag !=1) {

# annual averages
  bigtable[cont,colind]<-Ob_ann_tavg_tot[lonlon,latlat]
  colind<-colind+1
   
  bigtable[cont,colind]<-Ob_ann_tmax_tot[lonlon,latlat]
  colind<-colind+1

  bigtable[cont,colind]<-Ob_ann_tmin_tot[lonlon,latlat]
  colind<-colind+1
 
# monthly averages
  for (imonth in 1:12){
    bigtable[cont,colind]<-Ob_mon_tavg[lonlon,latlat,imonth]
      colind<-colind + 1
  }

 for (imonth in 1:12){
    bigtable[cont,colind]<-Ob_mon_tmax[lonlon,latlat,imonth]
      colind<-colind + 1
  }

 for (imonth in 1:12){
    bigtable[cont,colind]<-Ob_mon_tmin[lonlon,latlat,imonth]
      colind<-colind + 1
  }

#thresholds
  for (ithres in 1:Num_thre) {
     bigtable[cont,colind]<-Ob_thres_tmax[lonlon,latlat,ithres]
       colind<-colind+1
     }
    
# consecutive days for thresholds
  for (ithres in 1:Num_thre) {
      bigtable[cont,colind]<-Ob_thres_ctd[lonlon,latlat,ithres]
       colind<-colind+1
     }
    
# HDD
  bigtable[cont,colind]<-Ob_HDD[lonlon,latlat]
  colind<-colind+1

# CDD
  bigtable[cont,colind]<-Ob_CDD[lonlon,latlat]
  colind<-colind+1

#Diurnal
  bigtable[cont,colind]<-Ob_drange[lonlon,latlat]
  colind<-colind+1

#Daily FreezeThaw
  bigtable[cont,colind]<-Ob_frethaw[lonlon,latlat]
  colind<-colind+1
  
cont<-cont+1
#}  # tag !=1

}   # latitude
}    # longitude

# table to console
write.table(format(bigtable, justify="right"),  col.names=cnames, quote=F)

# output to csv
out_tab_fil<-c(paste(sep="",output_dir,"\\",US_region,"_GIS_data_obs_Temp_",Scen,".csv"))

write.table(format(bigtable, justify="right"), file=out_tab_fil, col.names=cnames, quote=F,sep=",")

