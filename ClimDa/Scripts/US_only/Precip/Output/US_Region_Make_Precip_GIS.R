#
# Using USGCRP 2014 Climate assessment for U.S. continental regions;  except SE <> Puerto Rico, U.S. Virgin Islands
#
#  note - without return periods
#
print("For GIS")

thresnames<-rep(0,Num_thres)
for (numthre  in 1:Num_thre) {
  thresnames[numthre]<-c(paste("Threshold > ",Thres_val[numthre]))
}
thresnames[Num_thres]<-c(paste("Threshold > ",Thres_val[numthre]))

cvarnames<-c("annual","avgdaily","Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sep","Oct","Nov","Dec",
thresnames,"max5day","maxCDD","dailyindex","almon1","almon2","almon3") 

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
  bigtable[cont,colind]<-AC_ann_pr_tot[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_ann_pr_tot[lonlon,latlat]
  colind<-colind+1

# monthly averages
  for (imonth in 1:12){
   for (im in 1:mod_num) {
      bigtable[cont,colind]<-AC_mon_avg_pr[lonlon,latlat,imonth,im]
      colind<-colind + 1
    }
    bigtable[cont,colind]<-ES_AC_mon_avg_pr[lonlon,latlat,imonth]
    colind<-colind + 1
   }
#thresholds
  for (ithres in 1:Num_thre) {
    for (im in 1:mod_num) {
       bigtable[cont,colind]<-AC_thres_pr[lonlon,latlat,ithres,im]
       colind<-colind+1
     }
     bigtable[cont,colind]<-ES_AC_threvalue[lonlon,latlat,ithres]
     colind<-colind+1
   }   
# max5day
  for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_5day[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_5day[lonlon,latlat]
  colind<-colind+1

# maxCDD
  for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_consdd[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_consdd[lonlon,latlat]
  colind<-colind+1

# dailyindex
  for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_sdii[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_sdii[lonlon,latlat]
  colind<-colind+1

if (almonindex ==1){
# almond indicator 1
  for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_al1[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_al1[lonlon,latlat]
  colind<-colind+1

# almond indicator 2
  for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_al2[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_al2[lonlon,latlat]
  colind<-colind+1

# almond indicator 3
  for (im in 1:mod_num) {
  bigtable[cont,colind]<-AC_al3[lonlon,latlat,im]
  colind<-colind+1
  }
  bigtable[cont,colind]<-ES_AC_al3[lonlon,latlat]
  colind<-colind+1
} # almond index

cont <- cont + 1

} # latitude
} #longitude

# table to console
#write.table(format(bigtable, justify="right"),  col.names=cnames, quote=F)

# output to csv
out_tab_fil<-c(paste(sep="",output_dir,US_region,"_GIS_data_GCM_Precip_",Scen,".csv"))

write.table(format(bigtable, justify="right"), file=out_tab_fil, col.names=cnames, quote=F,sep=",")

print("output model table")

#--- obs
cvarnames<-c("annual","avgdaily","Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sep","Oct","Nov","Dec",
thresnames,"max5day","maxCDD","dailyindex","almon1","almon2","almon3") 

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
  bigtable[cont,colind]<-Ob_ann_pr_tot[lonlon,latlat]
  colind<-colind+1
   
# monthly averages
  for (imonth in 1:12){
    bigtable[cont,colind]<-Ob_mon_avg_pr[lonlon,latlat,imonth]
      colind<-colind + 1
  }
 
#thresholds
  for (ithres in 1:Num_thre) {
     bigtable[cont,colind]<-Ob_thres_pr[lonlon,latlat,ithres]
       colind<-colind+1
     }
    
# max5day
  bigtable[cont,colind]<-Ob_5day[lonlon,latlat]
  colind<-colind+1

# maxCDD
  bigtable[cont,colind]<-Ob_consdd[lonlon,latlat]
  colind<-colind+1

#daily index
  bigtable[cont,colind]<-Ob_sdii[lonlon,latlat]
  colind<-colind+1

if (almonindex == 1){
# almon 1
  bigtable[cont,colind]<-Ob_al1[lonlon,latlat]
  colind<-colind+1
  
# almon 2
  bigtable[cont,colind]<-Ob_al2[lonlon,latlat]
  colind<-colind+1

# almon 3
  bigtable[cont,colind]<-Ob_al3[lonlon,latlat]
  colind<-colind+1
}

cont<-cont+1
#}  # tag !=1

}   # latitude
}    # longitude

# table to console
#write.table(format(bigtable, justify="right"),  col.names=cnames, quote=F)

# output to csv
out_tab_fil<-c(paste(sep="",output_dir,"\\",US_region,"_GIS_data_obs_Precip_",Scen,".csv"))

write.table(format(bigtable, justify="right"), file=out_tab_fil, col.names=cnames, quote=F,sep=",")

print("output obs table")


