#  match  up lat/lon
# for county/state boundaries so indices match 12 km indices
# working with getlatlon
#lon1<-getlatlon[1]  # farthest west (neg #)
#lon2<-getlatlon[2]  # farthest east (neg #)
#lat1<-getlatlon[3]  # most southern (pos #)
#lat2<-getlatlon[4]  # most northern (pos #)
# lat/lon decimal choices for 12 km (1/8 degree)
ind_12km<-c(0.0625,0.1875,0.3125,0.4375,0.5625,0.6875,0.8125,0.9375)

# longitudes - negative values (use ceiling)
new_lon=0
lon_temp=abs(lon1-ceiling(lon1))
for (ifind in 1:8) {
 if (lon_temp < ind_12km[ifind]) {
     new_lon=ind_12km[ifind]
     break}
}

if (new_lon==0) {
new_lon=-0.0625}

lon1=ceiling(lon1)-new_lon

# 
new_lon=0
lon_temp=abs(lon2-ceiling(lon2))
for (ifind in 8:1) {
 if (lon_temp > ind_12km[ifind]) {
     new_lon=ind_12km[ifind]
     break}
}
if (new_lon==0) {
lon2=lon2+1
new_lon=-0.9375}

lon2=ceiling(lon2)-new_lon

# latitudes - positive value (use floor)
new_lat=0
lat_temp=lat2-floor(lat2)
for (ifind in 1:8) {
 if (lat_temp < ind_12km[ifind]) {
     new_lat=ind_12km[ifind]
     break}
}
# if the latitude has not decimals, e.g., 37, then move start index to 36.9375
if (new_lat==0) {
new_lat=0.0625}

lat2=floor(lat2)+new_lat

# 
new_lat=0
lat_temp=lat1-floor(lat1)
for (ifind in 8:1) {
 if (lat_temp > ind_12km[ifind]) {
     new_lat=ind_12km[ifind]
     break}
}

# if the latitude has not decimals, e.g., 37
if (new_lat==0) {
new_lat=0.9375
lat1=floor(lat1)-1}

lat1=floor(lat1)+new_lat

