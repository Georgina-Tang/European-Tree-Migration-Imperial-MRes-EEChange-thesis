#everything about data cleaning together: Compile, change extent, aggregation, cleaning, MI conversion and save



setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/input data cleaning")

load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/input data cleaning/data cleaning.RData")

save.image(file = "data cleaning.RData")

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with squared weights/calculation r data")


#modern time using CRU

require(raster)
#get dataset


modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


modern_raster_gam <-CRU
modern_raster_pts <- as.data.frame(rasterToPoints(modern_raster))

modern_raster_gam$rtmi=sqrt(modern_raster_gam$MI)

names(modern_raster_gam)[names(modern_raster_gam) == 'Tmin'] <- 'tmin'
names(modern_raster_gam)[names(modern_raster_gam) == 'Tmax'] <- 'tmax'

names(modern_raster_gam)[names(modern_raster_gam) == 'lon'] <- 'longitude'
names(modern_raster_gam)[names(modern_raster_gam) == 'lat'] <- 'latitude'



save(modern_raster_gam, file='modern_raster_gam.rdata')




#1-1.2 ka
require(raster)
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")

library(dplyr)
paleo_1=left_join(tps_alpha_1_elv_w, tps_gdd_1_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_1_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_1_elv_w, by =c("longitude","latitude")) 

paleo_1$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_1, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-12, 45, 31.5, 73)
common_extent<-extent(-9.5,45,31,71)
#common_extent <- extent(-12, 45, ymin, 73)


modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_1 <- stack(modern_raster, 
                      paleo_raster_agg)



alpha_data_1_pts <- as.data.frame(rasterToPoints(alpha_data_1))


#alpha to MI
library(dplyr)
alpha_data_1_pts_NA <- alpha_data_1_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_1_pts_NA$MI
delta_a<-alpha_data_1_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_1_pts_NA$MI_anomaly=delta_m
alpha_data_1_pts_NA$paleo_MI=alpha_data_1_pts_NA$MI+alpha_data_1_pts_NA$MI_anomaly
alpha_data_1_pts_NA$paleo_Tmin=alpha_data_1_pts_NA$Tmin+alpha_data_1_pts_NA$Tmin_anomaly
alpha_data_1_pts_NA$paleo_Tmax=alpha_data_1_pts_NA$Tmax+alpha_data_1_pts_NA$Tmax_anomaly
alpha_data_1_pts_NA$paleo_gdd=alpha_data_1_pts_NA$gdd0+alpha_data_1_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_1_pts_gam=alpha_data_1_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_1_pts_gam$rtmi=sqrt(alpha_data_1_pts_gam$paleo_MI)

names(alpha_data_1_pts_gam)[names(alpha_data_1_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_1_pts_gam)[names(alpha_data_1_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_1_pts_gam)[names(alpha_data_1_pts_gam) == 'paleo_Tmax'] <- 'tmax'

names(alpha_data_1_pts_gam)[names(alpha_data_1_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_1_pts_gam)[names(alpha_data_1_pts_gam) == 'y'] <- 'latitude'



save(alpha_data_1_pts_gam, file='alpha_data_1_pts_gam.rdata')


#2-2.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_2=left_join(tps_alpha_2_elv_w, tps_gdd_2_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_2_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_2_elv_w, by =c("longitude","latitude")) 

paleo_2$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_2, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-9.5, 45, 31.5, 73)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_2 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_2_pts <- as.data.frame(rasterToPoints(alpha_data_2))


#alpha to MI
alpha_data_2_pts_NA <- alpha_data_2_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_2_pts_NA$MI
delta_a<-alpha_data_2_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_2_pts_NA$MI_anomaly=delta_m
alpha_data_2_pts_NA$paleo_MI=alpha_data_2_pts_NA$MI+alpha_data_2_pts_NA$MI_anomaly
alpha_data_2_pts_NA$paleo_Tmin=alpha_data_2_pts_NA$Tmin+alpha_data_2_pts_NA$Tmin_anomaly
alpha_data_2_pts_NA$paleo_Tmax=alpha_data_2_pts_NA$Tmax+alpha_data_2_pts_NA$Tmax_anomaly
alpha_data_2_pts_NA$paleo_gdd=alpha_data_2_pts_NA$gdd0+alpha_data_2_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_2_pts_gam=alpha_data_2_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_2_pts_gam$rtmi=sqrt(alpha_data_2_pts_gam$paleo_MI)

names(alpha_data_2_pts_gam)[names(alpha_data_2_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_2_pts_gam)[names(alpha_data_2_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_2_pts_gam)[names(alpha_data_2_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_2_pts_gam)[names(alpha_data_2_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_2_pts_gam)[names(alpha_data_2_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_2_pts_gam, file='alpha_data_2_pts_gam.rdata')


#3-3.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_3=left_join(tps_alpha_3_elv_w, tps_gdd_3_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_3_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_3_elv_w, by =c("longitude","latitude")) 

paleo_3$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_3, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-9.5, 45, 31.5, 73)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_3 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_3_pts <- as.data.frame(rasterToPoints(alpha_data_3))


#alpha to MI
alpha_data_3_pts_NA <- alpha_data_3_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_3_pts_NA$MI
delta_a<-alpha_data_3_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_3_pts_NA$MI_anomaly=delta_m
alpha_data_3_pts_NA$paleo_MI=alpha_data_3_pts_NA$MI+alpha_data_3_pts_NA$MI_anomaly
alpha_data_3_pts_NA$paleo_Tmin=alpha_data_3_pts_NA$Tmin+alpha_data_3_pts_NA$Tmin_anomaly
alpha_data_3_pts_NA$paleo_Tmax=alpha_data_3_pts_NA$Tmax+alpha_data_3_pts_NA$Tmax_anomaly
alpha_data_3_pts_NA$paleo_gdd=alpha_data_3_pts_NA$gdd0+alpha_data_3_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_3_pts_gam=alpha_data_3_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_3_pts_gam$rtmi=sqrt(alpha_data_3_pts_gam$paleo_MI)

names(alpha_data_3_pts_gam)[names(alpha_data_3_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_3_pts_gam)[names(alpha_data_3_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_3_pts_gam)[names(alpha_data_3_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_3_pts_gam)[names(alpha_data_3_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_3_pts_gam)[names(alpha_data_3_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_3_pts_gam, file='alpha_data_3_pts_gam.rdata')


#4-4.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_4=left_join(tps_alpha_4_elv_w, tps_gdd_4_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_4_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_4_elv_w, by =c("longitude","latitude")) 

paleo_4$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_4, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-9.5, 45, 31.5, 71)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_4 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_4_pts <- as.data.frame(rasterToPoints(alpha_data_4))


#alpha to MI
alpha_data_4_pts_NA <- alpha_data_4_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_4_pts_NA$MI
delta_a<-alpha_data_4_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_4_pts_NA$MI_anomaly=delta_m
alpha_data_4_pts_NA$paleo_MI=alpha_data_4_pts_NA$MI+alpha_data_4_pts_NA$MI_anomaly
alpha_data_4_pts_NA$paleo_Tmin=alpha_data_4_pts_NA$Tmin+alpha_data_4_pts_NA$Tmin_anomaly
alpha_data_4_pts_NA$paleo_Tmax=alpha_data_4_pts_NA$Tmax+alpha_data_4_pts_NA$Tmax_anomaly
alpha_data_4_pts_NA$paleo_gdd=alpha_data_4_pts_NA$gdd0+alpha_data_4_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_4_pts_gam=alpha_data_4_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_4_pts_gam$rtmi=sqrt(alpha_data_4_pts_gam$paleo_MI)

names(alpha_data_4_pts_gam)[names(alpha_data_4_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_4_pts_gam)[names(alpha_data_4_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_4_pts_gam)[names(alpha_data_4_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_4_pts_gam)[names(alpha_data_4_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_4_pts_gam)[names(alpha_data_4_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_4_pts_gam, file='alpha_data_4_pts_gam.rdata')


#5-5.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_5=left_join(tps_alpha_5_elv_w, tps_gdd_5_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_5_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_5_elv_w, by =c("longitude","latitude")) 

paleo_5$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_5, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-9.5, 45, 33, 71)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_5 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_5_pts <- as.data.frame(rasterToPoints(alpha_data_5))


#alpha to MI
alpha_data_5_pts_NA <- alpha_data_5_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_5_pts_NA$MI
delta_a<-alpha_data_5_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_5_pts_NA$MI_anomaly=delta_m
alpha_data_5_pts_NA$paleo_MI=alpha_data_5_pts_NA$MI+alpha_data_5_pts_NA$MI_anomaly
alpha_data_5_pts_NA$paleo_Tmin=alpha_data_5_pts_NA$Tmin+alpha_data_5_pts_NA$Tmin_anomaly
alpha_data_5_pts_NA$paleo_Tmax=alpha_data_5_pts_NA$Tmax+alpha_data_5_pts_NA$Tmax_anomaly
alpha_data_5_pts_NA$paleo_gdd=alpha_data_5_pts_NA$gdd0+alpha_data_5_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_5_pts_gam=alpha_data_5_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_5_pts_gam$rtmi=sqrt(alpha_data_5_pts_gam$paleo_MI)

names(alpha_data_5_pts_gam)[names(alpha_data_5_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_5_pts_gam)[names(alpha_data_5_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_5_pts_gam)[names(alpha_data_5_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_5_pts_gam)[names(alpha_data_5_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_5_pts_gam)[names(alpha_data_5_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_5_pts_gam, file='alpha_data_5_pts_gam.rdata')


#6-6.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_6=left_join(tps_alpha_6_elv_w, tps_gdd_6_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_6_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_6_elv_w, by =c("longitude","latitude")) 

paleo_6$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_6, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-9.5, 45, 31.5, 71)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_6 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_6_pts <- as.data.frame(rasterToPoints(alpha_data_6))


#alpha to MI
alpha_data_6_pts_NA <- alpha_data_6_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_6_pts_NA$MI
delta_a<-alpha_data_6_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_6_pts_NA$MI_anomaly=delta_m
alpha_data_6_pts_NA$paleo_MI=alpha_data_6_pts_NA$MI+alpha_data_6_pts_NA$MI_anomaly
alpha_data_6_pts_NA$paleo_Tmin=alpha_data_6_pts_NA$Tmin+alpha_data_6_pts_NA$Tmin_anomaly
alpha_data_6_pts_NA$paleo_Tmax=alpha_data_6_pts_NA$Tmax+alpha_data_6_pts_NA$Tmax_anomaly
alpha_data_6_pts_NA$paleo_gdd=alpha_data_6_pts_NA$gdd0+alpha_data_6_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_6_pts_gam=alpha_data_6_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_6_pts_gam$rtmi=sqrt(alpha_data_6_pts_gam$paleo_MI)

names(alpha_data_6_pts_gam)[names(alpha_data_6_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_6_pts_gam)[names(alpha_data_6_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_6_pts_gam)[names(alpha_data_6_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_6_pts_gam)[names(alpha_data_6_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_6_pts_gam)[names(alpha_data_6_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_6_pts_gam, file='alpha_data_6_pts_gam.rdata')


#7-7.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_7=left_join(tps_alpha_7_elv_w, tps_gdd_7_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_7_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_7_elv_w, by =c("longitude","latitude")) 

paleo_7$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_7, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-9.5, 45, 31.5, 71)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_7 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_7_pts <- as.data.frame(rasterToPoints(alpha_data_7))


#alpha to MI
alpha_data_7_pts_NA <- alpha_data_7_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_7_pts_NA$MI
delta_a<-alpha_data_7_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_7_pts_NA$MI_anomaly=delta_m
alpha_data_7_pts_NA$paleo_MI=alpha_data_7_pts_NA$MI+alpha_data_7_pts_NA$MI_anomaly
alpha_data_7_pts_NA$paleo_Tmin=alpha_data_7_pts_NA$Tmin+alpha_data_7_pts_NA$Tmin_anomaly
alpha_data_7_pts_NA$paleo_Tmax=alpha_data_7_pts_NA$Tmax+alpha_data_7_pts_NA$Tmax_anomaly
alpha_data_7_pts_NA$paleo_gdd=alpha_data_7_pts_NA$gdd0+alpha_data_7_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_7_pts_gam=alpha_data_7_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_7_pts_gam$rtmi=sqrt(alpha_data_7_pts_gam$paleo_MI)

names(alpha_data_7_pts_gam)[names(alpha_data_7_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_7_pts_gam)[names(alpha_data_7_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_7_pts_gam)[names(alpha_data_7_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_7_pts_gam)[names(alpha_data_7_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_7_pts_gam)[names(alpha_data_7_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_7_pts_gam, file='alpha_data_7_pts_gam.rdata')


#8-8.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_8=left_join(tps_alpha_8_elv_w, tps_gdd_8_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_8_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_8_elv_w, by =c("longitude","latitude")) 

paleo_8$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_8, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-9.5, 45, 33, 71)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_8 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_8_pts <- as.data.frame(rasterToPoints(alpha_data_8))


#alpha to MI
alpha_data_8_pts_NA <- alpha_data_8_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_8_pts_NA$MI
delta_a<-alpha_data_8_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_8_pts_NA$MI_anomaly=delta_m
alpha_data_8_pts_NA$paleo_MI=alpha_data_8_pts_NA$MI+alpha_data_8_pts_NA$MI_anomaly
alpha_data_8_pts_NA$paleo_Tmin=alpha_data_8_pts_NA$Tmin+alpha_data_8_pts_NA$Tmin_anomaly
alpha_data_8_pts_NA$paleo_Tmax=alpha_data_8_pts_NA$Tmax+alpha_data_8_pts_NA$Tmax_anomaly
alpha_data_8_pts_NA$paleo_gdd=alpha_data_8_pts_NA$gdd0+alpha_data_8_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_8_pts_gam=alpha_data_8_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_8_pts_gam$rtmi=sqrt(alpha_data_8_pts_gam$paleo_MI)

names(alpha_data_8_pts_gam)[names(alpha_data_8_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_8_pts_gam)[names(alpha_data_8_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_8_pts_gam)[names(alpha_data_8_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_8_pts_gam)[names(alpha_data_8_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_8_pts_gam)[names(alpha_data_8_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_8_pts_gam, file='alpha_data_8_pts_gam.rdata')


#9-9.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_9=left_join(tps_alpha_9_elv_w, tps_gdd_9_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_9_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_9_elv_w, by =c("longitude","latitude")) 

paleo_9$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_9, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-9.5, 45, 31.5, 71)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_9 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_9_pts <- as.data.frame(rasterToPoints(alpha_data_9))


#alpha to MI
alpha_data_9_pts_NA <- alpha_data_9_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_9_pts_NA$MI
delta_a<-alpha_data_9_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_9_pts_NA$MI_anomaly=delta_m
alpha_data_9_pts_NA$paleo_MI=alpha_data_9_pts_NA$MI+alpha_data_9_pts_NA$MI_anomaly
alpha_data_9_pts_NA$paleo_Tmin=alpha_data_9_pts_NA$Tmin+alpha_data_9_pts_NA$Tmin_anomaly
alpha_data_9_pts_NA$paleo_Tmax=alpha_data_9_pts_NA$Tmax+alpha_data_9_pts_NA$Tmax_anomaly
alpha_data_9_pts_NA$paleo_gdd=alpha_data_9_pts_NA$gdd0+alpha_data_9_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_9_pts_gam=alpha_data_9_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_9_pts_gam$rtmi=sqrt(alpha_data_9_pts_gam$paleo_MI)

names(alpha_data_9_pts_gam)[names(alpha_data_9_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_9_pts_gam)[names(alpha_data_9_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_9_pts_gam)[names(alpha_data_9_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_9_pts_gam)[names(alpha_data_9_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_9_pts_gam)[names(alpha_data_9_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_9_pts_gam, file='alpha_data_9_pts_gam.rdata')


#10-10.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_10=left_join(tps_alpha_10_elv_w, tps_gdd_10_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_10_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_10_elv_w, by =c("longitude","latitude")) 

paleo_10$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_10, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-8.5, 45, 33, 71)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_10 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_10_pts <- as.data.frame(rasterToPoints(alpha_data_10))


#alpha to MI
alpha_data_10_pts_NA <- alpha_data_10_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_10_pts_NA$MI
delta_a<-alpha_data_10_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_10_pts_NA$MI_anomaly=delta_m
alpha_data_10_pts_NA$paleo_MI=alpha_data_10_pts_NA$MI+alpha_data_10_pts_NA$MI_anomaly
alpha_data_10_pts_NA$paleo_Tmin=alpha_data_10_pts_NA$Tmin+alpha_data_10_pts_NA$Tmin_anomaly
alpha_data_10_pts_NA$paleo_Tmax=alpha_data_10_pts_NA$Tmax+alpha_data_10_pts_NA$Tmax_anomaly
alpha_data_10_pts_NA$paleo_gdd=alpha_data_10_pts_NA$gdd0+alpha_data_10_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_10_pts_gam=alpha_data_10_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_10_pts_gam$rtmi=sqrt(alpha_data_10_pts_gam$paleo_MI)

names(alpha_data_10_pts_gam)[names(alpha_data_10_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_10_pts_gam)[names(alpha_data_10_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_10_pts_gam)[names(alpha_data_10_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_10_pts_gam)[names(alpha_data_10_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_10_pts_gam)[names(alpha_data_10_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_10_pts_gam, file='alpha_data_10_pts_gam.rdata')


#11-11.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_11=left_join(tps_alpha_11_elv_w, tps_gdd_11_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_11_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_11_elv_w, by =c("longitude","latitude")) 

paleo_11$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_11, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-8.5, 45, 33, 71)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_11 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_11_pts <- as.data.frame(rasterToPoints(alpha_data_11))


#alpha to MI
alpha_data_11_pts_NA <- alpha_data_11_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_11_pts_NA$MI
delta_a<-alpha_data_11_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_11_pts_NA$MI_anomaly=delta_m
alpha_data_11_pts_NA$paleo_MI=alpha_data_11_pts_NA$MI+alpha_data_11_pts_NA$MI_anomaly
alpha_data_11_pts_NA$paleo_Tmin=alpha_data_11_pts_NA$Tmin+alpha_data_11_pts_NA$Tmin_anomaly
alpha_data_11_pts_NA$paleo_Tmax=alpha_data_11_pts_NA$Tmax+alpha_data_11_pts_NA$Tmax_anomaly
alpha_data_11_pts_NA$paleo_gdd=alpha_data_11_pts_NA$gdd0+alpha_data_11_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_11_pts_gam=alpha_data_11_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_11_pts_gam$rtmi=sqrt(alpha_data_11_pts_gam$paleo_MI)

names(alpha_data_11_pts_gam)[names(alpha_data_11_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_11_pts_gam)[names(alpha_data_11_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_11_pts_gam)[names(alpha_data_11_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_11_pts_gam)[names(alpha_data_11_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_11_pts_gam)[names(alpha_data_11_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_11_pts_gam, file='alpha_data_11_pts_gam.rdata')


#12-12.2 ka
#get dataset
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")


paleo_12=left_join(tps_alpha_12_elv_w, tps_gdd_12_elv_w,by =c("longitude","latitude")) %>%
  left_join(., tps_tmin_12_elv_w, by =c("longitude","latitude"))  %>%
  left_join(., tps_tmax_12_elv_w, by =c("longitude","latitude")) 

paleo_12$X <- NULL

paleo_raster <- rasterFromXYZ(paleo_12, crs="+init=epsg:4326")

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)

# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
#common_extent <- extent(-9.5, 45, 33.5, 71)#-12 changed to -9.5 for it to match
#common_extent <- extent(-12, 45, ymin, 73)
common_extent<-extent(-9.5,45,31,71)

modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

# par(mfrow=c(3,1))
# plot(modern_raster[['alpha']])
# plot(paleo_raster_agg[['alpha_anomaly']])
# plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_12 <- stack(modern_raster, 
                      paleo_raster_agg)

alpha_data_12_pts <- as.data.frame(rasterToPoints(alpha_data_12))


#alpha to MI
alpha_data_12_pts_NA <- alpha_data_12_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_12_pts_NA$MI
delta_a<-alpha_data_12_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_12_pts_NA$MI_anomaly=delta_m
alpha_data_12_pts_NA$paleo_MI=alpha_data_12_pts_NA$MI+alpha_data_12_pts_NA$MI_anomaly
alpha_data_12_pts_NA$paleo_Tmin=alpha_data_12_pts_NA$Tmin+alpha_data_12_pts_NA$Tmin_anomaly
alpha_data_12_pts_NA$paleo_Tmax=alpha_data_12_pts_NA$Tmax+alpha_data_12_pts_NA$Tmax_anomaly
alpha_data_12_pts_NA$paleo_gdd=alpha_data_12_pts_NA$gdd0+alpha_data_12_pts_NA$gdd_anomaly



#data cleaning, preparing gam input data
alpha_data_12_pts_gam=alpha_data_12_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_12_pts_gam$rtmi=sqrt(alpha_data_12_pts_gam$paleo_MI)

names(alpha_data_12_pts_gam)[names(alpha_data_12_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_12_pts_gam)[names(alpha_data_12_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_12_pts_gam)[names(alpha_data_12_pts_gam) == 'paleo_Tmax'] <- 'tmax'
names(alpha_data_12_pts_gam)[names(alpha_data_12_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_12_pts_gam)[names(alpha_data_12_pts_gam) == 'y'] <- 'latitude'


save(alpha_data_12_pts_gam, file='alpha_data_12_pts_gam.rdata')
