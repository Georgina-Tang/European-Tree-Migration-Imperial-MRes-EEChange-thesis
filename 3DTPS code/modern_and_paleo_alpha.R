
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/CRU plots")

load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/CRU plots/CRU aggregation.RData")

save.image(file = "CRU aggregation.RData")

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
common_extent <- extent(-12, 45, 31.5, 73)
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
write.csv(alpha_data_1_pts, 'modern_and_anomaly 1ka.csv')




########################################3
# Load modern data into a raster, making sure to separate row names, so that
# the first two columns are lat and long
#modern <- read.csv('CRU modern climate variables.csv', row.names=1)
#modern_raster <- rasterFromXYZ(modern, crs="+init=epsg:4326")
modern_raster <- rasterFromXYZ(CRU, crs="+init=epsg:4326")

# load paleo into a raster, dropping first column of row IDs
#load("tps_alpha_1_elv_w.rdata")
tps_alpha_1_elv_w$X <- NULL
paleo_raster <- rasterFromXYZ(tps_alpha_1_elv_w, crs="+init=epsg:4326")

par(mfrow=c(1, 2))
plot(modern_raster[['alpha']])
plot(paleo_raster[['alpha_anomaly']])

# the extents do not match...
extent(modern_raster)
extent(paleo_raster)
# ... so define the common area at the 0.5° resolution and crop to that
#common_extent <- extent(-20.5, 129.5, 31.5, 79)
common_extent <- extent(-12, 45, 31.5, 73)
#common_extent <- extent(-12, 45, ymin, 73)


modern_raster <- crop(modern_raster, common_extent)
paleo_raster <- crop(paleo_raster, common_extent)

# Now aggregate TPS from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
paleo_raster_agg <- aggregate(paleo_raster, fact = 6, fun = mean)

par(mfrow=c(3,1))
plot(modern_raster[['alpha']])
plot(paleo_raster_agg[['alpha_anomaly']])
plot(modern_raster[['alpha']] + paleo_raster_agg[['alpha_anomaly']])

# Save a CSV of alpha and anomaly
# alpha_data <- stack(modern_raster[['alpha']], 
#                     paleo_raster_agg[['alpha_anomaly']])

alpha_data_1 <- stack(modern_raster, 
                    paleo_raster_agg[['alpha_anomaly']])



alpha_data_1_pts <- as.data.frame(rasterToPoints(alpha_data_1))
write.csv(alpha_data_1_pts, 'modern_and_anomaly 1ka alpha.csv')




#map
greens <- RColorBrewer::brewer.pal(6, "Greens")
White<-c("#FFFFFF")#adding white colour as 0
browns_rev <- rev(RColorBrewer::brewer.pal(6, "YlOrBr"))
#RColorBrewer::display.brewer.pal(4, "YlOrBr")
COLOURS <- c(browns_rev, greens)
COLOURS



p<-alpha_data_pts %>%
  dplyr::mutate(
    alpha = alpha %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$y, na.rm = TRUE), 73), )
p


p<-alpha_data_pts %>%
  smpds::plot_climate_tiles(var = "alpha",
                            units = "\u00B0C",
                                xlim = c(-12, 45),
                            ylim = c(min(.$y, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/CRU plots")
library(ggplot2)
ggsave(file="1-1.2ka Map of alpha.jpeg",p,width=22,height=11)



save.image(file = "CRU aggregation.RData")
