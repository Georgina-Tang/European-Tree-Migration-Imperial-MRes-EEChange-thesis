#checking resolution

#all is [1] 0.08333334 0.08333334
require(raster)

gridded(tps_alpha_1_elv_w) <- ~longitude + latitude
tps_alpha_1_elv_w_raster <- raster(tps_alpha_1_elv_w)
res(tps_alpha_1_elv_w_raster)

gridded(tps_alpha_2_elv_w) <- ~longitude + latitude
tps_alpha_2_elv_w_raster <- raster(tps_alpha_2_elv_w)
res(tps_alpha_2_elv_w_raster)

gridded(tps_alpha_3_elv_w) <- ~longitude + latitude
tps_alpha_3_elv_w_raster <- raster(tps_alpha_3_elv_w)
res(tps_alpha_3_elv_w_raster)

gridded(tps_alpha_4_elv_w) <- ~longitude + latitude
tps_alpha_4_elv_w_raster <- raster(tps_alpha_4_elv_w)
res(tps_alpha_4_elv_w_raster)

gridded(tps_alpha_5_elv_w) <- ~longitude + latitude
tps_alpha_5_elv_w_raster <- raster(tps_alpha_5_elv_w)
res(tps_alpha_5_elv_w_raster)

gridded(tps_alpha_6_elv_w) <- ~longitude + latitude
tps_alpha_6_elv_w_raster <- raster(tps_alpha_6_elv_w)
res(tps_alpha_6_elv_w_raster)

gridded(tps_alpha_7_elv_w) <- ~longitude + latitude
tps_alpha_7_elv_w_raster <- raster(tps_alpha_7_elv_w)
res(tps_alpha_7_elv_w_raster)

gridded(tps_alpha_8_elv_w) <- ~longitude + latitude
tps_alpha_8_elv_w_raster <- raster(tps_alpha_8_elv_w)
res(tps_alpha_8_elv_w_raster)

gridded(tps_alpha_9_elv_w) <- ~longitude + latitude
tps_alpha_9_elv_w_raster <- raster(tps_alpha_9_elv_w)
res(tps_alpha_9_elv_w_raster)

gridded(tps_alpha_10_elv_w) <- ~longitude + latitude
tps_alpha_10_elv_w_raster <- raster(tps_alpha_10_elv_w)
res(tps_alpha_10_elv_w_raster)

gridded(tps_alpha_11_elv_w) <- ~longitude + latitude
tps_alpha_11_elv_w_raster <- raster(tps_alpha_11_elv_w)
res(tps_alpha_11_elv_w_raster)

gridded(tps_alpha_12_elv_w) <- ~longitude + latitude
tps_alpha_12_elv_w_raster <- raster(tps_alpha_12_elv_w)
res(tps_alpha_12_elv_w_raster)


gridded(tps_alpha_4_elv_w_cut) <- ~longitude + latitude
tps_alpha_4_elv_w_cut_raster <- raster(tps_alpha_4_elv_w_cut)
res(tps_alpha_4_elv_w_cut_raster)



gridded(tps_gdd_1_elv_w) <- ~longitude + latitude
tps_gdd_1_elv_w_raster <- raster(tps_gdd_1_elv_w)
res(tps_gdd_1_elv_w_raster)



#checking resolution of bookss from Wei for GAMs

bookss_1<-sp::SpatialPixelsDataFrame(points =points , data = bookss@data, tolerance = 3.31759e-06)#not working
coordinates(bookss) <-~Long + Lat
gridded(bookss) <- TRUE
points <- SpatialPoints(bookss[,c('Long','Lat')])
bookss_1<- SpatialPixelsDataFrame(points, tolerance = 3.31759e-06, points@data)#not working
coordinates


gridded(bookss) <- ~Long + Lat
# suggested tolerance minimum: 3.31759e-06 
# Error in points2grid(points, tolerance, round) : 
#   dimension 1 : coordinate intervals are not constant
bookss <- raster(bookss)
res(bookss)
dd=abs(diff(bookss$Lat))
dd <- as.data.frame(dd)
max(abs(diff(bookss$Lat)))

dd=abs(diff(bookss$Long))
dd <- as.data.frame(dd)
