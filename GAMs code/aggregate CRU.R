#aggregate CRU from 0.5 degree to 1 degree

require(raster)

# Create spatial point data frame
CRU_sp <- CRU
coordinates(CRU_sp) <- ~lon + lat

gridded(CRU_sp) <- TRUE

res(tmp)
# Stack each variable as a band in the raster:
tmp <- raster(CRU_sp, layer = 1)
for (i in 2:8) {
  tmp <- stack(tmp, raster(CRU_sp, layer = i))
}
CRU_sp<- tmp

# Aggregate to 1 degree resolution (6 times as coarse):
CRU_sp <- aggregate(CRU_sp, fact = 2, fun = mean)

# Get it back to a dataframe
coords <- as.matrix(coordinates(CRU_sp))
CRU_sp <- data.frame(Longitude = coords[, 1], Latitude = coords[, 2],
                 as.data.frame(CRU_sp))



save(tps_alpha_1_elv_w, file='tps_alpha_1_elv_w.rdata')
save(tps_alpha_8_elv_w, file='tps_alpha_8_elv_w.rdata')


library(raster)

#get some sample data

gridded(CRU_sp) <- ~lon + lat
CRU_raster <- raster(CRU_sp)
res(CRU_raster)
#[1] 0.5 0.5


#aggregate from 0.5*0.5 resolution to 1*1 (factor = 2)
CRU_raster_aggregate <- aggregate(CRU_raster, fact=2)
res(CRU_raster_aggregate)
#[1] 1 1


# Aggregate to 1 degree resolution (4 times as coarse):
r <- aggregate(r, fact = 4, fun = mean)


# Get it back to a dataframe
coords <- as.matrix(coordinates(CRU_raster_aggregate))
CRU_aggregate <- data.frame(Longitude = coords[, 1], Latitude = coords[, 2],
                 as.data.frame(CRU_raster_aggregate))




coordinates(tps_alpha_1_elv_w_cut) <- ~longitude + latitude
gridded(tps_alpha_1_elv_w_cut) <- TRUE
gridded(tps_alpha_1_elv_w_cut) <- ~lon + lat

# Convert to simple feature

coordinates(tps_alpha_10_elv_w_cut) <- ~longitude + latitude
tps_alpha_10_elv_w_cut_sp <- st_as_sf(tps_alpha_10_elv_w_cut)


# Set projection based on the epsg code
st_crs(tps_alpha_10_elv_w_cut_sp) <- 4326


tps_alpha_10_elv_w_cut_raster <- raster(tps_alpha_10_elv_w_cut_sp)
res(tps_alpha_10_elv_w_cut_raster)
#[1] 0.08333334 0.08333334

coordinates(tps_alpha_1_elv_w) <- ~longitude + latitude
res(raster(st_as_sf(tps_alpha_1_elv_w)))
#[1] 5.341667 3.816667
#[1] 5.483333 3.950000
#these are the resolution of cut TPS output

#aggregate from 0.08333334*0.08333334 resolution to 0.5*0.5 (factor = 6)
tps_alpha_1_elv_w_raster_aggregate <- aggregate(tps_alpha_1_elv_w_raster, fact=6)
res(tps_alpha_1_elv_w_raster_aggregate)
#[1] 1 1



# Get it back to a dataframe
coords <- as.matrix(coordinates(tps_alpha_1_elv_w_raster_aggregate))
tps_alpha_1_elv_w_a <- data.frame(Longitude = coords[, 1], Latitude = coords[, 2],
                            as.data.frame(tps_alpha_1_elv_w_raster_aggregate))


