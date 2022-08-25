#aggregate TPS from 0.08333334 degree to 0.5 degree

require(raster)

# Create spatial point data frame
load("tps_alpha_1_elv_w.rdata")
tps_alpha_1_elv_w_sp <- tps_alpha_1_elv_w
coordinates(tps_alpha_1_elv_w_sp) <- ~longitude + latitude

gridded(tps_alpha_1_elv_w_sp) <- TRUE
#change it from spatial points to spatial pixels


# Stack each variable as a band in the raster:there is only one column, so this step is not needed.
#only change it to raster because there is only one column
tps_alpha_1_elv_w_sp <- raster(tps_alpha_1_elv_w_sp, layer = 1)


# Aggregate from 0.08333334 degree to 0.5 degree resolution (6 times as coarse):
tps_alpha_1_elv_w_sp <- aggregate(tps_alpha_1_elv_w_sp, fact = 6, fun = mean)
res(tps_alpha_1_elv_w_sp)
#[1] 0.5 0.5

# Get it back to a dataframe
coords <- as.matrix(coordinates(tps_alpha_1_elv_w_sp))
tps_alpha_1_elv_w_a <- data.frame(Longitude = coords[, 1], Latitude = coords[, 2],
                     as.data.frame(tps_alpha_1_elv_w_sp))
#28896 obs



write.csv(tps_alpha_1_elv_w_a, "aggregated weighted 3DTPS output of alpha between 1-1.2ka.csv")



length(which(is.na(tps_alpha_1_elv_w_a)))
#[1] 8720


summary(tps_alpha_1_elv_w_a)
summary(tps_alpha_1_elv_w)


summary(tps_alpha_1_elv_w$alpha_anomaly)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.123269 -0.048007 -0.012706 -0.012813  0.008511  0.189755 
summary(tps_alpha_1_elv_w_a$alpha_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.123  -0.047  -0.013  -0.013   0.008   0.143    8720 


tps_alpha_1_elv_w_cut<- tps_alpha_1_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#182219 out of 687896 left
tps_alpha_1_elv_w_a_cut<- tps_alpha_1_elv_w_a  %>%dplyr::filter(Longitude>-12&Longitude<45&Latitude<73)
#9576 out of 28896 left
CRU_cut<- CRU  %>%dplyr::filter(lon>-12&lon<45&lat<73)
#5919 out of 22730 left

summary(tps_alpha_1_elv_w_cut$alpha_anomaly)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.112964 -0.024914 -0.002291 -0.009835  0.008724  0.088372 
summary(tps_alpha_1_elv_w_a_cut$alpha_anomaly)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -0.111  -0.024  -0.003  -0.010   0.008   0.073    3722 

summary(CRU$alpha)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.001389 0.430214 0.786790 0.694853 0.967092 1.250195 
summary(CRU_cut$alpha)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.001389 0.514330 0.838903 0.728777 0.987346 1.250195 




# Create ID for my_df_1 and my_df_2 based on row id
# This step is not required, just help me to better distinguish each point
CRU_cut_1 <- CRU_cut %>% mutate(ID1 = row.names(.))
tps_alpha_1_elv_w_a_cut_1 <- tps_alpha_1_elv_w_a_cut %>% mutate(ID2 = row.names(.))

# Create spatial point data frame
CRU_cut_1_sp <- CRU_cut_1
coordinates(CRU_cut_1_sp) <- ~lon + lat

tps_alpha_1_elv_w_a_cut_1_sp <- tps_alpha_1_elv_w_a_cut_1
coordinates(tps_alpha_1_elv_w_a_cut_1_sp) <- ~Longitude + Latitude


# Convert to simple feature
CRU_cut_1_sp <- st_as_sf(CRU_cut_1_sp)
tps_alpha_1_elv_w_a_cut_1_sp <- st_as_sf(tps_alpha_1_elv_w_a_cut_1_sp)

# Set projection based on the epsg code
st_crs(CRU_cut_1_sp) <- 4326
st_crs(tps_alpha_1_elv_w_a_cut_1_sp) <- 4326

# Calculate the distance
m_dist <- st_distance(CRU_cut_1_sp, tps_alpha_1_elv_w_a_cut_1_sp)

# Filter for the nearest
near_index <- apply(m_dist, 1, order)[1, ]

# Based on the index in near_index to select the rows in my_df_2
# Combine with my_df_1
my_df_final <- cbind(CRU_cut_1_sp, tps_alpha_1_elv_w_a_cut_1_sp[near_index, ])

my_df_final$distance<-m_dist
my_df_final$Tmin_calculated<-my_df_final$Tmin_anomaly+my_df_final$Tmin
library(units)
my_df_final$distance<-drop_units(my_df_final$distance)

sort1.my_df_final <- my_df_final[order(distance) , ]


summary(my_df_final$distance)







tps_alpha_1_elv_w_merge<-rbind(tps_alpha_1_elv_w_a_cut,CRU_cut)
greens <- RColorBrewer::brewer.pal(6, "Greens")
White<-c("#FFFFFF")#adding white colour as 0
browns_rev <- rev(RColorBrewer::brewer.pal(6, "YlOrBr"))
#RColorBrewer::display.brewer.pal(4, "YlOrBr")
COLOURS <- c(browns_rev, greens)
COLOURS

p<-CRU %>%
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
                            xlim = c(min(.$lon, na.rm = TRUE),max(.$lon, na.rm = TRUE)),
                            ylim = c(min(.$lat, na.rm = TRUE), max(.$lat, na.rm = TRUE)), )
p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/CRU plots")

ggsave(file="modern Map of alpha.jpeg",p,width=22,height=11)

p<-CRU_cut %>%
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
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )
p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/CRU plots")

ggsave(file="modern cut Map of alpha.jpeg",p,width=22,height=11)





p<-CRU %>%
  smpds::plot_climate_tiles(var = "alpha",
                            units = "\u00B0C",
                            xlim = c(min(.$lon, na.rm = TRUE),max(.$lon, na.rm = TRUE)),
                            ylim = c(min(.$lat, na.rm = TRUE), max(.$lat, na.rm = TRUE)), )

ggsave(file="modern Map of alpha 2.jpeg",p,width=22,height=11)


p<-CRU_cut %>%
  smpds::plot_climate_tiles(var = "alpha",
                            units = "\u00B0C",
                            xlim = c(min(.$lon, na.rm = TRUE),max(.$lon, na.rm = TRUE)),
                            ylim = c(min(.$lat, na.rm = TRUE), max(.$lat, na.rm = TRUE)), )

ggsave(file="modern cut Map of alpha 2.jpeg",p,width=22,height=11)



bookss_1 = bookss[,c(4,5,6,7,8,9)]
# library(dplyr)
# bookss_1=bookss %>%
#   group_by(Entity.name) %>%
#   filter(n_distinct(Lat)==1) %>%
#   distinct()
# 
# bookss_1=bookss_1 %>%
#   filter(n_distinct(Lat)==1) %>%
#   distinct()

bookss_1=bookss_1[!duplicated(bookss_1$Lat), ]
bookss_1=bookss_1[!duplicated(bookss_1$Long), ]

p<-bookss_1 %>%
  smpds::plot_climate_tiles(var = "Tmin",
                            units = "\u00B0C",
                             )

#trying to visualise bookss from Wei
library(ggplot2)
library(viridis)
library(sf)
bookss_sf<-st_as_sf(bookss, coords = c('Long', 'Lat'),crs = 4326)
ggplot1=ggplot() + 
  geom_sf(data = subset(bookss_sf, !is.na(Tmin)),  mapping = aes(x =Tmin, color=Tmin), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=FALSE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(min(bookss$Long, na.rm = TRUE),max(bookss$Long, na.rm = TRUE)), ylim = c(min(bookss$Lat, na.rm = TRUE), max(bookss$Lat, na.rm = TRUE)), expand = FALSE) 
print(ggplot1)




library(raster)

#get some sample data

gridded(tps_alpha_9_elv_w) <- ~longitude + latitude
tps_alpha_9_elv_w_raster <- raster(tps_alpha_9_elv_w)
res(tps_alpha_9_elv_w_raster)
#[1] 0.08333334 0.08333334


#aggregate from 0.08333334*0.08333334 degrees (5 minutes) resolution to 0.5*0.5 degrees (factor = 6)
tps_alpha_9_elv_w_raster_aggregate <- aggregate(tps_alpha_9_elv_w_raster, fact=6)
res(tps_alpha_9_elv_w_raster_aggregate)
#[1] 0.5 0.5



# Get it back to a dataframe
coords <- as.matrix(coordinates(tps_alpha_9_elv_w_raster_aggregate))
tps_alpha_9_elv_w_a <- data.frame(Longitude = coords[, 1], Latitude = coords[, 2],
                            as.data.frame(tps_alpha_9_elv_w_raster_aggregate))


length(which(is.na(tps_alpha_9_elv_w_a)))
#[1] 3251


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with squared weights/calculation r data")
load("tps_alpha_9_elv_w.rdata")
#535948 rows

535948/6
89324.67

coords <- as.matrix(coordinates(tps_alpha_9_elv_w_a))
tps_alpha_9_elv_w <- data.frame(Longitude = coords[, 1], Latitude = coords[, 2],
                                  as.data.frame(tps_alpha_9_elv_w_a))
