#adding elevation using ETOPO instead of Amazon service

#reading in last saved environment
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new site.RData")


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map/4DTPS with elevation new.RData")

save.image(file = "4DTPS with elevation new.RData")
#updating smpds package
remotes::install_github("special-uor/smpds")

# New Tps ----
 `%>%` <- magrittr::`%>%`
# subset1 <-
#   "~/Downloads/subset1.csv" %>%
#   readr::read_csv() %>%
#   dplyr::select(age:elv_label)


#1-1.2ka
# Filter entities with missing elevations
subset1_2 <- subset1 %>%
  dplyr::filter(!is.na(elv))

BREAKS <- c(-Inf, -28, -24, -20, -16, -12, -8, -4, +Inf)
CPUS <- 10
RESOLUTION <- 1
subset1_3 <- subset1_2 %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(breaks = BREAKS)
  )
#rm(range)
## Without elevations ----
tps_output_3 <- subset1_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )


save(tps_output_3, file='tps_output_3.rdata')
load('tps_output_3.rdata')
## With elevation ----
### Elevations as independent variable ----
tps_output_3_elv_independent <- subset1_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

# ### Elevation as linear covariate ----
# tps_output_3_elv_covariate <- subset1_2 %>%
#   smpds::tps(
#     var = "mean_Tmin",
#     z_var = "elv",
#     z_mode = "covariate",
#     z_ref = "~/Downloads/DEM_geotiff/alwdgg.tif",
#     cpus = CPUS
#   )

#The new option here is z_ref which takes the path to a dataset like ETOPO5, which you can download from here https://www.eea.europa.eu/data-and-maps/data/world-digital-elevation-model-etopo5/zip[.]eographic-tag-image-file-format-raster-data/at_download/file (or https://www.eea.europa.eu/data-and-maps/data/world-digital-elevation-model-etopo5). 
#Just make sure to download the file in GeoTIFF raster format .TIF.
#There's no need to specify RESOLUTION as we did before, as the code will use whatever resolution the z_ref object has, so if you want a higher resolution, then you could use ETOPO1, but I think ETOPO5 should work just fine.





##############################################################

#Finally, you can create plots for all three cases:
#rm(range)
# tps_output_3 %>%
#   smpds::plot_climate_tiles(var = "Tmin_anomaly",
#                             units = "\u00B0C",
#                             fill_scale =
#                               ggplot2::scale_fill_manual(
#                                 name = toupper("Tmin_anomaly"),
#                                 values =
#                                   wesanderson::wes_palette("Zissou1",
#                                                            9,
#                                                            type = "continuous")
#                               ),
#                             xlim = range(.$longitude, na.rm = TRUE),
#                             ylim = range(.$latitude, na.rm = TRUE),
#                             .overlay_data = subset1_2)
# 
# 
# 
# tps_output_3 %>%
#   smpds::plot_climate_tiles(var = "Tmin_anomaly",
#                             units = "\u00B0C",
#                             xlim = range(.$longitude, na.rm = TRUE),
#                             ylim = range(.$latitude, na.rm = TRUE))
# 

RColorBrewer::display.brewer.all()
#And so, for example, to have blues and reds, you can run the following:
blues <- RColorBrewer::brewer.pal(4, "Blues")
reds <- RColorBrewer::brewer.pal(4, "Reds")
#And if you want to see them:
RColorBrewer::display.brewer.pal(4, "Blues")
RColorBrewer::display.brewer.pal(4, "Reds")

RColorBrewer::display.brewer.pal(9, "RdBu")
White<-c("#FFFFFF")#adding white colour as 0
#Now, since you want the blues to be darker as they get colder, we can reverse the output from RColorBrewer::brewer.pal:
blues_rev <- rev(RColorBrewer::brewer.pal(4, "Blues"))
#And if you concatenate those two vectors, then you obtain your colours:
COLOURS <- c(blues_rev,White, reds)
COLOURS
RColorBrewer::display.brewer.pal(COLOURS)

summary(tps_output_3$Tmin_anomaly)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1.31542  0.08984  0.31675  0.25388  0.47987  2.02155 

# tps_output_3 %>%
#   dplyr::mutate(
#     Tmin_anomaly = Tmin_anomaly %>%
#       cut(
#         breaks = c(-Inf, -10, -5, -1, 0, 1, 5, 10, Inf),
#         labels = c("<-10", "-5", "-1", "0", "1", "5", "10", "10+"),
#       )
#   ) %>%
#   smpds::plot_climate_tiles(var = "Tmin_anomaly",
#                             units = "\u00B0C",
#                             fill_scale =
#                               ggplot2::scale_fill_manual(
#                                 values = c(COLOURS),
#                               ),
#                             xlim = range(.$longitude, na.rm = TRUE),
#                             ylim = range(.$latitude, na.rm = TRUE))

#using original colour.
#change key instead of colour
library(ggplot2)
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")


p<-tps_output_3 %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -1.3,-1.25,  -1,-0.001,0.001,1,1.5, 2, Inf),
        labels = c("<-1.3", "-1.3","-1.25","-1~0",  "0","0~1","1","1.5",  "2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))

p
ggsave(file="1-1.2ka Map of Tmin_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
#change xlim ylim so it is only focused on the region we are interested in
#-12 xlim to get rid of iceland, 45 to draw a line between the Black sea and the Caspian Sea
#73 ylim to go just north of Scandinavia
p<-tps_output_3 %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -1.3,-1.25,  -1,-0.001,0.001,1,1.5, 2, Inf),
        labels = c("<-1.3", "-1.3","-1.25","-1~0",  "0","0~1","1","1.5",  "2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))

p
ggsave(file="1-1.2ka Map of Tmin_anomaly colour 1 cut out.jpeg",p,width=22,height=11)




#change colour instead of key
reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(4, "Blues"))
COLOURS <- c(blues_rev,White, reds)

tps_output_3 %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -1, 0, 1,1.5, 2, Inf),
        labels = c("<-1", "-1","0", "1","1.5", "2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))



#with elevation
summary(tps_output_3_elv_independent$Tmin_anomaly)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -6.14138 -0.20914  0.31169  0.03814  0.53857  1.95854 

reds <- RColorBrewer::brewer.pal(3, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(7, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_output_3_elv_independent %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -6,-5, -4,-3,-2, -1,-0.001,0.001,1, Inf),
        labels = c("<-6", "-6","-5","-4","-3","-2","-1~0",  "0","0~1",  "1+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))

p
ggsave(file="1-1.2ka Map of Tmin_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_output_3_elv_independent %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -6,-5, -4,-3,-2, -1,-0.001,0.001,1, Inf),
        labels = c("<-6", "-6","-5","-4","-3","-2","-1~0",  "0","0~1",  "1+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="1-1.2ka Map of Tmin_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_output_3_elv_independent_cut<- tps_output_3_elv_independent  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 687896 left
190994/687896
#0.2776495

summary(tps_output_3_elv_independent_cut$Tmin_anomaly)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.9771 -0.1158  0.3627  0.3131  0.7988  1.9585 

reds <- RColorBrewer::brewer.pal(3, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
COLOURS <- c(blues_rev,White, reds)

p<-tps_output_3_elv_independent_cut %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -2, -1,-0.001,0.001,1, Inf),
        labels = c("<-2", "-2","-1~0",  "0","0~1",  "1+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))

p
ggsave(file="1-1.2ka Map of Tmin_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)


#  scale_fill_gradientn(
#    colours=colours,
#    values=val.remap,
#    breaks=quantile.vals,# Necessary to get legend values spread appropriately
#    guide="legend"
#"red", "white", "blue"
# get_col <- colorRamp(c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red"))  # make fun to interpolate colors
# quantiles <- (0:6) / 6                            # how many quantiles we want to map 
# quantile.vals <- quantile(subset6$Tmax_anomaly, quantiles, names=F,na.rm=TRUE)# the values for each quantile
# colours <- rgb(get_col(quantiles), max=255)       # 7 evenly interpolated colors 
# val.remap <- (quantile.vals - min(subset6[["Tmax_anomaly"]],na.rm=TRUE))/  diff(range(subset6[["Tmax_anomaly"]]))                                # The values corresponding to the quantiles
# 
# tps_output_4 %>%
#   smpds::plot_climate_tiles(var = "Tmax_anomaly",
#                             units = "\u00B0C",
#                             fill_scale =
#                               ggplot2::scale_colour_gradientn(
#                                 colours=colours,
#                                 values=val.remap,
#                                 breaks=quantile.vals,# Necessary to get legend values spread appropriately
#                                 guide="legend"),
#                             xlim = range(.$longitude, na.rm = TRUE),
#                             ylim = range(.$latitude, na.rm = TRUE))
# 
# 
# library(ggplot2)
# splits <- 7     # should be odd number
# mid.point <- 0
# pos.vals <- subset6$Tmax_anomaly[subset6$Tmax_anomaly > mid.point]
# neg.vals <- subset6$Tmax_anomaly[subset6$Tmax_anomaly < mid.point]
# pos.quants <- quantile(c(mid.point, pos.vals), 0:((splits - 1) / 2) / ((splits - 1) / 2), names=F,na.rm = TRUE)
# neg.quants <- quantile(c(mid.point, neg.vals), 0:((splits - 1) / 2) / ((splits - 1) / 2), names=F,na.rm = TRUE)
# quants <- c(neg.quants, pos.quants[-1])  # drop of the mid-point from pos.quants since otherwise double counted
# 
# get_col <- colorRamp(c("red", "white", "blue"))  # make fun to interpolate colors
# colours <- rgb(get_col(0:(splits - 1)/(splits - 1)), max=255)       # 7 evenly interpolated colors 
# val.remap <- (quants - min(quants)) / 
#   diff(range(quants))                                # The values corresponding to the quantiles
# 
# tps_output_4 %>%
#   smpds::plot_climate_tiles(var = "Tmax_anomaly",
#                             units = "\u00B0C",
#                             fill_scale =
#                               ggplot2::scale_colour_gradientn(
#                                 colours=colours,
#                                 values=val.remap,
#                                 breaks=quants,# Necessary to get legend values spread appropriately
#                                 guide="legend"),
#                             xlim = range(.$longitude, na.rm = TRUE),
#                             ylim = range(.$latitude, na.rm = TRUE))
# 
# scale_fill_gradientn(colours = c("white", "green", "red"), values = c(0,0.1,1))
# tps_output_4 %>%
#   smpds::plot_climate_tiles(var = "Tmax_anomaly",
#                             units = "\u00B0C",
#                             fill_scale =
#                               ggplot2::scale_colour_gradientn(
#                                 colours=c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red"),
#                                 values=c(0,0.1,1)),
#                             xlim = range(.$longitude, na.rm = TRUE),
#                             ylim = range(.$latitude, na.rm = TRUE))
# 
# tps_output_3_elv_independent %>%
#   smpds::plot_climate_tiles(var = "Tmin_anomaly",
#                             units = "\u00B0C",
#                             xlim = range(.$longitude, na.rm = TRUE),
#                             ylim = range(.$latitude, na.rm = TRUE))
# 
# 
# tps_output_3_elv_covariate %>%
#   smpds::plot_climate_tiles(var = "Tmin_anomaly",
#                             units = "\u00B0C",
#                             xlim = range(.$longitude, na.rm = TRUE),
#                             ylim = range(.$latitude, na.rm = TRUE))
# 
# 
# 
# tps_output_3_elv_independent %>%
#   smpds::plot_climate_tiles(var = "Tmin_anomaly",
#                             units = "\u00B0C",
#                             fill_scale =
#                               ggplot2::scale_fill_manual(
#                                 name = toupper("Tmin_anomaly"),
#                                 values =
#                                   wesanderson::wes_palette("Zissou1",
#                                                            9,
#                                                            type = "continuous")
#                               ),
#                             xlim = range(.$longitude, na.rm = TRUE),
#                             ylim = range(.$latitude, na.rm = TRUE),
#                             .overlay_data = subset1_2)
# 
# tps_output_3_elv_covariate %>%
#   smpds::plot_climate_tiles(var = "Tmin_anomaly",
#                             units = "\u00B0C",
#                             fill_scale =
#                               ggplot2::scale_fill_manual(
#                                 name = toupper("Tmin_anomaly"),
#                                 values =
#                                   wesanderson::wes_palette("Zissou1",
#                                                            9,
#                                                            type = "continuous")
#                               ),
#                             xlim = range(.$longitude, na.rm = TRUE),
#                             ylim = range(.$latitude, na.rm = TRUE),
#                             .overlay_data = subset1_2)
# 
# 
# 
# 

#6-6.2ka
subset6_2 <- subset6 %>%
  dplyr::filter(!is.na(elv))

###############Tmin
#rm(range)
## Without elevations ----
tps_output_6 <- subset6_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )


save(tps_output_6, file='tps_output_6.rdata')


summary(tps_output_6$Tmin_anomaly)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3.9670 -1.7405 -0.7619 -0.6741  0.5253  4.7815

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")

reds <- RColorBrewer::brewer.pal(5, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(4, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_output_6 %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -3,-2,  -1,-0.001,0.001,1,2, 3,4, Inf),
        labels = c("<-3", "-3","-2","-1~0",  "0","0~1","1","2","3",  "4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))


ggsave(file="6-6.2ka Map of Tmin_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_output_6 %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -3,-2,  -1,-0.001,0.001,1,2, 3,4, Inf),
        labels = c("<-3", "-3","-2","-1~0",  "0","0~1","1","2","3",  "4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of Tmin_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----
tps_output_6_elv_independent <- subset6_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )


summary(tps_output_6_elv_independent$Tmin_anomaly)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -4.7076 -1.7476 -0.8095 -0.7103  0.4294  4.7995 

reds <- RColorBrewer::brewer.pal(5, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(5, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_output_6_elv_independent %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -4,-3, -2, -1,-0.001,0.001,1,2,3,4, Inf),
        labels = c("<-4", "-4","-3","-2","-1~0",  "0","0~1", "1", "2","3","4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))

p
ggsave(file="6-6.2ka Map of Tmin_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_output_6_elv_independent %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -4,-3, -2, -1,-0.001,0.001,1,2,3,4, Inf),
        labels = c("<-4", "-4","-3","-2","-1~0",  "0","0~1", "1", "2","3","4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of Tmin_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_output_6_elv_independent_cut<- tps_output_6_elv_independent  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 687896 left

summary(tps_output_6_elv_independent_cut$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-3.6197 -0.7970  0.2333  0.2086  1.1813  3.5149 

reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(4, "Blues"))
COLOURS <- c(blues_rev,White, reds)

p<-tps_output_6_elv_independent_cut %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -3, -2, -1,-0.001,0.001,1,2,3, Inf),
        labels = c("<-3", "-3","-2","-1~0",  "0","0~1", "1", "2","3+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))

p
ggsave(file="6-6.2ka Map of Tmin_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)



########################################################################################################
####################################  Tmax ##################################################
########################################################################################################
#rm(range)
## Without elevations ----
tps_tmax_6 <- subset6_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )


#save(tps_tmax_6, file='tps_tmax_6.rdata')


summary(tps_tmax_6$Tmax_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2.47084 -0.56057 -0.12489  0.08766  0.69526  4.31515 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")

reds <- RColorBrewer::brewer.pal(5, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_tmax_6 %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf, -2,  -1,-0.001,0.001,1,2, 3,4, Inf),
        labels = c("<-2","-2","-1~0",  "0","0~1","1","2","3",  "4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))


ggsave(file="6-6.2ka Map of Tmax_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_tmax_6 %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf, -2,  -1,-0.001,0.001,1,2, 3,4, Inf),
        labels = c("<-2","-2","-1~0",  "0","0~1","1","2","3",  "4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of Tmax_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----
tps_tmax_6_elv_independent <- subset6_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )


summary(tps_tmax_6_elv_independent$Tmax_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2.68789 -0.57243 -0.17000  0.07726  0.72774  4.33371 

reds <- RColorBrewer::brewer.pal(5, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_tmax_6_elv_independent %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,  -2, -1,-0.001,0.001,1,2,3,4, Inf),
        labels = c("<-2", "-2","-1~0",  "0","0~1", "1", "2","3","4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))

p
ggsave(file="6-6.2ka Map of Tmax_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_tmax_6_elv_independent %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,  -2, -1,-0.001,0.001,1,2,3,4, Inf),
        labels = c("<-2", "-2","-1~0",  "0","0~1", "1", "2","3","4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of Tmax_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_tmax_6_elv_independent_cut<- tps_tmax_6_elv_independent  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 687896 left

summary(tps_tmax_6_elv_independent_cut$Tmax_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1.7567  0.1677  0.8251  1.0014  1.7958  4.3337  

reds <- RColorBrewer::brewer.pal(5, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
blues_rev
COLOURS <- c("#9ECAE1", "#DEEBF7",White, reds)

p<-tps_tmax_6_elv_independent_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,  -1,-0.001,0.001,1,2,3, 4,Inf),
        labels = c("<-1", "-1~0",  "0","0~1", "1", "2","3","4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))

p
ggsave(file="6-6.2ka Map of Tmax_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)



########################################################################################################
####################################  gdd ##################################################
########################################################################################################
#rm(range)
## Without elevations ----

subset6_3 <- subset1 %>%
  dplyr::filter(!is.na(gdd))

library(smpds)
tps_gdd_6 <- subset6_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )


#save(tps_gdd_6, file='tps_gdd_6.rdata')
load('tps_gdd_6.rdata')

summary(tps_gdd_6$gdd_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2.47084 -0.56057 -0.12489  0.08766  0.69526  4.31515 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")

reds <- RColorBrewer::brewer.pal(5, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_gdd_6 %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf, -2,  -1,-0.001,0.001,1,2, 3,4, Inf),
        labels = c("<-2","-2","-1~0",  "0","0~1","1","2","3",  "4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))


ggsave(file="6-6.2ka Map of gdd_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_gdd_6 %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf, -2,  -1,-0.001,0.001,1,2, 3,4, Inf),
        labels = c("<-2","-2","-1~0",  "0","0~1","1","2","3",  "4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of gdd_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----
tps_gdd_6_elv_independent <- subset6_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )


summary(tps_gdd_6_elv_independent$gdd_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2.68789 -0.57243 -0.17000  0.07726  0.72774  4.33371 

reds <- RColorBrewer::brewer.pal(5, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_gdd_6_elv_independent %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,  -2, -1,-0.001,0.001,1,2,3,4, Inf),
        labels = c("<-2", "-2","-1~0",  "0","0~1", "1", "2","3","4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))

p
ggsave(file="6-6.2ka Map of gdd_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_gdd_6_elv_independent %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,  -2, -1,-0.001,0.001,1,2,3,4, Inf),
        labels = c("<-2", "-2","-1~0",  "0","0~1", "1", "2","3","4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of gdd_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_gdd_6_elv_independent_cut<- tps_gdd_6_elv_independent  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 687896 left

summary(tps_gdd_6_elv_independent_cut$gdd_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1.7567  0.1677  0.8251  1.0014  1.7958  4.3337  

reds <- RColorBrewer::brewer.pal(5, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
blues_rev
COLOURS <- c("#9ECAE1", "#DEEBF7",White, reds)

p<-tps_gdd_6_elv_independent_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,  -1,-0.001,0.001,1,2,3, 4,Inf),
        labels = c("<-1", "-1~0",  "0","0~1", "1", "2","3","4+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))

p
ggsave(file="6-6.2ka Map of gdd_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)
