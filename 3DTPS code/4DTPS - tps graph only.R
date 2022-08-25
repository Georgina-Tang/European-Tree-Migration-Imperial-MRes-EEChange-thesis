########################################################################################################
####################################  4DTPS - graph only ##################################################
########################################################################################################


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map/4DTPS with elevation new.RData")

save.image(file = "4DTPS with elevation new.RData")
save.image(file = "4DTPS with elevation new graph.RData")
#This R script is graph only

#3-3.2ka Tmin
load('tps_tmin_3.rdata')

summary(tps_tmin_3$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2.5878 -0.6724  0.1186  0.2885  1.3297  2.6650 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")

# Colour scales should be chosen with the following criteria in mind:
# 1 The zero anomaly isoline should be very obvious, corresponding to a break in colour. For example, positive anomalies of temperature might be represented by red colours (deeper red with increasing value) and negative anomalies, similarly, by blue colours. 
#For moisture index values, use green for positive anomalies and brown for negative anomalies.
# 2 Each shade should correspond to a sensible range of anomaly values. For example, you might choose one colour for MTCO anomalies of 0 degree to 0.5 degree, a deeper colour for 0.5 degree to 1 degree, and so on.


reds <- RColorBrewer::brewer.pal(6, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(6, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_tmin_3 %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -2.5,  -2,-1.5,-1,-0.5,-0.001,0.001,0.5,1, 1.5,2,2.5, Inf),
        labels = c("<-2.5","-2.5","-2","-1.5","-1","-0.5~0",  "0","0~0.5","0.5","1","1.5","2",  "2.5+"),
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


ggsave(file="3-3.2ka Map of Tmin_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_tmin_3 %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -2.5,  -2,-1.5,-1,-0.5,-0.001,0.001,0.5,1, 1.5,2,2.5, Inf),
        labels = c("<-2.5","-2.5","-2","-1.5","-1","-0.5~0",  "0","0~0.5","0.5","1","1.5","2",  "2.5+"),
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


ggsave(file="3-3.2ka Map of Tmin_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----

load('tps_tmin_3_elv.rdata')


summary(tps_tmin_3_elv$Tmin_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-10.1289  -0.5002   0.3110   0.1318   1.3322   3.3648

reds <- RColorBrewer::brewer.pal(3, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(7, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_tmin_3_elv %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf,-9,-7.5,-6,-4.5 ,-3,  -1.5,-0.001,0.001,1.5, 3, Inf),
        labels = c("<-9","-9","-7.5","-6","-4.5" ,"-3","-1.5~0",  "0","0~1.5","1.5",  "3+"),
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


ggsave(file="3-3.2ka Map of Tmin_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_tmin_3_elv %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf,-9,-7.5,-6,-4.5 ,-3,  -1.5,-0.001,0.001,1.5, 3, Inf),
        labels = c("<-9","-9","-7.5","-6","-4.5" ,"-3","-1.5~0",  "0","0~1.5","1.5",  "3+"),
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


ggsave(file="3-3.2ka Map of Tmin_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_tmin_3_elv_cut<- tps_tmin_3_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_tmin_3_elv_cut$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-5.5485 -0.2466  0.6677  0.5185  1.4358  3.3648 

reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(6, "Blues"))
COLOURS <- c(blues_rev,White, reds)



p<-tps_tmin_3_elv_cut %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3 ,-2,  -1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-5","-5","-4","-3" ,"-2","-1~0",  "0","0~1","1","2" , "3+"),
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


ggsave(file="3-3.2ka Map of Tmin_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)


#3-3.2ka Tmax
load('tps_tmax_3.rdata')


summary(tps_tmax_3$Tmax_anomaly)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-3.344118 -0.725799 -0.005147 -0.148146  0.555943  3.573774 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")

reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(4, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_tmax_3 %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-3 ,-2,  -1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-3","-3" ,"-2","-1~0",  "0","0~1","1","2" , "3+"),
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


ggsave(file="3-3.2ka Map of Tmax_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_tmax_3 %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-3 ,-2,  -1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-3","-3" ,"-2","-1~0",  "0","0~1","1","2" , "3+"),
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


ggsave(file="3-3.2ka Map of Tmax_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----

load('tps_tmax_3_elv.rdata')


summary(tps_tmax_3_elv$Tmax_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-5.6539 -0.6140 -0.0843 -0.1677  0.6177  3.5882 

reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(6, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_tmax_3_elv %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3 ,-2,  -1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-5","-5","-4","-3" ,"-2","-1~0",  "0","0~1","1","2" , "3+"),
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


ggsave(file="3-3.2ka Map of Tmax_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_tmax_3_elv %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3 ,-2,  -1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-5","-5","-4","-3" ,"-2","-1~0",  "0","0~1","1","2" , "3+"),
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


ggsave(file="3-3.2ka Map of Tmax_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_tmax_3_elv_cut<- tps_tmax_3_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_tmax_3_elv_cut$Tmax_anomaly)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -3.45587  0.03438  0.51053  0.61532  1.44442  3.58821 


reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(4, "Blues"))
COLOURS <- c(blues_rev,White, reds)



p<-tps_tmax_3_elv_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-3 ,-2,  -1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-3","-3" ,"-2","-1~0",  "0","0~1","1","2" , "3+"),
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


ggsave(file="3-3.2ka Map of Tmax_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)



#3-3.2ka gdd
load('tps_gdd_3.rdata')


summary(tps_gdd_3$gdd_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2163.76  -649.45    25.63   -43.54   493.66  1957.34 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")

# Colour scales should be chosen with the following criteria in mind:
# 1 The zero anomaly isoline should be very obvious, corresponding to a break in colour. For example, positive anomalies of temperature might be represented by red colours (deeper red with increasing value) and negative anomalies, similarly, by blue colours. 
#For moisture index values, use green for positive anomalies and brown for negative anomalies.
# 2 Each shade should correspond to a sensible range of anomaly values. For example, you might choose one colour for MTCO anomalies of 0 degree to 0.5 degree, a deeper colour for 0.5 degree to 1 degree, and so on.


reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(5, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_gdd_3 %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf, -2000,-1500,  -1000,-500,-0.001,0.001,500, 1000,1500, Inf),
        labels = c("<-2000","-2000","-1500","-1000","-500~0",  "0","0~500","500","1000",  "1500+"),
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


ggsave(file="3-3.2ka Map of gdd_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_gdd_3 %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf, -2000,-1500,  -1000,-500,-0.001,0.001,500, 1000,1500, Inf),
        labels = c("<-2000","-2000","-1500","-1000","-500~0",  "0","0~500","500","1000",  "1500+"),
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


ggsave(file="3-3.2ka Map of gdd_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----

load('tps_gdd_3_elv.rdata')


summary(tps_gdd_3_elv$gdd_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2828.74  -654.59    67.51   -80.22   478.07  1929.12 

reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(6, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_gdd_3_elv %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-2500, -2000,-1500,  -1000,-500,-0.001,0.001,500, 1000,1500, Inf),
        labels = c("<-2500","-2500","-2000","-1500","-1000","-500~0",  "0","0~500","500","1000",  "1500+"),
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


ggsave(file="3-3.2ka Map of gdd_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_gdd_3_elv %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-2500, -2000,-1500,  -1000,-500,-0.001,0.001,500, 1000,1500, Inf),
        labels = c("<-2500","-2500","-2000","-1500","-1000","-500~0",  "0","0~500","500","1000",  "1500+"),
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


ggsave(file="3-3.2ka Map of gdd_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_gdd_3_elv_cut<- tps_gdd_3_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_gdd_3_elv_cut$gdd_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1383.48  -176.31    92.15    27.94   242.58  1112.65

reds <- RColorBrewer::brewer.pal(3, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
COLOURS <- c(blues_rev,White, reds)



p<-tps_gdd_3_elv_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,   -1000,-500,-0.001,0.001,500, 1000,Inf),
        labels = c("<-1000","-1000","-500~0",  "0","0~500","500",  "1000+"),
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


ggsave(file="3-3.2ka Map of gdd_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)



#3-3.2ka alpha

load('tps_alpha_3.rdata')


summary(tps_alpha_3$alpha_anomaly)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.213535 -0.040394 -0.005420  0.004531  0.028790  0.293964 


greens <- RColorBrewer::brewer.pal(3, "Greens")
White<-c("#FFFFFF")#adding white colour as 0
browns_rev <- rev(RColorBrewer::brewer.pal(3, "YlOrBr"))
#RColorBrewer::display.brewer.pal(4, "YlOrBr")
COLOURS <- c(browns_rev,White, greens)


p<-tps_alpha_3 %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.2,  -0.1,-0.001,0.001,0.1,0.2, Inf),
        labels = c("<-0.2","-0.2","-0.1~0",  "0","0~0.1","0.1",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))


ggsave(file="3-3.2ka Map of alpha_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_alpha_3 %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.2,  -0.1,-0.001,0.001,0.1,0.2, Inf),
        labels = c("<-0.2","-0.2","-0.1~0",  "0","0~0.1","0.1",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="3-3.2ka Map of alpha_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----

load('tps_alpha_3_elv.rdata')


summary(tps_alpha_3_elv$alpha_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21758 -0.03949 -0.00263  0.01335  0.04571  0.32045 

greens <- RColorBrewer::brewer.pal(4, "Greens")
White<-c("#FFFFFF")#adding white colour as 0
browns_rev <- rev(RColorBrewer::brewer.pal(3, "YlOrBr"))
#RColorBrewer::display.brewer.pal(4, "YlOrBr")
COLOURS <- c(browns_rev,White, greens)


p<-tps_alpha_3_elv %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.2,  -0.1,-0.001,0.001,0.1,0.2,0.3, Inf),
        labels = c("<-0.2","-0.2","-0.1~0",  "0","0~0.1","0.1", "0.2", "0.3+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))


ggsave(file="3-3.2ka Map of alpha_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_alpha_3_elv %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.2,  -0.1,-0.001,0.001,0.1,0.2,0.3, Inf),
        labels = c("<-0.2","-0.2","-0.1~0",  "0","0~0.1","0.1", "0.2", "0.3+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="3-3.2ka Map of alpha_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_alpha_3_elv_cut<- tps_alpha_3_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_alpha_3_elv_cut$alpha_anomaly)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.217582 -0.045824 -0.010270 -0.006264  0.034717  0.239643 

greens <- RColorBrewer::brewer.pal(3, "Greens")
White<-c("#FFFFFF")#adding white colour as 0
browns_rev <- rev(RColorBrewer::brewer.pal(3, "YlOrBr"))
#RColorBrewer::display.brewer.pal(4, "YlOrBr")
COLOURS <- c(browns_rev,White, greens)

p<-tps_alpha_3_elv_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.2,  -0.1,-0.001,0.001,0.1,0.2, Inf),
        labels = c("<-0.2","-0.2","-0.1~0",  "0","0~0.1","0.1", "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="3-3.2ka Map of alpha_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)



#4-4.2ka Tmin
load('tps_tmin_4.rdata')

summary(tps_tmin_4$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.35088  0.09453  0.82097  0.98088  1.95589  3.76006 


reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(5, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_tmin_4 %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -4,  -3,-2,-1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-4","-4","-3","-2","-1~0",  "0","0~1","1","2","3+"),
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


ggsave(file="4-4.2ka Map of Tmin_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_tmin_4 %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf, -4,  -3,-2,-1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-4","-4","-3","-2","-1~0",  "0","0~1","1","2","3+"),
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


ggsave(file="4-4.2ka Map of Tmin_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----

load('tps_tmin_4_elv.rdata')


summary(tps_tmin_4_elv$Tmin_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-7.52998 -0.06525  0.98152  0.71966  1.81831  3.79970 

reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(8, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_tmin_4_elv %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf,-7,-6,-5 ,-4,  -3,-2,-1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-7","-7","-6","-5" ,"-4","-3","-2","-1~0",  "0","0~1","1","2","3+"),
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


ggsave(file="4-4.2ka Map of Tmin_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_tmin_4_elv %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf,-7,-6,-5 ,-4,  -3,-2,-1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-7","-7","-6","-5" ,"-4","-3","-2","-1~0",  "0","0~1","1","2","3+"),
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


ggsave(file="4-4.2ka Map of Tmin_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_tmin_4_elv_cut<- tps_tmin_4_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_tmin_4_elv_cut$Tmin_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-6.27667 -0.05762  0.91706  0.81702  1.89596  3.79970

reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(7, "Blues"))
COLOURS <- c(blues_rev,White, reds)



p<-tps_tmin_4_elv_cut %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf,-6,-5 ,-4,  -3,-2,-1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-6","-6","-5" ,"-4","-3","-2","-1~0",  "0","0~1","1","2","3+"),
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


ggsave(file="4-4.2ka Map of Tmin_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)


#4-4.2ka Tmax
load('tps_tmax_4.rdata')


summary(tps_tmax_4$Tmax_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1.0464 -0.1125  0.2810  0.3850  0.8118  2.5653 

reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(4, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_tmax_4 %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-3 ,-2,  -1,-0.001,0.001,1, 2,2.5, Inf),
        labels = c("<-3","-3" ,"-2","-1~0",  "0","0~1","1","2" , "2.5+"),
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


ggsave(file="4-4.2ka Map of Tmax_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_tmax_4 %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-3 ,-2,  -1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-3","-3" ,"-2","-1~0",  "0","0~1","1","2" , "3+"),
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


ggsave(file="4-4.2ka Map of Tmax_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----

load('tps_tmax_4_elv.rdata')


summary(tps_tmax_4_elv$Tmax_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-5.6539 -0.6140 -0.0843 -0.1677  0.6177  3.5882 

reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(6, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_tmax_4_elv %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3 ,-2,  -1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-5","-5","-4","-3" ,"-2","-1~0",  "0","0~1","1","2" , "3+"),
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


ggsave(file="4-4.2ka Map of Tmax_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_tmax_4_elv %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3 ,-2,  -1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-5","-5","-4","-3" ,"-2","-1~0",  "0","0~1","1","2" , "3+"),
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


ggsave(file="4-4.2ka Map of Tmax_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_tmax_4_elv_cut<- tps_tmax_4_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_tmax_4_elv_cut$Tmax_anomaly)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -3.45587  0.03438  0.51053  0.61532  1.44442  3.58821 


reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(4, "Blues"))
COLOURS <- c(blues_rev,White, reds)



p<-tps_tmax_4_elv_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-3 ,-2,  -1,-0.001,0.001,1, 2,3, Inf),
        labels = c("<-3","-3" ,"-2","-1~0",  "0","0~1","1","2" , "3+"),
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


ggsave(file="4-4.2ka Map of Tmax_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)



#4-4.2ka gdd
load('tps_gdd_4.rdata')


summary(tps_gdd_4$gdd_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2163.76  -649.45    25.63   -43.54   493.66  1957.34 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")

# Colour scales should be chosen with the following criteria in mind:
# 1 The zero anomaly isoline should be very obvious, corresponding to a break in colour. For example, positive anomalies of temperature might be represented by red colours (deeper red with increasing value) and negative anomalies, similarly, by blue colours. 
#For moisture index values, use green for positive anomalies and brown for negative anomalies.
# 2 Each shade should correspond to a sensible range of anomaly values. For example, you might choose one colour for MTCO anomalies of 0 degree to 0.5 degree, a deeper colour for 0.5 degree to 1 degree, and so on.


reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(5, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_gdd_4 %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf, -2000,-1500,  -1000,-500,-0.001,0.001,500, 1000,1500, Inf),
        labels = c("<-2000","-2000","-1500","-1000","-500~0",  "0","0~500","500","1000",  "1500+"),
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


ggsave(file="4-4.2ka Map of gdd_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_gdd_4 %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf, -2000,-1500,  -1000,-500,-0.001,0.001,500, 1000,1500, Inf),
        labels = c("<-2000","-2000","-1500","-1000","-500~0",  "0","0~500","500","1000",  "1500+"),
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


ggsave(file="4-4.2ka Map of gdd_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----

load('tps_gdd_4_elv.rdata')


summary(tps_gdd_4_elv$gdd_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2828.74  -654.59    67.51   -80.22   478.07  1929.12 

reds <- RColorBrewer::brewer.pal(4, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(6, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_gdd_4_elv %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-2500, -2000,-1500,  -1000,-500,-0.001,0.001,500, 1000,1500, Inf),
        labels = c("<-2500","-2500","-2000","-1500","-1000","-500~0",  "0","0~500","500","1000",  "1500+"),
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


ggsave(file="4-4.2ka Map of gdd_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_gdd_4_elv %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-2500, -2000,-1500,  -1000,-500,-0.001,0.001,500, 1000,1500, Inf),
        labels = c("<-2500","-2500","-2000","-1500","-1000","-500~0",  "0","0~500","500","1000",  "1500+"),
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


ggsave(file="4-4.2ka Map of gdd_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_gdd_4_elv_cut<- tps_gdd_4_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_gdd_4_elv_cut$gdd_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1383.48  -176.31    92.15    27.94   242.58  1112.65

reds <- RColorBrewer::brewer.pal(3, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
COLOURS <- c(blues_rev,White, reds)



p<-tps_gdd_4_elv_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,   -1000,-500,-0.001,0.001,500, 1000,Inf),
        labels = c("<-1000","-1000","-500~0",  "0","0~500","500",  "1000+"),
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


ggsave(file="4-4.2ka Map of gdd_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)








#6-6.2ka alpha

load('tps_alpha_6.rdata')


summary(tps_alpha_6$alpha_anomaly)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -0.198329 -0.041657 -0.008268  0.006490  0.049785  0.244765 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")

# Colour scales should be chosen with the following criteria in mind:
# 1 The zero anomaly isoline should be very obvious, corresponding to a break in colour. For example, positive anomalies of temperature might be represented by red colours (deeper red with increasing value) and negative anomalies, similarly, by blue colours. 
#For moisture index values, use green for positive anomalies and brown for negative anomalies.
# 2 Each shade should correspond to a sensible range of anomaly values. For example, you might choose one colour for MTCO anomalies of 0 degree to 0.5 degree, a deeper colour for 0.5 degree to 1 degree, and so on.

greens <- RColorBrewer::brewer.pal(5, "Greens")
White<-c("#FFFFFF")#adding white colour as 0
browns_rev <- rev(RColorBrewer::brewer.pal(4, "YlOrBr"))
#RColorBrewer::display.brewer.pal(4, "YlOrBr")
COLOURS <- c(browns_rev,White, greens)


p<-tps_alpha_6 %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.15,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15,0.2, Inf),
        labels = c("<-0.15","-0.15","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1","0.15",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))


ggsave(file="6-6.2ka Map of alpha_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_alpha_6 %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.15,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15,0.2, Inf),
        labels = c("<-0.15","-0.15","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1","0.15",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of alpha_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----

load('tps_alpha_6_elv.rdata')


summary(tps_alpha_6_elv$alpha_anomaly)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.199251 -0.039328 -0.009127  0.006889  0.049831  0.249803


p<-tps_alpha_6_elv %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.15,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15,0.2, Inf),
        labels = c("<-0.15","-0.15","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1","0.15",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))


ggsave(file="6-6.2ka Map of alpha_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_alpha_6_elv %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.15,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15,0.2, Inf),
        labels = c("<-0.15","-0.15","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1","0.15",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of alpha_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_alpha_6_elv_cut<- tps_alpha_6_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_alpha_6_elv_cut$alpha_anomaly)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.19925 -0.06508 -0.02285 -0.01517  0.02852  0.24980 


p<-tps_alpha_6_elv_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.15,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15,0.2, Inf),
        labels = c("<-0.15","-0.15","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1","0.15",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of alpha_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)







#6-6.2ka gdd
load('tps_gdd_6.rdata')


 summary(tps_gdd_6$gdd_anomaly)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -995.8469 -319.2740    0.2551  -73.2420  165.1985 1222.9777 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")

# Colour scales should be chosen with the following criteria in mind:
# 1 The zero anomaly isoline should be very obvious, corresponding to a break in colour. For example, positive anomalies of temperature might be represented by red colours (deeper red with increasing value) and negative anomalies, similarly, by blue colours. 
#For moisture index values, use green for positive anomalies and brown for negative anomalies.
# 2 Each shade should correspond to a sensible range of anomaly values. For example, you might choose one colour for MTCO anomalies of 0 degree to 0.5 degree, a deeper colour for 0.5 degree to 1 degree, and so on.


reds <- RColorBrewer::brewer.pal(5, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(4, "Blues"))
COLOURS <- c(blues_rev,White, reds)


p<-tps_gdd_6 %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf, -900,  -600,-300,-0.001,0.001,300,600, 900,1200, Inf),
        labels = c("<-900","-900","-600","-300~0",  "0","0~300","300","600","900",  "1200+"),
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

library(ggplot2)
ggsave(file="6-6.2ka Map of gdd_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_gdd_6 %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf, -900,  -600,-300,-0.001,0.001,300,600, 900,1200, Inf),
        labels = c("<-900","-900","-600","-300~0",  "0","0~300","300","600","900",  "1200+"),
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

load('tps_gdd_6_elv.rdata')


summary(tps_gdd_6_elv$gdd_anomaly)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1011.61  -322.84   -44.04   -76.84   189.69  1220.08 


p<-tps_gdd_6_elv %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf, -900,  -600,-300,-0.001,0.001,300,600, 900,1200, Inf),
        labels = c("<-900","-900","-600","-300~0",  "0","0~300","300","600","900",  "1200+"),
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


ggsave(file="6-6.2ka Map of gdd_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_gdd_6_elv %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf, -900,  -600,-300,-0.001,0.001,300,600, 900,1200, Inf),
        labels = c("<-900","-900","-600","-300~0",  "0","0~300","300","600","900",  "1200+"),
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
tps_gdd_6_elv_cut<- tps_gdd_6_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_gdd_6_elv_cut$gdd_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-782.2  -149.7   106.3   115.3   367.7  1220.1 

reds <- RColorBrewer::brewer.pal(5, "Reds")
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
COLOURS <- c(blues_rev,White, reds)



p<-tps_gdd_6_elv_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,   -600,-300,-0.001,0.001,300,600, 900,1200, Inf),
        labels = c("<-600","-600","-300~0",  "0","0~300","300","600","900",  "1200+"),
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


ggsave(file="6-6.2ka Map of gdd_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)


#6-6.2ka Tmin

load('tps_tmin_6_elv.rdata')


summary(tps_tmin_6_elv$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.7076 -1.7476 -0.8095 -0.7103  0.4294  4.7995 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")

# Colour scales should be chosen with the following criteria in mind:
# 1 The zero anomaly isoline should be very obvious, corresponding to a break in colour. For example, positive anomalies of temperature might be represented by red colours (deeper red with increasing value) and negative anomalies, similarly, by blue colours. 
#For moisture index values, use green for positive anomalies and brown for negative anomalies.
# 2 Each shade should correspond to a sensible range of anomaly values. For example, you might choose one colour for MTCO anomalies of 0 degree to 0.5 degree, a deeper colour for 0.5 degree to 1 degree, and so on.

greens <- RColorBrewer::brewer.pal(5, "Greens")
White<-c("#FFFFFF")#adding white colour as 0
browns_rev <- rev(RColorBrewer::brewer.pal(4, "YlOrBr"))
#RColorBrewer::display.brewer.pal(4, "YlOrBr")
COLOURS <- c(browns_rev,White, greens)


p<-tps_alpha_6 %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.15,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15,0.2, Inf),
        labels = c("<-0.15","-0.15","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1","0.15",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))


ggsave(file="6-6.2ka Map of alpha_anomaly colour 1.jpeg",p,width=22,height=11)

#change map boundary
p<-tps_alpha_6 %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.15,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15,0.2, Inf),
        labels = c("<-0.15","-0.15","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1","0.15",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of alpha_anomaly colour 1 cut out.jpeg",p,width=22,height=11)


## With elevation ----
### Elevations as independent variable ----

load('tps_alpha_6_elv.rdata')


summary(tps_alpha_6_elv$alpha_anomaly)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.199251 -0.039328 -0.009127  0.006889  0.049831  0.249803


p<-tps_alpha_6_elv %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.15,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15,0.2, Inf),
        labels = c("<-0.15","-0.15","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1","0.15",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = range(.$longitude, na.rm = TRUE),
                            ylim = range(.$latitude, na.rm = TRUE))


ggsave(file="6-6.2ka Map of alpha_anomaly with elevation colour 1.jpeg",p,width=22,height=11)


p<-tps_alpha_6_elv %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.15,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15,0.2, Inf),
        labels = c("<-0.15","-0.15","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1","0.15",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of alpha_anomaly with elevation colour 1 cut out.jpeg",p,width=22,height=11)

#data outside map should not be influencing the map
tps_alpha_6_elv_cut<- tps_alpha_6_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_alpha_6_elv_cut$alpha_anomaly)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.19925 -0.06508 -0.02285 -0.01517  0.02852  0.24980 


p<-tps_alpha_6_elv_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.15,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15,0.2, Inf),
        labels = c("<-0.15","-0.15","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1","0.15",  "0.2+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of alpha_anomaly with elevation filtered data colour 1 cut out.jpeg",p,width=22,height=11)

