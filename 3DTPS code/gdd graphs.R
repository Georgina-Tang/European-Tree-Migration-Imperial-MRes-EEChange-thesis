#gdd graphs


#gdd graphs

#4DTPS with weighting


#my own reconstruction using the new data Roberto gave me on 04-05

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new site.RData")

save.image(file = "reconstruction new site.RData")



summary(tps_gdd_1_elv_w_cut$gdd_anomaly)
summary(tps_gdd_2_elv_w_cut$gdd_anomaly)
summary(tps_gdd_3_elv_w_cut$gdd_anomaly)
summary(tps_gdd_4_elv_w_cut$gdd_anomaly)
summary(tps_gdd_5_elv_w_cut$gdd_anomaly)
summary(tps_gdd_6_elv_w_cut$gdd_anomaly)
summary(tps_gdd_7_elv_w_cut$gdd_anomaly)
summary(tps_gdd_8_elv_w_cut$gdd_anomaly)
summary(tps_gdd_9_elv_w_cut$gdd_anomaly)
summary(tps_gdd_10_elv_w_cut$gdd_anomaly)
summary(tps_gdd_11_elv_w_cut$gdd_anomaly)
summary(tps_gdd_12_elv_w_cut$gdd_anomaly)

#-2014 to 1144
#-1000 to 1000
########################################################################################################
#################################   gdd graphs  ########################################
########################################################################################################

#gdd -6 to 4


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with squared weights/gdd")


p1<-tps_gdd_1_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset1_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="1-1.2ka Map of gdd_anomaly.jpeg",p1,width=22,height=11)

p2<-tps_gdd_2_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset2_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="2-2.2ka Map of gdd_anomaly.jpeg",p2,width=22,height=11)

p3<-tps_gdd_3_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset3_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="3-3.2ka Map of gdd_anomaly.jpeg",p3,width=22,height=11)

p4<-tps_gdd_4_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset4_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="4-4.2ka Map of gdd_anomaly.jpeg",p4,width=22,height=11)

p5<-tps_gdd_5_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset5_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="5-5.2ka Map of gdd_anomaly.jpeg",p5,width=22,height=11)

p6<-tps_gdd_6_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset6_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="6-6.2ka Map of gdd_anomaly.jpeg",p6,width=22,height=11)

p7<-tps_gdd_7_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset7_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="7-7.2ka Map of gdd_anomaly.jpeg",p7,width=22,height=11)

p8<-tps_gdd_8_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset8_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="8-8.2ka Map of gdd_anomaly.jpeg",p8,width=22,height=11)

p9<-tps_gdd_9_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset9_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="9-9.2ka Map of gdd_anomaly.jpeg",p9,width=22,height=11)

p10<-tps_gdd_10_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset10_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="10-10.2ka Map of gdd_anomaly.jpeg",p10,width=22,height=11)

p11<-tps_gdd_11_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset11_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="11.0-11.2ka Map of gdd_anomaly.jpeg",p11,width=22,height=11)

p12<-tps_gdd_12_elv_w_cut %>%
  dplyr::mutate(
    gdd_anomaly = gdd_anomaly %>%
      cut(
        breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
        labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "gdd_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name="GDD0 Anomaly",
                                values = c( "<-1000"="#08519C" ,  "-1000"="#3182BD" ,  "-800"="#6BAED6","-600"="#9ECAE1","-400"="#C6DBEF","-200"="#EFF3FF", "0"="#FEE5D9", "200"="#FCBBA1",
                                            "400"="#FC9272" ,"600"="#FB6A4A" ,"800"="#DE2D26", "1000+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset12_3 %>%
                              dplyr::mutate(
                                gdd_anomaly = gdd_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-1000,-800,-600,-400,  -200,0,200, 400,600,800,1000,Inf),
                                    labels = c("<-1000","-1000","-800","-600","-400","-200",  "0","200","400","600","800","1000+"),
                                  )
                              ) ,size=2)

ggsave(file="11.8-12.0ka Map of gdd_anomaly.jpeg",p12,width=22,height=11)


