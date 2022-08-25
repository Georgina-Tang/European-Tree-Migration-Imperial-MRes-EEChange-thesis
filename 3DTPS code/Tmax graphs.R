#Tmax graphs

#4DTPS with weighting


#my own reconstruction using the new data Roberto gave me on 04-05

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new site.RData")

save.image(file = "reconstruction new site.RData")


########################################################################################################
#################################   Tmax graphs  ########################################
########################################################################################################

#Tmax -6 to 4


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with squared weights/Tmax")

#xlab <- " °C"
p1<-tps_tmax_1_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset1_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="1-1.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p2<-tps_tmax_2_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset2_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="2-2.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p3<-tps_tmax_3_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset3_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="3-3.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p4<-tps_tmax_4_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset4_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="4-4.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p5<-tps_tmax_5_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset5_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="5-5.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p6<-tps_tmax_6_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset6_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="6-6.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p7<-tps_tmax_7_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset7_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="7-7.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p8<-tps_tmax_8_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset8_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="8-8.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p9<-tps_tmax_9_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset9_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="9-9.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p10<-tps_tmax_10_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset10_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="10-10.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p11<-tps_tmax_11_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), 
                            .overlay_data = subset11_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="11.0-11.2ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)

p12<-tps_tmax_12_elv_w_cut %>%
  dplyr::mutate(
    Tmax_anomaly = Tmax_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmax_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "MTWA Anomaly",
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset12_3 %>%
                              dplyr::mutate(
                                Tmax_anomaly = Tmax_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
                                    labels = c("<-5","-5","-4","-3","-2","-1",  "0","1","2","3","4","5+"),
                                  )
                              ) ,size=2)

ggsave(file="11.8-12.0ka Map of Tmax_anomaly.jpeg",p,width=22,height=11)


