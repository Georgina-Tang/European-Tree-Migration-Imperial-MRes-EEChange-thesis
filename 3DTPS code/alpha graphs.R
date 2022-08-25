

#alpha graphs

#4DTPS with weighting


#my own reconstruction using the new data Roberto gave me on 04-05

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new site.RData")

save.image(file = "reconstruction new site.RData")



summary(tps_alpha_1_elv_w_cut$alpha_anomaly)
summary(tps_alpha_2_elv_w_cut$alpha_anomaly)
summary(tps_alpha_3_elv_w_cut$alpha_anomaly)
summary(tps_alpha_4_elv_w_cut$alpha_anomaly)
summary(tps_alpha_5_elv_w_cut$alpha_anomaly)
summary(tps_alpha_6_elv_w_cut$alpha_anomaly)
summary(tps_alpha_7_elv_w_cut$alpha_anomaly)
summary(tps_alpha_8_elv_w_cut$alpha_anomaly)
summary(tps_alpha_9_elv_w_cut$alpha_anomaly)
summary(tps_alpha_10_elv_w_cut$alpha_anomaly)
summary(tps_alpha_11_elv_w_cut$alpha_anomaly)
summary(tps_alpha_12_elv_w_cut$alpha_anomaly)

#-0.28 to 0.45
#-0.25 to 0.35




greens <- RColorBrewer::brewer.pal(6, "Greens")
White<-c("#FFFFFF")#adding white colour as 0
browns_rev <- rev(RColorBrewer::brewer.pal(6, "YlOrBr"))
#RColorBrewer::display.brewer.pal(4, "YlOrBr")
COLOURS <- c(browns_rev, greens)
COLOURS
########################################################################################################
#################################   alpha graphs  ########################################
########################################################################################################

#setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with squared weights/extrapolate calculation r data")
# load("tps_alpha_1_elv_w.rdata")
# load("tps_alpha_2_elv_w.rdata")
# load("tps_alpha_3_elv_w.rdata")
# load("tps_alpha_4_elv_w.rdata")
# load("tps_alpha_5_elv_w.rdata")
# load("tps_alpha_6_elv_w.rdata")
# load("tps_alpha_7_elv_w.rdata")
# load("tps_alpha_8_elv_w.rdata")
# load("tps_alpha_9_elv_w.rdata")
# load("tps_alpha_10_elv_w.rdata")
# load("tps_alpha_11_elv_w.rdata")
# load("tps_alpha_12_elv_w.rdata")
# save.image(file = "alpha anomalies.RData")


#alpha -6 to 4


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with squared weights")


p1<-tps_alpha_1_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset1_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="1-1.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p2<-tps_alpha_2_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset2_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),                                  )
                              ) ,size=2)

ggsave(file="2-2.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p3<-tps_alpha_3_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset3_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="3-3.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p4<-tps_alpha_4_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset4_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="4-4.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p5<-tps_alpha_5_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset5_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="5-5.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p6<-tps_alpha_6_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset6_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="6-6.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p7<-tps_alpha_7_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset7_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="7-7.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p8<-tps_alpha_8_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset8_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="8-8.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p9<-tps_alpha_9_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset9_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="9-9.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p10<-tps_alpha_10_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset10_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="10-10.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p11<-tps_alpha_11_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset11_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="11.0-11.2ka Map of alpha_anomaly.jpeg",p,width=22,height=11)

p12<-tps_alpha_12_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
        labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Alpha Anomaly",
                                values = c( "<-0.25"="#993404" ,  "-0.25"="#D95F0E" ,  "-0.2"="#FE9929","-0.15"="#FEC44F","-0.1"="#FEE391","-0.05"="#FFFFD4", "0"="#EDF8E9", "0.05"="#C7E9C0",
                                            "0.1"="#A1D99B" ,"0.15"="#74C476" ,"0.2"="#31A354", "0.25+"="#006D2C") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), 
                            .overlay_data = subset12_3 %>%
                              dplyr::mutate(
                                alpha_anomaly = alpha_anomaly %>%
                                  cut(
                                    breaks = c(-Inf,-0.25,-0.2,-0.15,-0.1,  -0.05,0,0.05, 0.1,0.15,0.2,0.25,Inf),
                                    labels = c("<-0.25","-0.25","-0.2","-0.15","-0.1","-0.05",  "0","0.05","0.1","0.15","0.2","0.25+"),
                                  )
                              ) ,size=2)

ggsave(file="11.8-12.0ka Map of alpha_anomaly.jpeg",p,width=22,height=11)


