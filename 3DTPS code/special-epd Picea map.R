#special-epd Picea map


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/special epd comparison.RData")

save.image(file = "special epd comparison.RData")



###############################################################################################################################################################


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/TPS paleo calculation Picea")

load("tps_special_epd_1_elv.rdata")
load("tps_special_epd_2_elv.rdata")
load("tps_special_epd_3_elv.rdata")
load("tps_special_epd_4_elv.rdata")
load("tps_special_epd_5_elv.rdata")
load("tps_special_epd_6_elv.rdata")
load("tps_special_epd_7_elv.rdata")
load("tps_special_epd_8_elv.rdata")
load("tps_special_epd_9_elv.rdata")
load("tps_special_epd_10_elv.rdata")
load("tps_special_epd_11_elv.rdata")
load("tps_special_epd_12_elv.rdata")

summary(tps_special_epd_1_elv$Picea_mean)
summary(tps_special_epd_2_elv$Picea_mean)
summary(tps_special_epd_3_elv$Picea_mean)
summary(tps_special_epd_4_elv$Picea_mean)
summary(tps_special_epd_5_elv$Picea_mean)
summary(tps_special_epd_6_elv$Picea_mean)
summary(tps_special_epd_7_elv$Picea_mean)
summary(tps_special_epd_8_elv$Picea_mean)
summary(tps_special_epd_9_elv$Picea_mean)
summary(tps_special_epd_10_elv$Picea_mean)
summary(tps_special_epd_11_elv$Picea_mean)
summary(tps_special_epd_12_elv$Picea_mean)
# hist(tps_special_epd_12_elv$Picea_mean)
# hist(special_epd_1_3$Picea_mean)
#0~1

#############################################################################
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/TPS paleo map Picea")
library(wesanderson)

#1-1.2ka
p1<-tps_special_epd_1_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

library(ggplot2)
ggsave(file="Picea 1-1.2ka Special EPD Map.jpeg",p1,width=22,height=11)


#2-2.2ka
p2<-tps_special_epd_2_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 2-2.2ka Special EPD Map.jpeg",p2,width=22,height=11)

#3-3.2ka
p3<-tps_special_epd_3_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 3-3.2ka Special EPD Map.jpeg",p3,width=22,height=11)

#4-4.2ka
p4<-tps_special_epd_4_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 4-4.2ka Special EPD Map.jpeg",p4,width=22,height=11)

#5-5.2ka
p5<-tps_special_epd_5_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 5-5.2ka Special EPD Map.jpeg",p5,width=22,height=11)

#6-6.2ka
p6<-tps_special_epd_6_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 6-6.2ka Special EPD Map.jpeg",p6,width=22,height=11)

#7-7.2ka
p7<-tps_special_epd_7_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 7-7.2ka Special EPD Map.jpeg",p7,width=22,height=11)

#8-8.2ka
p8<-tps_special_epd_8_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 8-8.2ka Special EPD Map.jpeg",p8,width=22,height=11)

#9-9.2ka
p9<-tps_special_epd_9_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 9-9.2ka Special EPD Map.jpeg",p9,width=22,height=11)

#10-10.2ka
p10<-tps_special_epd_10_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 10-10.2ka Special EPD Map.jpeg",p10,width=22,height=11)

#11-11.2ka
p11<-tps_special_epd_11_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 11-11.2ka Special EPD Map.jpeg",p11,width=22,height=11)

#11.8-12.0 ka
p12<-tps_special_epd_12_elv %>%
  dplyr::mutate(
    Picea_mean = Picea_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea 11.8-12.0 ka Special EPD Map.jpeg",p12,width=22,height=11)

