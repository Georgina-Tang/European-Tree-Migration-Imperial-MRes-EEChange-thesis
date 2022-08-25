


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM paleo maps 2/Picea")

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")


#modern time using CRU
Picea_m=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea modern CRU.csv")
#Fagus_1=Fagus_1[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_m$taxon.abundance=as.numeric(Picea_m$taxon.abundance)


p0_p_CRU<-Picea_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


save(p0_p_CRU,file="p0_p_CRU.rdata")
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
library(dplyr)
library(ggplot2)
ggsave(file="Picea modern CRU map.jpeg",p0,width=22,height=11)

summary(Picea_m$taxon.abundance)



#1-1.2ka
Picea_1=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 1-1.2ka.csv")
#Picea_1=Picea_1[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_1$taxon.abundance=as.numeric(Picea_1$taxon.abundance)


p1<-Picea_1 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 1-1.2ka Map.jpeg",p1,width=22,height=11)


#2-2.2ka
Picea_2=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 2-2.2ka.csv")
#Picea_2=Picea_2[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_2$taxon.abundance=as.numeric(Picea_2$taxon.abundance)


p2<-Picea_2 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 2-2.2ka Map.jpeg",p2,width=22,height=11)


#3-3.2ka
Picea_3=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 3-3.2ka.csv")
#Picea_3=Picea_3[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_3$taxon.abundance=as.numeric(Picea_3$taxon.abundance)


p3<-Picea_3 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 3-3.2ka Map.jpeg",p3,width=22,height=11)


#4-4.2ka
Picea_4=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 4-4.2ka.csv")
#Picea_4=Picea_4[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_4$taxon.abundance=as.numeric(Picea_4$taxon.abundance)


p4<-Picea_4 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 4-4.2ka Map.jpeg",p4,width=22,height=11)


#5-5.2ka
Picea_5=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 5-5.2ka.csv")

Picea_5$taxon.abundance=as.numeric(Picea_5$taxon.abundance)


p5<-Picea_5 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 5-5.2ka Map.jpeg",p5,width=22,height=11)


#6-6.2ka
Picea_6=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 6-6.2ka.csv")
#Picea_6=Picea_6[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_6$taxon.abundance=as.numeric(Picea_6$taxon.abundance)


p6<-Picea_6 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 6-6.2ka Map.jpeg",p6,width=22,height=11)


#7-7.2ka
Picea_7=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 7-7.2ka.csv")
#Picea_7=Picea_7[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_7$taxon.abundance=as.numeric(Picea_7$taxon.abundance)


p7<-Picea_7 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 7-7.2ka Map.jpeg",p7,width=22,height=11)


#8-8.2ka
Picea_8=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 8-8.2ka.csv")
#Picea_8=Picea_8[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_8$taxon.abundance=as.numeric(Picea_8$taxon.abundance)


p8<-Picea_8 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 8-8.2ka Map.jpeg",p8,width=22,height=11)


#9-9.2ka
Picea_9=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 9-9.2ka.csv")
#Picea_9=Picea_9[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_9$taxon.abundance=as.numeric(Picea_9$taxon.abundance)


p9<-Picea_9 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 9-9.2ka Map.jpeg",p9,width=22,height=11)


#10-10.2ka
Picea_10=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 10-10.2ka.csv")
#Picea_10=Picea_10[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_10$taxon.abundance=as.numeric(Picea_10$taxon.abundance)


p10<-Picea_10 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 10-10.2ka Map.jpeg",p10,width=22,height=11)


#11-11.2ka
Picea_11=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 11-11.2ka.csv")
#Picea_11=Picea_11[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_11$taxon.abundance=as.numeric(Picea_11$taxon.abundance)


p11<-Picea_11 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 11-11.2ka Map.jpeg",p11,width=22,height=11)


#12-12.2ka
Picea_12=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea 12-12.2ka.csv")
#Picea_12=Picea_12[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Picea_12$taxon.abundance=as.numeric(Picea_12$taxon.abundance)


p12<-Picea_12 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Picea reconstruction 12-12.2ka Map.jpeg",p12,width=22,height=11)

