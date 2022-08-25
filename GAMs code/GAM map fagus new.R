
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM paleo maps 2/Fagus")

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")



#modern time using CRU
Fagus_m=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus modern CRU.csv")
#Fagus_1=Fagus_1[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_m$taxon.abundance=as.numeric(Fagus_m$taxon.abundance)


p0_f_CRU<-Fagus_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

save(p0_f_CRU,file="p0_f_CRU.rdata")
p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
library(dplyr)
library(ggplot2)
ggsave(file="Fagus modern CRU map.jpeg",p0,width=22,height=11)

summary(Fagus_m$taxon.abundance)



#1-1.2ka
Fagus_1=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 1-1.2ka.csv")
#Fagus_1=Fagus_1[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_1$taxon.abundance=as.numeric(Fagus_1$taxon.abundance)


p1<-Fagus_1 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 1-1.2ka Map.jpeg",p1,width=22,height=11)


#2-2.2ka
Fagus_2=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 2-2.2ka.csv")
#Fagus_2=Fagus_2[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_2$taxon.abundance=as.numeric(Fagus_2$taxon.abundance)


p2<-Fagus_2 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 2-2.2ka Map.jpeg",p2,width=22,height=11)


#3-3.2ka
Fagus_3=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 3-3.2ka.csv")
#Fagus_3=Fagus_3[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_3$taxon.abundance=as.numeric(Fagus_3$taxon.abundance)


p3<-Fagus_3 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 3-3.2ka Map.jpeg",p3,width=22,height=11)


#4-4.2ka
Fagus_4=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 4-4.2ka.csv")
#Fagus_4=Fagus_4[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_4$taxon.abundance=as.numeric(Fagus_4$taxon.abundance)


p4<-Fagus_4 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 4-4.2ka Map.jpeg",p4,width=22,height=11)


#5-5.2ka
Fagus_5=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 5-5.2ka.csv")

Fagus_5$taxon.abundance=as.numeric(Fagus_5$taxon.abundance)


p5<-Fagus_5 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 5-5.2ka Map.jpeg",p5,width=22,height=11)


#6-6.2ka
Fagus_6=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 6-6.2ka.csv")
#Fagus_6=Fagus_6[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_6$taxon.abundance=as.numeric(Fagus_6$taxon.abundance)


p6<-Fagus_6 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 6-6.2ka Map.jpeg",p6,width=22,height=11)


#7-7.2ka
Fagus_7=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 7-7.2ka.csv")
#Fagus_7=Fagus_7[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_7$taxon.abundance=as.numeric(Fagus_7$taxon.abundance)


p7<-Fagus_7 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 7-7.2ka Map.jpeg",p7,width=22,height=11)


#8-8.2ka
Fagus_8=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 8-8.2ka.csv")
#Fagus_8=Fagus_8[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_8$taxon.abundance=as.numeric(Fagus_8$taxon.abundance)


p8<-Fagus_8 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 8-8.2ka Map.jpeg",p8,width=22,height=11)


#9-9.2ka
Fagus_9=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 9-9.2ka.csv")
#Fagus_9=Fagus_9[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_9$taxon.abundance=as.numeric(Fagus_9$taxon.abundance)


p9<-Fagus_9 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 9-9.2ka Map.jpeg",p9,width=22,height=11)


#10-10.2ka
Fagus_10=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 10-10.2ka.csv")
#Fagus_10=Fagus_10[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_10$taxon.abundance=as.numeric(Fagus_10$taxon.abundance)


p10<-Fagus_10 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 10-10.2ka Map.jpeg",p10,width=22,height=11)


#11-11.2ka
Fagus_11=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 11-11.2ka.csv")
#Fagus_11=Fagus_11[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_11$taxon.abundance=as.numeric(Fagus_11$taxon.abundance)


p11<-Fagus_11 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 11-11.2ka Map.jpeg",p11,width=22,height=11)


#12-12.2ka
Fagus_12=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 12-12.2ka.csv")
#Fagus_12=Fagus_12[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_12$taxon.abundance=as.numeric(Fagus_12$taxon.abundance)


p12<-Fagus_12 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Fagus reconstruction 12-12.2ka Map.jpeg",p12,width=22,height=11)
