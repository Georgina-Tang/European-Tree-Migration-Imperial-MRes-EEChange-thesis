


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM paleo maps 2/Quercus")
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")


#modern time using CRU
Quercus.deciduous_m=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous modern CRU.csv")
#Fagus_1=Fagus_1[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_m$taxon.abundance=as.numeric(Quercus.deciduous_m$taxon.abundance)


p0_q_CRU<-Quercus.deciduous_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

save(p0_q_CRU,file="p0_q_CRU.rdata")
p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM modern maps 2")
library(dplyr)
library(ggplot2)
ggsave(file="Quercus.deciduous modern CRU map.jpeg",p0,width=22,height=11)
save(p0,file="p0.rdata")
summary(Quercus.deciduous_m$taxon.abundance)


#1-1.2ka
Quercus.deciduous_1=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 1-1.2ka.csv")
#Quercus.deciduous_1=Quercus.deciduous_1[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_1$taxon.abundance=as.numeric(Quercus.deciduous_1$taxon.abundance)


p1<-Quercus.deciduous_1 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 1-1.2ka Map.jpeg",p1,width=22,height=11)


#2-2.2ka
Quercus.deciduous_2=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 2-2.2ka.csv")
#Quercus.deciduous_2=Quercus.deciduous_2[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_2$taxon.abundance=as.numeric(Quercus.deciduous_2$taxon.abundance)


p2<-Quercus.deciduous_2 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 2-2.2ka Map.jpeg",p2,width=22,height=11)


#3-3.2ka
Quercus.deciduous_3=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 3-3.2ka.csv")
#Quercus.deciduous_3=Quercus.deciduous_3[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_3$taxon.abundance=as.numeric(Quercus.deciduous_3$taxon.abundance)


p3<-Quercus.deciduous_3 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 3-3.2ka Map.jpeg",p3,width=22,height=11)


#4-4.2ka
Quercus.deciduous_4=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 4-4.2ka.csv")
#Quercus.deciduous_4=Quercus.deciduous_4[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_4$taxon.abundance=as.numeric(Quercus.deciduous_4$taxon.abundance)


p4<-Quercus.deciduous_4 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 4-4.2ka Map.jpeg",p4,width=22,height=11)


#5-5.2ka
Quercus.deciduous_5=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 5-5.2ka.csv")

Quercus.deciduous_5$taxon.abundance=as.numeric(Quercus.deciduous_5$taxon.abundance)


p5<-Quercus.deciduous_5 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 5-5.2ka Map.jpeg",p5,width=22,height=11)


#6-6.2ka
Quercus.deciduous_6=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 6-6.2ka.csv")
#Quercus.deciduous_6=Quercus.deciduous_6[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_6$taxon.abundance=as.numeric(Quercus.deciduous_6$taxon.abundance)


p6<-Quercus.deciduous_6 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 6-6.2ka Map.jpeg",p6,width=22,height=11)


#7-7.2ka
Quercus.deciduous_7=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 7-7.2ka.csv")
#Quercus.deciduous_7=Quercus.deciduous_7[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_7$taxon.abundance=as.numeric(Quercus.deciduous_7$taxon.abundance)


p7<-Quercus.deciduous_7 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 7-7.2ka Map.jpeg",p7,width=22,height=11)


#8-8.2ka
Quercus.deciduous_8=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 8-8.2ka.csv")
#Quercus.deciduous_8=Quercus.deciduous_8[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_8$taxon.abundance=as.numeric(Quercus.deciduous_8$taxon.abundance)


p8<-Quercus.deciduous_8 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 8-8.2ka Map.jpeg",p8,width=22,height=11)


#9-9.2ka
Quercus.deciduous_9=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 9-9.2ka.csv")
#Quercus.deciduous_9=Quercus.deciduous_9[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_9$taxon.abundance=as.numeric(Quercus.deciduous_9$taxon.abundance)


p9<-Quercus.deciduous_9 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 9-9.2ka Map.jpeg",p9,width=22,height=11)


#10-10.2ka
Quercus.deciduous_10=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 10-10.2ka.csv")
#Quercus.deciduous_10=Quercus.deciduous_10[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_10$taxon.abundance=as.numeric(Quercus.deciduous_10$taxon.abundance)


p10<-Quercus.deciduous_10 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 10-10.2ka Map.jpeg",p10,width=22,height=11)


#11-11.2ka
Quercus.deciduous_11=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 11-11.2ka.csv")
#Quercus.deciduous_11=Quercus.deciduous_11[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_11$taxon.abundance=as.numeric(Quercus.deciduous_11$taxon.abundance)


p11<-Quercus.deciduous_11 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 11-11.2ka Map.jpeg",p11,width=22,height=11)


#12-12.2ka
Quercus.deciduous_12=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous 12-12.2ka.csv")
#Quercus.deciduous_12=Quercus.deciduous_12[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Quercus.deciduous_12$taxon.abundance=as.numeric(Quercus.deciduous_12$taxon.abundance)


p12<-Quercus.deciduous_12 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Reconstructed Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


ggsave(file="Quercus.deciduous reconstruction 12-12.2ka Map.jpeg",p12,width=22,height=11)

