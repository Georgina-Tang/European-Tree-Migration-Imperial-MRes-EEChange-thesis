#GAM map for Fagus
#all map with the name 12-12.2ka is actually 11.8-12.0. This is just for the ease of naming

#modern time using CRU
Fagus_m=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus modern CRU.csv")
#Fagus_1=Fagus_1[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_m$taxon.abundance=as.numeric(Fagus_m$taxon.abundance)

p<-Fagus_m %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
library(dplyr)
library(ggplot2)
ggsave(file="Fagus modern CRU map.jpeg",p,width=22,height=11)

summary(Fagus_m$taxon.abundance)




#1-1.2ka
Fagus_1=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 1-1.2ka.csv")
#Fagus_1=Fagus_1[,c("paleo_MI","rtmi","Tmin","Tmax","gdd","long","lat","taxon.abundance")] 
Fagus_1$taxon.abundance=as.numeric(Fagus_1$taxon.abundance)

p<-Fagus_1 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
library(dplyr)
library(ggplot2)
ggsave(file="Fagus 1-1.2ka Map colour 2.jpeg",p,width=22,height=11)

summary(Fagus_1$taxon.abundance)


#2-2.2ka

Fagus_2=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Fagus 2-2.2ka.csv")

Fagus_2$taxon.abundance=as.numeric(Fagus_2$taxon.abundance)

p<-Fagus_2 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 2-2.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_2$taxon.abundance)


#3-3.2ka

Fagus_3=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Fagus 3-3.2ka.csv")

Fagus_3$taxon.abundance=as.numeric(Fagus_3$taxon.abundance)

p<-Fagus_3 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 3-3.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_3$taxon.abundance)

#4-4.2ka

Fagus_4=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Fagus 4-4.2ka.csv")

Fagus_4$taxon.abundance=as.numeric(Fagus_4$taxon.abundance)

p<-Fagus_4 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 4-4.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_4$taxon.abundance)


#5-5.2ka

Fagus_5=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus 5-5.2ka.csv")

Fagus_5$taxon.abundance=as.numeric(Fagus_5$taxon.abundance)

p<-Fagus_5 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 5-5.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_5$taxon.abundance)


#6-6.2ka

Fagus_6=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Fagus 6-6.2ka.csv")

Fagus_6$taxon.abundance=as.numeric(Fagus_6$taxon.abundance)

p<-Fagus_6 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 6-6.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_6$taxon.abundance)



#7-7.2ka

Fagus_7=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Fagus 7-7.2ka.csv")

Fagus_7$taxon.abundance=as.numeric(Fagus_7$taxon.abundance)

p<-Fagus_7 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 7-7.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_7$taxon.abundance)



#8-8.2ka

Fagus_8=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Fagus 8-8.2ka.csv")

Fagus_8$taxon.abundance=as.numeric(Fagus_8$taxon.abundance)

p<-Fagus_8 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 8-8.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_8$taxon.abundance)

#9-9.2ka

Fagus_9=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Fagus 9-9.2ka.csv")

Fagus_9$taxon.abundance=as.numeric(Fagus_9$taxon.abundance)

p<-Fagus_9 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 9-9.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_9$taxon.abundance)

#10-10.2ka

Fagus_10=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Fagus 10-10.2ka.csv")

Fagus_10$taxon.abundance=as.numeric(Fagus_10$taxon.abundance)

p<-Fagus_10 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 10-10.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_10$taxon.abundance)


#11-11.2ka

Fagus_11=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Fagus 11-11.2ka.csv")

Fagus_11$taxon.abundance=as.numeric(Fagus_11$taxon.abundance)

p<-Fagus_11 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 11-11.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_11$taxon.abundance)


#12-12.2ka

Fagus_12=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Fagus 12-12.2ka.csv")

Fagus_12$taxon.abundance=as.numeric(Fagus_12$taxon.abundance)

p<-Fagus_12 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Fagus 12-12.2ka Map.jpeg",p,width=22,height=11)


summary(Fagus_12$taxon.abundance)
