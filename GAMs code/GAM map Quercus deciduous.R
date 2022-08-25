#GAM map Quercus.deciduous


#all map with the name 12-12.2ka is actually 11.8-12.0. This is just for the ease of naming



#1-1.2ka
Quercus.deciduous_1=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 1-1.2ka.csv")

Quercus.deciduous_1$taxon.abundance=as.numeric(Quercus.deciduous_1$taxon.abundance)

p<-Quercus.deciduous_1 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            units = "\u00B0C",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

library(ggplot2)
ggsave(file="Quercus.deciduous 1-1.2ka Map.jpeg",p,width=22,height=11)

summary(Quercus.deciduous_1$taxon.abundance)


#2-2.2ka

Quercus.deciduous_2=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 2-2.2ka.csv")

Quercus.deciduous_2$taxon.abundance=as.numeric(Quercus.deciduous_2$taxon.abundance)

p<-Quercus.deciduous_2 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

ggsave(file="Quercus.deciduous 2-2.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_2$taxon.abundance)


#3-3.2ka

Quercus.deciduous_3=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 3-3.2ka.csv")

Quercus.deciduous_3$taxon.abundance=as.numeric(Quercus.deciduous_3$taxon.abundance)

p<-Quercus.deciduous_3 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Quercus.deciduous 3-3.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_3$taxon.abundance)

#4-4.2ka

Quercus.deciduous_4=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 4-4.2ka.csv")

Quercus.deciduous_4$taxon.abundance=as.numeric(Quercus.deciduous_4$taxon.abundance)

p<-Quercus.deciduous_4 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Quercus.deciduous 4-4.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_4$taxon.abundance)


#5-5.2ka

Quercus.deciduous_5=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 5-5.2ka.csv")

Quercus.deciduous_5$taxon.abundance=as.numeric(Quercus.deciduous_5$taxon.abundance)

p<-Quercus.deciduous_5 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Quercus.deciduous 5-5.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_5$taxon.abundance)


#6-6.2ka

Quercus.deciduous_6=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 6-6.2ka.csv")

Quercus.deciduous_6$taxon.abundance=as.numeric(Quercus.deciduous_6$taxon.abundance)

p<-Quercus.deciduous_6 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Quercus.deciduous 6-6.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_6$taxon.abundance)



#7-7.2ka

Quercus.deciduous_7=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 7-7.2ka.csv")

Quercus.deciduous_7$taxon.abundance=as.numeric(Quercus.deciduous_7$taxon.abundance)

p<-Quercus.deciduous_7 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Quercus.deciduous 7-7.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_7$taxon.abundance)



#8-8.2ka

Quercus.deciduous_8=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 8-8.2ka.csv")

Quercus.deciduous_8$taxon.abundance=as.numeric(Quercus.deciduous_8$taxon.abundance)

p<-Quercus.deciduous_8 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Quercus.deciduous 8-8.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_8$taxon.abundance)

#9-9.2ka

Quercus.deciduous_9=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 9-9.2ka.csv")

Quercus.deciduous_9$taxon.abundance=as.numeric(Quercus.deciduous_9$taxon.abundance)

p<-Quercus.deciduous_9 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Quercus.deciduous 9-9.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_9$taxon.abundance)

#10-10.2ka

Quercus.deciduous_10=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 10-10.2ka.csv")

Quercus.deciduous_10$taxon.abundance=as.numeric(Quercus.deciduous_10$taxon.abundance)

p<-Quercus.deciduous_10 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Quercus.deciduous 10-10.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_10$taxon.abundance)


#11-11.2ka

Quercus.deciduous_11=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 11-11.2ka.csv")

Quercus.deciduous_11$taxon.abundance=as.numeric(Quercus.deciduous_11$taxon.abundance)

p<-Quercus.deciduous_11 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Quercus.deciduous 11-11.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_11$taxon.abundance)


#12-12.2ka

Quercus.deciduous_12=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Quercus.deciduous 12-12.2ka.csv")

Quercus.deciduous_12$taxon.abundance=as.numeric(Quercus.deciduous_12$taxon.abundance)

p<-Quercus.deciduous_12 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Quercus.deciduous 12-12.2ka Map.jpeg",p,width=22,height=11)


summary(Quercus.deciduous_12$taxon.abundance)

