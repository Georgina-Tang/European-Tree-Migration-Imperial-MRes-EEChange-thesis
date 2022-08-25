#GAM map Picea


#all map with the name 12-12.2ka is actually 11.8-12.0. This is just for the ease of naming



#1-1.2ka
Picea_1=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 1-1.2ka.csv")

Picea_1$taxon.abundance=as.numeric(Picea_1$taxon.abundance)

p<-Picea_1 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            units = "\u00B0C",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

library(ggplot2)
ggsave(file="Picea 1-1.2ka Map.jpeg",p,width=22,height=11)

summary(Picea_1$taxon.abundance)


#2-2.2ka

Picea_2=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 2-2.2ka.csv")

Picea_2$taxon.abundance=as.numeric(Picea_2$taxon.abundance)

p<-Picea_2 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

ggsave(file="Picea 2-2.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_2$taxon.abundance)


#3-3.2ka

Picea_3=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 3-3.2ka.csv")

Picea_3$taxon.abundance=as.numeric(Picea_3$taxon.abundance)

p<-Picea_3 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Picea 3-3.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_3$taxon.abundance)

#4-4.2ka

Picea_4=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 4-4.2ka.csv")

Picea_4$taxon.abundance=as.numeric(Picea_4$taxon.abundance)

p<-Picea_4 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Picea 4-4.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_4$taxon.abundance)


#5-5.2ka

Picea_5=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 5-5.2ka.csv")

Picea_5$taxon.abundance=as.numeric(Picea_5$taxon.abundance)

p<-Picea_5 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Picea 5-5.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_5$taxon.abundance)


#6-6.2ka

Picea_6=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 6-6.2ka.csv")

Picea_6$taxon.abundance=as.numeric(Picea_6$taxon.abundance)

p<-Picea_6 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Picea 6-6.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_6$taxon.abundance)



#7-7.2ka

Picea_7=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 7-7.2ka.csv")

Picea_7$taxon.abundance=as.numeric(Picea_7$taxon.abundance)

p<-Picea_7 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Picea 7-7.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_7$taxon.abundance)



#8-8.2ka

Picea_8=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 8-8.2ka.csv")

Picea_8$taxon.abundance=as.numeric(Picea_8$taxon.abundance)

p<-Picea_8 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Picea 8-8.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_8$taxon.abundance)

#9-9.2ka

Picea_9=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 9-9.2ka.csv")

Picea_9$taxon.abundance=as.numeric(Picea_9$taxon.abundance)

p<-Picea_9 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Picea 9-9.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_9$taxon.abundance)

#10-10.2ka

Picea_10=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 10-10.2ka.csv")

Picea_10$taxon.abundance=as.numeric(Picea_10$taxon.abundance)

p<-Picea_10 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Picea 10-10.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_10$taxon.abundance)


#11-11.2ka

Picea_11=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 11-11.2ka.csv")

Picea_11$taxon.abundance=as.numeric(Picea_11$taxon.abundance)

p<-Picea_11 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Picea 11-11.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_11$taxon.abundance)


#12-12.2ka

Picea_12=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/Picea 12-12.2ka.csv")

Picea_12$taxon.abundance=as.numeric(Picea_12$taxon.abundance)

p<-Picea_12 %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

ggsave(file="Picea 12-12.2ka Map.jpeg",p,width=22,height=11)


summary(Picea_12$taxon.abundance)

