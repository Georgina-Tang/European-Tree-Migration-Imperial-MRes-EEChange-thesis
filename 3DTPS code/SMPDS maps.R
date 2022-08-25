#SMPDS maps

#modern_pollen is SMPDS


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/SMPDS modern calculations")


#update smpds so that the cannot allocate vector size issue will be fixed
#remotes::install_github("special-uor/smpds") 
require(smpds)
library(dplyr)
`%>%` <- magrittr::`%>%`
#get dataset


modern_smpds<-modern_pollen[c("Fagus", "Picea", "Quercus.deciduous","Lat","Long","Elv","Entity.name")]

names(modern_smpds)[names(modern_smpds) == 'Long'] <- 'longitude'
names(modern_smpds)[names(modern_smpds) == 'Lat'] <- 'latitude'

# 
# dd<- modern_smpds  %>%dplyr::filter(latitude==30.82)
# b=modern_smpds %>%
# group_by(Entity.name) %>%
# filter(n_distinct(Elv)==2) 

save(modern_smpds, file='modern_smpds.rdata')
load("modern_smpds.rdata")
memory.limit(size = 560000)
gc()
###############################calculation
#Fagus
tps_smpds_fagus<- modern_smpds %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::mutate(
    Fagus = mean(Fagus, na.rm = TRUE),
    Elv = mean(Elv, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(latitude, longitude, .keep_all = TRUE) %>%
  smpds::tps(
    var = "Fagus",
    z_var = "Elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 2
  ) %>%
  smpds::pb()


#modern_smpds_cut<- modern_smpds  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73&latitude>31)

str(modern_smpds)

save(tps_smpds_fagus, file='tps_smpds_fagus.rdata')

load('tps_smpds_fagus.rdata')
load('tps_smpds_picea.rdata')
load('tps_smpds_Quercus.deciduous.rdata')

summary(modern_smpds$Fagus)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.017413 0.005742 0.775536 

summary(tps_smpds_fagus$Fagus)
#       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0622465 -0.0035348  0.0000021  0.0017822  0.0029246  0.3301422 



tps_smpds_fagus_cut<- tps_smpds_fagus  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_smpds_fagus_cut$Fagus)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.0296979 -0.0003134  0.0009805  0.0099914  0.0098846  0.3301422 

#Picea
tps_smpds_picea<- modern_smpds %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::mutate(
    Picea = mean(Picea, na.rm = TRUE),
    Elv = mean(Elv, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(latitude, longitude, .keep_all = TRUE) %>%
  smpds::tps(
    var = "Picea",
    z_var = "Elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 2
  ) %>%
  smpds::pb()

#modern_smpds_cut<- modern_smpds  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73&latitude>31)

str(modern_smpds)

save(tps_smpds_picea, file='tps_smpds_picea.rdata')
load('tps_smpds_picea.rdata')


summary(modern_smpds$Picea)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.04215 0.03059 0.70918 

summary(tps_smpds_picea$Picea)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.196165 -0.012424  0.002928  0.049180  0.060310  1.314430 



tps_smpds_picea_cut<- tps_smpds_picea  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_smpds_picea_cut$Picea)
#  Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0333878  0.0002164  0.0064011  0.0465164  0.0574237  0.5553124


#Quercus.deciduous
tps_smpds_Quercus.deciduous<- modern_smpds %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::mutate(
    Quercus.deciduous = mean(Quercus.deciduous, na.rm = TRUE),
    Elv = mean(Elv, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(latitude, longitude, .keep_all = TRUE) %>%
  smpds::tps(
    var = "Quercus.deciduous",
    z_var = "Elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4
  ) %>%
  smpds::pb()


#modern_smpds_cut<- modern_smpds  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73&latitude>31)

str(modern_smpds)

save(tps_smpds_Quercus.deciduous, file='tps_smpds_Quercus.deciduous.rdata')



summary(modern_smpds$Quercus.deciduous)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.01124 0.05300 0.04885 0.92593 

summary(tps_smpds_Quercus.deciduous$Quercus.deciduous)
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.069001 -0.000847  0.006160  0.056990  0.081147  0.473355 



tps_smpds_Quercus.deciduous_cut<- tps_smpds_Quercus.deciduous  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_smpds_Quercus.deciduous_cut$Quercus.deciduous)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0690014  0.0005303  0.0137130  0.0372789  0.0571612  0.4733546 

######################################################################################map
#Fagus map
p0_f_smpds<-tps_smpds_fagus %>%
  dplyr::mutate(
    Fagus = Fagus %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Fagus",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Fagus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


save(p0_f_smpds,file="p0_f_smpds.rdata")
p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
library(dplyr)
library(ggplot2)
ggsave(file="Fagus modern SMPDS map.jpeg",p,width=22,height=11)

summary(modern_smpds$Fagus)


#Picea
p0_p_smpds<-tps_smpds_picea %>%
  dplyr::mutate(
    Picea = Picea %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Picea",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Picea Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

save(p0_p_smpds,file="p0_p_smpds.rdata")
#p+annotate("text", x=-3, y=65, label= "1 ka",col="black", size=3)
# annotate(geom="text", x=3, y=30, label="Scatter plot",
#    color="red")

p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
library(dplyr)
library(ggplot2)
ggsave(file="Picea modern SMPDS map.jpeg",p,width=22,height=11)

summary(modern_smpds$Picea)

#Quercus.deciduous
p0_q_smpds<-tps_smpds_Quercus.deciduous %>%
  dplyr::mutate(
    Quercus.deciduous = Quercus.deciduous %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Quercus.deciduous",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "Quercus Abundance",
                                                         values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )


p
save(p0_q_smpds,file="p0_q_smpds.rdata")
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
library(dplyr)
library(ggplot2)
ggsave(file="Quercus.deciduous modern SMPDS map.jpeg",p,width=22,height=11)

summary(modern_smpds$Quercus.deciduous)


#checking
special_epd_10_3 <- special_epd_10_3 %>%
  dplyr::filter(Picea_mean>0.04)
p<-special_epd_10_3 %>%
  smpds::plot_climate(var = "Picea_mean",
                                        xlim = c(-12, 45),
                            ylim = c(31, 73), )
p

#checking
special_epd_11_3 <- special_epd_11_3 %>%
  dplyr::filter(Picea_mean>0.04)
p<-special_epd_11_3 %>%
  smpds::plot_climate(var = "Picea_mean",
                      xlim = c(-12, 45),
                      ylim = c(31, 73), )
p


special_epd_1_3 <- special_epd_1_3 %>%
  dplyr::filter(Picea_mean>0.04)
p<-special_epd_1_3 %>%
  smpds::plot_climate(var = "Picea_mean",
                      xlim = c(-12, 45),
                      ylim = c(31, 73), )
p


special_epd_9_3 <- special_epd_9_3 %>%
  dplyr::filter(Picea_mean>0.04)
p<-special_epd_9_3 %>%
  smpds::plot_climate(var = "Picea_mean",
                      xlim = c(-12, 45),
                      ylim = c(31, 73), )
p


special_epd_9<-paleo_compare_2 %>% filter(age ==9300)
special_epd_9_2 <- special_epd_9 %>%
  dplyr::filter(!is.na(elv))
special_epd_9_3 <- special_epd_9_2 %>%
  dplyr::filter(!is.na(Picea_mean))

