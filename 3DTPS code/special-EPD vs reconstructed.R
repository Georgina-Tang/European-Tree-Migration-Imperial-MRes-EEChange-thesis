#Special-EPD vs reconstructed

#Georgina
#27/07/2022
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/special epd comparison.RData")

save.image(file = "special epd comparison.RData")


####################################################################################################################################


library(wesanderson)
colours = wesanderson::wes_palette("Zissou1", 9, type = "continuous")
colours = wesanderson::wes_palette("Zissou1", 6, type = "continuous")
colours = wesanderson::wes_palette("Zissou1", 11, type = "continuous")
# library(viridis)
library(scales)
show_col(colours)
colours
colours[1:9]
#[1] "#3B9AB2" "#59A8BB" "#78B7C5" "#B1C177" "#EBCC2A" "#E5BD15" "#E1AF00" "#E96400" "#F21A00"
colours[1:11]
#[1] "#3B9AB2" "#53A5B9" "#6BB1C1" "#8FBBA5" "#BDC367" "#EBCC2A" "#E6C019" "#E3B408" "#E49100" "#EB5500" "#F21A00"
White<-c("#FFFFFF")
##################################################################binning


# columns_to_keep=c("latitude","longitude","Fagus", "Picea", "Quercus.deciduous")
# paleo_compare<-raw_paleo[columns_to_keep]
# paleo_compare$Fagus<-paleo_compare$Fagus/rowSums(paleo_compare[,3:5])
#Quercus<-raw_paleo[c("Quercus", "Quercus deciduous", "Quercus.deciduous")]
paleo_compare<-core[c("Fagus", "Picea", "Quercus.deciduous")]
paleo_compare$latitude<-raw_paleo$latitude
paleo_compare$longitude<-raw_paleo$longitude
paleo_compare$age<-raw_paleo$median
paleo_compare$elv<-raw_paleo$elevation
paleo_compare$site<-raw_paleo$site_name


summary(paleo_compare$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -96    1631    3995    4517    7017   12000 

library(dplyr)


#changing original age to the format of age as previously in the binning process
paleo_compare_1<-paleo_compare[which(paleo_compare$age>=0),]
# paleo_compare_1<-paleo_compare_1 %>% mutate(age_2 = cut_width(age, width = 200, center = -100))
# paleo_compare_1$age_3<-as.numeric(as.factor(paleo_compare_1$age_2))

paleo_compare_1<-paleo_compare_1 %>% mutate(age_2 = cut(age, breaks = seq(0,12000,200), labels=seq(100,12000,200),include.lowest = TRUE))



#age 2 is in the format of binning reconstructed values

save(paleo_compare_1,file="paleo_compare_1.rdata")


##############binning in the way of averaging values for each site

pollendata<-paleo_compare_1

# dd=pollendata %>%
#   group_by(site) %>%
#   filter(n_distinct(longitude)>1) %>%
#   distinct()



#cleaning site with more than 1 value for lon, lat and elv
pollendata["latitude"][pollendata["site"] == "Stuphallet"] <- "78.96083"
pollendata["longitude"][pollendata["site"] == "Stuphallet"] <- "11.59694"

pollendata["latitude"][pollendata["site"] == "Majen El Orbi"] <- "37.15294"
pollendata["longitude"][pollendata["site"] == "Majen El Orbi"] <- "9.0984"
pollendata["elv"][pollendata["site"] == "Majen El Orbi"] <- "200"


pollendata["longitude"][pollendata["site"] == "Rosenbergdalen"] <- "20.9258"

pollendata["latitude"][pollendata["site"] == "Nile Delta"] <- "31.3"
pollendata["longitude"][pollendata["site"] == "Nile Delta"] <- "31.6"
pollendata["elv"][pollendata["site"] == "Nile Delta"] <- "1"

pollendata$age_2<-NULL
# 
# pollendata_2<-pollendata
# pollendata_2$site<-as.factor(pollendata_2$site)
# mean_env<-data.frame(matrix(NA,ncol=8,nrow=nlevels(pollendata_2$site)))
# pollendata_each<-pollendata_2[which(pollendata_2$site==levels(pollendata_2$site)[2]),]
# # 
# m=2
# age=10100
# range=100
# Fagus_mean<-mean(as.matrix(pollendata_each[which(pollendata_each$age>age-range & pollendata_each$age<age+range),"Fagus"]),na.rm = TRUE)
# Picea_mean<-mean(as.matrix(pollendata_each[which(pollendata_each$age>age-range & pollendata_each$age<age+range),"Picea"]),na.rm = TRUE)
# Quercus.deciduous_mean<-mean(as.matrix(pollendata_each[which(pollendata_each$age>age-range & pollendata_each$age<age+range),"Quercus.deciduous"]),na.rm = TRUE)
# 
# mean_env[m,]<-c(age,levels(pollendata_2$site)[m],unique(pollendata_each$longitude),unique(pollendata_each$latitude),unique(pollendata_each$elv),
#                 Fagus_mean,Picea_mean,Quercus.deciduous_mean)
# colnames(mean_env)<-c("age","site","lon","lat","elv","Fagus_mean","Picea_mean","Quercus.deciduous_mean")
# return(mean_env)
# 
# rm(age)
# rm(Age)
# rm(m)
# rm(range)
# rm(age)

get_pollen<-function(pollendata,age,range){
  pollendata$site<-as.factor(pollendata$site)
  mean_env<-data.frame(matrix(NA,ncol=8,nrow=nlevels(pollendata$site)))
  for(m in 1:nlevels(pollendata$site)){
    pollendata_each<-pollendata[which(pollendata$site==levels(pollendata$site)[m]),]
    
    
    Fagus_mean<-mean(as.matrix(pollendata_each[which(pollendata_each$age>age-range & pollendata_each$age<age+range),"Fagus"]),na.rm = TRUE)
    Picea_mean<-mean(as.matrix(pollendata_each[which(pollendata_each$age>age-range & pollendata_each$age<age+range),"Picea"]),na.rm = TRUE)
    Quercus.deciduous_mean<-mean(as.matrix(pollendata_each[which(pollendata_each$age>age-range & pollendata_each$age<age+range),"Quercus.deciduous"]),na.rm = TRUE)
    
    mean_env[m,]<-c(age,levels(pollendata$site)[m],unique(pollendata_each$longitude),unique(pollendata_each$latitude),unique(pollendata_each$elv),
                    Fagus_mean,Picea_mean,Quercus.deciduous_mean)
  }
  colnames(mean_env)<-c("age","site","lon","lat","elv","Fagus_mean","Picea_mean","Quercus.deciduous_mean")
  return(mean_env)
}


#range

# Get fossil gradient 
#1950 is 0, bin is 200 yr


paleo_compare_2<-rbind.data.frame(
  get_pollen(pollendata,100,100),get_pollen(pollendata,300,100),
  get_pollen(pollendata,500,100),get_pollen(pollendata,700,100),
  get_pollen(pollendata,900,100),get_pollen(pollendata,1100,100),
  get_pollen(pollendata,1300,100),get_pollen(pollendata,1500,100),
  get_pollen(pollendata,1700,100),get_pollen(pollendata,1900,100),
  get_pollen(pollendata,2100,100),get_pollen(pollendata,2300,100),
  get_pollen(pollendata,2500,100),get_pollen(pollendata,2700,100),
  get_pollen(pollendata,2900,100),get_pollen(pollendata,3100,100),
  get_pollen(pollendata,3300,100),get_pollen(pollendata,3500,100),
  get_pollen(pollendata,3700,100),get_pollen(pollendata,3900,100),
  get_pollen(pollendata,4100,100),get_pollen(pollendata,4300,100),
  get_pollen(pollendata,4500,100),get_pollen(pollendata,4700,100),
  get_pollen(pollendata,4900,100),get_pollen(pollendata,5100,100),
  get_pollen(pollendata,5300,100),get_pollen(pollendata,5500,100),
  get_pollen(pollendata,5700,100),get_pollen(pollendata,5900,100),
  get_pollen(pollendata,6100,100),get_pollen(pollendata,6300,100),
  get_pollen(pollendata,6500,100),get_pollen(pollendata,6700,100),
  get_pollen(pollendata,6900,100),get_pollen(pollendata,7100,100),
  get_pollen(pollendata,7300,100),get_pollen(pollendata,7500,100),
  get_pollen(pollendata,7700,100),get_pollen(pollendata,7900,100),
  get_pollen(pollendata,8100,100),get_pollen(pollendata,8300,100),
  get_pollen(pollendata,8500,100),get_pollen(pollendata,8700,100),
  get_pollen(pollendata,8900,100),get_pollen(pollendata,9100,100),
  get_pollen(pollendata,9300,100),get_pollen(pollendata,9500,100),
  get_pollen(pollendata,9700,100),get_pollen(pollendata,9900,100),
  get_pollen(pollendata,10100,100),get_pollen(pollendata,10300,100),
  get_pollen(pollendata,10500,100),get_pollen(pollendata,10700,100),
  get_pollen(pollendata,10900,100),get_pollen(pollendata,11100,100),
  get_pollen(pollendata,11300,100),get_pollen(pollendata,11500,100),
  get_pollen(pollendata,11700,100),get_pollen(pollendata,11900,100))


str(paleo_compare_2)

paleo_compare_2$age <- as.numeric( paleo_compare_2$age )#important: this shows ages after 9900
paleo_compare_2$Fagus_mean <- as.numeric( paleo_compare_2$Fagus_mean )
paleo_compare_2$Picea_mean <- as.numeric( paleo_compare_2$Picea_mean )
paleo_compare_2$Quercus.deciduous_mean <- as.numeric( paleo_compare_2$Quercus.deciduous_mean )
paleo_compare_2$elv <- as.numeric( paleo_compare_2$elv )
paleo_compare_2$lon <- as.numeric( paleo_compare_2$lon )
paleo_compare_2$lat <- as.numeric( paleo_compare_2$lat )


paleo_compare_2$agef<-factor(paleo_compare_2$age,labels=c("0.1 ka", "0.3 ka",  "0.5 ka",  "0.7 ka",  "0.9 ka",  "1.1 ka",  "1.3 ka",  "1.5 ka",  "1.7 ka",  "1.9 ka",  "2.1 ka", 
                                                    "2.3 ka", "2.5 ka",  "2.7 ka",  "2.9 ka",  "3.1 ka",  "3.3 ka",  "3.5 ka",  "3.7 ka",  "3.9 ka",  "4.1 ka",  "4.3 ka", 
                                                    "4.5 ka", "4.7 ka",  "4.9 ka",  "5.1 ka",  "5.3 ka",  "5.5 ka",  "5.7 ka",  "5.9 ka",  "6.1 ka",  "6.3 ka",  "6.5 ka", 
                                                    "6.7 ka", "6.9 ka",  "7.1 ka",  "7.3 ka",  "7.5 ka",  "7.7 ka",  "7.9 ka",  "8.1 ka",  "8.3 ka",  "8.5 ka",  "8.7 ka", 
                                                    "8.9 ka", "9.1 ka",  "9.3 ka",  "9.5 ka",  "9.7 ka",  "9.9 ka",  "10.1 ka", "10.3 ka", "10.5 ka", "10.7 ka", "10.9 ka",
                                                    "11.1 ka", "11.3 ka", "11.5 ka", "11.7 ka", "11.9 ka"))

save(paleo_compare_2, file = "paleo_compare_2.rdata") 

#setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("paleo_compare_2.rdata")

#binning finishes

#####################################produce comparison maps
#paleo_compare_2$Fagus_mean[is.nan(paleo_compare_2$Fagus_mean)]<-NA
#special_epd_1$Fagus_mean[is.nan(special_epd_1$Fagus_mean)]<-NA
#special_epd_1 <- special_epd_1 %>% mutate_all(~ifelse(is.nan(.), NA, .))
paleo_compare_2[paleo_compare_2 =="NaN"] <- NA_character_

paleo_compare_2$Fagus_mean <- as.numeric( paleo_compare_2$Fagus_mean )
paleo_compare_2$Picea_mean <- as.numeric( paleo_compare_2$Picea_mean )
paleo_compare_2$Quercus.deciduous_mean <- as.numeric( paleo_compare_2$Quercus.deciduous_mean )
save(paleo_compare_2, file = "paleo_compare_2.rdata") 


#########################################3DTPS  calculation first
#1-1.2ka
special_epd_1<-paleo_compare_2 %>% filter(age ==1100)
special_epd_1_2 <- special_epd_1 %>%
  dplyr::filter(!is.na(elv))#1302 out of 1304 obs left
special_epd_1_3 <- special_epd_1_2 %>%
  dplyr::filter(!is.na(Fagus_mean))#715 out of 1304 left


str(special_epd_1_3)
#using se_Tmax_anomaly as weighting

tps_special_epd_1 <- special_epd_1_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif"
  )


tps_special_epd_1_elv <- special_epd_1_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )


save(tps_special_epd_1_elv, file='tps_special_epd_1_elv.rdata')


summary(special_epd_1_3$Fagus_mean)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.02787 0.10145 0.14691 0.83873
summary(tps_special_epd_1$Fagus_mean)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.0632688 -0.0047727  0.0000933  0.0183506  0.0019221  0.5250226 
summary(tps_special_epd_1_elv$Fagus_mean)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.086101 -0.003897  0.002146  0.019676  0.016082  0.535286 



tps_special_epd_1_elv_cut<- tps_special_epd_1_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_1_elv_cut$Fagus_mean)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.76374 -0.10520  0.06052  0.33779  0.57445  2.59286 


p<-special_epd_1_3 %>%
  smpds::plot_climate_tiles(var = "Fagus_mean",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )
#cannot produce any meaningful graphs


p<-tps_special_epd_1_elv %>%
  smpds::plot_climate_tiles(var = "Fagus_mean",
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

ggsave(file="Fagus 1-1.2ka Special EPD Map.jpeg",p,width=22,height=11)


p<-tps_special_epd_1_elv %>%
  dplyr::mutate(
    Fagus_mean = Fagus_mean %>%
      cut(
        breaks = c(-Inf,0.03,0.05,  0.1,0.2,0.3,0.4,Inf),
                labels = c("absence","0","0.05", "0.1","0.2","0.3","0.4+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "Fagus_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" , "0.05"="#6BB1C1", "0.1"="#BDC367", "0.2"="#E6C019",
                                                                 "0.3"="#E49100" ,"0.4+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p

ggsave(file="Fagus 1-1.2ka Special EPD Map 0.03 threshold.jpeg",p,width=22,height=11)



#actual data points map
library(sf)
library(viridis)
special_epd_1_3_s= st_as_sf(special_epd_1_3, coords = c('lon', 'lat'),crs = 4326)
ggplot1=ggplot() + 
  geom_sf(data = special_epd_1_3_s, mapping = aes(x = Fagus_mean,color=Fagus_mean), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=FALSE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-12, 45), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)

ggsave(ggplot1,filename=paste("Fagus 1-1.2ka Special EDP Map actual.jpeg",sep=""),width = 20, height = 20, units = "cm")





#12 ka Fagus points map
special_epd_12_5 <- special_epd_12_2 %>%
  dplyr::filter(!is.na(Fagus_mean), Fagus_mean != 0)


special_epd_12_5<-special_epd_12_5 %>%
  dplyr::mutate(
    Fagus_mean = Fagus_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  

library(sf)
special_epd_12_2_s= st_as_sf(special_epd_12_5, coords = c('lon', 'lat'),crs = 4326)

theme_set(theme_bw())

ggplot1=ggplot() + 
  geom_sf(data = special_epd_12_2_s, mapping = aes(x = Fagus_mean,color=Fagus_mean), show.legend = TRUE) +
 # scale_color_viridis(option="viridis",discrete=FALSE,direction=-1) +
  scale_color_manual(name = "taxon abundance",
                             values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                         "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                         "0.1"="#E6C019", "0.2"="#E3B408",
                                         "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") )+
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-12, 45), ylim = c(29.4, 79.7742), expand = FALSE) +
  theme(panel.grid.major = element_blank() )

print(ggplot1)

# 
# ggplot1=ggplot() + 
#   geom_sf(data = special_epd_12_2_s, mapping = aes(x = Fagus_mean,color=Fagus_mean), show.legend = TRUE) +
#    scale_color_viridis(option="viridis",discrete=FALSE,direction=-1) +
#   geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-12, 45), ylim = c(29.4, 79.7742), expand = FALSE) 
# print(ggplot1)


ggsave(ggplot1,filename=paste("Fagus 11.8-12.0 ka Special EDP Map actual.jpeg",sep=""),width = 20, height = 20, units = "cm")



                                        
                                        
p12<-special_epd_12_5 %>%
  dplyr::mutate(
    Fagus_mean = Fagus_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  )  %>%
  arrange(by=desc(Fagus_mean))  %>% 
  smpds::plot_climate(var = "Fagus_mean",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#9E9E9E" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                                                     "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                                     "0.1"="#E6C019", "0.2"="#E3B408",
                                                                     "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(31, 73), size=3,
                      legend.position = "none")

ggsave(p12,filename=paste("Fagus 11.8-12.0 ka Special EDP Map actual 3.jpeg",sep=""),width = 20, height = 20, units = "cm")





  
  
special_epd_12_6<-special_epd_12_5%>%filter(Fagus_mean>0.04) 
#special_epd_12_6$Fagus_mean=0.1
special_epd_12_6<-special_epd_12_6 %>% 
  dplyr::mutate(
    Fagus_mean = Fagus_mean %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  ) 
special_epd_12_7<-special_epd_12_5%>%filter(Fagus_mean<0.04) 
# special_epd_12_7$Fagus_mean="absence"
p12<- special_epd_12_7 %>%
    dplyr::mutate(
      Fagus_mean = Fagus_mean %>%
        cut(
          breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
          labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
        )
    )  %>%
  smpds::plot_climate(var = "Fagus_mean",
                      fill_scale =
                        ggplot2::scale_fill_manual(name = "taxon abundance",
                                                   values = c( "absence"="#9E9E9E" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9",
                                                               "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                                               "0.1"="#E6C019", "0.2"="#E3B408",
                                                               "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") ),
                      xlim = c(-12, 45),
                      ylim = c(31, 73), size=3,
                      legend.position = "none")



p12<-p12 +
  geom_point(data =special_epd_12_6,aes(x = lon, y = lat,color=Fagus_mean),  size = 3)+
  scale_color_manual(     values = c( "absence"="#9E9E9E" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                      "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                      "0.1"="#E6C019", "0.2"="#E3B408",
                                      "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00"))
####modern smpds

tps_smpds_fagus<- modern_smpds%>%
  dplyr::mutate(
    Fagus = Fagus %>%
      cut(
        breaks = c(-Inf,0.04,0.05, 0.06,0.07,0.08,0.09, 0.1,0.2,0.3,0.4,0.5,Inf),
        labels = c("absence","0.04","0.05", "0.06","0.07","0.08","0.09","0.1","0.2","0.3","0.4","0.5+"),
      )
  ) 


modern_smpds_s= st_as_sf(tps_smpds_fagus, coords = c('longitude', 'latitude'),crs = 4326)
ggplot1=ggplot() + 
  geom_sf(data = modern_smpds_s, mapping = aes(x = Fagus,color=Fagus), show.legend = TRUE) +
  # scale_color_viridis(option="viridis",discrete=FALSE,direction=-1) +
  scale_color_manual(name = "taxon abundance",
                     values = c( "absence"="#E0E0E0" ,  "0.04"="#3B9AB2" , "0.05"="#53A5B9", 
                                 "0.06"="#6BB1C1","0.07"="#8FBBA5","0.08"="#BDC367","0.09"="#EBCC2A",
                                 "0.1"="#E6C019", "0.2"="#E3B408",
                                 "0.3"="#E49100" ,"0.4"="#EB5500","0.5+"="#F21A00") )+
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-12, 45), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)

