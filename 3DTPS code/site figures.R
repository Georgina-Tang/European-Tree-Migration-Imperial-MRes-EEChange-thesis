#site maps- figures
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/Figure list")
#SMPDS maps 

SMPDS_site<-modern_pollen[c("Lat", "Long","Elv")]


names(SMPDS_site)[names(SMPDS_site) == 'Lat'] <- 'latitude'
names(SMPDS_site)[names(SMPDS_site) == 'Long'] <- 'longitude'
names(SMPDS_site)[names(SMPDS_site) == 'Elv'] <- 'Elevation'
SMPDS_site$ID<-1

# str(SMPDS_site)
# p<-SMPDS_site %>%
#   smpds::plot_climate(var="Elevation",
#                       fill_scale = ggplot2::scale_fill_viridis_d(name = toupper("Elevation")),
#                       xlim = c(-12, 45),
#                       ylim = c(31, 73),size=2)
# 
# ggsave(p,filename=paste("SMPDS site map.jpeg",sep=""),width = 20, height = 20, units = "cm")
# 
# p<-SMPDS_site %>%
#   smpds::plot_climate(var="Elevation",
#                       fill_scale = ggplot2::scale_fill_viridis_d(name = toupper("Elevation")),
#                       xlim = c( -23,  163),
#                       ylim=c(26, 82),
#                       size=2)

# greens <- RColorBrewer::brewer.pal(9, "Greens")
# greens
# # "#F7FCF5" "#E5F5E0" "#C7E9C0" "#A1D99B" "#74C476" "#41AB5D" "#238B45" "#006D2C" "#00441B"
# library(scales)
# show_col(greens)
# # library(viridis)
show_col(viridis_pal()(10))
viridis_pal()(10)
show_col(viridis_pal(option="G")(6))


# SMPDS_site_1<-SMPDS_site %>%
#   mutate(grp = case_when(
#     Elevation <=2000 ~ "<=2000",
#     Elevation >=2000 ~ ">2000",
#     #TRUE ~ NA_character_
#   )) %>%
#   group_by(grp) 

  


library(dplyr)

SMPDS_site_1<-SMPDS_site%>%filter(Elevation>=2000) 
SMPDS_site_2<-SMPDS_site%>%filter(Elevation<2000) 
#special_epd_12_6$Fagus_mean=0.1
SMPDS_site_1<-SMPDS_site_1 %>% 
  dplyr::mutate(
    Elevation = Elevation %>%
      cut(
        breaks = c(-Inf,2000,Inf),
        labels = c("<=2000",">2000"),
      )
  ) 
p_smpds<-SMPDS_site_2 %>%
  dplyr::mutate(
    Elevation = Elevation %>%
      cut(
        breaks = c(-Inf,2000,Inf),
        labels = c("<=2000",">2000"),
      )
  )  %>%smpds::plot_climate(var="Elevation",
                            fill_scale = ggplot2::scale_fill_manual(name="Elevation [m]",values =alpha(c("<=2000"="#3E4A89FF",">2000"="#FDE725FF"), 0.5)),
                            xlim = c( -23,  163),
                            ylim=c(28, 81),
                            #elevation_cut=c(-Inf,1000,2000,3000,4000,Inf),
                            size=2)

p_smpds<-p_smpds+
  geom_point(data =SMPDS_site_1,aes(x = longitude, y = latitude,color=Elevation),  size = 2)+
 scale_color_manual( name="Elevation [m]",values =alpha(c("<=2000"="#3E4A89FF",">2000"="#FDE725FF"), 0.5 ))+
xlim( -23,  163)+
  ylim(28, 81)
#  # lims(x= c(-23,  163), y = c(27, 82))+
#   scale_x_discrete(limits=factor(-23,  163))+ 
#   scale_y_discrete(limits=factor(27, 82)) 
# coord_sf(xlim = c(min, max), ylim = c(min, max))
#   

p_smpds
save(p_smpds,file="p_site.rdata")
ggsave(p_smpds,filename=paste("SMPDS site map.jpeg",sep=""),width = 25, height = 25, units = "cm")

  
#Special-EPD maps

Special_EPD_site<-raw_paleo[c("latitude", "longitude","elevation")]

Special_EPD_site$ID<-1

str(Special_EPD_site)

p<-Special_EPD_site %>%
  smpds::plot_climate(var="ID",
                      fill_scale = ggplot2::scale_fill_manual(values =alpha(c("1"="#238B45"), .5)),
                      xlim = c( min(.$longitude, na.rm = TRUE),  max(.$longitude, na.rm = TRUE)),
                      ylim=c(min(.$latitude, na.rm = TRUE), max(.$latitude, na.rm = TRUE)),
                      legend.position ="none",
                      size=2)
ggsave(p,filename=paste("Special-EPD site map.jpeg",sep=""),width = 30, height = 30, units = "cm")



  
p<- Special_EPD_site %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::mutate(
    elevation = mean(elevation, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(latitude, longitude, .keep_all = TRUE)  %>%
  smpds::plot_climate(var="ID",
                      fill_scale = ggplot2::scale_fill_manual(values =alpha(c("1"="#238B45"), .7)),
                      xlim = c( min(.$longitude, na.rm = TRUE),  max(.$longitude, na.rm = TRUE)),
                      ylim=c(min(.$latitude, na.rm = TRUE), max(.$latitude, na.rm = TRUE)),
                      legend.position ="none",
                      size=2)

ggsave(p,filename=paste("Special-EPD site map 2.jpeg",sep=""),width = 30, height = 30, units = "cm")

############
Special_EPD_site_1<-Special_EPD_site%>%filter(elevation>=2000) 
Special_EPD_site_2<-Special_EPD_site%>%filter(elevation<2000) 
#special_epd_12_6$Fagus_mean=0.1
Special_EPD_site_1<-Special_EPD_site_1 %>% 
  dplyr::mutate(
    elevation = elevation %>%
      cut(
        breaks = c(-Inf,2000,Inf),
        labels = c("<=2000",">2000"),
      )
  ) 
p_epd<-Special_EPD_site_2 %>%
  dplyr::mutate(
    elevation = elevation %>%
      cut(
        breaks = c(-Inf,2000,Inf),
        labels = c("<=2000",">2000"),
      )
  )  %>%smpds::plot_climate(var="elevation",
                            fill_scale = ggplot2::scale_fill_manual(name="Elevation",values =alpha(c("<=2000"="#3E4A89FF",">2000"="#FDE725FF"), 0.3)),
                            xlim = c( -23,  163),
                            ylim=c(28, 81),
                            #elevation_cut=c(-Inf,1000,2000,3000,4000,Inf),
                            size=2)

p_epd<-p_epd +
  geom_point(data =Special_EPD_site_1,aes(x = longitude, y = latitude,color=elevation),  size = 2)+
  scale_color_manual( name="Elevation",values =alpha(c("<=2000"="#3E4A89FF",">2000"="#FDE725FF"), 0.3))+
  xlim( -23,  163)+
  ylim(28, 81)
p_epd
save(p_epd,file="p_site_Special_epd.rdata")
ggsave(p_epd,filename=paste("Special-EPD site map.jpeg",sep=""),width = 25, height = 25, units = "cm")


#common extent:
#lat :29~80
#long:-21~161
#CRU-not needed
# 
# CRU_site<-CRU[c("lat", "lon","elv")]
# 
# CRU_site$ID<-1
# 
# 
# p<- CRU_site%>%
#   smpds::plot_climate(var="ID",
#                       fill_scale = ggplot2::scale_fill_manual(values =alpha(c("1"="#238B45"), .7)),
#                       xlim = c( min(.$lon, na.rm = TRUE),  max(.$lon, na.rm = TRUE)),
#                       ylim=c(min(.$lat, na.rm = TRUE), max(.$lat, na.rm = TRUE)),
#                       legend.position ="none",
#                       size=2)
# ggsave(p,filename=paste("CRU site map 2.jpeg",sep=""),width = 30, height = 30, units = "cm")
# 


