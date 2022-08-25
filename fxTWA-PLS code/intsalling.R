#installing the fxTWAPLS

install.packages("fxTWAPLS")

install.packages("remotes")
remotes::install_github("special-uor/fxTWAPLS", "dev")


remotes::install_github("special-uor/fxTWAPLS@v0.0.7",force=TRUE)


wd<-"C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/fxTWAPLS"

#mapping


# for loading our data
library(jsonlite)
library(rgdal)
library(sf)
# for plotting
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
library(vapoRwave)
# for data wrangling
library(dplyr)

install.packages('mapview')
library(mapview)


install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("rnaturalearth")
library("rnaturalearthdata")
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)



modern_pollen<- read.csv("C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/data/Modern_Pollen_gdd_alpha_Tmin.csv", row.names=1)
modern_pollen[modern_pollen == 0] <- NA


ggplot(data = world) +
  geom_sf()+coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE)

modern_sf <- st_as_sf(modern_pollen, coords = c('Long', 'Lat'),crs = 4326)


sf::st_crs(modern_sf)

ggplot(modern_sf) + 
  geom_sf(aes(color = Aesculus))



woody_list=c("Abies",
             "Acer",
            "Alnus",
             "Alnus.alnobetula",
             "Betula",
            "Betula..Chamaebetula.",
             "Buxus",
            "Carpinus.betulus",
            "Carpinus.orientalis.Ostrya",
             "Castanea",
            "Cedrus",
             "Corylus",
            "Fagus",
             "Ilex",
             "Juglans",
            "Larix",
            "Picea",
            "Picea.orientalis",
             "Pinus..diploxylon.",
             "Pinus..haploxylon.",
             "Platanus",
             "Populus",
             "Quercus.deciduous",
            "Quercus.evergreen",
            "Quercus.intermediate",
             "Salix",
             "Sorbus",
             "Taxus",
             "Tilia",
            "Ulmus",
             "Ulmus.Zelkova"
             
)



woody_list[!woody_list %in% colnames(modern_sf)]

slist <- colnames(modern_sf)

#"Betula.Chamaebetula"       "Carpinus.orientalis.ostya" "Pinus.diploxylon"          "Pinus.haploxyon"          


ggplot(data = world) +
  geom_sf()+coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE)


plot_species <- function(i){
  ggplot1=ggplot() + 
    geom_sf(data = subset(modern_sf,select = i),  mapping = aes(x = i,color=i), show.legend = TRUE) +
    geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE)
  return (ggplot1)
}

print(plot_species("Abies"))

for (i in woody_list) {
print(plot_species(i))
}


require(tidyverse)

MyWrangledData <- modern_pollen %>% pivot_longer(cols = !colnames(modern_pollen)[1:7],names_to = "Species", values_to = "Percentage")

Myfinaldata <- subset(MyWrangledData,MyWrangledData$Species %in% woody_list) %>% drop_na()

unique(Myfinaldata$Species)

final_sf <- st_as_sf(Myfinaldata, coords = c('Long', 'Lat'),crs = 4326)

for (i in woody_list) {
ggplot1=ggplot() + 
  geom_sf(data = subset(final_sf,Species == i),  mapping =aes(x=Species,color=Percentage), show.legend = TRUE) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) + xlab (i)
print (ggplot1)
}



for (i in woody_list) {
data=subset(Myfinaldata)
}



for (i in woody_list) {
  ggplot1=ggplot() + 
    geom_sf(data = subset(final_sf,Species == i),  mapping =aes(x=Species,color=Percentage), show.legend = TRUE) +
    geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) + xlab (i)
  ggsave(ggplot1,filename=paste("Distribution of ",i,".png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")
}



for (i in woody_list) {
ggplot1=ggplot() + 
  geom_sf(data = modern_sf,  mapping =aes(x=i,color=i), show.legend = TRUE) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE)
print (ggplot1)
}

mapview(modern_sf)

######################## after Colin's first check
################################Carpinus.betulus##########Carpinus betulus shows up in northern Fennoscandia, and in the Russian Far East. 
#extract the weird data points
#ggplot() + 
#  geom_sf(data = subset(modern_sf,select = Carpinus.betulus),  mapping = aes(x = Carpinus.betulus,color=Carpinus.betulus), show.legend = TRUE) +
#  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE)+ 
  
#cleaning the data, changing them from portions to percentages, excluding data smaller than 0.5%, and omit the na
modern_pollen_2<- read.csv("C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/data/Modern pollen data for climate reconstructions, version 1 (SMPDS)/SMPDS_Feb2019.csv")
modern_pollen_2[modern_pollen_2 == 0] <- NA
taxaColMin <- which(colnames(modern_pollen_2) == "Abies")
taxaColMax <- which(colnames(modern_pollen_2) == "Zygophyllaceae")
modern_pollen_2[, taxaColMin:taxaColMax]=100*modern_pollen_2[, taxaColMin:taxaColMax]
#modern_pollen_2=filter(modern_pollen_2, modern_pollen_2[, taxaColMin:taxaColMax]>0.5)
modern_pollen_2[,taxaColMin:taxaColMax][modern_pollen_2[,taxaColMin:taxaColMax]<0.5]=NA


max(modern_pollen_2[,taxaColMin:taxaColMax],na.rm=T)

#maximum of every column selected
apply(modern_pollen_2[woody_list],2,max, na.rm = TRUE)




summary(modern_pollen_2$Longitude)
#modern_pollen_2 =modern_pollen_2 %>% filter(modern_pollen_2,Longitude=NA)
#modern_pollen_3 <- as_tibble(modern_pollen_3)
#modern_pollen_3=modern_pollen_2[,taxaColMin:taxaColMax]
#modern_pollen_3=modern_pollen_3[modern_pollen_3[,]>0.5,]
#modern_pollen_3 <- filter(modern_pollen_3, modern_pollen_3[1:247]>0.5)
#modern_pollen_3<- na.omit(modern_pollen_3)
#modern_pollen_3 =modern_pollen_3 %>% filter_all(any_vars(.> 0.5))
#modern_pollen_3[modern_pollen_3<0.5]=NA

#If you want control over how many NAs are valid for each row, try this function. For many survey data sets, too many blank question responses can ruin the results. So they are deleted after a certain threshold. This function will allow you to choose how many NAs the row can have before it's deleted:
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
#By default, it will eliminate all NAs.Here we specify the maximum number of NAs allowed:243 (252 columns- 5 non-species columns - 4 values) . 
#The number of rows dropped from 6458 to 6439, meaning that 19 sites that have less than 5 data are dropped

modern_pollen_4=delete.na(modern_pollen, 200)#205-5=200 for the new data

sites_dropped=anti_join(modern_pollen_2,modern_pollen_4)
write.csv(sites_dropped,"C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/sites_dropped.csv", row.names = FALSE)

modern_pollen_2=delete.na(modern_pollen_2, 243)

#modern_pollen_2<- na.omit(modern_pollen_2)

modern_sf_2 <- st_as_sf(modern_pollen_2, coords = c('Longitude', 'Latitude'),crs = 4326)
Carpinus_betulus_2=(select(modern_pollen_2,Carpinus.betulus,Latitude,Longitude))
Carpinus_betulus_2<- na.omit(Carpinus_betulus_2)



#after cleaning the data, number of weird point for carpinus changed from 34 to 3
#Carpinus_betulus_weird_2=Carpinus_betulus_2[Carpinus_betulus_2$Latitude > 60,]
#Carpinus_betulus_weird_2<- na.omit(Carpinus_betulus_weird_2)
#Carpinus_betulus_weird_sf <- st_as_sf(Carpinus_betulus_weird_2, coords = c('Longitude', 'Latitude'),crs = 4326)
#write.csv(Carpinus_betulus_weird_2,"C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/maps produced/Carpinus_betulus_weird.csv", row.names = FALSE)


#Carpinus_betulus=subset(modern_sf,select = Carpinus.betulus)
#Carpinus_betulus=(select(modern_pollen,Carpinus.betulus,Lat,Long))
#Carpinus_betulus[Carpinus_betulus$Lat > 60 & Carpinus_betulus$Long < 35,]
#Carpinus_betulus_weird=Carpinus_betulus[Carpinus_betulus$Lat > 60,]
#Carpinus_betulus_weird<- na.omit(Carpinus_betulus_weird)
#write.csv(Carpinus_betulus_weird,"C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/maps produced/Carpinus_betulus_weird.csv", row.names = FALSE)


#use different colour scheme
library(classInt)
library(RColorBrewer)
library(viridis)
?scale_colour_brewer

# get quantile breaks. Add .00001 offset to catch the lowest value
#breaks_qt <- classIntervals(c(min(modern_sf_2$Carpinus.betulus) - .00001, modern_sf_2$Carpinus.betulus), n = 5, style = "equal")
#breaks_qt
#modern_sf_2 <- mutate(modern_sf_2, Carpinus.betulus_cat = cut(Carpinus.betulus, breaks_qt$brks)) 

Breaks=c(1,5,10,20,50,100)

modern_sf_2 <- mutate(modern_sf_2, Carpinus.betulus_cat = cut(Carpinus.betulus, Breaks)) 




ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Carpinus.betulus_cat)),  mapping = aes(x = Carpinus.betulus_cat,color=Carpinus.betulus_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 

print(ggplot1)
 ggsave(ggplot1,filename=paste("Distribution of Carpinus.betulus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")

#  geom_sf(data = Carpinus_betulus_weird_sf,  aes(x = Carpinus.betulus,color="red")) +
#mycols4 <- c('#7b3294','#c2a5cf','#f7f7f7','#a6dba0','#008837')


 
 
##################Castanea#######Castanea appears in Kazakhstan.

 #Castanea=(select(modern_pollen_2,Castanea,Latitude,Longitude))
 #Castanea_weird=na.omit(Castanea[Castanea$Longitude > 50,])
 #write.csv(Castanea_weird,"C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/maps produced/Castanea_weird.csv", row.names = FALSE)
 
 
 #breaks_qt <- classIntervals(c(min(modern_sf_2$Castanea) - .00001, modern_sf_2$Castanea), n = 5, style = "quantile")
 #breaks_qt
 #modern_sf_2 <- mutate(modern_sf_2, Castanea_cat = cut(Castanea, breaks_qt$brks)) 
 

 modern_sf_2 <- mutate(modern_sf_2, Castanea_cat = cut(Castanea, Breaks)) 
 
 
 ggplot1=ggplot() + 
   geom_sf(data = subset(modern_sf_2, !is.na(Castanea_cat)),  mapping = aes(x = Castanea_cat,color=Castanea_cat), show.legend = TRUE) +
   scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
   geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
 
  ggsave(ggplot1,filename=paste("Distribution of Castanea_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")
 
 
 
############Corylus################# in Siberia

 #Corylus=(select(modern_pollen_2,Corylus,Latitude,Longitude))
 #Corylus_weird=na.omit(Corylus[Corylus$Longitude > 65&Corylus$Latitude >50 ,])
 #write.csv(Corylus_weird,"C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/maps produced/Corylus_weird.csv", row.names = FALSE)
 
 
 #breaks_qt <- classIntervals(c(min(modern_sf_2$Corylus) - .00001, modern_sf_2$Corylus), n = 5, style = "quantile")
 #breaks_qt
 modern_sf_2 <- mutate(modern_sf_2, Corylus_cat = cut(Corylus, Breaks)) 
 
 
 ggplot1=ggplot() + 
   geom_sf(data = subset(modern_sf_2, !is.na(Corylus_cat)),  mapping = aes(x = Corylus_cat,color=Corylus_cat), show.legend = TRUE) +
   scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
   geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
  ggsave(ggplot1,filename=paste("Distribution of Corylus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")

 # Corylus_weird_sf <- st_as_sf(Corylus_weird, coords = c('Longitude', 'Latitude'),crs = 4326)
 # ggplot()+
 # geom_sf(data = Corylus_weird_sf,  aes(x = Corylus,color="red"))
 
############Fagus################# in Siberia
  
#Fagus=(select(modern_pollen_2,Fagus,Latitude,Longitude))
#Fagus_weird=na.omit(Fagus[Fagus$Longitude > 65&Fagus$Latitude >50 ,])
#write.csv(Fagus_weird,"C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/maps produced/Fagus_weird.csv", row.names = FALSE)


#breaks_qt <- classIntervals(c(min(modern_sf_2$Fagus) - .00001, modern_sf_2$Fagus), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Fagus_cat = cut(Fagus, Breaks)) 
  
  
ggplot1=ggplot() + 
    geom_sf(data = subset(modern_sf_2, !is.na(Fagus_cat)),  mapping = aes(x = Fagus_cat,color=Fagus_cat), show.legend = TRUE) +
    scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
    geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
  print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Fagus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")
  
  # Fagus_weird_sf <- st_as_sf(Fagus_weird, coords = c('Longitude', 'Latitude'),crs = 4326)
  # ggplot()+
  # geom_sf(data = Fagus_weird_sf,  aes(x = Fagus,color="red"))
  
############Juglans################# in Siberia

#Juglans=(select(modern_pollen_2,Juglans,Latitude,Longitude))
#Juglans_weird=na.omit(Juglans[Juglans$Longitude > 65&Juglans$Latitude >50 ,])
#write.csv(Juglans_weird,"C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/maps produced/Juglans_weird.csv", row.names = FALSE)


#breaks_qt <- classIntervals(c(min(modern_sf_2$Juglans) - .00001, modern_sf_2$Juglans), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Juglans_cat = cut(Juglans, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Juglans_cat)),  mapping = aes(x = Juglans_cat,color=Juglans_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Juglans_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")

# Juglans_weird_sf <- st_as_sf(Juglans_weird, coords = c('Longitude', 'Latitude'),crs = 4326)
# ggplot()+
# geom_sf(data = Juglans_weird_sf,  aes(x = Juglans,color="red"))

############Quercus.deciduous################# in Siberia

#Quercus.deciduous=(select(modern_pollen_2,Quercus.deciduous,Latitude,Longitude))
#Quercus.deciduous_weird=na.omit(Quercus.deciduous[Quercus.deciduous$Longitude > 65&Quercus.deciduous$Latitude >50 ,])
#write.csv(Quercus.deciduous_weird,"C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/maps produced/Quercus.deciduous_weird.csv", row.names = FALSE)


#breaks_qt <- classIntervals(c(min(modern_sf_2$Quercus.deciduous) - .00001, modern_sf_2$Quercus.deciduous), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Quercus.deciduous_cat = cut(Quercus.deciduous, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Quercus.deciduous_cat)),  mapping = aes(x = Quercus.deciduous_cat,color=Quercus.deciduous_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Quercus.deciduous_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")

# Quercus.deciduous_weird_sf <- st_as_sf(Quercus.deciduous_weird, coords = c('Longitude', 'Latitude'),crs = 4326)
# ggplot()+
# geom_sf(data = Quercus.deciduous_weird_sf,  aes(x = Quercus.deciduous,color="red"))


############Ulmus################# in Siberia

#Ulmus=(select(modern_pollen_2,Ulmus,Latitude,Longitude))
#Ulmus_weird=na.omit(Ulmus[Ulmus$Longitude > 65&Ulmus$Latitude >50 ,])
#write.csv(Ulmus_weird,"C:/Users/X1 Carbon/Desktop/Tree Migration- Colin/maps produced/Ulmus_weird.csv", row.names = FALSE)


#breaks_qt <- classIntervals(c(min(modern_sf_2$Ulmus) - .00001, modern_sf_2$Ulmus), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Ulmus_cat = cut(Ulmus, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Ulmus_cat)),  mapping = aes(x = Ulmus_cat,color=Ulmus_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Ulmus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")

# Ulmus_weird_sf <- st_as_sf(Ulmus_weird, coords = c('Longitude', 'Latitude'),crs = 4326)
# ggplot()+
# geom_sf(data = Ulmus_weird_sf,  aes(x = Ulmus,color="red"))



#####################################

MyWrangledData_2 <- modern_pollen_2 %>% pivot_longer(cols = !colnames(modern_pollen_2)[1:5],names_to = "Species", values_to = "Percentage")
Myfinaldata_2 <- subset(MyWrangledData_2,MyWrangledData_2$Species %in% woody_list) %>% drop_na()
unique(Myfinaldata_2$Species)
final_sMyfinaldata_2final_sf <- st_as_sf(Myfinaldata_2, coords = c('Longitude', 'Latitude'),crs = 4326)


#output <- vector("double", ncol(df))  # 1. output
#for (i in seq_along(df)) {            # 2. sequence
#  output[[i]] <- median(df[[i]])      # 3. body
#}

#colMins(as.matrix(modern_sf_2[sapply(modern_sf_2, is.numeric)]))
library(dplyr)

#custom_min <- function(i) {min(modern_sf_2$i,na.rm = TRUE)- .00001 }
#custom_min(Abies)

#for (i in woody_list) {
 #breaks_qt <- classIntervals(c(min(modern_sf_2$i, na.rm = TRUE) - .00001, modern_sf_2$i), n = 5, style = "quantile")
# modern_sf_2 <- mutate(modern_sf_2, i_cat = cut(i, breaks_qt$brks)) 
#}
  
#  ggplot1=ggplot() + 
#    geom_sf(data = subset(final_sf,Species == i),  mapping =aes(x=Species,color=Percentage), show.legend = TRUE) +
#    geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) + xlab (i)
#  ggsave(ggplot1,filename=paste("Distribution of ",i,".png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")
#}


max(modern_pollen_2$Abies)
?min



########################################################
############Abies################# cut

#Abies=(select(modern_pollen_2,Abies,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Abies) - .00001, modern_sf_2$Abies), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Abies_cat = cut(Abies, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Abies_cat)),  mapping = aes(x = Abies_cat,color=Abies_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Abies_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")



############Acer################# cut

#Acer=(select(modern_pollen_2,Acer,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Acer) - .00001, modern_sf_2$Acer), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Acer_cat = cut(Acer, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Acer_cat)),  mapping = aes(x = Acer_cat,color=Acer_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Acer_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Alnus################# cut

Alnus=(select(modern_pollen_2,Alnus,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Alnus) - .00001, modern_sf_2$Alnus), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Alnus_cat = cut(Alnus, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Alnus_cat)),  mapping = aes(x = Alnus_cat,color=Alnus_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Alnus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Alnus.alnobetula################# cut

Alnus.alnobetula=(select(modern_pollen_2,Alnus.alnobetula,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Alnus.alnobetula) - .00001, modern_sf_2$Alnus.alnobetula), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Alnus.alnobetula_cat = cut(Alnus.alnobetula, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Alnus.alnobetula_cat)),  mapping = aes(x = Alnus.alnobetula_cat,color=Alnus.alnobetula_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Alnus.alnobetula_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Betula################# cut

Betula=(select(modern_pollen_2,Betula,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Betula) - .00001, modern_sf_2$Betula), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Betula_cat = cut(Betula, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Betula_cat)),  mapping = aes(x = Betula_cat,color=Betula_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Betula_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")

############Betula..Chamaebetula.################# cut

Betula..Chamaebetula.=(select(modern_pollen_2,Betula..Chamaebetula.,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Betula..Chamaebetula.) - .00001, modern_sf_2$Betula..Chamaebetula.), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Betula..Chamaebetula._cat = cut(Betula..Chamaebetula., Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Betula..Chamaebetula._cat)),  mapping = aes(x = Betula..Chamaebetula._cat,color=Betula..Chamaebetula._cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Betula..Chamaebetula._cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")

############Buxus################# cut

Buxus=(select(modern_pollen_2,Buxus,Latitude,Longitude))

max(modern_sf_2$Buxus,na.rm=T)
#breaks_qt <- classIntervals(c(min(modern_sf_2$Buxus) - .00001, modern_sf_2$Buxus), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Buxus_cat = cut(Buxus, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Buxus_cat)),  mapping = aes(x = Buxus_cat,color=Buxus_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Buxus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")




############Carpinus.orientalis.Ostrya################# cut

Carpinus.orientalis.Ostrya=(select(modern_pollen_2,Carpinus.orientalis.Ostrya,Latitude,Longitude))
max(modern_sf_2$Carpinus.orientalis.Ostrya,na.rm=T)

#breaks_qt <- classIntervals(c(min(modern_sf_2$Carpinus.orientalis.Ostrya) - .00001, modern_sf_2$Carpinus.orientalis.Ostrya), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Carpinus.orientalis.Ostrya_cat = cut(Carpinus.orientalis.Ostrya, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Carpinus.orientalis.Ostrya_cat)),  mapping = aes(x = Carpinus.orientalis.Ostrya_cat,color=Carpinus.orientalis.Ostrya_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Carpinus.orientalis.Ostrya_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Cedrus################# cut

Cedrus=(select(modern_pollen_2,Cedrus,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Cedrus) - .00001, modern_sf_2$Cedrus), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Cedrus_cat = cut(Cedrus, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Cedrus_cat)),  mapping = aes(x = Cedrus_cat,color=Cedrus_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Cedrus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Ilex################# cut

Ilex=(select(modern_pollen_2,Ilex,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Ilex) - .00001, modern_sf_2$Ilex), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Ilex_cat = cut(Ilex, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Ilex_cat)),  mapping = aes(x = Ilex_cat,color=Ilex_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Ilex_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")



############Larix################# cut

Larix=(select(modern_pollen_2,Larix,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Larix) - .00001, modern_sf_2$Larix), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Larix_cat = cut(Larix, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Larix_cat)),  mapping = aes(x = Larix_cat,color=Larix_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Larix_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")



############Picea################# cut

Picea=(select(modern_pollen_2,Picea,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Picea) - .00001, modern_sf_2$Picea), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Picea_cat = cut(Picea, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Picea_cat)),  mapping = aes(x = Picea_cat,color=Picea_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Picea_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Picea.orientalis################# cut

Picea.orientalis=(select(modern_pollen_2,Picea.orientalis,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Picea.orientalis) - .00001, modern_sf_2$Picea.orientalis), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Picea.orientalis_cat = cut(Picea.orientalis, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Picea.orientalis_cat)),  mapping = aes(x = Picea.orientalis_cat,color=Picea.orientalis_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Picea.orientalis_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")



############Pinus..diploxylon.################# cut

Pinus..diploxylon.=(select(modern_pollen_2,Pinus..diploxylon.,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Pinus..diploxylon.) - .00001, modern_sf_2$Pinus..diploxylon.), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Pinus..diploxylon._cat = cut(Pinus..diploxylon., Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Pinus..diploxylon._cat)),  mapping = aes(x = Pinus..diploxylon._cat,color=Pinus..diploxylon._cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Pinus..diploxylon._cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Pinus..haploxylon.################# cut

Pinus..haploxylon.=(select(modern_pollen_2,Pinus..haploxylon.,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Pinus..haploxylon.) - .00001, modern_sf_2$Pinus..haploxylon.), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Pinus..haploxylon._cat = cut(Pinus..haploxylon., Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Pinus..haploxylon._cat)),  mapping = aes(x = Pinus..haploxylon._cat,color=Pinus..haploxylon._cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Pinus..haploxylon._cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Platanus################# cut

Platanus=(select(modern_pollen_2,Platanus,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Platanus) - .00001, modern_sf_2$Platanus), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Platanus_cat = cut(Platanus, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Platanus_cat)),  mapping = aes(x = Platanus_cat,color=Platanus_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Platanus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Populus################# cut

Populus=(select(modern_pollen_2,Populus,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Populus) - .00001, modern_sf_2$Populus), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Populus_cat = cut(Populus, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Populus_cat)),  mapping = aes(x = Populus_cat,color=Populus_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Populus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")



############Quercus.evergreen################# cut

Quercus.evergreen=(select(modern_pollen_2,Quercus.evergreen,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Quercus.evergreen) - .00001, modern_sf_2$Quercus.evergreen), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Quercus.evergreen_cat = cut(Quercus.evergreen, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Quercus.evergreen_cat)),  mapping = aes(x = Quercus.evergreen_cat,color=Quercus.evergreen_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Quercus.evergreen_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Quercus.intermediate################# cut

Quercus.intermediate=(select(modern_pollen_2,Quercus.intermediate,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Quercus.intermediate) - .00001, modern_sf_2$Quercus.intermediate), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Quercus.intermediate_cat = cut(Quercus.intermediate, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Quercus.intermediate_cat)),  mapping = aes(x = Quercus.intermediate_cat,color=Quercus.intermediate_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Quercus.intermediate_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Salix################# cut

Salix=(select(modern_pollen_2,Salix,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Salix) - .00001, modern_sf_2$Salix), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Salix_cat = cut(Salix, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Salix_cat)),  mapping = aes(x = Salix_cat,color=Salix_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Salix_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


############Sorbus################# cut

Sorbus=(select(modern_pollen_2,Sorbus,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Sorbus) - .00001, modern_sf_2$Sorbus), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Sorbus_cat = cut(Sorbus, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Sorbus_cat)),  mapping = aes(x = Sorbus_cat,color=Sorbus_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Sorbus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")



############Taxus################# cut

Taxus=(select(modern_pollen_2,Taxus,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Taxus) - .00001, modern_sf_2$Taxus), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Taxus_cat = cut(Taxus, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Taxus_cat)),  mapping = aes(x = Taxus_cat,color=Taxus_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Taxus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")

############Tilia################# cut

Tilia=(select(modern_pollen_2,Tilia,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Tilia) - .00001, modern_sf_2$Tilia), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Tilia_cat = cut(Tilia, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Tilia_cat)),  mapping = aes(x = Tilia_cat,color=Tilia_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Tilia_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")

############Ulmus.Zelkova################# cut

Ulmus.Zelkova=(select(modern_pollen_2,Ulmus.Zelkova,Latitude,Longitude))


#breaks_qt <- classIntervals(c(min(modern_sf_2$Ulmus.Zelkova) - .00001, modern_sf_2$Ulmus.Zelkova), n = 5, style = "quantile")
#breaks_qt
modern_sf_2 <- mutate(modern_sf_2, Ulmus.Zelkova_cat = cut(Ulmus.Zelkova, Breaks)) 


ggplot1=ggplot() + 
  geom_sf(data = subset(modern_sf_2, !is.na(Ulmus.Zelkova_cat)),  mapping = aes(x = Ulmus.Zelkova_cat,color=Ulmus.Zelkova_cat), show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=TRUE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)
ggsave(ggplot1,filename=paste("Distribution of Ulmus.Zelkova_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")





