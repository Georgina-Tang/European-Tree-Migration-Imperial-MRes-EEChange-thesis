#compare actual paleo pollen distribution with the reconstructed


#25/07/2022
#Georgina


##################################################sorting out white space in GAM output

p<-tps_alpha_1_elv_w %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p

p<-CRU %>%
  smpds::plot_climate_tiles(var = "alpha",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

p


p<-CRU_Tmin_gdd %>%
  smpds::plot_climate_tiles(var = "Tmin",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

p<-CRU_gdd %>%
  smpds::plot_climate_tiles(var = "gdd",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )


p<-CRU_MI %>%
  smpds::plot_climate_tiles(var = "MI",
                            xlim = c(-12, 45),
                            ylim = c(min(.$lat, na.rm = TRUE), 73), )

#CRU gdd and CRU MI is where the white space comes from
#the blank missing data is from CRU: GDD and MI

##########################second white space: the -8~-9 longitude line in TPS outputs:

#using raw paleo data:
raw_paleo_white<-raw_paleo[which(raw_paleo$median>=11000&raw_paleo$median<=11200),]
raw_paleo_white<-raw_paleo_white[which(raw_paleo_white$longitude>=-10&raw_paleo_white$longitude<=-6),]

ggplot(data = raw_paleo_white, aes(x = longitude, y = latitude)) +
  geom_point()

#using reconstructions results: TPS again
gradient_white<-gradient_env_NA_removed[which(gradient_env_NA_removed$age>=11000&gradient_env_NA_removed$age<=11200),]
gradient_white<-gradient_white[which(gradient_white$lon>=-10&gradient_white$lon<=-6),]

gradient_white<-gradient_white[c("lon", "lat", "Tmin_anomaly","elv")]
gradient_white <- gradient_white %>%
  dplyr::filter(!is.na(Tmin_anomaly))

vec1=gradient_white$elv
vec2=vec1*vec1
tps_gradient <- gradient_white %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS,
    weights = vec2
  )
p<-tps_gradient %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73),)

gradient_white= st_as_sf(gradient_white, coords = c('lon', 'lat'),crs = 4326)
ggplot1=ggplot() + 
  geom_sf(data = gradient_white, show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=FALSE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-12, 45), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)


###################################################sorting out colours, bands, etc
# Retrieve the color of Zissou1
#  colour =
#    wesanderson::wes_palette("Zissou1", 5, type = "discrete")
# colors <- ggplot_build(p)$data[[2]]$fill
# colors
# colors<-c("#3B9AB2")
#show_col(colors)
colours = wesanderson::wes_palette("Zissou1", 9, type = "continuous")
colours = wesanderson::wes_palette("Zissou1", 6, type = "continuous")
# library(viridis)
 library(scales)
show_col(colours)
colours
colours[1:9]
#[1] "#3B9AB2" "#59A8BB" "#78B7C5" "#B1C177" "#EBCC2A" "#E5BD15" "#E1AF00" "#E96400" "#F21A00"
White<-c("#FFFFFF")


summary(Fagus_1$taxon.abundance)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.000000 0.001194 0.005070 0.013096 0.019865 0.078453       13 


#save(Fagus_1,file="Fagus_1.rdata")

#tring different threshold
p<-Fagus_1 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.0005,0.005,0.01,0.02,  0.03,0.04,0.05, 0.06,0.07,Inf),
        labels = c("absence","0","0.005","0.01","0.02",  "0.03","0.04","0.05", "0.06","0.07+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" ,  "0.005"="#59A8BB","0.01"="#78B7C5","0.02"="#B1C177","0.03"="#EBCC2A", "0.04"="#E5BD15", "0.05"="#E1AF00",
                                                                     "0.06"="#E96400" ,"0.07+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p
ggsave(file="Fagus 1-1.2ka Map 0.0005 threshold.jpeg",p,width=22,height=11)


p<-Fagus_1 %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.005,0.01,0.02,  0.03,0.04,0.05, 0.06,0.07,Inf),
        labels = c("absence","0","0.01","0.02",  "0.03","0.04","0.05", "0.06","0.07+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" ,"0.01"="#78B7C5","0.02"="#B1C177","0.03"="#EBCC2A", "0.04"="#E5BD15", "0.05"="#E1AF00",
                                                                     "0.06"="#E96400" ,"0.07+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p
ggsave(file="Fagus 1-1.2ka Map 0.005 threshold.jpeg",p,width=22,height=11)


################finding below which value can the abundance be seen as 0 with comparison to modern in EFI
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/EFI/EUTreeSpeciesMap_GeoTIFF/EU_TreeMap")
install.packages("tiff")
library(tiff)
install.packages("rgdal")
library(rgdal)
library(raster)
library(rtiff)
# Fagus_EFI<-'FagusSpp.tif' 
# Fagus_EFI
# Fagus_EFI = readTIFF("FagusSpp.tif")

# view attributes associated with your DTM geotiff
GDALinfo("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/EFI/EUTreeSpeciesMap_GeoTIFF/EU_TreeMap/FagusSpp.tif")
GDALinfo("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/EFI/EUTreeSpeciesMap_GeoTIFF/EU_TreeMap/PiceaSpp.tif")
GDALinfo("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/EFI/EUTreeSpeciesMap_GeoTIFF/EU_TreeMap/QuercusRoburPetraea.tif")


# view attributes / metadata of raster
# rows        4032 
# columns     3847 
# bands       1 
# open raster data
Fagus_EFI <- raster(x = "FagusSpp.tif")
Picea_EFI <- raster(x = "PiceaSpp.tif")
Quercus.d_EFI <- raster(x = "QuercusRoburPetraea.tif")
# view crs
crs(Fagus_EFI)
crs(Picea_EFI)
crs(Quercus.d_EFI)
# Coordinate Reference System:
#   Deprecated Proj.4 representation:
#   +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs 
# WKT2 2019 representation:blabla
# view extent via the slot - note that slot names can change so this may not always work.
Fagus_EFI@extent
Picea_EFI@extent
Quercus.d_EFI@extent
# class      : Extent 
# xmin       : 2635000 
# xmax       : 6482000 
# ymin       : 1385000 
# ymax       : 5417000 
nlayers(Fagus_EFI)
#[1] 1

plot(Fagus_EFI)
plot(Picea_EFI)
plot(Quercus.d_EFI)



setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
# # 1. Open jpeg file
# jpeg("Fagus_EFI.jpeg", width = 22, height = 11,units="in",res=4)
# # 2. Create the plot
# plot(Fagus_EFI)
# # 3. Close the file
# dev.off()


jpeg('Fagus_EFI.jpeg', height=4032*2, width=3847*2)
plot(Fagus_EFI, maxpixels=ncell(Fagus_EFI))
dev.off()

jpeg('Picea_EFI.jpeg', height=4032*2, width=3847*2)
plot(Picea_EFI, maxpixels=ncell(Picea_EFI))
dev.off()

jpeg('Quercus.d_EFI.jpeg', height=4032*2, width=3847*2)
plot(Quercus.d_EFI, maxpixels=ncell(Quercus.d_EFI))
dev.off()

nrow(Fagus_EFI)
ncol(Fagus_EFI)

jpeg('Fagus_EFI.jpeg', height=nrow(Fagus_EFI), width=ncol(Fagus_EFI)) 
plot(Fagus_EFI, maxpixels=ncell(Fagus_EFI))
dev.off()


#crop
# 
# sp <- SpatialPoints( tps_alpha_2_elv_w_cut[ c("longitude" , "latitude") ], proj4string = CRS("+proj=longlat +datum=WGS84") )
# r <- raster(nrow=5, ncol=5, extent( sp ) )
# r
# 
# Fagus_EFI1 <- projectRaster(Fagus_EFI, 
#                                   crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 
# 
# library(rgdal)
# Fagus_t <- spTransform(Fagus_EFI, crs(cut_boundary))
# crs(Fagus_EFI)
# crs(Fagus_EFI) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
# Fagus_t <- spTransform(Fagus_EFI,"+init=epsg:4326")
# 
# 
# library(sf)
# CHM_HARV_Cropped <- crop(x = CHM_HARV, y = aoi_boundary_HARV)
# 
# cut_boundary <- st_as_sf(tps_alpha_2_elv_w_cut, coords = c('longitude', 'latitude'),crs = 4326)
# 
# Fagus_Cropped <- crop(x = Fagus_EFI, y = cut_boundary)
# r <- raster( crs=proj4string(cut_boundary))
# 
# 
# cut_boundary
# e <- as(extent(c(xmin= -9.87499 , xmax= 44.95835 , ymin= 31.45833, ymax= 71.125)), 'SpatialPolygons')
# crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
# crs(Fagus_EFI) <- "+proj=longlat +datum=WGS84 +no_defs" 
# r <- crop(Fagus_EFI, e)
# Fagus_EFI@extent
# extent(cut_boundary)

##################################################################binning


# columns_to_keep=c("latitude","longitude","Fagus", "Picea", "Quercus.deciduous")
# paleo_compare<-raw_paleo[columns_to_keep]
# paleo_compare$Fagus<-paleo_compare$Fagus/rowSums(paleo_compare[,3:5])
paleo_compare<-core[c("Fagus", "Picea", "Quercus.deciduous")]
paleo_compare$latitude<-raw_paleo$latitude
paleo_compare$longitude<-raw_paleo$longitude
paleo_compare$age<-raw_paleo$median
paleo_compare$elv<-raw_paleo$elevation

summary(paleo_compare$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -96    1631    3995    4517    7017   12000 

library(dplyr)


#changing original age to the format of age as previously in the binning process
paleo_compare_1<-paleo_compare[which(paleo_compare$age>=0),]
# paleo_compare_1<-paleo_compare_1 %>% mutate(age_2 = cut_width(age, width = 200, center = -100))
# paleo_compare_1$age_3<-as.numeric(as.factor(paleo_compare_1$age_2))

paleo_compare_1<-paleo_compare_1 %>% mutate(age_2 = cut(age, breaks = seq(0,12000,200), labels=seq(100,12000,200),include.lowest = TRUE))



#age 3 is in the format of binning reconstructed values

save(paleo_compare_1,file="paleo_compare_1.rdata")
getwd()
# 
# library(dplyr)
# for (i in seq(0,12000,200)){
#   if (i<paleo_compare_1$age &paleo_compare_1$age<i+200){
#   #select(paleo_compare_1,num_range["age",i:i+200])
#   paleo_compare_1$age_2<-i+100}
#  else ( i=i+200)
# }
# 
# paleo_compare_1$age_3<-median(paleo_compare_1$age_2)
# 
# library(dplyr)
# for (i in seq(0,12000,200)){
#   temp = paleo_compare_1 %>% filter(i<age<i+200)
#   
#     #select(paleo_compare_1,num_range["age",i:i+200])
#     paleo_compare_1$age_2<-i+100}
#   else ( i=i+200)
# }




#subset 10 and 11 white space issue- not fixed yet
subset11_<-paleo_compare_1 %>% filter(age_2 ==11100)
subset11_2 <- subset11 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset11_3 <- subset11_2 %>%
  dplyr::filter(!is.na(se_Tmax_anomaly))#486 out of 714 left
subset11_3 <- subset11_3 %>%
  dplyr::filter(!is.na(Tmax_anomaly))#486 out of 714 left

vec1 <- subset11_3$se_Tmax_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmax_anomaly as weighting
tps_tmax_11_elv_w <- subset11_3 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS,
    weights = vec2
  )









paleo_compare_1= st_as_sf(paleo_compare_1, coords = c('longitude', 'latitude'),crs = 4326)
ggplot1=ggplot() + 
  geom_sf(data = gradient_white, show.legend = TRUE) +
  scale_color_viridis(option="viridis",discrete=FALSE,direction=-1) +
  geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-12, 45), ylim = c(29.4, 79.7742), expand = FALSE) 
print(ggplot1)






get_gradient<-function(plotdata,age,range){
  plotdata$site<-as.factor(plotdata$site)
  mean_env<-data.frame(matrix(NA,ncol=5+4+4,nrow=nlevels(plotdata$site)))
  for(m in 1:nlevels(plotdata$site)){
    plotdata_each<-plotdata[which(plotdata$site==levels(plotdata$site)[m]),]
    
    plot_Tmin<-plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),c("Tmin","sse_Tmin")]
    mean_Tmin<-mean(as.matrix(plot_Tmin$Tmin),na.rm = TRUE)
    se_mean_Tmin<-sqrt(sum(plot_Tmin$sse_Tmin^2))/nrow(plot_Tmin)
    
    plot_Tmax<-plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),c("Tmax","sse_Tmax")]
    mean_Tmax<-mean(as.matrix(plot_Tmax$Tmax),na.rm = TRUE)
    se_mean_Tmax<-sqrt(sum(plot_Tmax$sse_Tmax^2))/nrow(plot_Tmax)
    
    plot_gdd<-plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),c("gdd","sse_gdd")]
    mean_gdd<-mean(as.matrix(plot_gdd$gdd),na.rm = TRUE)
    se_mean_gdd<-sqrt(sum(plot_gdd$sse_gdd^2))/nrow(plot_gdd)
    
    plot_alpha<-plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),c("alpha","sse_alpha")]
    mean_alpha<-mean(as.matrix(plot_alpha$alpha),na.rm = TRUE)
    se_mean_alpha<-sqrt(sum(plot_alpha$sse_alpha^2))/nrow(plot_alpha)
    
    mean_gdd<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"gdd"]),na.rm = TRUE)
    mean_alpha<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"alpha"]),na.rm = TRUE)
    mean_Tmax<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"Tmax"]),na.rm = TRUE)
    
    # mean_insol_summer<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"insol_summer"]),na.rm = TRUE)
    # mean_insol_winter<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"insol_winter"]),na.rm = TRUE)
    #mean_env[m,]<-c(age,levels(plotdata$site)[m],unique(plotdata_each$lon),unique(plotdata_each$lat),unique(plotdata_each$elv),mean_Tmin,mean_gdd,mean_alpha,mean_Tmax,mean_insol_summer,mean_insol_winter)
    mean_env[m,]<-c(age,levels(plotdata$site)[m],unique(plotdata_each$lon),unique(plotdata_each$lat),unique(plotdata_each$elv),mean_Tmin,mean_gdd,mean_alpha,mean_Tmax,se_mean_Tmin,se_mean_Tmax,se_mean_gdd,se_mean_alpha)
  }
  colnames(mean_env)<-c("age","site","lon","lat","elv","mean_Tmin","mean_gdd","mean_alpha","mean_Tmax","se_mean_Tmin","se_mean_Tmax","se_mean_gdd","se_mean_alpha")
  return(mean_env)
}

# Get fossil gradient 
#1950 is 0, bin is 200 yr

# age<-1000*seq(from = 0.1, to = 11.9, by = 0.2)
# range<-100
# gradient_env<-data.frame()
# for(i in 1:length(age)){
#   each<-get_gradient(plotdata,age[i],range)
#   gradient_env<-rbind.data.frame(gradient_env,each)
# }
gradient_env<-rbind.data.frame(
  get_gradient(plotdata,100,100),get_gradient(plotdata,300,100),
  get_gradient(plotdata,500,100),get_gradient(plotdata,700,100),
  get_gradient(plotdata,900,100),get_gradient(plotdata,1100,100),
  get_gradient(plotdata,1300,100),get_gradient(plotdata,1500,100),
  get_gradient(plotdata,1700,100),get_gradient(plotdata,1900,100),
  get_gradient(plotdata,2100,100),get_gradient(plotdata,2300,100),
  get_gradient(plotdata,2500,100),get_gradient(plotdata,2700,100),
  get_gradient(plotdata,2900,100),get_gradient(plotdata,3100,100),
  get_gradient(plotdata,3300,100),get_gradient(plotdata,3500,100),
  get_gradient(plotdata,3700,100),get_gradient(plotdata,3900,100),
  get_gradient(plotdata,4100,100),get_gradient(plotdata,4300,100),
  get_gradient(plotdata,4500,100),get_gradient(plotdata,4700,100),
  get_gradient(plotdata,4900,100),get_gradient(plotdata,5100,100),
  get_gradient(plotdata,5300,100),get_gradient(plotdata,5500,100),
  get_gradient(plotdata,5700,100),get_gradient(plotdata,5900,100),
  get_gradient(plotdata,6100,100),get_gradient(plotdata,6300,100),
  get_gradient(plotdata,6500,100),get_gradient(plotdata,6700,100),
  get_gradient(plotdata,6900,100),get_gradient(plotdata,7100,100),
  get_gradient(plotdata,7300,100),get_gradient(plotdata,7500,100),
  get_gradient(plotdata,7700,100),get_gradient(plotdata,7900,100),
  get_gradient(plotdata,8100,100),get_gradient(plotdata,8300,100),
  get_gradient(plotdata,8500,100),get_gradient(plotdata,8700,100),
  get_gradient(plotdata,8900,100),get_gradient(plotdata,9100,100),
  get_gradient(plotdata,9300,100),get_gradient(plotdata,9500,100),
  get_gradient(plotdata,9700,100),get_gradient(plotdata,9900,100),
  get_gradient(plotdata,10100,100),get_gradient(plotdata,10300,100),
  get_gradient(plotdata,10500,100),get_gradient(plotdata,10700,100),
  get_gradient(plotdata,10900,100),get_gradient(plotdata,11100,100),
  get_gradient(plotdata,11300,100),get_gradient(plotdata,11500,100),
  get_gradient(plotdata,11700,100),get_gradient(plotdata,11900,100))



###############################compare CRU distribution to EFI to define 0 abundance
#cleaning CRU data
CRU_gam<-CRU[c("lon", "lat", "gdd0","Tmin","MI")]

CRU_gam$rtmi=sqrt(CRU_gam$MI)


names(CRU_gam)[names(CRU_gam) == 'Tmin'] <- 'tmin'
names(CRU_gam)[names(CRU_gam) == 'gdd0'] <- 'gdd'
names(CRU_gam)[names(CRU_gam) == 'lon'] <- 'longitude'
names(CRU_gam)[names(CRU_gam) == 'lat'] <- 'latitude'



#CRU data through GAM
#GAM output

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

library(nlme)
library(mgcv)
library(reshape2)

bookss=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/we_.csv")
taxon=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/we_.csv",header=FALSE,nrow=1)


taxon_1=taxon[,c("V80","V138","V159")] 
bookss_1=bookss[,c("rtmi","Tmin","gdd","Fagus","Picea","Quercus.deciduous")] 

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= CRU_gam,type="response"),nrow=nrow( CRU_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  CRU_gam$'taxon abundance'=abundance 
  
  
  write.csv(CRU_gam,file = paste(taxon.name," CRU modern.csv",sep=""), row.names=FALSE)
}


#CRU GAM map

#Fagus
Fagus_m=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Fagus CRU modern.csv")

Fagus_m$taxon.abundance=as.numeric(Fagus_m$taxon.abundance)

p<-Fagus_m %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

library(ggplot2)
ggsave(file="Fagus CRU modern Map.jpeg",p,width=22,height=11)

summary(Fagus_m$taxon.abundance)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.000e-07 1.437e-05 1.328e-04 4.516e-03 1.325e-03 7.817e-02 

p<-Fagus_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.005,0.01,0.02,  0.03,0.04,0.05, 0.06,0.07,Inf),
        labels = c("absence","0","0.01","0.02",  "0.03","0.04","0.05", "0.06","0.07+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" ,  "0.01"="#78B7C5","0.02"="#B1C177","0.03"="#EBCC2A", "0.04"="#E5BD15", "0.05"="#E1AF00",
                                                                     "0.06"="#E96400" ,"0.07+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p

ggsave(file="Fagus CRU modern Map 0.005 threshold.jpeg",p,width=22,height=11)


p<-Fagus_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.0005,0.005,0.01,0.02,  0.03,0.04,0.05, 0.06,0.07,Inf),
        labels = c("absence","0","0.005","0.01","0.02",  "0.03","0.04","0.05", "0.06","0.07+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" ,  "0.005"="#59A8BB","0.01"="#78B7C5","0.02"="#B1C177","0.03"="#EBCC2A", "0.04"="#E5BD15", "0.05"="#E1AF00",
                                                                     "0.06"="#E96400" ,"0.07+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p
ggsave(file="Fagus CRU modern Map 0.0005 threshold.jpeg",p,width=22,height=11)



p<-Fagus_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.025, 0.03,0.04,0.05, 0.06,0.07,Inf),
        labels = c("absence","0", "0.03","0.04","0.05", "0.06","0.07+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2","0.03"="#6BB1C1", "0.04"="#BDC367", "0.05"="#E6C019",
                                                                     "0.06"="#E49100" ,"0.07+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

#"#3B9AB2" "#6BB1C1" "#BDC367" "#E6C019" "#E49100" "#F21A00"
ggsave(file="Fagus CRU modern Map 0.025 threshold colour 3.jpeg",p,width=22,height=11)

#Picea
Picea_m=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Picea CRU modern.csv")

Picea_m$taxon.abundance=as.numeric(Picea_m$taxon.abundance)

p<-Picea_m %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

library(ggplot2)
ggsave(file="Picea CRU modern Map.jpeg",p,width=22,height=11)

summary(Picea_m$taxon.abundance)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000001 0.0013271 0.0088107 0.0442336 0.0511347 0.6781822

#Picea with better colour, as original colour does not show a good pattern due to big pollen values

p<-Picea_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.005,0.01,0.02,  0.03,0.04,0.05, 0.06,0.07,Inf),
        labels = c("absence","0","0.01","0.02",  "0.03","0.04","0.05", "0.06","0.07+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" ,  "0.01"="#78B7C5","0.02"="#B1C177","0.03"="#EBCC2A", "0.04"="#E5BD15", "0.05"="#E1AF00",
                                                                     "0.06"="#E96400" ,"0.07+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p

ggsave(file="Picea CRU modern Map 0.005 threshold.jpeg",p,width=22,height=11)


p<-Picea_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.0005,0.005,0.01,0.02,  0.03,0.04,0.05, 0.06,0.07,Inf),
        labels = c("absence","0","0.005","0.01","0.02",  "0.03","0.04","0.05", "0.06","0.07+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" ,  "0.005"="#59A8BB","0.01"="#78B7C5","0.02"="#B1C177","0.03"="#EBCC2A", "0.04"="#E5BD15", "0.05"="#E1AF00",
                                                                     "0.06"="#E96400" ,"0.07+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p
ggsave(file="Picea CRU modern Map 0.0005 threshold.jpeg",p,width=22,height=11)



p<-Picea_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.01,0.02,  0.03,0.04,0.05, 0.06,0.07,0.08,Inf),
        labels = c("absence","0","0.02",  "0.03","0.04","0.05", "0.06","0.07","0.08+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" ,  "0.02"="#78B7C5","0.03"="#B1C177","0.04"="#EBCC2A", "0.05"="#E5BD15", "0.06"="#E1AF00",
                                                                     "0.07"="#E96400" ,"0.08+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p
ggsave(file="Picea CRU modern Map 0.01 threshold colour 2.jpeg",p,width=22,height=11)


#Quercus.deciduous
Quercus.deciduous_m=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data/Quercus.deciduous CRU modern.csv")

Quercus.deciduous_m$taxon.abundance=as.numeric(Quercus.deciduous_m$taxon.abundance)

p<-Quercus.deciduous_m %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")

library(ggplot2)
ggsave(file="Quercus.deciduous CRU modern Map.jpeg",p,width=22,height=11)

summary(Quercus.deciduous_m$taxon.abundance)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000006 0.001490 0.007693 0.025468 0.025329 0.462823 

#Quercus.deciduous with better colour, as original colour does not show a good pattern due to big pollen values

p<-Quercus.deciduous_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.005,0.01,0.02,  0.03,0.04,0.05, 0.06,0.07,Inf),
        labels = c("absence","0","0.01","0.02",  "0.03","0.04","0.05", "0.06","0.07+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" ,  "0.01"="#78B7C5","0.02"="#B1C177","0.03"="#EBCC2A", "0.04"="#E5BD15", "0.05"="#E1AF00",
                                                                     "0.06"="#E96400" ,"0.07+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p

ggsave(file="Quercus.deciduous CRU modern Map 0.005 threshold.jpeg",p,width=22,height=11)


p<-Quercus.deciduous_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.0005,0.005,0.01,0.02,  0.03,0.04,0.05, 0.06,0.07,Inf),
        labels = c("absence","0","0.005","0.01","0.02",  "0.03","0.04","0.05", "0.06","0.07+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" ,  "0.005"="#59A8BB","0.01"="#78B7C5","0.02"="#B1C177","0.03"="#EBCC2A", "0.04"="#E5BD15", "0.05"="#E1AF00",
                                                                     "0.06"="#E96400" ,"0.07+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p
ggsave(file="Quercus.deciduous CRU modern Map 0.0005 threshold.jpeg",p,width=22,height=11)


p<-Quercus.deciduous_m %>%
  dplyr::mutate(
    taxon.abundance = taxon.abundance %>%
      cut(
        breaks = c(-Inf,0.01,0.02,  0.03,0.04,0.05, 0.06,0.07,0.1,Inf),
        labels = c("absence","0","0.02",  "0.03","0.04","0.05", "0.06","0.07","0.1+"),
      )
  )  %>%
  smpds::plot_climate_tiles(var = "taxon.abundance",
                            fill_scale =
                              ggplot2::scale_fill_manual(name = "taxon abundance",
                                                         values = c( "absence"="#CCCCCC" ,  "0"="#3B9AB2" ,  "0.02"="#78B7C5","0.03"="#B1C177","0.04"="#EBCC2A", "0.05"="#E5BD15", "0.06"="#E1AF00",
                                                                     "0.07"="#E96400" ,"0.1+"="#F21A00") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73), )

p
ggsave(file="Quercus.deciduous CRU modern Map 0.01 threshold colour 2.jpeg",p,width=22,height=11)
