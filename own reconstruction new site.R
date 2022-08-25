#own reconstruction new site

#04-05 new data

#my own reconstruction using the new data Roberto gave me on 04-05

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new site.RData")
#load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/special epd comparison Quercus new.RData")

save.image(file = "reconstruction new site.RData")
#load("C:/Users/gt221/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new.RData")
#load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new.RData")
#save.image(file = "special epd comparison Quercus new.RData")

########################################################################################################
#################################    Maps of the climate space  ########################################
########################################################################################################
library(raster)
library(rgdal) # for spTransform
if(!require(ncdf4)){ install.packages("ncdf4");library(ncdf4)}

#get CRU background for climate space
#from Turner, M., Wei, D., Prentice, I., & Harrison, S. (2021). The impact of methodological decisions on climate reconstructions using WA-PLS. Quaternary Research, 99, 341-356. doi:10.1017/qua.2020.44

CRU_Tmin_gdd<-read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/CRU Europe Sep18.csv")
colnames(CRU_Tmin_gdd)<-c("ID","lat","lon","elv","Tmin","gdd0")
#ggplot(CRU_Tmin_gdd,aes(lon,lat,color=Tmin))+geom_point()
#ggplot(CRU_Tmin_gdd,aes(lon,lat,color=gdd0))+geom_point()

#from Roberto
CRU_MI<-brick("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/cru_ts4.04-clim-1961-1990-mi.nc")
CRU_MI<-rasterToPoints(CRU_MI, spatial=TRUE)
CRU_MI<-spTransform(CRU_MI, CRS(CRU_MI@proj4string@projargs))
CRU_MI <- data.frame(lon=coordinates(CRU_MI)[,1],lat=coordinates(CRU_MI)[,2],CRU_MI@data)  
CRU_MI<-CRU_MI[which(CRU_MI$lon>=(-21)&CRU_MI$lon<=150&CRU_MI$lat>=29&CRU_MI$lat<=82),]
colnames(CRU_MI)<-c("lon","lat","MI")
#ggplot(CRU_MI,aes(lon,lat,color=MI))+geom_point()

CRU_gdd<-brick("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/cru_ts4.04-clim-1961-1990-daily.tmp-gdd0.nc")
CRU_gdd<-rasterToPoints(CRU_gdd, spatial=TRUE)
CRU_gdd<-spTransform(CRU_gdd, CRS(CRU_gdd@proj4string@projargs))
CRU_gdd <- data.frame(lon=coordinates(CRU_gdd)[,1],lat=coordinates(CRU_gdd)[,2],CRU_gdd@data)  
CRU_gdd<-CRU_gdd[which(CRU_gdd$lon>=(-21)&CRU_gdd$lon<=150&CRU_gdd$lat>=29&CRU_gdd$lat<=82),]
colnames(CRU_gdd)<-c("lon","lat","gdd")
#ggplot(CRU_gdd,aes(lon,lat,color=gdd))+geom_point()

CRU<-merge(CRU_MI,CRU_gdd,by=c("lon","lat"))
CRU<-merge(CRU,CRU_Tmin_gdd,by=c("lon","lat"))

for(i in 1:nrow(CRU)){
  #i=i+1
  CRU[i,"Tmax"]<-get_MTWA(MTCO=CRU[i,"Tmin"],GDD0=CRU[i,"gdd"])
}
summary(CRU$Tmax);hist(CRU$Tmax)

MI<-CRU$MI
w<-3
fai<-1/MI
F<-1+fai-(1+fai^w)^(1/w)
alpha<-1.26*MI*F
hist(alpha)
plot(alpha~MI)
CRU$alpha<-alpha

summary(lm(CRU$gdd0~CRU$gdd)) #values from Mark and from Roberto are comparable

i=1
for(i in 1:nrow(modern_gradient)){
  #i=i+1
  sitelat<-modern_gradient[i,"lat"]
  sitelon<-modern_gradient[i,"lon"]
  modern_gradient[i,"CRU_Tmin"]<-mean(CRU[which(CRU$lat>(sitelat-0.5/2)&CRU$lat<=(sitelat+0.5/2)&
                                                  CRU$lon>(sitelon-0.5/2)&CRU$lon<=(sitelon+0.5/2)),"Tmin"],na.rm=T)

  modern_gradient[i,"CRU_Tmax"]<-mean(CRU[which(CRU$lat>(sitelat-0.5/2)&CRU$lat<=(sitelat+0.5/2)&
                                                      CRU$lon>(sitelon-0.5/2)&CRU$lon<=(sitelon+0.5/2)),"Tmax"],na.rm=T)
  modern_gradient[i,"CRU_gdd"]<-mean(CRU[which(CRU$lat>(sitelat-0.5/2)&CRU$lat<=(sitelat+0.5/2)&
                                                      CRU$lon>(sitelon-0.5/2)&CRU$lon<=(sitelon+0.5/2)),"gdd"],na.rm=T)
  modern_gradient[i,"CRU_alpha"]<-mean(CRU[which(CRU$lat>(sitelat-0.5/2)&CRU$lat<=(sitelat+0.5/2)&
                                                 CRU$lon>(sitelon-0.5/2)&CRU$lon<=(sitelon+0.5/2)),"alpha"],na.rm=T)
  modern_gradient[i,"CRU_elv"]<-CRU[i,"elv"]
}
ggplot(data=modern_gradient,aes(CRU_Tmin,mean_Tmin))+theme_bw()+geom_point()+geom_abline(intercept = 0,slope=1)
ggplot(data=modern_gradient,aes(CRU_Tmax,mean_Tmax))+theme_bw()+geom_point()+geom_abline(intercept = 0,slope=1)
ggplot(data=modern_gradient,aes(CRU_gdd,mean_gdd))+theme_bw()+geom_point()+geom_abline(intercept = 0,slope=1)
ggplot(data=modern_gradient,aes(CRU_alpha,mean_alpha))+theme_bw()+geom_point()+geom_abline(intercept = 0,slope=1)

saveRDS(modern_gradient, file = "modern_gradient new site with CRU.RDS") 



#checking extreme values
dd=modern_gradient %>%
filter(CRU_gdd>=6000) 
#100
#El Acebron
dd=modern_gradient %>%
  filter(CRU_gdd/mean_gdd>=3|mean_gdd/CRU_gdd>=3) 
write.csv(dd, "site with CRU gdd 0.3 or 3 times reconstructed GDD.csv")#9 obs

dd=modern_gradient %>%
  filter(CRU_Tmin-mean_Tmin>=5|mean_Tmin/CRU_Tmin>=5) 
write.csv(dd, "site with Tmin difference of and over 5 degrees.csv")#296 out of 1302 sites

dd=modern_gradient %>%
  filter(CRU_Tmax-mean_Tmax>=5|mean_Tmax/CRU_Tmax>=5) 
write.csv(dd, "site with Tmax difference of and over 5 degrees.csv")#47 out of 1302 sites

########################################################################################################
####################################  anomaly sse  ##################################################
########################################################################################################


########################################################################################################
####################################  my own Reconstruction  ##################################################
########################################################################################################
############ Import dataset
#install.packages("special.epd")
#And the development version from GitHub with:
#install.packages("remotes")
#remotes::install_github("special-uor/special.epd", "dev")
#data("entity", package = "special.epd")


#import the raw data from rob
#setwd("C:/Users/gt221/OneDrive - Imperial College London/Tree Migration- Colin/data/special-epd_2022-04-05")

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/special-epd_2022-04-05")
library(tidyverse)

entity_tb <- readr::read_csv("special_epd_2022-05-04_metadata_2.csv")
dates_tb <- readr::read_csv("special_epd_2022-04-05_dates.csv")
samples_tb <- readr::read_csv("special_epd_2022-04-05_samples.csv")
age_model_tb <- readr::read_csv("special_epd_2022-04-05_age_model.csv")
#pollen_counts_clean_tb <- readr::read_csv("special_epd_2022-04-05_pollen_counts_clean.csv", col_types = readr::cols(.default = readr::col_double()))
pollen_counts_amalgamated_tb <- readr::read_csv("special_epd_2022-04-05_pollen_counts_amalgamated.csv", col_types = readr::cols(.default = readr::col_double()))
#pollen_counts_intermediate_tb <- readr::read_csv("special_epd_2022-04-05_pollen_counts_intermediate.csv",col_types = readr::cols(.default = readr::col_double()))
#rm(pollen_counts_intermediate_tb)

# left join in R - join the dataframes! 72420 observations
merge1<-merge(x=entity_tb,y=dates_tb,by="ID_ENTITY",all.x=TRUE)
merge2<-merge(x=entity_tb,y=samples_tb,by="ID_ENTITY",all.x=TRUE)
merge3<-merge(x=age_model_tb,y=samples_tb,by="ID_SAMPLE",all.x=TRUE)
merge4<-merge(x=pollen_counts_amalgamated_tb,y=samples_tb,by="ID_SAMPLE",all.x=TRUE)
merge5<-merge(x=merge3,y=pollen_counts_amalgamated_tb,by="ID_SAMPLE",all.x=TRUE)
merge6<-merge(x=entity_tb,y=merge5,by="ID_ENTITY",all.x=TRUE)#which(is.na(merge6$publication))

write.csv(merge6, "raw paleo pollen data merge 6.csv")#96145 obs


#install.packages("readxl")
library(readxl)

raw_paleo<-merge6[which(merge6$median<=12000),] #get samples during the Holocene,84051 observations left
colnames(raw_paleo);str(raw_paleo)
summary(raw_paleo[,"median"]) #use INTCAL20 median age
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-96    1748    4222    4710    7333   12000 



############get average resolution
#this is a new part added to the code
resolution<-data.frame()
raw_paleo1<-raw_paleo
raw_paleo1$entity_name<-as.factor(raw_paleo1$entity_name)

for(k in 1:nlevels(raw_paleo1$entity_name)){
  eachsite<-raw_paleo1[which(raw_paleo1$entity_name==levels(raw_paleo1$entity_name)[k]),]
  eachsite<-eachsite[order(eachsite$median),]
  resolution<-rbind.data.frame(resolution,as.matrix(eachsite[-1,"median"])-as.matrix(eachsite[-nrow(eachsite),"median"]))
 # print(c(k,summary(resolution$median)))
}
mean(resolution$V1)
# [1] 119.0186
summary(resolution$V1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0      33      75     119     145    8887 



#############Exclude samples with large age uncertainty
#calculate the standard error/deviation of the uncertainties, 1.645 is the z score for 90%. 
#90% is approximation of 95%-5% as it might not be a normal distribution.
#90%=2x1.645×standard error
summary(abs(raw_paleo[,"UNCERT_95"]-raw_paleo[,"UNCERT_5"])/(2*1.645))
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000    5.775   15.198   36.484   38.602 1681.155  

which_age_keep<-which(abs((raw_paleo[,"UNCERT_95"]-raw_paleo[,"UNCERT_5"])/(2*1.645))<=100)
raw_paleo<-raw_paleo[which_age_keep,] #76890 observations left


#Extract fossil taxa
#old woody list with Carpinus.betulus,Picea.orientalis,Quercus.intermediate,Ulmus.Zelkova removed. Names match with modern data, 'taxa'.
woody_list=c("Abies",
             "Acer",
             "Alnus",
             "Alnus.alnobetula",
             "Betula",
             "Betula..Chamaebetula.",
             "Buxus",
             "Carpinus.orientalis.Ostrya",
             "Castanea",
             "Cedrus",
             "Corylus",
             "Fagus",
             "Ilex",
             "Juglans",
             "Larix",
             "Picea",
             "Pinus..diploxylon.",
             "Pinus..haploxylon.",
             "Platanus",
             "Populus",
             "Quercus.deciduous",
             "Quercus.evergreen",
             "Salix",
             "Sorbus",
             "Taxus",
             "Tilia",
             "Ulmus"
             
)



woody_list[!woody_list %in% colnames(raw_paleo)] #checking if the taxa in woody_list are all present in raw paleo data

#[1] "Alnus.alnobetula"           "Betula..Chamaebetula."      "Carpinus.orientalis.Ostrya" "Pinus..diploxylon."        
#[5] "Pinus..haploxylon."         "Quercus.deciduous"          "Quercus.evergreen"   

#rename the column in the raw paleo as we will need to match them up with modern ones later
names(raw_paleo)[names(raw_paleo) == "Alnus subgen Alnobetula"] <- "Alnus.alnobetula"
names(raw_paleo)[names(raw_paleo) == "Betula (Chamaebetula)"] <- "Betula..Chamaebetula."
names(raw_paleo)[names(raw_paleo) == "Carpinus/Ostrya"] <- "Carpinus.orientalis.Ostrya"#name mismatch. Awaiting decision from Colin and Sandy
names(raw_paleo)[names(raw_paleo) == "Pinus (Diploxylon)"] <- "Pinus..diploxylon."
names(raw_paleo)[names(raw_paleo) == "Pinus (Haploxylon)"] <- "Pinus..haploxylon."
#names(raw_paleo)[names(raw_paleo) == "Quercus deciduous"] <- "Quercus.deciduous"
names(raw_paleo)[names(raw_paleo) == "Quercus evergreen"] <- "Quercus.evergreen"



#Quercus and Quercus deciduous are the same thing
#Sandy has confirmed that there is indeed a taxon called "Quercus" in the data set and that it refers to the (many!) cases where the analyst has recorded "Quercus", rather than classifying it as deciduous or evergreen. This is very common in the regions where there are only deciduous oaks. In soutern Europe both types are present and they are routinely distinguished.
#Therefore, it is safe (enough) to assume that "Quercus" actually means "Quercus (deciduous)", and you should make maps based on the sum of "Quercus" and "Quercus (deciduous)".
raw_paleo$`Quercus deciduous`[is.na(raw_paleo$`Quercus deciduous`)]<-0
raw_paleo$Quercus[is.na(raw_paleo$Quercus)]<-0

raw_paleo$Quercus.deciduous=raw_paleo$`Quercus deciduous`+raw_paleo$Quercus


 # Quercus<-raw_paleo[c("Quercus", "Quercus deciduous", "Quercus.deciduous","median")]
 # Quercus_1<-Quercus%>% filter(1000<=median&median <=1200)

#check if the paleo taxa names match with modern ones again.
woody_list[!woody_list %in% colnames(raw_paleo)]
#character(0)
#after new data is provided by Roberto on 05-04, all taxa remains now


core0<-raw_paleo[woody_list]

core0[is.na(core0)]<-0
str(core0)
core<-core0


#sum(is.na(core0$Quercus.deciduous))



#get common taxa
colnames(core)[!(colnames(core)%in% colnames(taxa))]#should be character(0)
colnames(taxa)[!(colnames(taxa)%in% colnames(core))]
colnames(core)[(colnames(core)%in% colnames(taxa))]

#make the fossil taxa name match with modern taxa name

#dont really need the first few lines as I selected taxa manuanlly, rather than in Mengmeng's code,
#where the taxa is selected using a range.
#taxa_to_delete<-colnames(core)[!(colnames(core)%in% colnames(taxa))]
#core[,taxa_to_delete]<-NULL
taxa_to_add<-colnames(taxa)[!(colnames(taxa)%in% colnames(core))]#getting the taxa that is in modern taxa list but not in paleo core data
core[,taxa_to_add]<-0#add the columns with these taxa
core<-core[,order(colnames(core))]#change the order of the columns according to the column names
summary(rowSums(core,na.rm=TRUE))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0     116     266     349     477    6259 
core<-core/rowSums(core,na.rm=TRUE)#change counts to proportions/normalize it to 1.
summary(rowSums(core,na.rm=TRUE))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      1       1       1       1       1       1       284 
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  1.0000  1.0000  0.9973  1.0000  1.0000 

str(core)
write.csv(core, "core new with Quercus updated.csv")#76890 obs

# core<-read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/core new with Quercus updated.csv")
# core <- core[,-1]
# fagus<-core[c("Fagus")]
# fagus$age<-raw_paleo$median
# fagus$site<-raw_paleo$site_name
# 
# dd <- fagus  %>%      group_by(site) %>%filter(site=="Lago di Martignano")
# dd <- fagus  %>%      group_by(site) %>%filter(11800<=age&age<=12000)
# 


#Reconstruct the past climates
#setwd("C:/Users/gt221/OneDrive - Imperial College London/Tree Migration- Colin/Output data/special epd")
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/special epd")
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
#MTCO
fossil_tf_Tmin<-fxTWAPLS::TWAPLS.predict.w(fit_tf_Tmin,core)
#GDD0
fossil_tf_gdd<-fxTWAPLS::TWAPLS.predict.w(fit_tf_gdd,core)
#alpha
fossil_tf_alpha<-fxTWAPLS::TWAPLS.predict.w(fit_tf_alpha,core)
#MTWA
fossil_tf_Tmax<-fxTWAPLS::TWAPLS.predict.w(fit_tf_Tmax,core)


#Use the last significant number of components
core_sig<-cbind.data.frame(raw_paleo[,c("site_name","latitude","longitude","elevation","median")],
                           fossil_tf_Tmin[["fit"]][,4],fossil_tf_gdd[["fit"]][,2],fossil_tf_alpha[["fit"]][,3], fossil_tf_Tmax[["fit"]][,4])
colnames(core_sig)<-c("site","lat","lon","elv","age","Tmin","gdd","alpha","Tmax")
#reconstruction results
write.csv(core_sig,"Special_epd_core_sig.csv")




#Get the sample specific errors, use nboot=1000
`%>%` <- magrittr::`%>%`
#MTCO
sse_tf_Tmin<-fxTWAPLS::sse.sample(modern_taxa=taxa,modern_climate=modern_pollen$Tmin,fossil_taxa=core,trainfun=fxTWAPLS::TWAPLS.w2,predictfun=fxTWAPLS::TWAPLS.predict.w,
                                  nboot=1000,nPLS=5,nsig=4,usefx=TRUE,fx_method = "pspline",bin=0.02)%>%fxTWAPLS::pb()

save(sse_tf_Tmin, file='sse_tf_Tmin.rdata')
load('sse_tf_Tmin.rdata')

#GDD0
sse_tf_gdd<-fxTWAPLS::sse.sample(modern_taxa=taxa,modern_climate=modern_pollen$gdd,fossil_taxa=core,trainfun=fxTWAPLS::TWAPLS.w2,predictfun=fxTWAPLS::TWAPLS.predict.w,
                                 nboot=1000,nPLS=5,nsig=2,usefx=TRUE,fx_method = "pspline",bin=20)%>%fxTWAPLS::pb()
save(sse_tf_gdd, file='sse_tf_gdd.rdata')
load('sse_tf_gdd.rdata')


#alpha
sse_tf_alpha<-fxTWAPLS::sse.sample(modern_taxa=taxa,modern_climate=modern_pollen$alpha,fossil_taxa=core,trainfun=fxTWAPLS::TWAPLS.w2,predictfun=fxTWAPLS::TWAPLS.predict.w,
                                   nboot=1000,nPLS=5,nsig=3,usefx=TRUE,fx_method = "pspline",bin=0.002)%>%fxTWAPLS::pb()
save(sse_tf_alpha, file='sse_tf_alpha.rdata')
load('sse_tf_alpha.rdata')

#MTWA
sse_tf_Tmax<-fxTWAPLS::sse.sample(modern_taxa=taxa,modern_climate=modern_pollen$Tmax,fossil_taxa=core,trainfun=fxTWAPLS::TWAPLS.w2,predictfun=fxTWAPLS::TWAPLS.predict.w,
                                  nboot=1000,nPLS=5,nsig=4,usefx=TRUE,fx_method = "pspline",bin=0.02)%>%fxTWAPLS::pb()

save(sse_tf_Tmax, file='sse_tf_Tmax.rdata')
load('sse_tf_Tmax 1.rdata')


sse_core_sig<-cbind.data.frame(sse_tf_Tmin,sse_tf_gdd,sse_tf_alpha,sse_tf_Tmax)
colnames(sse_core_sig)<-c("sse_Tmin","sse_gdd","sse_alpha","sse_Tmax")
write.csv(sse_core_sig,"Special_epd_sse_core_sig.csv")

save(sse_core_sig, file='sse_core_sig.rdata')

#merge after creating ID
sse_core_sig$Id  <- 1:nrow(sse_core_sig)
core_sig$Id  <- 1:nrow(core_sig)
total <- merge(core_sig, sse_core_sig, by="Id", all.x=TRUE) 
total$Id <- NULL
write.csv(total,"total_core_sig_with_sse.csv")
save(total, file='total.rdata')


# library(plyr)
# total <- left_join(core_sig, sse_core_sig, by = character(), copy = FALSE)
# total <- join(core_sig, sse_core_sig,)
# total = core_sig(sse_core_sig, ignore_index=True)

#relationship between variables and sses
#all significant, negative for Tmin and alpha, positive for Tmax and gdd
#look more like a log
fit1 <- lm(Tmin ~ sse_Tmin, data = total)
summary(fit1)
plot(Tmin ~ sse_Tmin, data = total)
abline(fit1)

fit1 <- lm(Tmax ~ sse_Tmax, data = total)
summary(fit1)
plot(Tmax ~ sse_Tmax, data = total)
abline(fit1)

fit1 <- lm(gdd ~ sse_gdd, data = total)
summary(fit1)
plot(gdd ~ sse_gdd, data = total)
abline(fit1)

fit1 <- lm(alpha ~ sse_alpha, data = total)
summary(fit1)
plot(alpha ~ sse_alpha, data = total)
abline(fit1)
# ggplotRegression <- function (fit) {
#   
#   require(ggplot2)
#   
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#     geom_point() +
#     stat_smooth(method = "lm", col = "red") +
#     labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#                        "Intercept =",signif(fit$coef[[1]],5 ),
#                        " Slope =",signif(fit$coef[[2]], 5),
#                        " P =",signif(summary(fit)$coef[2,4], 5)))+theme_minimal()
# }
# 
# ggplotRegression(fit1)
# 
# 
# total$errorbar_Tmin  <- abs(total$sse_Tmin/total$Tmin)
# total$errorbar_Tmax  <- abs(total$sse_Tmax/total$Tmax)
# total$errorbar_gdd  <- abs(total$sse_gdd/total$gdd)
# total$errorbar_alpha  <- abs(total$sse_alpha/total$alpha)
# write.csv(total,"total_core_sig_with_sse.csv")
# 
# #Tmin got much larger errors than all others
# 
# summary(total$errorbar_Tmin)
# # Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# #      0.03      0.13      0.29      3.71      0.78 130933.81       284 
# summary(total$errorbar_Tmax)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# # 0.01243 0.02678 0.03453 0.04171 0.04677 0.42560     284 
# summary(total$errorbar_gdd)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# # 0.01081 0.02776 0.03435 0.04495 0.04398 2.58949     284 
# summary(total$errorbar_alpha)
# #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# # 0.01117 0.02576 0.03926 0.04344 0.05485 0.80685     284
# 
# #using RMSEP as the threshold
# 
# 
# total_cleaned<- total  %>%      filter(sse_Tmin>4.7)
# #646 out of 76890, 0.8%
# total_cleaned<- total  %>%      filter(sse_Tmax>3.47)
# #33
# total_cleaned<- total  %>%      filter(sse_alpha>0.158)
# #60
# total_cleaned<- total  %>%      filter(sse_gdd>897.7)
# #14 obs
# 

#95% confidence interval=mean +-1.96×sse
#The sample mean plus or minus 1.96 times its standard error gives the following two figures: 
#This is called the 95% confidence interval , and we can say that there is only a 5% chance that the range a to b excludes the mean of the population.

#margin of error=z-score*standard error=1.96 *sse
#How much margin of error is acceptable?
#between 4% and 8%
#An acceptable margin of error used by most survey researchers typically falls between 4% and 8% at the 95% confidence level. It is affected by sample size, population size, and percentage.

# 
# #delete the extreme values according to RMSEP
# total_cleaned<- total
# total_cleaned$Tmin[total_cleaned$sse_Tmin>4.7] <- NA  
# total_cleaned$Tmin[total_cleaned$sse_Tmax>3.47] <- NA  
# total_cleaned$Tmin[total_cleaned$sse_alpha>0.158] <- NA  
# total_cleaned$Tmin[total_cleaned$sse_gdd>897.7] <- NA  
# 
# #margin of error(MOE)
# total$MOE_Tmin  <- total$sse_Tmin*1.96
# total$MOE_Tmax  <- total$sse_Tmax*1.96
# total$MOE_gdd  <- total$sse_gdd*1.96
# total$MOE_alpha  <- total$sse_alpha*1.96


# total1<-merge(modern_pollen, modern_gradient,by.x="Long",by.y="lon")
# total1<-merge(modern_pollen, modern_gradient,by.x="Entity.name",by.y="site")

########################################################################################################
####################################### Sitelist Table ##############################################
########################################################################################################
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")

#Get information for sites
#sitelist<-raw_paleo[!duplicated(raw_paleo$site_name),]
sitename<-unique(raw_paleo$site_name)
sitelist<-as.data.frame(sitename)
colnames(raw_paleo)[1:18]
#raw_paleo_1=raw_paleo

#changing NA values in publication to "NA"

which(is.na(raw_paleo$publication))
raw_paleo$publication[is.na(raw_paleo$publication)] <- "NA"
which(is.na(raw_paleo$publication))


#which(raw_paleo$site_name%in%sitename)
#which(raw_paleo$site_name==sitelist)
#which(raw_paleo$site_name==sitelist$sitename)
#raw_paleo_1$check <- ifelse(is.na(match(paste0(raw_paleo_1$site_name, raw_paleo_1$longitude), 
#                                paste0(df2$site_name, df2$longitude))),"No", "Yes")
#b=unique(raw_paleo$elevation)
#c=unique(raw_paleo$site_name)
#d=unique(raw_paleo$publication)
#e=unique(raw_paleo$longitude)
#e=unique(raw_paleo$latitude)


#We can use tidyverse. After grouping by 'id', filter the groups of 'id' having only a single unique value for 'plan' and get the distinct rows
library(dplyr)
#b=raw_paleo %>%
#group_by(site_name) %>%
#filter(n_distinct(elevation)==2) %>%
#distinct()
#76628 obs with only one value for elevation

#b=raw_paleo_1 %>%
#  group_by(site_name) %>%
#  filter(n_distinct(elevation)==2) %>%
#  distinct()
#247 obs with two value for elevation: site Majen El Orbi


length(unique(raw_paleo[["site_name"]]))
#[1] 1305
length(unique(raw_paleo[["entity_name"]]))
#[1] 1423

length(unique(merge6[["site_name"]]))
#[1] 1490
length(unique(merge6[["entity_name"]]))
#[1] 1667

b=raw_paleo %>%
  group_by(site_name) %>%
  filter(n_distinct(publication)==2) %>%
  distinct()
#4364 obs with two value for publication

#extracting site list info
sitelist=unique(raw_paleo[c("site_name", "publication","longitude","latitude","elevation")])
c=subset(sitelist,duplicated(site_name))#get the ones with duplicated site names
sitelist=sitelist %>% anti_join(c)#delete the duplicated ones

#which(raw_paleo$site_name==sitelist[,"sitename"])
#siteinfo<-raw_paleo[sitelist["sitename"]]
#sitelist[,"sitename"]
#for(i in 1:nrow(sitelist)){
#  #i=i+1
#  siteinfo<-raw_paleo[which(raw_paleo$site_name==sitelist[i,"sitename"]),]
#  sitelist[i,c("lon","lat","elv")]<-unique(siteinfo[,c("longitude","latitude","elevation")])
#  ref<-unique(siteinfo[,"publication"])
#  if(is.na(raw_paleo$publication[i])<-TRUE){
#    sitelist[i,"ref"]<-"NA"
#  }else{
#    sitelist[i,"ref"]<-siteinfo[i,"publication"]
#  }
#  sitelist[i,"length_yr"]<-max(siteinfo[,"median"])-min(siteinfo[,"median"])
#  sitelist[i,"n_sample"]<-sum(!is.na(siteinfo[,"median"]))

#}
#sitelist<-sitelist[order(sitelist$lon, sitelist$lat), ]


for(i in 1:nrow(sitelist)){
  #  #i=i+1
  siteinfo<-raw_paleo[which(raw_paleo$site_name==sitelist[i,"site_name"]),]
  #  sitelist[i,c("lon","lat","elv")]<-unique(siteinfo[,c("longitude","latitude","elevation")])
  #  sitelist[i,"ref"]<-match(unique(siteinfo[,"publication"]))
  sitelist[i,"length_yr"]<-max(siteinfo[,"median"])-min(siteinfo[,"median"])
  sitelist[i,"n_sample"]<-sum(!is.na(siteinfo[,"median"]))
  
}
sitelist<-sitelist[order(sitelist$longitude, sitelist$latitude), ]



#elv_label
sitelist$elv_label<-NA
for(j in 1:nrow(sitelist)){
  elv<-as.numeric(sitelist[j,"elevation"])
  if(elv<=1000){
    sitelist[j,"elv_label"]<-"low"
  }else if(elv>1000){
    sitelist[j,"elv_label"]<-"high"
  }
}
library(readr)
library(xlsx)
write.csv(sitelist,"sitelist.csv",fileEncoding = 'UTF-8')
write.xlsx(sitelist,"sitelist.xlsx")


########################################################################################################
#######################################    CCA  Table  ###############################################
########################################################################################################
if(!require(vegan)){ install.packages("vegan");library(vegan)}
ord<-cca(formula=taxa~modern_pollen$Tmin+modern_pollen$Tmax+modern_pollen$alpha)
ord
round(vif.cca(ord),digits=2)
#modern_pollen$Tmin  modern_pollen$Tmax modern_pollen$alpha 
#1.31                3.34                3.39 
round(ord[["CCA"]][["biplot"]],digits = 3)
#                      CCA1   CCA2   CCA3
#modern_pollen$Tmin  -0.815  0.579  0.012
#modern_pollen$Tmax  -0.700 -0.203  0.685
#modern_pollen$alpha  0.883  0.430 -0.187
anova(ord)
anova(ord, by="term", permutations=999)
anova(ord, by="axis", permutations=999)
plot(ord,display=c("species","bp"))
text(ord, "species", col="black", cex=0.8)



ord<-cca(formula=core~core_sig$Tmin+core_sig$Tmax+core_sig$alpha,na.action=na.omit)
ord
round(vif.cca(ord),digits=2)
round(ord[["CCA"]][["biplot"]],digits = 3)
anova(ord)
anova(ord, by="term", permutations=999)
anova(ord, by="axis", permutations=999)
plot(ord,display=c("bp"),xlim=c(-3.5,2.5),ylim=c(-3,4.5))
plot(ord,display=c("bp","sp"))
plot(ord,display=c("bp"))
points(ord,display=c("bp"),col="red")
text(ord, "species", col="black", cex=0.7)
#text(ord, "species", col="black", labels=sp_score$taxa_number,cex=0.7)


########################################################################################################
##################################   Find out taxa contribution  #######################################
########################################################################################################

#u and t are both centered in fxTWAPLS
#nsig_Tmin=4
u_t2_Tmin<-as.data.frame(fit_tf_Tmin[["u"]]/fit_tf_Tmin[["t"]]^2)
para_Tmin<-fit_tf_Tmin[["alpha"]]
u_t2_Tmin$taxa<-colnames(taxa)
u_t2_Tmin_common<-u_t2_Tmin[-which(u_t2_Tmin$taxa%in%taxa_to_add),] #get those exist in fossil taxa
u_t2_Tmin_common<-u_t2_Tmin_common[order(u_t2_Tmin_common$V1),]
u_t2_Tmin_common<-u_t2_Tmin_common[rev(order(u_t2_Tmin_common$V1)),]

#nsig_Tmax=4
u_t2_Tmax<-as.data.frame(fit_tf_Tmax[["u"]]/fit_tf_Tmax[["t"]]^2)
para_Tmax<-fit_tf_Tmax[["alpha"]]
u_t2_Tmax$taxa<-colnames(taxa)
u_t2_Tmax_common<-u_t2_Tmax[-which(u_t2_Tmax$taxa%in%taxa_to_add),] #get those exist in fossil taxa
u_t2_Tmax_common<-u_t2_Tmax_common[order(u_t2_Tmax_common$V1),]
u_t2_Tmax_common<-u_t2_Tmax_common[rev(order(u_t2_Tmax_common$V1)),]

#nsig_alpha=3
u_t2_alpha<-as.data.frame(fit_tf_alpha[["u"]]/fit_tf_alpha[["t"]]^2)
para_alpha<-fit_tf_alpha[["alpha"]]
u_t2_alpha$taxa<-colnames(taxa)
u_t2_alpha_common<-u_t2_alpha[-which(u_t2_alpha$taxa%in%taxa_to_add),] #get those exist in fossil taxa
u_t2_alpha_common<-u_t2_alpha_common[order(u_t2_alpha_common$V1),]
u_t2_alpha_common<-u_t2_alpha_common[rev(order(u_t2_alpha_common$V1)),]

#check common important taxa of Tmax and alpha
intersect(u_t2_Tmax_common[1:18,"taxa"],u_t2_alpha_common[1:18,"taxa"])

#check common important taxa of Tmax and alpha
intersect(u_t2_Tmax_common[1:6,"taxa"],u_t2_alpha_common[1:6,"taxa"])

########################################################################################################
###########################    Calculate insolation at each site  #######################################
########################################################################################################
plotdata<-core_sig
# total_cleaned$Id<-NULL
# total_cleaned$errorbar_Tmin<-NULL
# total_cleaned$errorbar_Tmax<-NULL
# total_cleaned$errorbar_gdd<-NULL
# total_cleaned$errorbar_alpha<-NULL
# total_cleaned$MOE_Tmin<-NULL
# total_cleaned$MOE_Tmax<-NULL
# total_cleaned$MOE_gdd<-NULL
# total_cleaned$MOE_alpha<-NULL
# total_cleaned$sse_Tmin<-NULL
# total_cleaned$sse_alpha<-NULL
# total_cleaned$sse_gdd<-NULL
# total_cleaned$sse_Tmax<-NULL
# 
# plotdata<-total
#summary(plotdata$alpha)
#exlcude outliers of the climate varaibles.natural limit of alpha is 0-1.26/theoretical limit due to the relationship of Ep and Eq. 
#Please see Supplementary material 3 - An improved statistical approach for reconstructing past climates from biotic assemblages
#6822 to 6811 obs
#57457 to 57237
plotdata<-plotdata[which(plotdata$alpha>=0&plotdata$alpha<=1.26),]

#alpha is the only variable with theoretical threshold
#76382 left

library(gsl)
for(i in 1:nrow(plotdata)){
  if (i%%1000 ==0){print (i)}
  Lat=plotdata[i,"lat"]
  Age<-plotdata[i,"age"]
  plotdata[i,c("insol_summer","insol_winter",
               "Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")]<-get_insolation(Lat,age_start =Age,age_end=Age, interval=0 )[,c("insol_summer","insol_winter",
                                                                                                                          "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                                                                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")]
}

dd <-plotdata %>%
  na.omit()
#76269 out of 76382
library(dplyr)
dd <- core_sig  %>%      group_by(site) %>%filter(site=="Stuphallet")
#dd <- as.data.frame(dd)
#for(i in 1:nrow(dd)){
#  Lat=dd[i,"lat"]
#  Age<-dd[i,"age"]
#  dd[i,c("insol_summer","insol_winter",
#               "Jan", "Feb", "Mar", "Apr", "May", "Jun",
#               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")]<-get_insolation(Lat,age_start =Age,age_end=Age, interval=0 )[,c("insol_summer","insol_winter",
#                                                                                                                         "Jan", "Feb", "Mar", "Apr", "May", "Jun",
#                                                                                                                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")]
#}


write.csv(plotdata,"plotdata.csv")

########################################################################################################
###########################   Relationship between MTWA and alpha  #####################################
########################################################################################################
#Figure 
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots")
summary(plotdata$Tmax);summary(modern_pollen$Tmax)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.841  15.055  18.607  18.042  21.055  26.479 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.19   15.20   18.46   18.37   22.02   33.54 

p1<-ggplot(modern_pollen,aes(alpha,Tmax))+geom_point(size=0.8)+theme_bw()+
  labs(x= expression(alpha), y = "MTWA (°C)")+ylim(0,35)+
  theme(legend.position = "none",
        axis.text=element_text(size=14),axis.title=element_text(size=14))+
  annotate("text", y= 35, x =0,label="(a)",size=6)
p2<-ggplot(plotdata,aes(alpha,Tmax))+geom_point(size=0.8)+theme_bw()+
  labs(x= expression(alpha), y = "MTWA (°C)")+ylim(0,35)+
  theme(legend.position = "none",axis.title.y = element_blank(),axis.text.y = element_blank(),
        axis.text=element_text(size=14),axis.title=element_text(size=14))+
  annotate("text", y= 35, x =0,label="(b)",size=6)
p<-ggarrange(p1,p2,ncol=2)
ggsave(file="Relationship between MTWA and alpha.jpeg",p,width=12,height=6)

########################################################################################################
########################    Maps to show the training data  (Figure S1)  ################################
########################################################################################################
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots")

#Modern sites
if(!require(ggmap)){ install.packages("ggmap");library(ggmap)}
if(!require(ggsn)){ install.packages("ggsn");library(ggsn)}
if(!require(maps)){ install.packages("maps");library(maps)}
if(!require(mapdata)){ install.packages("mapdata");library(mapdata)}

world <- map_data("world") 
minLong<-min(modern_pollen$Long);maxLong<-max(modern_pollen$Long);
minLat<-min(modern_pollen$Lat);maxLat<-max(modern_pollen$Lat);

region<-world[which(world$long>minLong&world$long<maxLong&world$lat>minLat&world$lat<maxLat),]

xat <- pretty(region$long)
yat <- pretty(region$lat)

if(!require(ggplot2)){ install.packages("ggplot2");library(ggplot2)}
if(!require(egg)){ install.packages("egg");library(egg)}

detach("package:raster", unload = TRUE)

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray60',color='gray30') + theme_bw()+
  geom_point(data = modern_pollen, aes(x = Long, y = Lat, color= alpha), size = 2)+
  scale_colour_gradientn(colours = rev(c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha))

#p=ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray60',color='gray30') + theme_bw()+
#  geom_point(data = modern_pollen, aes(x = Long, y = Lat, color= alpha), size = 1)+
# scale_colour_gradientn(colours = rev(c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey")+
# ggsn::scalebar(region, anchor = c(x=149.5928-8,y=29.4+3),dist = 1000, st.size=3, height=0.01, dist_unit = "km",transform = TRUE, model = 'WGS84')+
# theme(axis.text=element_text(size=12),legend.position = "right")+
# labs(x="Longitude",y="Latitude",colour =expression(alpha)) +geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 

ggsave(file="Map of SMPDS alpha.jpeg",p,width=40,height=20)




########################################################################################################
#####################################   Map of fossil sites   #########################################
########################################################################################################
#Map at 0.5 ka

background1 <- read.csv(paste(wd,"/Master Project/Data/Input data/add on region_1.csv",sep=""), row.names=1)
background2 <- read.csv(paste(wd,"/Master Project/Data/Input data/add on region_2.csv",sep=""))
background1<-background1[,c("lat","lon","elv","mtco","gdd","MI")]
background2<-background2[,c("latitude","longitude","elevation_m","MTCM","GDD0","MI")]
colnames(background2)=colnames(background1)=c("lat","lon","elv","Tmin","gdd","MI")
background<-rbind.data.frame(background1,background2)
if(!require(ggmap)){ install.packages("ggmap");library(ggmap)}
if(!require(ggsn)){ install.packages("ggsn");library(ggsn)}
if(!require(maps)){ install.packages("maps");library(maps)}
if(!require(mapdata)){ install.packages("mapdata");library(mapdata)}
if(!require(sf)){ install.packages("sf");library(sf)}
world <- map_data("world") 
summary(Iberian$longitude);summary(Iberian$latitude)
region<-world[which(world$long>(-10)&world$long<5&world$lat>36&world$lat<44),]
Iberian_background<-background[which(background$lon>(-10)&background$lon<5&background$lat>36&background$lat<44),]

#tranfer MI to alpha
MI<-Iberian_background$MI
w<-1.5
fai<-1/MI
F<-1+fai-(1+fai^w)^(1/w)
alpha<-1.26*MI*F
hist(alpha)
plot(alpha~MI)
Iberian_background$alpha<-alpha

#get MTWA
for(i in 1:nrow(Iberian_background)){
  #i=i+1
  Iberian_background[i,"Tmax"]<-get_MTWA(MTCO=Iberian_background[i,"Tmin"],GDD0=Iberian_background[i,"gdd"])
}

write.csv(Iberian_background,paste(wd,"/Master Project/Data/Input data/Iberian background.csv",sep=""))

Iberian_background<-read.csv(paste(wd,"/Master Project/Data/Input data/Iberian background.csv",sep=""),row.names = 1)


#plot 
p1<-ggplot()+geom_point(data=Iberian_background,aes(lon,lat,color=Tmin),size=3,shape=15)+theme_bw()+
  geom_polygon(data = region,aes(x=long, y = lat, group = group),alpha=0,color='black') +
  geom_point(data=sitelist,aes(lon,lat,shape=elv_label),size=2)+
  scale_colour_gradientn(colours = c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red"),na.value = "grey")+
  labs(x="Longitude",y="Latitude",colour ="MTCO (°C)")+
  scale_x_continuous(breaks = c(-10,-5,0,5),labels=c("10 °W","5 °W","0 °E","5 °E"))+
  scale_y_continuous(breaks = c(36,38,40,42,44),labels=c("36 °N","38 °N","40 °N","42 °N","44 °N"))+ 
  scale_shape_manual(values=c("high"=17,"low"=15),guide = 'none')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  annotate("text",x=-10,y=44,label="(a)")
p2<-ggplot()+geom_point(data=Iberian_background,aes(lon,lat,color=Tmax),size=3,shape=15)+theme_bw()+
  geom_polygon(data = region,aes(x=long, y = lat, group = group),alpha=0,color='black') +
  geom_point(data=sitelist,aes(lon,lat,shape=elv_label),size=2)+
  scale_colour_gradientn(colours = c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red"),na.value = "grey")+
  labs(x="Longitude",y="Latitude",colour ="MTWA (°C)")+
  scale_x_continuous(breaks = c(-10,-5,0,5),labels=c("10 °W","5 °W","0 °E","5 °E"))+
  scale_y_continuous(breaks = c(36,38,40,42,44),labels=c("36 °N","38 °N","40 °N","42 °N","44 °N"))+ 
  scale_shape_manual(values=c("high"=17,"low"=15),guide = 'none')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  annotate("text",x=-10,y=44,label="(b)")
p3<-ggplot()+geom_point(data=Iberian_background,aes(lon,lat,color=alpha),size=3,shape=15)+theme_bw()+
  geom_polygon(data = region,aes(x=long, y = lat, group = group),alpha=0,color='black') +
  geom_point(data=sitelist,aes(lon,lat,shape=elv_label),size=2)+
  scale_colour_gradientn(colours = rev(c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha))+
  scale_x_continuous(breaks = c(-10,-5,0,5),labels=c("10 °W","5 °W","0 °E","5 °E"))+
  scale_y_continuous(breaks = c(36,38,40,42,44),labels=c("36 °N","38 °N","40 °N","42 °N","44 °N"))+ 
  scale_shape_manual(values=c("high"=17,"low"=15),guide = 'none')+
  annotate("text",x=-10,y=44,label="(c)")
p<-ggarrange(p1,p2,p3,ncol=1)
ggsave(file="Map of modern alpha.jpeg",p,width=6,height=10)



########################################################################################################
######################################   Get the gradient with bin 200  #############################################
########################################################################################################
#Site Stuphallet has two sets of coordinates which create problems in looping,
#select one combination and replace the others to stop the gdd replacing Tmax issue, leading to 1000 degrees in Tmax on this site
#from Colin:I don't understand how that could happen! But in any case you don't want to have two sets of corodinates 
#for the same site. It seems unlikely that the location of the site was recorded to this level of precision (before GPS was invented),
#so you could just pick one of them.

#same issue for site Majen El Orbi that lead to extreme alpha values
#dd <- plotdata  %>%      group_by(site) %>%filter(site=="Majen El Orbi")
#dd <- plotdata  %>%      group_by(site) %>%filter(site=="Rosenbergdalen")
#dd<- plotdata  %>%      group_by(site) %>%filter(site=="Stuphallet")
#dt_env<-rbind.data.frame(
#  get_gradient(dt,100,100),get_gradient(dt,300,100))
#dd$lat=78.96083
#dd$lon=11.59694
#checking if my age models are in the metadata
#they are
dd=raw_paleo%>%filter(entity_name=="HENSP")
#dd=CRU %>%
 # filter(31<lat&lat<32&-10<lon) 

dd=plotdata %>%
  group_by(site) %>%
  filter(n_distinct(lon)>1) %>%
  distinct()
#404
#2 sites with two values for lat
#3 sites with two values for lon
#2 sites with two values for elv
#1 site with three values for lon,lat and two values for elv


saveRDS(plotdata, file = "plotdata new site_1.RDS") 
plotdata<-readRDS('plotdata new site_1.RDS')

plotdata["lat"][plotdata["site"] == "Stuphallet"] <- "78.96083"
plotdata["lon"][plotdata["site"] == "Stuphallet"] <- "11.59694"

plotdata["lat"][plotdata["site"] == "Majen El Orbi"] <- "37.15294"
plotdata["lon"][plotdata["site"] == "Majen El Orbi"] <- "9.0984"
plotdata["elv"][plotdata["site"] == "Majen El Orbi"] <- "200"


plotdata["lon"][plotdata["site"] == "Rosenbergdalen"] <- "20.9258"

plotdata["lat"][plotdata["site"] == "Nile Delta"] <- "31.3"
plotdata["lon"][plotdata["site"] == "Nile Delta"] <- "31.6"
plotdata["elv"][plotdata["site"] == "Nile Delta"] <- "1"


write.csv(plotdata,"plotdata with sites issue fixed.csv")

saveRDS(plotdata, file = "plotdata with sites issue fixed new site_1.RDS") 


#plotdata<-plotdata_2
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

saveRDS(gradient_env, file = "gradient_env_1.RDS") 

# gradient_env_2<-rbind.data.frame(
#   get_gradient(plotdata,100,100),get_gradient(plotdata,300,100),
#   get_gradient(plotdata,500,100),get_gradient(plotdata,700,100),
#   get_gradient(plotdata,900,100),get_gradient(plotdata,1100,100),
#   get_gradient(plotdata,1300,100),get_gradient(plotdata,1500,100),
#   get_gradient(plotdata,1700,100),get_gradient(plotdata,1900,100),
#   get_gradient(plotdata,2100,100),get_gradient(plotdata,2300,100),
#   get_gradient(plotdata,2500,100),get_gradient(plotdata,2700,100),
#   get_gradient(plotdata,2900,100),get_gradient(plotdata,3100,100),
#   get_gradient(plotdata,3300,100),get_gradient(plotdata,3500,100),
#   get_gradient(plotdata,3700,100),get_gradient(plotdata,3900,100),
#   get_gradient(plotdata,4100,100),get_gradient(plotdata,4300,100),
#   get_gradient(plotdata,4500,100),get_gradient(plotdata,4700,100),
#   get_gradient(plotdata,4900,100),get_gradient(plotdata,5100,100),
#   get_gradient(plotdata,5300,100),get_gradient(plotdata,5500,100),
#   get_gradient(plotdata,5700,100),get_gradient(plotdata,5900,100),
#   get_gradient(plotdata,6100,100),get_gradient(plotdata,6300,100),
#   get_gradient(plotdata,6500,100),get_gradient(plotdata,6700,100),
#   get_gradient(plotdata,6900,100),get_gradient(plotdata,7100,100),
#   get_gradient(plotdata,7300,100),get_gradient(plotdata,7500,100),
#   get_gradient(plotdata,7700,100),get_gradient(plotdata,7900,100),
#   get_gradient(plotdata,8100,100),get_gradient(plotdata,8300,100),
#   get_gradient(plotdata,8500,100),get_gradient(plotdata,8700,100),
#   get_gradient(plotdata,8900,100),get_gradient(plotdata,9100,100),
#   get_gradient(plotdata,9300,100),get_gradient(plotdata,9500,100),
#   get_gradient(plotdata,9700,100),get_gradient(plotdata,9900,100),
#   get_gradient(plotdata,10100,100),get_gradient(plotdata,10300,100),
#   get_gradient(plotdata,10500,100),get_gradient(plotdata,10700,100),
#   get_gradient(plotdata,10900,100),get_gradient(plotdata,11100,100),
#   get_gradient(plotdata,11300,100),get_gradient(plotdata,11500,100),
#   get_gradient(plotdata,11700,100),get_gradient(plotdata,11900,100),get_gradient(plotdata,12100,100))

#78120 obs
#79422 obs when 12100, but looks problematic with max age being 9900

#get modern gradient
#this is 1750-1950
modern_gradient<-get_gradient(plotdata,100,100)
#combine
str(modern_gradient)
colnames(modern_gradient)<-colnames(gradient_env)
for(j in c(1,3:ncol(modern_gradient))){
  modern_gradient[,j]<-as.numeric(modern_gradient[,j])
}

str(gradient_env)
for(j in c(1,3:ncol(gradient_env))){
  gradient_env[,j]<-as.numeric(gradient_env[,j])
}
#gradient_env$agef<-factor(gradient_env$age,labels=c("0.5 ka","1.5 ka","2.5 ka","3.5 ka","4.5 ka","5.5 ka",
#                                                    "6.5 ka","7.5 ka","8.5 ka","9.5 ka","10.5 ka","11.5 ka"))

paste(seq(from = 0.1, to = 11.9, by = 0.2)," ka",sep="")

gradient_env$agef<-factor(gradient_env$age,labels=c("0.1 ka", "0.3 ka",  "0.5 ka",  "0.7 ka",  "0.9 ka",  "1.1 ka",  "1.3 ka",  "1.5 ka",  "1.7 ka",  "1.9 ka",  "2.1 ka", 
                                                    "2.3 ka", "2.5 ka",  "2.7 ka",  "2.9 ka",  "3.1 ka",  "3.3 ka",  "3.5 ka",  "3.7 ka",  "3.9 ka",  "4.1 ka",  "4.3 ka", 
                                                    "4.5 ka", "4.7 ka",  "4.9 ka",  "5.1 ka",  "5.3 ka",  "5.5 ka",  "5.7 ka",  "5.9 ka",  "6.1 ka",  "6.3 ka",  "6.5 ka", 
                                                    "6.7 ka", "6.9 ka",  "7.1 ka",  "7.3 ka",  "7.5 ka",  "7.7 ka",  "7.9 ka",  "8.1 ka",  "8.3 ka",  "8.5 ka",  "8.7 ka", 
                                                    "8.9 ka", "9.1 ka",  "9.3 ka",  "9.5 ka",  "9.7 ka",  "9.9 ka",  "10.1 ka", "10.3 ka", "10.5 ka", "10.7 ka", "10.9 ka",
                                                    "11.1 ka", "11.3 ka", "11.5 ka", "11.7 ka", "11.9 ka"))

#get anomaly to 0.5 ka
#paleo-modern(0.5 ka)
#anomaly in this realm stands for difference. Here it is the difference between paleo and modern (here it is 0.5ka)
gradient_env$Tmin_anomaly<-gradient_env$mean_Tmin-rep(modern_gradient[,"mean_Tmin"],nlevels(gradient_env$agef))
gradient_env$gdd_anomaly<-gradient_env$mean_gdd-rep(modern_gradient[,"mean_gdd"],nlevels(gradient_env$agef))
gradient_env$alpha_anomaly<-gradient_env$mean_alpha-rep(modern_gradient[,"mean_alpha"],nlevels(gradient_env$agef))
gradient_env$Tmax_anomaly<-gradient_env$mean_Tmax-rep(modern_gradient[,"mean_Tmax"],nlevels(gradient_env$agef))
gradient_env$se_Tmin_anomaly<-sqrt(gradient_env$se_mean_Tmin^2+rep(modern_gradient[,"se_mean_Tmin"],nlevels(gradient_env$agef))^2)
gradient_env$se_Tmax_anomaly<-sqrt(gradient_env$se_mean_Tmax^2+rep(modern_gradient[,"se_mean_Tmax"],nlevels(gradient_env$agef))^2)
gradient_env$se_gdd_anomaly<-sqrt(gradient_env$se_mean_gdd^2+rep(modern_gradient[,"se_mean_gdd"],nlevels(gradient_env$agef))^2)
gradient_env$se_alpha_anomaly<-sqrt(gradient_env$se_mean_alpha^2+rep(modern_gradient[,"se_mean_alpha"],nlevels(gradient_env$agef))^2)

# gradient_env$insol_summer_anomaly<-gradient_env$mean_insol_summer-rep(modern_gradient[,"mean_insol_summer"],nlevels(gradient_env$agef))
# gradient_env$insol_winter_anomaly<-gradient_env$mean_insol_winter-rep(modern_gradient[,"mean_insol_winter"],nlevels(gradient_env$agef))

gradient_env$elv_label<-NA
for(j in 1:nrow(gradient_env)){
  elv<-as.numeric(gradient_env[j,"elv"])
  if(elv<=1000){
    gradient_env[j,"elv_label"]<-"low"
  }else if(elv>1000){
    gradient_env[j,"elv_label"]<-"high"
  }
}
gradient_env$elv_label<-factor(gradient_env$elv_label,levels = c("low","high"))
write.csv(gradient_env,"gradient_env with 200 year bin.csv")


#comparison plots
#Tmin anomaly conmpared after binned se of Tmin (mean Tmin'se)
ggplot(data=gradient_env,aes(Tmin_anomaly,se_mean_Tmin))+theme_bw()+geom_point()
#Tmin anomaly conmpared after binned se of Tmin_anomaly
ggplot(data=gradient_env,aes(Tmin_anomaly,se_Tmin_anomaly))+theme_bw()+geom_point()

#Tmax anomaly conmpared after binned se of Tmax (mean Tmax'se)
ggplot(data=gradient_env,aes(Tmax_anomaly,se_mean_Tmax))+theme_bw()+geom_point()
#Tmax anomaly conmpared after binned se of Tmax_anomaly
ggplot(data=gradient_env,aes(Tmax_anomaly,se_Tmax_anomaly))+theme_bw()+geom_point()

#gdd anomaly conmpared after binned se of gdd (mean gdd'se)
ggplot(data=gradient_env,aes(gdd_anomaly,se_mean_gdd))+theme_bw()+geom_point()
#gdd anomaly conmpared after binned se of gdd_anomaly
ggplot(data=gradient_env,aes(gdd_anomaly,se_gdd_anomaly))+theme_bw()+geom_point()

#alpha anomaly conmpared after binned se of alpha (mean alpha'se)
ggplot(data=gradient_env,aes(alpha_anomaly,se_mean_alpha))+theme_bw()+geom_point()
#alpha anomaly conmpared after binned se of alpha_anomaly
ggplot(data=gradient_env,aes(alpha_anomaly,se_alpha_anomaly))+theme_bw()+geom_point()

#check how many sites are lost(all NA)
sum(is.na(gradient_env$mean_Tmin))
# 45439
45439/78120
# 0.5816564
sum(is.na(gradient_env$mean_insol_summer))
# 45439
sum(is.na(gradient_env$mean_Tmax))
# 45439
sum(is.na(gradient_env$mean_gdd))
# 45439
sum(is.na(gradient_env$mean_alpha))
# 45439
sum(is.na(gradient_env$Tmax_anomaly))
#60372


which(is.na(gradient_env$mean_Tmin)&!is.na(gradient_env$mean_Tmax))
#integer(0)
sum(!is.na(gradient_env$mean_Tmin)&is.na(gradient_env$mean_Tmax))
#0

#60% of obs will be throw out if we choose 200 year bin
#gradient_env_NA_removed <- na.omit(gradient_env) does not work coz it contains anomaly, which lead to only 41 obs
gradient_env_NA_removed <-gradient_env[!is.na(gradient_env$mean_Tmax), ]#choosing Tmax as it contains max no of NAs,32681
gradient_env_NAs_removed <-gradient_env[!is.na(gradient_env$Tmax_anomaly), ]#17748 out of 78120
sitename_NA_removed<-unique(gradient_env_NA_removed$site)
#1301 sites left out of 1305 sites
1301/1305
#0.9969349
#sitename_NA_removed<-as.data.frame(sitename_NA_removed)
sitelist_NA_removed <-sitelist[sitelist$site_name %in% sitename_NA_removed,]



write.csv(gradient_env_NA_removed,"gradient_env_NA_removed with 200 year bin.csv",row.names=FALSE)
write.csv(gradient_env_NAs_removed,"gradient_env_NAs_removed with 200 year bin.csv",row.names=FALSE)

#gradient_env_NA_removed <- readr::read_csv("gradient_env_NA_removed with 200 year bin.csv")

write.csv(sitelist_NA_removed,"sitelist_NA_removed with 200 year bin.csv",fileEncoding = 'UTF-8')
library(xlsx)
write.xlsx(sitelist_NA_removed,"sitelist_NA_removed with 200 year bin.xlsx")
########################################################################################################
#####################################   bin with bin 200  ##########################################
########################################################################################################

library("dplyr") 
bins_NA_removed <- gradient_env_NA_removed  %>%                              # Applying group_by & summarise
  group_by(site) %>%
  summarise(number_of_bins_left = n_distinct(age))


summary(bins_NA_removed$number_of_bins_left)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   14.00   23.00   25.12   36.00   60.00 


sum(bins_NA_removed$number_of_bins_left>30)#number of bins over 30
#442
442/1031
# 0.42871
#43% sites have more than half of the bins left
#57% sites have more than 25 bins left

sum(bins_NA_removed$number_of_bins_left)
# 32681

write.csv(bins_NA_removed,"bins_NA_removed per site with 200 year bin.csv")



########################################################################################################
#####################################   anomaly map with bin 200  ##########################################
########################################################################################################
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots")

#modern
modern_gradient_NA_removed <-modern_gradient[!is.na(modern_gradient$mean_Tmax), ]#633 out of 1302 obs left
633/1302
#0.4861751
#49% of obs left


d1=data.frame(unclass(summary(gradient_env)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(d1,"summary of gradient env.csv")




#1-1.2ka
subset1<-gradient_env_NA_removed %>% filter(age ==1100)

#3-3.2ka
subset2<-gradient_env_NA_removed %>% filter(age ==3100)

#5-5.2ka
subset3<-gradient_env_NA_removed %>% filter(age ==5100)

#7-7.2ka
subset4<-gradient_env_NA_removed %>% filter(age ==7100)

#9-9.2ka
subset5<-gradient_env_NA_removed %>% filter(age ==9100)

#11-11.2ka
subset6<-gradient_env_NA_removed %>% filter(age ==11100)



# get quantile breaks. Add .00001 offset to catch the lowest value
#breaks_qt <- classIntervals(c(min(subset5$Tmin_anomaly) - .00001, subset5$Tmin_anomaly), n = 5, style = "equal")
#breaks_qt
#subset5 <- mutate(subset5, Tmin_anomaly_cat = cut(Tmin_anomaly, breaks_qt$brks)) 
#Breaks=c(1,5,10,20,50,100)
#subset5 <- mutate(subset5, Tmin_anomaly_cat = cut(Tmin_anomaly, Breaks)) 
library(classInt)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(sf)
#subset5 <- st_as_sf(subset5, coords = c('lon', 'lat'),crs = 4326)
#ggplot1=ggplot() + 
#  geom_sf(data = subset(subset5, !is.na(Tmin_anomaly)),  color=Tmin_anomaly_cat, show.legend = TRUE) 
#  scale_color_viridis(option="viridis") +
#  geom_sf(data = world, fill = NA)
#print(ggplot1)
#ggsave(ggplot1,filename=paste("Distribution of Carpinus.betulus_cat.png",sep=""),width = 20, height = 20, units = "cm",path = "C:/Users/X1 Carbon/Desktop/Tree Migration- Colin")


#Modern sites
if(!require(ggmap)){ install.packages("ggmap");library(ggmap)}
if(!require(ggsn)){ install.packages("ggsn");library(ggsn)}
if(!require(maps)){ install.packages("maps");library(maps)}
if(!require(mapdata)){ install.packages("mapdata");library(mapdata)}

world <- map_data("world") 
minLong<-min(modern_gradient$lon);maxLong<-max(modern_gradient$lon);
minLat<-min(modern_gradient$lat);maxLat<-max(modern_gradient$lat);


region<-world[which(world$long>minLong&world$long<maxLong&world$lat>minLat&world$lat<maxLat),]

xat <- pretty(region$long)
yat <- pretty(region$lat)

if(!require(ggplot2)){ install.packages("ggplot2");library(ggplot2)}
if(!require(egg)){ install.packages("egg");library(egg)}

detach("package:raster", unload = TRUE)

#modern map
#Tmin
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = modern_gradient, aes(x = lon, y = lat, color= mean_Tmin), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(mean_Tmin))

p
ggsave(file="Modern Map of T min.jpeg",p,width=22,height=11)


#Tmax
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = modern_gradient, aes(x = lon, y = lat, color= mean_Tmax), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(mean_Tmax))

p
ggsave(file="Modern Map of T max.jpeg",p,width=22,height=11)

modern_gradient$mean_alpha
#gdd
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = modern_gradient, aes(x = lon, y = lat, color= mean_gdd), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(mean_gdd))

p
ggsave(file="Modern Map of mean GDD.jpeg",p,width=22,height=11)


#alpha
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = modern_gradient, aes(x = lon, y = lat, color= mean_alpha), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(mean_alpha))

p
ggsave(file="Modern Map of mean alpha.jpeg",p,width=22,height=11)





#general map of all the bins for each anomaly
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = gradient_env_NAs_removed, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p
ggsave(file="general Tmin anomaly map of all the bins.jpeg",p,width=22,height=11)

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = gradient_env_NAs_removed, aes(x = lon, y = lat, color= Tmax_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmax_anomaly))

p
ggsave(file="general Tmax anomaly map of all the bins.jpeg",p,width=22,height=11)

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = gradient_env_NAs_removed, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p
ggsave(file="general GDD anomaly map of all the bins.jpeg",p,width=22,height=11)

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = gradient_env_NAs_removed, aes(x = lon, y = lat, color= alpha_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p
ggsave(file="general alpha anomaly map of all the bins.jpeg",p,width=22,height=11)



#1-1.2ka

#p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
#  geom_point(data = subset1, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
#  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
#  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
#  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
#  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
#  theme(axis.text=element_text(size=12),legend.position = "right")+
#  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

#p
#ggsave(file="1-1.2ka Map of Tmin_anomaly colour 1.jpeg",p,width=22,height=11)



p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset1, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c( 0, 5 , 10 , 11, 12 ,17, 22 )/22,na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

#ggsave(file="1-1.2ka Map of Tmin_anomaly colour 2.jpeg",p,width=22,height=11)
ggsave(file="1-1.2ka Map of Tmin_anomaly with key colour rescaled.jpeg",p,width=22,height=11)


summary(subset1$Tmin_anomaly)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-21.1903  -1.1868   0.2339   0.3587   1.8840  21.6964      227 
summary(subset1$Tmax_anomaly)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-8.9970 -0.4077  0.3066  0.6596  1.8817  9.0340     227 

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset1, aes(x = lon, y = lat, color= Tmax_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c( 0, 2 , 4 , 5, 6 ,8, 10 )/10,na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmax_anomaly))

p


v=c( 0, 2 , 4 , 5, 6 ,8, 10 )/10
v*2998
#[1]    0.0  599.6 1199.2 1499.0 1798.8 2398.4 2998.0

ggsave(file="1-1.2ka Map of Tmax_anomaly with key colour rescaled.jpeg",p,width=22,height=11)

summary(subset1$gdd_anomaly)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-2873.57  -142.47    44.42   103.83   364.87  2997.74      227 


p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset1, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c( 0, 699 , 1398 , 1499, 1600 ,2299, 2998 )/2998,na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p

ggsave(file="1-1.2ka Map of GDD_anomaly with key colour rescaled.jpeg",p,width=22,height=11)



summary(subset1$alpha_anomaly)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-0.33679 -0.06062 -0.00685 -0.01271  0.01834  0.44330      227 
v*0.5#does not work, centre is blue instead of white

#values=c(0, (midpoint-min(subset1$alpha_anomaly,na.rm=T))/(max(subset1$alpha_anomaly,na.rm=T)-min(subset1$alpha_anomaly,na.rm=T)),1)


p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset1, aes(x = lon, y = lat, color= alpha_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p



ggsave(file="1-1.2ka Map of alpha_anomaly with original colour.jpeg",p,width=22,height=11)


midpoint=0
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset1, aes(x = lon, y = lat, colour= alpha_anomaly), size = 2)+
  scale_color_gradientn(colours = c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red"),values=c(0, (midpoint-min(subset1$alpha_anomaly,na.rm=T))/(max(subset1$alpha_anomaly,na.rm=T)-min(subset1$alpha_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p


ggsave(file="1-1.2ka Map of alpha_anomaly with mid point reset.jpeg",p,width=22,height=11)


#3-3.2ka


p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset2, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="3-3.2ka Map of Tmin_anomaly with original colour.jpeg",p,width=22,height=11)


(midpoint-min(subset2$Tmin_anomaly,na.rm=T))/(max(subset2$Tmin_anomaly,na.rm=T)-min(subset2$Tmin_anomaly,na.rm=T))
#0.6095435

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset2, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0, (midpoint-min(subset2$Tmin_anomaly,na.rm=T))/(max(subset2$Tmin_anomaly,na.rm=T)-min(subset2$Tmin_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="3-3.2ka Map of Tmin_anomaly with mid point reset.jpeg",p,width=22,height=11)

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset2, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0, 0.45,(midpoint-min(subset2$Tmin_anomaly,na.rm=T))/(max(subset2$Tmin_anomaly,na.rm=T)-min(subset2$Tmin_anomaly,na.rm=T)),0.75,1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="3-3.2ka Map of Tmin_anomaly with mid point reset and key colour rescaled.jpeg",p,width=22,height=11)


summary(subset2$Tmin_anomaly)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#-26.42377  -1.49435   0.09548   0.11906   2.04533  16.92632       301 
summary(subset2$Tmax_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-8.88838 -0.07514  0.70992  1.07773  2.56382 10.12386      301


(midpoint-min(subset2$Tmax_anomaly,na.rm=T))/(max(subset2$Tmax_anomaly,na.rm=T)-min(subset2$Tmax_anomaly,na.rm=T))
#0.4675083

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset2, aes(x = lon, y = lat, color= Tmax_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c( 0, 0.4675083,1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmax_anomaly))

p


v=c( 0, 2 , 4 , 5, 6 ,8, 10 )/10
v*2998
#[1]    0.0  599.6 1199.2 1499.0 1798.8 2398.4 2998.0

ggsave(file="3-3.2ka Map of Tmax_anomaly with mid point reset.jpeg",p,width=22,height=11)

summary(subset2$gdd_anomaly)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-2448.96   -80.02   131.06   153.42   435.08  2348.13      301 


#0.0  ,489.6,  979.2, 1224.0 ,1468.8, 1958.4, 2448.0)/2448,
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset2, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p

ggsave(file="3-3.2ka Map of GDD_anomaly with original colour.jpeg",p,width=22,height=11)



summary(subset2$alpha_anomaly)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-0.34863 -0.07300 -0.01424 -0.02005  0.01635  0.46395      301 
v*0.5#does not work, centre is blue instead of white

#values=c(0, (midpoint-min(subset2$alpha_anomaly,na.rm=T))/(max(subset2$alpha_anomaly,na.rm=T)-min(subset2$alpha_anomaly,na.rm=T)),1)


p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset2, aes(x = lon, y = lat, color= alpha_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p



ggsave(file="3-3.2ka Map of alpha_anomaly with original colour.jpeg",p,width=22,height=11)


midpoint=0
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset2, aes(x = lon, y = lat, colour= alpha_anomaly), size = 2)+
  scale_color_gradientn(colours = c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red"),values=c(0, (midpoint-min(subset2$alpha_anomaly,na.rm=T))/(max(subset2$alpha_anomaly,na.rm=T)-min(subset2$alpha_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p


ggsave(file="3-3.2ka Map of alpha_anomaly with mid point reset.jpeg",p,width=22,height=11)

#5-5.2ka

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset3, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="5-5.2ka  Map of Tmin_anomaly with original colour.jpeg",p,width=22,height=11)


(midpoint-min(subset3$Tmin_anomaly,na.rm=T))/(max(subset3$Tmin_anomaly,na.rm=T)-min(subset3$Tmin_anomaly,na.rm=T))
#0.598767

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset3, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0, (midpoint-min(subset3$Tmin_anomaly,na.rm=T))/(max(subset3$Tmin_anomaly,na.rm=T)-min(subset3$Tmin_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="5-5.2ka  Map of Tmin_anomaly with mid point reset.jpeg",p,width=22,height=11)

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset3, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0, 0.45,(midpoint-min(subset3$Tmin_anomaly,na.rm=T))/(max(subset3$Tmin_anomaly,na.rm=T)-min(subset3$Tmin_anomaly,na.rm=T)),0.75,1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="5-5.2ka  Map of Tmin_anomaly with mid point reset and key colour rescaled.jpeg",p,width=22,height=11)


summary(subset3$Tmin_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-26.7572  -1.7053   0.3716   0.3114   3.0251  17.9299      327 
summary(subset3$Tmax_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-11.4686  -0.1557   0.9504   1.2461   3.0461  11.3540      327 


(midpoint-min(subset3$Tmax_anomaly,na.rm=T))/(max(subset3$Tmax_anomaly,na.rm=T)-min(subset3$Tmax_anomaly,na.rm=T))
#0.5025105

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset3, aes(x = lon, y = lat, color= Tmax_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmax_anomaly))

p


v=c( 0, 2 , 4 , 5, 6 ,8, 10 )/10
v*2998
#[1]    0.0  599.6 1199.2 1499.0 1798.8 2398.4 2998.0

ggsave(file="5-5.2ka  Map of Tmax_anomaly with original colour.jpeg",p,width=22,height=11)

summary(subset3$gdd_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-3075.6  -125.3   162.1   190.5   583.9  2276.0     327 


#0.0  ,489.6,  979.2, 1224.0 ,1468.8, 1958.4, 2448.0)/2448,
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset3, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p

ggsave(file="5-5.2ka  Map of GDD_anomaly with original colour.jpeg",p,width=22,height=11)



p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset3, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0,(midpoint-min(subset3$gdd_anomaly,na.rm=T))/(max(subset3$gdd_anomaly,na.rm=T)-min(subset3$gdd_anomaly,na.rm=T)) ,1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p

ggsave(file="5-5.2ka  Map of GDD_anomaly with mid point reset.jpeg",p,width=22,height=11)


summary(subset3$alpha_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.3143 -0.0821 -0.0273 -0.0250  0.0214  0.4692     327 
v*0.5#does not work, centre is blue instead of white

#values=c(0, (midpoint-min(subset3$alpha_anomaly,na.rm=T))/(max(subset3$alpha_anomaly,na.rm=T)-min(subset3$alpha_anomaly,na.rm=T)),1)


p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset3, aes(x = lon, y = lat, color= alpha_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p



ggsave(file="5-5.2ka  Map of alpha_anomaly with original colour.jpeg",p,width=22,height=11)


midpoint=0
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset3, aes(x = lon, y = lat, colour= alpha_anomaly), size = 2)+
  scale_color_gradientn(colours = c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red"),values=c(0, (midpoint-min(subset3$alpha_anomaly,na.rm=T))/(max(subset3$alpha_anomaly,na.rm=T)-min(subset3$alpha_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p


ggsave(file="5-5.2ka  Map of alpha_anomaly with mid point reset.jpeg",p,width=22,height=11)

#7-7.2ka
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset4, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="7-7.2ka  Map of Tmin_anomaly with original colour.jpeg",p,width=22,height=11)


(midpoint-min(subset4$Tmin_anomaly,na.rm=T))/(max(subset4$Tmin_anomaly,na.rm=T)-min(subset4$Tmin_anomaly,na.rm=T))
#0.5507488

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset4, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0, (midpoint-min(subset4$Tmin_anomaly,na.rm=T))/(max(subset4$Tmin_anomaly,na.rm=T)-min(subset4$Tmin_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="7-7.2ka  Map of Tmin_anomaly with mid point reset.jpeg",p,width=22,height=11)

#p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
#  geom_point(data = subset4, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
#  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0, 0.3,(midpoint-min(subset4$Tmin_anomaly,na.rm=T))/(max(subset4$Tmin_anomaly,na.rm=T)-min(subset4$Tmin_anomaly,na.rm=T)),0.8,1),na.value = "grey60")+
#  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
#  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
# scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
#  theme(axis.text=element_text(size=12),legend.position = "right")+
#  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

#p

#ggsave(file="7-7.2ka  Map of Tmin_anomaly with mid point reset and key colour rescaled.jpeg",p,width=22,height=11)


summary(subset4$Tmin_anomaly)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#-22.48614  -2.29022   0.19869   0.09119   3.13044  18.34216       302 
summary(subset4$Tmax_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-9.30793 -0.08222  1.13200  1.38112  3.16540  9.59727      302 


(midpoint-min(subset4$Tmax_anomaly,na.rm=T))/(max(subset4$Tmax_anomaly,na.rm=T)-min(subset4$Tmax_anomaly,na.rm=T))
#0.4923476

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset4, aes(x = lon, y = lat, color= Tmax_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmax_anomaly))

p


v=c( 0, 2 , 4 , 5, 6 ,8, 10 )/10
v*2998
#[1]    0.0  599.6 1199.2 1499.0 1798.8 2398.4 2998.0

ggsave(file="7-7.2ka  Map of Tmax_anomaly with original colour.jpeg",p,width=22,height=11)

summary(subset4$gdd_anomaly)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-3330.7  -105.3   207.5   227.1   684.3  2298.8     302 
(midpoint-min(subset4$gdd_anomaly,na.rm=T))/(max(subset4$gdd_anomaly,na.rm=T)-min(subset4$gdd_anomaly,na.rm=T))
#0.5916485

#0.0  ,489.6,  979.2, 1224.0 ,1468.8, 1958.4, 2448.0)/2448,
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset4, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p

ggsave(file="7-7.2ka  Map of GDD_anomaly with original colour.jpeg",p,width=22,height=11)



p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset4, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0,(midpoint-min(subset4$gdd_anomaly,na.rm=T))/(max(subset4$gdd_anomaly,na.rm=T)-min(subset4$gdd_anomaly,na.rm=T)) ,1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p

ggsave(file="7-7.2ka  Map of GDD_anomaly with mid point reset.jpeg",p,width=22,height=11)


summary(subset4$alpha_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-0.29079 -0.11144 -0.03709 -0.03860  0.01626  0.45947      302
v*0.5#does not work, centre is blue instead of white

#values=c(0, (midpoint-min(subset4$alpha_anomaly,na.rm=T))/(max(subset4$alpha_anomaly,na.rm=T)-min(subset4$alpha_anomaly,na.rm=T)),1)


p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset4, aes(x = lon, y = lat, color= alpha_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p



ggsave(file="7-7.2ka  Map of alpha_anomaly with original colour.jpeg",p,width=22,height=11)


midpoint=0
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset4, aes(x = lon, y = lat, colour= alpha_anomaly), size = 2)+
  scale_color_gradientn(colours = c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red"),values=c(0, (midpoint-min(subset4$alpha_anomaly,na.rm=T))/(max(subset4$alpha_anomaly,na.rm=T)-min(subset4$alpha_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p


ggsave(file="7-7.2ka  Map of alpha_anomaly with mid point reset.jpeg",p,width=22,height=11)

#9-9.2ka
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset5, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="9-9.2ka  Map of Tmin_anomaly with original colour.jpeg",p,width=22,height=11)


(midpoint-min(subset5$Tmin_anomaly,na.rm=T))/(max(subset5$Tmin_anomaly,na.rm=T)-min(subset5$Tmin_anomaly,na.rm=T))
# 0.5521807

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset5, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0, (midpoint-min(subset5$Tmin_anomaly,na.rm=T))/(max(subset5$Tmin_anomaly,na.rm=T)-min(subset5$Tmin_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="9-9.2ka  Map of Tmin_anomaly with mid point reset.jpeg",p,width=22,height=11)

#p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
#  geom_point(data = subset5, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
#  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0, 0.3,(midpoint-min(subset5$Tmin_anomaly,na.rm=T))/(max(subset5$Tmin_anomaly,na.rm=T)-min(subset5$Tmin_anomaly,na.rm=T)),0.8,1),na.value = "grey60")+
#  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
#  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
# scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
#  theme(axis.text=element_text(size=12),legend.position = "right")+
#  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

#p

#ggsave(file="9-9.2ka  Map of Tmin_anomaly with mid point reset and key colour rescaled.jpeg",p,width=22,height=11)


summary(subset5$Tmin_anomaly)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-20.2111  -3.0347  -0.3218   0.1471   3.7740  16.3912      215 
summary(subset5$Tmax_anomaly)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-7.1568 -0.9606  0.2943  0.5491  1.9690 10.4631     215


(midpoint-min(subset5$Tmax_anomaly,na.rm=T))/(max(subset5$Tmax_anomaly,na.rm=T)-min(subset5$Tmax_anomaly,na.rm=T))
#0.4061759

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset5, aes(x = lon, y = lat, color= Tmax_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmax_anomaly))

p


v=c( 0, 2 , 4 , 5, 6 ,8, 10 )/10
v*2998
#[1]    0.0  599.6 1199.2 1499.0 1798.8 2398.4 2998.0

ggsave(file="9-9.2ka  Map of Tmax_anomaly with original colour.jpeg",p,width=22,height=11)

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset5, aes(x = lon, y = lat, color= Tmax_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0,(midpoint-min(subset5$Tmax_anomaly,na.rm=T))/(max(subset5$Tmax_anomaly,na.rm=T)-min(subset5$Tmax_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmax_anomaly))

p

ggsave(file="9-9.2ka  Map of Tmax_anomaly with mid point reset.jpeg",p,width=22,height=11)



summary(subset5$gdd_anomaly)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-2319.16  -266.84    59.09    86.89   427.05  2566.18      215 
(midpoint-min(subset5$gdd_anomaly,na.rm=T))/(max(subset5$gdd_anomaly,na.rm=T)-min(subset5$gdd_anomaly,na.rm=T))
#0.474718

#0.0  ,489.6,  979.2, 1224.0 ,1468.8, 1958.4, 2448.0)/2448,
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset5, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p

ggsave(file="9-9.2ka  Map of GDD_anomaly with original colour.jpeg",p,width=22,height=11)



p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset5, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0,(midpoint-min(subset5$gdd_anomaly,na.rm=T))/(max(subset5$gdd_anomaly,na.rm=T)-min(subset5$gdd_anomaly,na.rm=T)) ,1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p

ggsave(file="9-9.2ka  Map of GDD_anomaly with mid point reset.jpeg",p,width=22,height=11)


summary(subset5$alpha_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-0.56381 -0.08307 -0.02200 -0.02306  0.04314  0.32334      215 
v*0.5#does not work, centre is blue instead of white

#values=c(0, (midpoint-min(subset5$alpha_anomaly,na.rm=T))/(max(subset5$alpha_anomaly,na.rm=T)-min(subset5$alpha_anomaly,na.rm=T)),1)


p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset5, aes(x = lon, y = lat, color= alpha_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p



ggsave(file="9-9.2ka  Map of alpha_anomaly with original colour.jpeg",p,width=22,height=11)


midpoint=0
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset5, aes(x = lon, y = lat, colour= alpha_anomaly), size = 2)+
  scale_color_gradientn(colours = c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red"),values=c(0, (midpoint-min(subset5$alpha_anomaly,na.rm=T))/(max(subset5$alpha_anomaly,na.rm=T)-min(subset5$alpha_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p


ggsave(file="9-9.2ka  Map of alpha_anomaly with mid point reset.jpeg",p,width=22,height=11)

#11-11.2ka

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset6, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="11-11.2ka  Map of Tmin_anomaly with original colour.jpeg",p,width=22,height=11)


(midpoint-min(subset6$Tmin_anomaly,na.rm=T))/(max(subset6$Tmin_anomaly,na.rm=T)-min(subset6$Tmin_anomaly,na.rm=T))
# 0.6163141

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset6, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0, (midpoint-min(subset6$Tmin_anomaly,na.rm=T))/(max(subset6$Tmin_anomaly,na.rm=T)-min(subset6$Tmin_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

ggsave(file="11-11.2ka  Map of Tmin_anomaly with mid point reset.jpeg",p,width=22,height=11)

#p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
#  geom_point(data = subset6, aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
#  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0, 0.3,(midpoint-min(subset6$Tmin_anomaly,na.rm=T))/(max(subset6$Tmin_anomaly,na.rm=T)-min(subset6$Tmin_anomaly,na.rm=T)),0.8,1),na.value = "grey60")+
#  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
#  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
# scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
#  theme(axis.text=element_text(size=12),legend.position = "right")+
#  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

#p

#ggsave(file="11-11.2ka  Map of Tmin_anomaly with mid point reset and key colour rescaled.jpeg",p,width=22,height=11)


summary(subset6$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-25.523 -11.870  -7.820  -7.504  -1.574  15.889     157 
summary(subset6$Tmax_anomaly)
#         Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-10.9882  -4.6658  -2.2777  -2.6238  -0.2792   5.3399      157 


(midpoint-min(subset6$Tmax_anomaly,na.rm=T))/(max(subset6$Tmax_anomaly,na.rm=T)-min(subset6$Tmax_anomaly,na.rm=T))
#0.6729614

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset6, aes(x = lon, y = lat, color= Tmax_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmax_anomaly))

p


v=c( 0, 2 , 4 , 5, 6 ,8, 10 )/10
v*2998
#[1]    0.0  599.6 1199.2 1499.0 1798.8 2398.4 2998.0

ggsave(file="11-11.2ka  Map of Tmax_anomaly with original colour.jpeg",p,width=22,height=11)

p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset6, aes(x = lon, y = lat, color= Tmax_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0,(midpoint-min(subset6$Tmax_anomaly,na.rm=T))/(max(subset6$Tmax_anomaly,na.rm=T)-min(subset6$Tmax_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmax_anomaly))

p

ggsave(file="11-11.2ka  Map of Tmax_anomaly with mid point reset.jpeg",p,width=22,height=11)



summary(subset6$gdd_anomaly)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#-2583.154 -1086.882  -485.813  -556.615     5.751  1980.962       157 
(midpoint-min(subset6$gdd_anomaly,na.rm=T))/(max(subset6$gdd_anomaly,na.rm=T)-min(subset6$gdd_anomaly,na.rm=T))
#0.5659702

#0.0  ,489.6,  979.2, 1224.0 ,1468.8, 1958.4, 2448.0)/2448,
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset6, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p

ggsave(file="11-11.2ka  Map of GDD_anomaly with original colour.jpeg",p,width=22,height=11)



p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset6, aes(x = lon, y = lat, color= gdd_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),values=c(0,(midpoint-min(subset6$gdd_anomaly,na.rm=T))/(max(subset6$gdd_anomaly,na.rm=T)-min(subset6$gdd_anomaly,na.rm=T)) ,1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(gdd_anomaly))

p

ggsave(file="11-11.2ka  Map of GDD_anomaly with mid point reset.jpeg",p,width=22,height=11)


summary(subset6$alpha_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-0.29378 -0.00200  0.04375  0.06370  0.14364  0.40343      157 
v*0.5#does not work, centre is blue instead of white

#values=c(0, (midpoint-min(subset6$alpha_anomaly,na.rm=T))/(max(subset6$alpha_anomaly,na.rm=T)-min(subset6$alpha_anomaly,na.rm=T)),1)


p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset6, aes(x = lon, y = lat, color= alpha_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p



ggsave(file="11-11.2ka  Map of alpha_anomaly with original colour.jpeg",p,width=22,height=11)


midpoint=0
p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray70',color='gray30') + theme_bw()+
  geom_point(data = subset6, aes(x = lon, y = lat, colour= alpha_anomaly), size = 2)+
  scale_color_gradientn(colours = c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red"),values=c(0, (midpoint-min(subset6$alpha_anomaly,na.rm=T))/(max(subset6$alpha_anomaly,na.rm=T)-min(subset6$alpha_anomaly,na.rm=T)),1),na.value = "grey60")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(alpha_anomaly))

p


ggsave(file="11-11.2ka  Map of alpha_anomaly with mid point reset.jpeg",p,width=22,height=11)



########################################################################################################
###### same maps with NA removed for a clearer trend? Also same colour scheme for each variable? #######
########################################################################################################




p<-ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray60',color='gray30') + theme_bw()+
  geom_point(data = subset(subset6, !is.na(Tmin_anomaly)), aes(x = lon, y = lat, color= Tmin_anomaly), size = 2)+
  scale_colour_gradientn(colours = (c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey")+
  scalebar(region, dist = 1000, dist_unit = "km",transform = TRUE, model = "WGS84",st.size = 3)+
  scale_y_continuous(breaks = yat, labels = paste0(yat,'°N')) +
  scale_x_continuous(breaks = xat, labels = paste0(xat,'°E')) +
  theme(axis.text=element_text(size=12),legend.position = "right")+
  labs(x="Longitude",y="Latitude",colour =expression(Tmin_anomaly))

p

#et_col <- colorRamp(c("red", "white", "blue"))  # make fun to interpolate colors
#quantiles <- (0:6) / 6                            # how many quantiles we want to map 
#quantile.vals <- quantile(subset6$Tmin_anomaly, quantiles, names=F,na.rm=TRUE)# the values for each quantile
#colours <- rgb(get_col(quantiles), max=255)       # 7 evenly interpolated colors 
#val.remap <- (quantile.vals - min(subset6[["Tmin_anomaly"]],na.rm=TRUE))/  diff(range(subset6[["Tmin_anomaly"]]))                                # The values corresponding to the quantiles

#ggplot(a, aes(x=x,y=y,fill=z)) + 
#  geom_bar(stat="identity") +
#  scale_fill_gradientn(
#    colours=colours,
#    values=val.remap,
#    breaks=quantile.vals,# Necessary to get legend values spread appropriately
#    guide="legend")      # N

#p=ggplot() + geom_polygon(data = region, aes(x=long, y = lat, group = group),fill='gray60',color='gray30') + theme_bw()+
#  geom_point(data = modern_pollen, aes(x = Long, y = Lat, color= alpha), size = 1)+
# scale_colour_gradientn(colours = rev(c("blue","dodgerblue2","lightskyblue","white","gold","orangered","red")),na.value = "grey")+
# ggsn::scalebar(region, anchor = c(x=149.5928-8,y=29.4+3),dist = 1000, st.size=3, height=0.01, dist_unit = "km",transform = TRUE, model = 'WGS84')+
# theme(axis.text=element_text(size=12),legend.position = "right")+
# labs(x="Longitude",y="Latitude",colour =expression(alpha)) +geom_sf(data = world, fill = NA) +coord_sf(xlim = c(-20.6097, 149.5928), ylim = c(29.4, 79.7742), expand = FALSE) 

ggsave(file="Map of T min anomaly.jpeg",p,width=40,height=20)

########################################################################################################
#####################################   Time series  Figure with bin 200  ##########################################
########################################################################################################
if(!require(sf)){ install.packages("sf");library(sf)}
range<-function(data,nboot,variable){
  data$site<-as.factor(data$site)
  k_samples <- replicate(nboot, sample(1:nlevels(data$site),
                                       size = nlevels(data$site),
                                       replace = T))
  xboot<-data.frame()
  
  for(i in 1:nboot){
    k <- k_samples[, i]
    select<-levels(data$site)[k]
    datak<-data[levels(data$site) %in% select,]
    datak$age<-as.factor(datak$age)
    out<-aggregate(datak[,colnames(datak)==variable]~datak$age, FUN=mean)[,2]
    xboot<-rbind.data.frame(xboot,out)
  }
  return(xboot)
}
age_ka<-seq(from = 0.1, to = 11.9, by = 0.2)

mean_site_env<-as.data.frame(age_ka)
mean_site_env$mean_alpha_anomaly<-aggregate(gradient_env$alpha_anomaly~gradient_env$age, FUN=mean)[,2]
mean_site_env$mean_Tmin_anomaly<-aggregate(gradient_env$Tmin_anomaly~gradient_env$age, FUN=mean)[,2]
mean_site_env$mean_Tmax_anomaly<-aggregate(gradient_env$Tmax_anomaly~gradient_env$age, FUN=mean)[,2]
mean_site_env$mean_insol_summer_anomaly<-aggregate(gradient_env$insol_summer_anomaly~gradient_env$age, FUN=mean)[,2]
mean_site_env$mean_insol_winter_anomaly<-aggregate(gradient_env$insol_winter_anomaly~gradient_env$age, FUN=mean)[,2]

mean_site_env$range_alpha_anomaly<-apply(range(gradient_env,nboot=1000,variable="alpha_anomaly"),2,sd,na.rm=T)
mean_site_env$range_Tmin_anomaly<-apply(range(gradient_env,nboot=1000,variable="Tmin_anomaly"),2,sd,na.rm=T)
mean_site_env$range_Tmax_anomaly<-apply(range(gradient_env,nboot=1000,variable="Tmax_anomaly"),2,sd,na.rm=T)
mean_site_env$range_insol_summer_anomaly<-apply(range(gradient_env,nboot=1000,variable="insol_summer_anomaly"),2,sd,na.rm=T)
mean_site_env$range_insol_winter_anomaly<-apply(range(gradient_env,nboot=1000,variable="insol_winter_anomaly"),2,sd,na.rm=T)

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/special epd plots")
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots")

#Time series
library(ggplot2)
p1<-ggplot()+theme_bw()+
  geom_point(data=mean_site_env,aes(age_ka,mean_Tmin_anomaly),size=1.5)+
  geom_line(data=mean_site_env,aes(age_ka,mean_Tmin_anomaly),size=2)+
  geom_pointrange(data=mean_site_env,aes(age_ka,mean_Tmin_anomaly,
                                         ymin=mean_Tmin_anomaly-range_Tmin_anomaly,
                                         ymax=mean_Tmin_anomaly+range_Tmin_anomaly))+
  labs(y="MTCO anomaly (°C)")+scale_x_continuous(breaks= seq(from = 0.1, to = 11.9, by = 0.2))+
  theme(axis.title.x = element_blank())+
  annotate("text", y= max(mean_site_env$mean_Tmin_anomaly+mean_site_env$range_Tmin_anomaly,na.rm=T), x =0,label="(a)",size=5)

p2<-ggplot()+theme_bw()+
  theme(legend.position="none")+
  geom_point(data=mean_site_env,aes(age_ka,mean_Tmax_anomaly),size=1.5)+
  geom_line(data=mean_site_env,aes(age_ka,mean_Tmax_anomaly),size=2)+
  geom_pointrange(data=mean_site_env,aes(age_ka,mean_Tmax_anomaly,
                                         ymin=mean_Tmax_anomaly-range_Tmax_anomaly,
                                         ymax=mean_Tmax_anomaly+range_Tmax_anomaly))+
  labs(y="MTWA anomaly (°C)")+scale_x_continuous(breaks= seq(from = 0.1, to = 11.9, by = 0.2))+
  theme(axis.title.x = element_blank())+
  annotate("text", y= max(mean_site_env$mean_Tmax_anomaly+mean_site_env$range_Tmax_anomaly,na.rm=T), x =0,label="(b)",size=5)

p3<-ggplot()+theme_bw()+
  theme(legend.position="none")+
  geom_point(data=mean_site_env,aes(age_ka,mean_alpha_anomaly),size=1.5)+
  geom_line(data=mean_site_env,aes(age_ka,mean_alpha_anomaly),size=2)+
  geom_pointrange(data=mean_site_env,aes(age_ka,mean_alpha_anomaly,
                                         ymin=mean_alpha_anomaly-range_alpha_anomaly,
                                         ymax=mean_alpha_anomaly+range_alpha_anomaly))+
  labs(y=expression(alpha* " anomaly"),x="Age (kyr BP)")+
  scale_x_continuous(breaks= seq(from = 0.1, to = 11.9, by = 0.2))+
  annotate("text", y= max(mean_site_env$mean_alpha_anomaly+mean_site_env$range_alpha_anomaly,na.rm=T), x =0,label="(c)",size=5)

p4<-ggplot()+theme_bw()+
  theme(legend.position="none")+
  geom_point(data=mean_site_env,aes(age_ka,mean_insol_summer_anomaly),size=1.5)+
  geom_line(data=mean_site_env,aes(age_ka,mean_insol_summer_anomaly),size=2)+
  geom_pointrange(data=mean_site_env,aes(age_ka,mean_insol_summer_anomaly,
                                         ymin=mean_insol_summer_anomaly-range_insol_summer_anomaly,
                                         ymax=mean_insol_summer_anomaly+range_insol_summer_anomaly))+
  labs(y=bquote('Summer insolation anomaly (W'~m^-2~')'),x="Age (kyr BP)")+
  scale_x_continuous(breaks= seq(from = 0.1, to = 11.9, by = 0.2))+
  annotate("text", y= max(gradient_env$insol_summer_anomaly,na.rm=T), x =-0.5,label="(e)",size=5)

p5<-ggplot()+theme_bw()+
  theme(legend.position="none")+
  geom_point(data=mean_site_env,aes(age_ka,mean_insol_winter_anomaly),size=1.5)+
  geom_line(data=mean_site_env,aes(age_ka,mean_insol_winter_anomaly),size=2)+
  geom_pointrange(data=mean_site_env,aes(age_ka,mean_insol_winter_anomaly,
                                         ymin=mean_insol_winter_anomaly-range_insol_winter_anomaly,
                                         ymax=mean_insol_winter_anomaly+range_insol_winter_anomaly))+
  labs(y=bquote('Winter insolation anomaly (W'~m^-2~')'))+
  scale_x_continuous(breaks= seq(from = 0.1, to = 11.9, by = 0.2))+  
  theme(axis.title.x = element_blank())+
  annotate("text", y= max(gradient_env$insol_winter_anomaly,na.rm=T), x =-0.5,label="(d)",size=5)

library(ggpubr)
require("ggplot2")
require("gridExtra")


#merge all three plots within one grid (and visualize this)
#arranges plots within grid
grid.arrange(p1,p5,p2,p4,p3,ncol=2) 
p <- arrangeGrob(p1,p5,p2,p4,p3,ncol=2)
ggsave(p,file="Time series with bin 200.jpeg",width=10,height=10)


########################################################################################################
######################################   Get the gradient old #############################################
########################################################################################################
get_gradient<-function(plotdata,age,range){
  plotdata$site<-as.factor(plotdata$site)
  mean_env<-data.frame(matrix(NA,ncol=5+4+2,nrow=nlevels(plotdata$site)))
  for(m in 1:nlevels(plotdata$site)){
    plotdata_each<-plotdata[which(plotdata$site==levels(plotdata$site)[m]),]
    
    mean_Tmin<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"Tmin"]),na.rm = TRUE)
    mean_gdd<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"gdd"]),na.rm = TRUE)
    mean_alpha<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"alpha"]),na.rm = TRUE)
    mean_Tmax<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"Tmax"]),na.rm = TRUE)
    
    mean_insol_summer<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"insol_summer"]),na.rm = TRUE)
    mean_insol_winter<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"insol_winter"]),na.rm = TRUE)
    mean_env[m,]<-c(age,levels(plotdata$site)[m],unique(plotdata_each$lon),unique(plotdata_each$lat),unique(plotdata_each$elv),mean_Tmin,mean_gdd,mean_alpha,mean_Tmax,mean_insol_summer,mean_insol_winter)
  }
  colnames(mean_env)<-c("age","site","lon","lat","elv","mean_Tmin","mean_gdd","mean_alpha","mean_Tmax","mean_insol_summer","mean_insol_winter")
  return(mean_env)
}

# Get fossil gradient 
gradient_env<-rbind.data.frame(
  get_gradient(plotdata,500,500),get_gradient(plotdata,1500,500),
  get_gradient(plotdata,2500,500),get_gradient(plotdata,3500,500),
  get_gradient(plotdata,4500,500),get_gradient(plotdata,5500,500),
  get_gradient(plotdata,6500,500),get_gradient(plotdata,7500,500),
  get_gradient(plotdata,8500,500),get_gradient(plotdata,9500,500),
  get_gradient(plotdata,10500,500),get_gradient(plotdata,11500,500))
#get modern gradient
modern_gradient<-get_gradient(plotdata,500,500)
#combine
str(modern_gradient)
colnames(modern_gradient)<-colnames(gradient_env)
for(j in c(1,3:ncol(modern_gradient))){
  modern_gradient[,j]<-as.numeric(modern_gradient[,j])
}

str(gradient_env)
for(j in c(1,3:ncol(gradient_env))){
  gradient_env[,j]<-as.numeric(gradient_env[,j])
}
gradient_env$agef<-factor(gradient_env$age,labels=c("0.5 ka","1.5 ka","2.5 ka","3.5 ka","4.5 ka","5.5 ka",
                                                    "6.5 ka","7.5 ka","8.5 ka","9.5 ka","10.5 ka","11.5 ka"))


#get anomaly to 0.5 ka
#paleo-modern(0.5 ka)
#anomaly in this realm stands for difference. Here it is the difference between paleo and modern (here it is 0.5ka)
gradient_env$Tmin_anomaly<-gradient_env$mean_Tmin-rep(modern_gradient[,"mean_Tmin"],nlevels(gradient_env$agef))
gradient_env$gdd_anomaly<-gradient_env$mean_gdd-rep(modern_gradient[,"mean_gdd"],nlevels(gradient_env$agef))
gradient_env$alpha_anomaly<-gradient_env$mean_alpha-rep(modern_gradient[,"mean_alpha"],nlevels(gradient_env$agef))
gradient_env$Tmax_anomaly<-gradient_env$mean_Tmax-rep(modern_gradient[,"mean_Tmax"],nlevels(gradient_env$agef))

gradient_env$insol_summer_anomaly<-gradient_env$mean_insol_summer-rep(modern_gradient[,"mean_insol_summer"],nlevels(gradient_env$agef))
gradient_env$insol_winter_anomaly<-gradient_env$mean_insol_winter-rep(modern_gradient[,"mean_insol_winter"],nlevels(gradient_env$agef))

gradient_env$elv_label<-NA
for(j in 1:nrow(gradient_env)){
  elv<-as.numeric(gradient_env[j,"elv"])
  if(elv<=1000){
    gradient_env[j,"elv_label"]<-"low"
  }else if(elv>1000){
    gradient_env[j,"elv_label"]<-"high"
  }
}
gradient_env$elv_label<-factor(gradient_env$elv_label,levels = c("low","high"))
write.csv(gradient_env,"gradient_env.csv")


########################################################################################################
#####################################   Time series  Figure old ##########################################
########################################################################################################
if(!require(sf)){ install.packages("sf");library(sf)}
range<-function(data,nboot,variable){
  data$site<-as.factor(data$site)
  k_samples <- replicate(nboot, sample(1:nlevels(data$site),
                                       size = nlevels(data$site),
                                       replace = T))
  xboot<-data.frame()
  
  for(i in 1:nboot){
    k <- k_samples[, i]
    select<-levels(data$site)[k]
    datak<-data[levels(data$site) %in% select,]
    datak$age<-as.factor(datak$age)
    out<-aggregate(datak[,colnames(datak)==variable]~datak$age, FUN=mean)[,2]
    xboot<-rbind.data.frame(xboot,out)
  }
  return(xboot)
}
age_ka<-
  
  mean_site_env<-as.data.frame(age_ka)
mean_site_env$mean_alpha_anomaly<-aggregate(gradient_env$alpha_anomaly~gradient_env$age, FUN=mean)[,2]
mean_site_env$mean_Tmin_anomaly<-aggregate(gradient_env$Tmin_anomaly~gradient_env$age, FUN=mean)[,2]
mean_site_env$mean_Tmax_anomaly<-aggregate(gradient_env$Tmax_anomaly~gradient_env$age, FUN=mean)[,2]
mean_site_env$mean_insol_summer_anomaly<-aggregate(gradient_env$insol_summer_anomaly~gradient_env$age, FUN=mean)[,2]
mean_site_env$mean_insol_winter_anomaly<-aggregate(gradient_env$insol_winter_anomaly~gradient_env$age, FUN=mean)[,2]

mean_site_env$range_alpha_anomaly<-apply(range(gradient_env,nboot=1000,variable="alpha_anomaly"),2,sd,na.rm=T)
mean_site_env$range_Tmin_anomaly<-apply(range(gradient_env,nboot=1000,variable="Tmin_anomaly"),2,sd,na.rm=T)
mean_site_env$range_Tmax_anomaly<-apply(range(gradient_env,nboot=1000,variable="Tmax_anomaly"),2,sd,na.rm=T)
mean_site_env$range_insol_summer_anomaly<-apply(range(gradient_env,nboot=1000,variable="insol_summer_anomaly"),2,sd,na.rm=T)
mean_site_env$range_insol_winter_anomaly<-apply(range(gradient_env,nboot=1000,variable="insol_winter_anomaly"),2,sd,na.rm=T)

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/special epd plots")
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots")

#Time series
p1<-ggplot()+theme_bw()+
  geom_point(data=mean_site_env,aes(age_ka,mean_Tmin_anomaly),size=1.5)+
  geom_line(data=mean_site_env,aes(age_ka,mean_Tmin_anomaly),size=2)+
  geom_pointrange(data=mean_site_env,aes(age_ka,mean_Tmin_anomaly,
                                         ymin=mean_Tmin_anomaly-range_Tmin_anomaly,
                                         ymax=mean_Tmin_anomaly+range_Tmin_anomaly))+
  labs(y="MTCO anomaly (°C)")+scale_x_continuous(breaks= seq(0.5,11.5))+
  theme(axis.title.x = element_blank())+
  annotate("text", y= max(mean_site_env$mean_Tmin_anomaly+mean_site_env$range_Tmin_anomaly,na.rm=T), x =0,label="(a)",size=5)

p2<-ggplot()+theme_bw()+
  theme(legend.position="none")+
  geom_point(data=mean_site_env,aes(age_ka,mean_Tmax_anomaly),size=1.5)+
  geom_line(data=mean_site_env,aes(age_ka,mean_Tmax_anomaly),size=2)+
  geom_pointrange(data=mean_site_env,aes(age_ka,mean_Tmax_anomaly,
                                         ymin=mean_Tmax_anomaly-range_Tmax_anomaly,
                                         ymax=mean_Tmax_anomaly+range_Tmax_anomaly))+
  labs(y="MTWA anomaly (°C)")+scale_x_continuous(breaks= seq(0.5,11.5))+
  theme(axis.title.x = element_blank())+
  annotate("text", y= max(mean_site_env$mean_Tmax_anomaly+mean_site_env$range_Tmax_anomaly,na.rm=T), x =0,label="(b)",size=5)

p3<-ggplot()+theme_bw()+
  theme(legend.position="none")+
  geom_point(data=mean_site_env,aes(age_ka,mean_alpha_anomaly),size=1.5)+
  geom_line(data=mean_site_env,aes(age_ka,mean_alpha_anomaly),size=2)+
  geom_pointrange(data=mean_site_env,aes(age_ka,mean_alpha_anomaly,
                                         ymin=mean_alpha_anomaly-range_alpha_anomaly,
                                         ymax=mean_alpha_anomaly+range_alpha_anomaly))+
  labs(y=expression(alpha* " anomaly"),x="Age (kyr BP)")+
  scale_x_continuous(breaks= seq(0.5,11.5))+
  annotate("text", y= max(mean_site_env$mean_alpha_anomaly+mean_site_env$range_alpha_anomaly,na.rm=T), x =0,label="(c)",size=5)

p4<-ggplot()+theme_bw()+
  theme(legend.position="none")+
  geom_point(data=mean_site_env,aes(age_ka,mean_insol_summer_anomaly),size=1.5)+
  geom_line(data=mean_site_env,aes(age_ka,mean_insol_summer_anomaly),size=2)+
  geom_pointrange(data=mean_site_env,aes(age_ka,mean_insol_summer_anomaly,
                                         ymin=mean_insol_summer_anomaly-range_insol_summer_anomaly,
                                         ymax=mean_insol_summer_anomaly+range_insol_summer_anomaly))+
  labs(y=bquote('Summer insolation anomaly (W'~m^-2~')'),x="Age (kyr BP)")+
  scale_x_continuous(breaks= seq(0.5,11.5))+
  annotate("text", y= max(gradient_env$insol_summer_anomaly,na.rm=T), x =-0.5,label="(e)",size=5)

p5<-ggplot()+theme_bw()+
  theme(legend.position="none")+
  geom_point(data=mean_site_env,aes(age_ka,mean_insol_winter_anomaly),size=1.5)+
  geom_line(data=mean_site_env,aes(age_ka,mean_insol_winter_anomaly),size=2)+
  geom_pointrange(data=mean_site_env,aes(age_ka,mean_insol_winter_anomaly,
                                         ymin=mean_insol_winter_anomaly-range_insol_winter_anomaly,
                                         ymax=mean_insol_winter_anomaly+range_insol_winter_anomaly))+
  labs(y=bquote('Winter insolation anomaly (W'~m^-2~')'))+
  scale_x_continuous(breaks= seq(0.5,11.5))+  
  theme(axis.title.x = element_blank())+
  annotate("text", y= max(gradient_env$insol_winter_anomaly,na.rm=T), x =-0.5,label="(d)",size=5)

p<-ggarrange(p1,p5,p2,p4,p3,ncol=2)
ggsave(p,file="Time series.jpeg",width=10,height=10)
