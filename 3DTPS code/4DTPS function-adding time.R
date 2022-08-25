#own reconstruction new site

#04-05 new data

#my own reconstruction using the new data Roberto gave me on 04-05

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new site add time.RData")

save.image(file = "reconstruction new site add time.RData")




plotdata<-total
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


plotdata["lat"][plotdata["site"] == "Stuphallet"] <- "78.96083"
plotdata["lon"][plotdata["site"] == "Stuphallet"] <- "11.59694"

plotdata["lat"][plotdata["site"] == "Majen El Orbi"] <- "37.15294"
plotdata["lon"][plotdata["site"] == "Majen El Orbi"] <- "9.0984"
plotdata["elv"][plotdata["site"] == "Majen El Orbi"] <- "200"


plotdata["lon"][plotdata["site"] == "Rosenbergdalen"] <- "20.9258"

plotdata["lat"][plotdata["site"] == "Nile Delta"] <- "31.3"
plotdata["lon"][plotdata["site"] == "Nile Delta"] <- "31.6"
plotdata["elv"][plotdata["site"] == "Nile Delta"] <- "1"


saveRDS(plotdata, file = "plotdata with sites issue fixed new site_1.RDS") 

plotdata<-readRDS('plotdata with sites issue fixed new site_1.RDS')

get_gradient_2<-function(plotdata,age,range){
  plotdata$site<-as.factor(plotdata$site)
  mean_env<-data.frame(matrix(NA,ncol=6+4+4,nrow=nlevels(plotdata$site)))
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
    
    mean_age<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"age"]),na.rm = TRUE)
    
    # mean_insol_summer<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"insol_summer"]),na.rm = TRUE)
    # mean_insol_winter<-mean(as.matrix(plotdata_each[which(plotdata_each$age>age-range & plotdata_each$age<age+range),"insol_winter"]),na.rm = TRUE)
    #mean_env[m,]<-c(age,levels(plotdata$site)[m],unique(plotdata_each$lon),unique(plotdata_each$lat),unique(plotdata_each$elv),mean_Tmin,mean_gdd,mean_alpha,mean_Tmax,mean_insol_summer,mean_insol_winter)
    mean_env[m,]<-c(age,levels(plotdata$site)[m],unique(plotdata_each$lon),unique(plotdata_each$lat),unique(plotdata_each$elv),mean_age,mean_Tmin,mean_gdd,mean_alpha,mean_Tmax,se_mean_Tmin,se_mean_Tmax,se_mean_gdd,se_mean_alpha)
  }
  colnames(mean_env)<-c("age","site","lon","lat","elv","mean_age","mean_Tmin","mean_gdd","mean_alpha","mean_Tmax","se_mean_Tmin","se_mean_Tmax","se_mean_gdd","se_mean_alpha")
  return(mean_env)
}



gradient_env_time<-rbind.data.frame(
  get_gradient_2(plotdata,100,100),get_gradient_2(plotdata,300,100),
  get_gradient_2(plotdata,500,100),get_gradient_2(plotdata,700,100),
  get_gradient_2(plotdata,900,100),get_gradient_2(plotdata,1100,100),
  get_gradient_2(plotdata,1300,100),get_gradient_2(plotdata,1500,100),
  get_gradient_2(plotdata,1700,100),get_gradient_2(plotdata,1900,100),
  get_gradient_2(plotdata,2100,100),get_gradient_2(plotdata,2300,100),
  get_gradient_2(plotdata,2500,100),get_gradient_2(plotdata,2700,100),
  get_gradient_2(plotdata,2900,100),get_gradient_2(plotdata,3100,100),
  get_gradient_2(plotdata,3300,100),get_gradient_2(plotdata,3500,100),
  get_gradient_2(plotdata,3700,100),get_gradient_2(plotdata,3900,100),
  get_gradient_2(plotdata,4100,100),get_gradient_2(plotdata,4300,100),
  get_gradient_2(plotdata,4500,100),get_gradient_2(plotdata,4700,100),
  get_gradient_2(plotdata,4900,100),get_gradient_2(plotdata,5100,100),
  get_gradient_2(plotdata,5300,100),get_gradient_2(plotdata,5500,100),
  get_gradient_2(plotdata,5700,100),get_gradient_2(plotdata,5900,100),
  get_gradient_2(plotdata,6100,100),get_gradient_2(plotdata,6300,100),
  get_gradient_2(plotdata,6500,100),get_gradient_2(plotdata,6700,100),
  get_gradient_2(plotdata,6900,100),get_gradient_2(plotdata,7100,100),
  get_gradient_2(plotdata,7300,100),get_gradient_2(plotdata,7500,100),
  get_gradient_2(plotdata,7700,100),get_gradient_2(plotdata,7900,100),
  get_gradient_2(plotdata,8100,100),get_gradient_2(plotdata,8300,100),
  get_gradient_2(plotdata,8500,100),get_gradient_2(plotdata,8700,100),
  get_gradient_2(plotdata,8900,100),get_gradient_2(plotdata,9100,100),
  get_gradient_2(plotdata,9300,100),get_gradient_2(plotdata,9500,100),
  get_gradient_2(plotdata,9700,100),get_gradient_2(plotdata,9900,100),
  get_gradient_2(plotdata,10100,100),get_gradient_2(plotdata,10300,100),
  get_gradient_2(plotdata,10500,100),get_gradient_2(plotdata,10700,100),
  get_gradient_2(plotdata,10900,100),get_gradient_2(plotdata,11100,100),
  get_gradient_2(plotdata,11300,100),get_gradient_2(plotdata,11500,100),
  get_gradient_2(plotdata,11700,100),get_gradient_2(plotdata,11900,100))

saveRDS(gradient_env_time, file = "gradient_env_1 with time.RDS") 



modern_gradient<-get_gradient_2(plotdata,100,100)
#combine
str(modern_gradient)
colnames(modern_gradient)<-colnames(gradient_env_time)
for(j in c(1,3:ncol(modern_gradient))){
  modern_gradient[,j]<-as.numeric(modern_gradient[,j])
}

str(gradient_env_time)
for(j in c(1,3:ncol(gradient_env_time))){
  gradient_env_time[,j]<-as.numeric(gradient_env_time[,j])
}
#gradient_env_time$agef<-factor(gradient_env_time$age,labels=c("0.5 ka","1.5 ka","2.5 ka","3.5 ka","4.5 ka","5.5 ka",
#                                                    "6.5 ka","7.5 ka","8.5 ka","9.5 ka","10.5 ka","11.5 ka"))

paste(seq(from = 0.1, to = 11.9, by = 0.2)," ka",sep="")

gradient_env_time$agef<-factor(gradient_env_time$age,labels=c("0.1 ka", "0.3 ka",  "0.5 ka",  "0.7 ka",  "0.9 ka",  "1.1 ka",  "1.3 ka",  "1.5 ka",  "1.7 ka",  "1.9 ka",  "2.1 ka", 
                                                    "2.3 ka", "2.5 ka",  "2.7 ka",  "2.9 ka",  "3.1 ka",  "3.3 ka",  "3.5 ka",  "3.7 ka",  "3.9 ka",  "4.1 ka",  "4.3 ka", 
                                                    "4.5 ka", "4.7 ka",  "4.9 ka",  "5.1 ka",  "5.3 ka",  "5.5 ka",  "5.7 ka",  "5.9 ka",  "6.1 ka",  "6.3 ka",  "6.5 ka", 
                                                    "6.7 ka", "6.9 ka",  "7.1 ka",  "7.3 ka",  "7.5 ka",  "7.7 ka",  "7.9 ka",  "8.1 ka",  "8.3 ka",  "8.5 ka",  "8.7 ka", 
                                                    "8.9 ka", "9.1 ka",  "9.3 ka",  "9.5 ka",  "9.7 ka",  "9.9 ka",  "10.1 ka", "10.3 ka", "10.5 ka", "10.7 ka", "10.9 ka",
                                                    "11.1 ka", "11.3 ka", "11.5 ka", "11.7 ka", "11.9 ka"))

#get anomaly to 0.5 ka
#paleo-modern(0.5 ka)
#anomaly in this realm stands for difference. Here it is the difference between paleo and modern (here it is 0.5ka)
gradient_env_time$Tmin_anomaly<-gradient_env_time$mean_Tmin-rep(modern_gradient[,"mean_Tmin"],nlevels(gradient_env_time$agef))
gradient_env_time$gdd_anomaly<-gradient_env_time$mean_gdd-rep(modern_gradient[,"mean_gdd"],nlevels(gradient_env_time$agef))
gradient_env_time$alpha_anomaly<-gradient_env_time$mean_alpha-rep(modern_gradient[,"mean_alpha"],nlevels(gradient_env_time$agef))
gradient_env_time$Tmax_anomaly<-gradient_env_time$mean_Tmax-rep(modern_gradient[,"mean_Tmax"],nlevels(gradient_env_time$agef))
gradient_env_time$se_Tmin_anomaly<-sqrt(gradient_env_time$se_mean_Tmin^2+rep(modern_gradient[,"se_mean_Tmin"],nlevels(gradient_env_time$agef))^2)
gradient_env_time$se_Tmax_anomaly<-sqrt(gradient_env_time$se_mean_Tmax^2+rep(modern_gradient[,"se_mean_Tmax"],nlevels(gradient_env_time$agef))^2)
gradient_env_time$se_gdd_anomaly<-sqrt(gradient_env_time$se_mean_gdd^2+rep(modern_gradient[,"se_mean_gdd"],nlevels(gradient_env_time$agef))^2)
gradient_env_time$se_alpha_anomaly<-sqrt(gradient_env_time$se_mean_alpha^2+rep(modern_gradient[,"se_mean_alpha"],nlevels(gradient_env_time$agef))^2)

# gradient_env_time$insol_summer_anomaly<-gradient_env_time$mean_insol_summer-rep(modern_gradient[,"mean_insol_summer"],nlevels(gradient_env_time$agef))
# gradient_env_time$insol_winter_anomaly<-gradient_env_time$mean_insol_winter-rep(modern_gradient[,"mean_insol_winter"],nlevels(gradient_env_time$agef))

gradient_env_time$elv_label<-NA
for(j in 1:nrow(gradient_env_time)){
  elv<-as.numeric(gradient_env_time[j,"elv"])
  if(elv<=1000){
    gradient_env_time[j,"elv_label"]<-"low"
  }else if(elv>1000){
    gradient_env_time[j,"elv_label"]<-"high"
  }
}
gradient_env_time$elv_label<-factor(gradient_env_time$elv_label,levels = c("low","high"))
write.csv(gradient_env_time,"gradient_env_time with 200 year bin.csv")




gradient_env_time_NA_removed <-gradient_env_time[!is.na(gradient_env_time$mean_Tmax), ]#choosing Tmax as it contains max no of NAs,32681





############################################3DTPS

subset1<-gradient_env_time_NA_removed %>% filter(age ==1100)



subset1_2 <- subset1 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset1_3 <- subset1_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset1_3 <- subset1_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left
#487/714
#0.6820728
#68% of data left
#Error in Krig.check.xY(x, Y, Z, weights, na.rm, verbose = verbose) : length of y and weights differ
#checking lengths. they are all the same
length(subset1_3$se_Tmin_anomaly)
length(subset1_3$Tmin_anomaly)
length(subset1_3$elv)
#turns out that the weights need to be a vector
vec1 <- subset1_3$se_Tmin_anomaly
vec3 <- subset1_3$mean_age
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_1_elv_w <- subset1_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS,
    weights = vec1&vec3
  )


#save(tps_tmin_1_elv_w, file='tps_tmin_1_elv_w.rdata')

summary(tps_tmin_1_elv_w$Tmin_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-5.74310 -0.27219  0.29713  0.02007  0.54522  1.67502 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with weights")

tps_tmin_1_elv_w_cut<- tps_tmin_1_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_tmin_1_elv_w_cut$Tmin_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2.53671 -0.03275  0.45717  0.36379  0.78676  1.67502 



reds <- RColorBrewer::brewer.pal(3, "Reds")
reds
White<-c("#FFFFFF")#adding white colour as 0
blues_rev <- rev(RColorBrewer::brewer.pal(3, "Blues"))
COLOURS <- c(blues_rev,White, "#FEE0D2"  ,"#FC9272")
COLOURS <- c(blues_rev,White, reds)



p<-tps_tmin_1_elv_w_cut %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf,-2,  -1,-0.001,0.001,1, Inf),
        labels = c("<-2","-2","-1~0",  "0","0~1","1+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="1-1.2ka Map of Tmin_anomaly with elevation and weights filtered data colour 1 cut out.jpeg",p,width=22,height=11)





#6-6.2ka alpha
subset6<-gradient_env_time_NA_removed %>% filter(age ==6100)
subset6_2 <- subset6 %>%
  dplyr::filter(!is.na(elv))#600 out of 601 obs left
subset6_3 <- subset6_2 %>%
  dplyr::filter(!is.na(se_alpha_anomaly))#272 out of 601 left

subset6_4 <- subset6_2 %>%
  dplyr::filter(se_alpha_anomaly<0.1)#272 out of 601 left
subset6_5 <- subset6_2 %>%
  dplyr::filter(se_mean_alpha<0.1)

#0.452579
#45% of data left

vec1 <- subset6_3$se_alpha_anomaly
#vec1 <-1/vec1 
vec2 <-1/(vec1*vec1)
vec3 <- subset6_3$mean_age
vec4 <-1/(vec1*vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_alpha_6_elv_w2 <- subset6_3 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS,
    weights = vec2
  )

# tps_alpha_6_elv_w2 <- subset6_3 %>%
#   smpds::tps(
#     var = "alpha_anomaly",
#      z_var="mean_age",
#      z_mode = "independent",
#     z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
#     weights = vec2
#   )



save(tps_alpha_6_elv_w, file='tps_alpha_6_elv_w.rdata')

summary(tps_alpha_6_elv_w2$alpha_anomaly)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.161062 -0.031238 -0.009967  0.005769  0.044027  0.182042 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with weights")

tps_alpha_6_elv_w_cut<- tps_alpha_6_elv_w2  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_alpha_6_elv_w_cut$alpha_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.16106 -0.05499 -0.01660 -0.01186  0.02810  0.17384 


greens <- RColorBrewer::brewer.pal(4, "Greens")
White<-c("#FFFFFF")#adding white colour as 0
browns_rev <- rev(RColorBrewer::brewer.pal(4, "YlOrBr"))
#RColorBrewer::display.brewer.pal(4, "YlOrBr")
COLOURS <- c(browns_rev,White, greens)


p<-tps_alpha_6_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.14,  -0.1,-0.05,-0.001,0.001,0.05,0.1, 0.15, Inf),
        labels = c("<-0.14","-0.14","-0.1","-0.05~0",  "0","0~0.05","0.05","0.1",  "0.15+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))


ggsave(file="6-6.2ka Map of alpha_anomaly with elevation and weights filtered data colour 1 cut out_new.jpeg",p,width=22,height=11)



greens <- RColorBrewer::brewer.pal(4, "Greens")
White<-c("#FFFFFF")#adding white colour as 0
browns_rev <- rev(RColorBrewer::brewer.pal(3, "YlOrBr"))
#RColorBrewer::display.brewer.pal(4, "YlOrBr")
browns_rev

COLOURS <- c("#FEC44F" ,White, greens)


p<-tps_alpha_6_elv_w_cut %>%
  dplyr::mutate(
    alpha_anomaly = alpha_anomaly %>%
      cut(
        breaks = c(-Inf, -0.004,-0.001,0.001,0.1,0.2, 0.3, Inf),
        labels = c("<-0.004","-0.004~0",  "0","0~0.1","0.1","0.2",  "0.3+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "alpha_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c(COLOURS),
                              ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73))

