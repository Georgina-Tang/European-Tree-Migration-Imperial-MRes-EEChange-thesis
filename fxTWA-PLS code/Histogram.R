#Histogram

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new site.RData")


#1-1.2ka
subset1<-gradient_env_NA_removed %>% filter(age ==1100)


jpeg("1-1.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset1$Tmin_anomaly)
dev.off()

jpeg("1-1.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset1$Tmax_anomaly)
dev.off()

jpeg("1-1.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset1$alpha_anomaly)
dev.off()

jpeg("1-1.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset1$gdd_anomaly)
dev.off()


#2-2.2ka
subset2<-gradient_env_NA_removed %>% filter(age ==2100)

jpeg("2-2.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset2$Tmin_anomaly)
dev.off()

jpeg("2-2.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset2$Tmax_anomaly)
dev.off()

jpeg("2-2.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset2$alpha_anomaly)
dev.off()

jpeg("2-2.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset2$gdd_anomaly)
dev.off()

#3-3.2ka
subset3<-gradient_env_NA_removed %>% filter(age ==3100)

jpeg("3-3.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset3$Tmin_anomaly)
dev.off()

jpeg("3-3.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset3$Tmax_anomaly)
dev.off()

jpeg("3-3.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset3$alpha_anomaly)
dev.off()

jpeg("3-3.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset3$gdd_anomaly)
dev.off()

#4-4.2ka
subset4<-gradient_env_NA_removed %>% filter(age ==4100)

jpeg("4-4.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset4$Tmin_anomaly)
dev.off()

jpeg("4-4.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset4$Tmax_anomaly)
dev.off()

jpeg("4-4.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset4$alpha_anomaly)
dev.off()

jpeg("4-4.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset4$gdd_anomaly)
dev.off()

#5-5.2ka
subset5<-gradient_env_NA_removed %>% filter(age ==5100)

jpeg("5-5.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset5$Tmin_anomaly)
dev.off()

jpeg("5-5.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset5$Tmax_anomaly)
dev.off()

jpeg("5-5.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset5$alpha_anomaly)
dev.off()

jpeg("5-5.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset5$gdd_anomaly)
dev.off()

#6-6.2ka
subset6<-gradient_env_NA_removed %>% filter(age ==6100)

jpeg("6-6.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset6$Tmin_anomaly)
dev.off()

jpeg("6-6.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset6$Tmax_anomaly)
dev.off()

jpeg("6-6.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset6$alpha_anomaly)
dev.off()

jpeg("6-6.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset6$gdd_anomaly)
dev.off()

#7-7.2ka
subset7<-gradient_env_NA_removed %>% filter(age ==7100)

jpeg("7-7.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset7$Tmin_anomaly)
dev.off()

jpeg("7-7.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset7$Tmax_anomaly)
dev.off()

jpeg("7-7.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset7$alpha_anomaly)
dev.off()

jpeg("7-7.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset7$gdd_anomaly)
dev.off()
#8-8.2ka
subset8<-gradient_env_NA_removed %>% filter(age ==8100)

jpeg("8-8.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset8$Tmin_anomaly)
dev.off()

jpeg("8-8.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset8$Tmax_anomaly)
dev.off()

jpeg("8-8.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset8$alpha_anomaly)
dev.off()

jpeg("8-8.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset8$gdd_anomaly)
dev.off()
#9-9.2ka
subset9<-gradient_env_NA_removed %>% filter(age ==9100)

jpeg("9-9.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset9$Tmin_anomaly)
dev.off()

jpeg("9-9.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset9$Tmax_anomaly)
dev.off()

jpeg("9-9.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset9$alpha_anomaly)
dev.off()

jpeg("9-9.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset9$gdd_anomaly)
dev.off()
#10-10.2ka
subset10<-gradient_env_NA_removed %>% filter(age ==10100)

jpeg("10-10.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset10$Tmin_anomaly)
dev.off()

jpeg("10-10.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset10$Tmax_anomaly)
dev.off()

jpeg("10-10.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset10$alpha_anomaly)
dev.off()

jpeg("10-10.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset10$gdd_anomaly)
dev.off()
#11-11.2ka
subset11<-gradient_env_NA_removed %>% filter(age ==11100)

jpeg("11-11.2ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset11$Tmin_anomaly)
dev.off()

jpeg("11-11.2ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset11$Tmax_anomaly)
dev.off()

jpeg("11-11.2ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset11$alpha_anomaly)
dev.off()

jpeg("11-11.2ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset11$gdd_anomaly)
dev.off()

#11.8-12.0ka
subset12<-gradient_env_NA_removed %>% filter(age ==11900)


jpeg("11.8-12.0ka histogram of Tmin_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset12$Tmin_anomaly)
dev.off()

jpeg("11.8-12.0ka histogram of Tmax_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset12$Tmax_anomaly)
dev.off()

jpeg("11.8-12.0ka histogram of alpha_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset12$alpha_anomaly)
dev.off()

jpeg("11.8-12.0ka histogram of gdd_anomaly.jpg",res=300,width=8,height=6,units = "in" )
hist(subset12$gdd_anomaly)
dev.off()



#histogram of all the bins
jpeg("histogram of Tmin_anomaly of all the bins.jpg",res=300,width=8,height=6,units = "in" )
hist(gradient_env_NA_removed$Tmin_anomaly)
dev.off()

jpeg("histogram of Tmax_anomaly of all the bins.jpg",res=300,width=8,height=6,units = "in" )
hist(gradient_env_NA_removed$Tmax_anomaly)
dev.off()

jpeg("histogram of alpha_anomaly of all the bins.jpg",res=300,width=8,height=6,units = "in" )
hist(gradient_env_NA_removed$alpha_anomaly)
dev.off()
jpeg("histogram of gdd_anomaly of all the bins.jpg",res=300,width=8,height=6,units = "in" )
hist(gradient_env_NA_removed$gdd_anomaly)
dev.off()
