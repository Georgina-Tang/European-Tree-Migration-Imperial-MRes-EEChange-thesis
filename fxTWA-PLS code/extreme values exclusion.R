#exclude extreme values


#delete the extreme values according to RMSEP
total_cleaned<- total
total_cleaned$Tmin[total_cleaned$sse_Tmin>4.7] <- NA  
total_cleaned$Tmin[total_cleaned$sse_Tmax>3.47] <- NA  
total_cleaned$Tmin[total_cleaned$sse_alpha>0.158] <- NA  
total_cleaned$Tmin[total_cleaned$sse_gdd>897.7] <- NA  




plotdata<-total_cleaned
#exlcude outliers of the climate varaibles.natural limit of alpha is 0-1.26/theoretical limit due to the relationship of Ep and Eq. 

plotdata<-plotdata[which(plotdata$alpha>=0&plotdata$alpha<=1.26),]

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



saveRDS(gradient_env, file = "gradient_env_with_extreme_excluded.RDS") 


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

