# #MI to alpha
# MI<-CRU$MI
# w<-3
# fai<-1/MI
# F<-1+fai-(1+fai^w)^(1/w)
# alpha<-1.26*MI*F
# hist(alpha)
# plot(alpha~MI)
# CRU$alpha<-alpha


#anomaly of alpha to anomaly of MI
#For MI, use the formula given as equation (3) in Gallego-Sala et al. (2016) Climate of the Past to convert anomalies of ?? into anomalies of MI.


#(delta a)/(delta m) = 1- [m/(1+m^w)^(1/w)]^(w-1) #this is the derivative of equation (2)
#delta a = delta m * (delta a)/(delta m) 
#delta a = delta m * derivative
#delta m = delta a/derivative (plus *1.26 because a from SPLASH, it has an upper bound of 1.26)
#m is MI

library(dplyr)
alpha_data_1_pts_NA <- alpha_data_1_pts %>% filter_at(vars(MI,alpha_anomaly),all_vars(!is.na(.)))


MI<-alpha_data_1_pts_NA$MI
delta_a<-alpha_data_1_pts_NA$alpha_anomaly
w<-3
deriv<- 1- (MI/(1+MI^w)^(1/w))^(w-1)
delta_m<-1.26*delta_a/deriv


alpha_data_1_pts_NA$MI_anomaly=delta_m
alpha_data_1_pts_NA$paleo_MI=alpha_data_1_pts_NA$MI+alpha_data_1_pts_NA$MI_anomaly
alpha_data_1_pts_NA$paleo_Tmin=alpha_data_1_pts_NA$Tmin+alpha_data_1_pts_NA$Tmin_anomaly
alpha_data_1_pts_NA$paleo_Tmax=alpha_data_1_pts_NA$Tmax+alpha_data_1_pts_NA$Tmax_anomaly
alpha_data_1_pts_NA$paleo_gdd=alpha_data_1_pts_NA$gdd0+alpha_data_1_pts_NA$gdd_anomaly
  
  
  
write.csv(alpha_data_1_pts_NA, 'modern_and_anomaly 1ka.csv')



#data cleaning, preparing gam input data
alpha_data_1_pts_gam=alpha_data_1_pts_NA[,c("paleo_MI","paleo_Tmax","paleo_Tmin","paleo_gdd","x","y")] 


alpha_data_1_pts_gam$rtmi=sqrt(alpha_data_1_pts_gam$paleo_MI)

names(alpha_data_1_pts_gam)[names(alpha_data_1_pts_gam) == 'paleo_Tmin'] <- 'tmin'
names(alpha_data_1_pts_gam)[names(alpha_data_1_pts_gam) == 'paleo_gdd'] <- 'gdd'
names(alpha_data_1_pts_gam)[names(alpha_data_1_pts_gam) == 'paleo_Tmax'] <- 'tmax'

names(alpha_data_1_pts_gam)[names(alpha_data_1_pts_gam) == 'x'] <- 'longitude'
names(alpha_data_1_pts_gam)[names(alpha_data_1_pts_gam) == 'y'] <- 'latitude'



save(alpha_data_1_pts_gam, file='alpha_data_1_pts_gam.rdata')
