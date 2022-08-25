#4DTPS with weighting


#my own reconstruction using the new data Roberto gave me on 04-05

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new site.RData")

save.image(file = "reconstruction new site.RData")


########################################################################################################
#################################   general keys  ########################################
########################################################################################################

summary(gradient_env_NA_removed$Tmin_anomaly)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-29.672  -1.979   0.000  -0.303   2.106  29.322   14933 
summary(gradient_env_NA_removed$Tmax_anomaly)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-13.708  -0.431   0.395   0.683   2.140  12.990   14933 
summary(gradient_env_NA_removed$gdd_anomaly)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-3338.25  -159.86    59.00    95.61   425.38  3745.45    14933 
summary(gradient_env_NA_removed$alpha_anomaly)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -0.783  -0.068  -0.011  -0.015   0.024   0.579   14933 


#after TPS, the extreme data is smoothed out
#for temperature, -5, -4 to 4,5
#for alpha,-0.2 -0.1 to 0.1
#for gdd, -2000, -1500 to 2000


########################################################################################################
#################################    weighting  ########################################
########################################################################################################
library(dplyr)
#1ka Tmin
subset1<-gradient_env_NA_removed %>% filter(age ==1100)
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
    weights = vec2
  )


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with squared weights")

save(tps_tmin_1_elv_w, file='tps_tmin_1_elv_w.rdata')

summary(tps_tmin_1_elv_w$Tmin_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-5.83648 -0.44620  0.23543 -0.05087  0.59595  2.02234 

tps_tmin_1_elv_w_cut<- tps_tmin_1_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_tmin_1_elv_w_cut$Tmin_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2.49155  0.04652  0.56072  0.39880  0.74625  1.58409 



#1ka Tmax

#turns out that the weights need to be a vector
vec1 <- subset1_3$se_Tmax_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)

#using se_Tmin_anomaly as weighting
tps_tmax_1_elv_w <- subset1_3 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS,
    weights = vec2
  )


save(tps_tmax_1_elv_w, file='tps_tmax_1_elv_w.rdata')

summary(tps_tmax_1_elv_w$Tmax_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-1.48809  0.03775  0.32567  0.43900  0.81579  3.61400 

tps_tmax_1_elv_w_cut<- tps_tmax_1_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_tmax_1_elv_w_cut$Tmax_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.76374 -0.10520  0.06052  0.33779  0.57445  2.59286 




#6-6.2ka Tmin
subset6<-gradient_env_NA_removed %>% filter(age ==6100)
subset6_2 <- subset6 %>%
  dplyr::filter(!is.na(elv))#600 out of 601 obs left
subset6_3 <- subset6_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#272 out of 601 left


#0.452579
#45% of data left

vec1 <- subset6_3$se_Tmin_anomaly
#vec1 <-1/vec1 
vec2 <-1/(vec1*vec1)
#using se_Tmin_anomaly as weighting
tps_tmin_6_elv_w <- subset6_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS,
    weights = vec2
  )


save(tps_tmin_6_elv_w, file='tps_tmin_6_elv_w.rdata')

summary(tps_tmin_6_elv_w$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.6008 -1.5050 -0.7213 -0.5260  0.4675  3.6307 

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with weights")

tps_tmin_6_elv_w_cut<- tps_tmin_6_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_tmin_6_elv_w_cut$Tmin_anomaly)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-3.838261 -0.900770 -0.007089  0.089478  1.077169  3.630699 



#6-6.2ka alpha
subset6<-gradient_env_NA_removed %>% filter(age ==6100)
subset6_2 <- subset6 %>%
  dplyr::filter(!is.na(elv))#600 out of 601 obs left
subset6_3 <- subset6_2 %>%
  dplyr::filter(!is.na(se_alpha_anomaly))#272 out of 601 left


#0.452579
#45% of data left

vec1 <- subset6_3$se_alpha_anomaly
#vec1 <-1/vec1 
vec2 <-1/(vec1*vec1)
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

