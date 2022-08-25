#Tmin calculation

#4DTPS with weighting


#my own reconstruction using the new data Roberto gave me on 04-05

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/reconstruction new site.RData")

save.image(file = "reconstruction new site.RData")

# install.packages("remotes")
# library(remotes)
# install.packages("smpds")
# install.packages("cli")
# 
# install.packages("rlang")
# install.packages("devtools")
# options(download.file.method = "libcurl")
# #update smpds, so that TPS smoothing can extrapolate
remotes::install_github("special-uor/smpds")
# devtools::install_github("special-uor/smpds")

#downloading from github failed so we are manually installing it from the laptop


install.packages("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/4DTPS code/smpds_2.0.0.tgz", repos = NULL, type = "source")


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

#Tmin -5(-20) to 4

p<-tps_tmin_1_elv_w_cut %>%
  dplyr::mutate(
    Tmin_anomaly = Tmin_anomaly %>%
      cut(
        breaks = c(-Inf,-5,-4,-3,-2,  -1,0,1, 2,3,4,5,Inf),
        labels = c("<-5","-5","-4","-3","-2","-1",  "0","1+","2","3","4","5+"),
      )
  ) %>%
  smpds::plot_climate_tiles(var = "Tmin_anomaly",
                            units = "\u00B0C",
                            fill_scale =
                              ggplot2::scale_fill_manual(
                                values = c( "<-5"="#08519C" ,  "-5"="#3182BD" ,  "-4"="#6BAED6","-3"="#9ECAE1","-2"="#C6DBEF","-1"="#EFF3FF", "0"="#FEE5D9", "1+"="#FCBBA1",
                                            "2"="#FC9272" ,"3"="#FB6A4A" ,"4"="#DE2D26", "5+"="#A50F15") ),
                            xlim = c(-12, 45),
                            ylim = c(min(.$latitude, na.rm = TRUE), 73) )



########################################################################################################
#################################    weighting  ########################################
########################################################################################################
library(dplyr)
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with squared weights/extrapolate calculation r data")
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
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_tmin_1_elv_w, file='tps_tmin_1_elv_w.rdata')

summary(tps_tmin_1_elv_w$Tmin_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-5.83648 -0.44620  0.23543 -0.05087  0.59595  2.02234 

tps_tmin_1_elv_w_cut<- tps_tmin_1_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_tmin_1_elv_w_cut$Tmin_anomaly)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-2.49155  0.04652  0.56072  0.39880  0.74625  1.58409 

#2ka Tmin
subset2<-gradient_env_NA_removed %>% filter(age ==2100)
subset2_2 <- subset2 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset2_3 <- subset2_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset2_3 <- subset2_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left

vec1 <- subset2_3$se_Tmin_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_2_elv_w <- subset2_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_tmin_2_elv_w, file='tps_tmin_2_elv_w.rdata')

summary(tps_tmin_2_elv_w$Tmin_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-5.10114 -0.61900  0.09482 -0.03155  0.76074  1.51026 

tps_tmin_2_elv_w_cut<- tps_tmin_2_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)


summary(tps_tmin_2_elv_w_cut$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2.7286 -0.2038  0.4742  0.3409  0.9670  1.5103



#3ka Tmin
subset3<-gradient_env_NA_removed %>% filter(age ==3100)
subset3_2 <- subset3 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset3_3 <- subset3_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset3_3 <- subset3_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left

vec1 <- subset3_3$se_Tmin_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_3_elv_w <- subset3_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_tmin_3_elv_w, file='tps_tmin_3_elv_w.rdata')

summary(tps_tmin_3_elv_w$Tmin_anomaly)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-5.83178 -0.68166  0.07915  0.21380  1.42776  3.00216 

tps_tmin_3_elv_w_cut<- tps_tmin_3_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)


summary(tps_tmin_3_elv_w_cut$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-3.3446 -0.3638  0.5205  0.6130  1.6295  2.9761 


#4ka Tmin
subset4<-gradient_env_NA_removed %>% filter(age ==4100)
subset4_2 <- subset4 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset4_3 <- subset4_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset4_3 <- subset4_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left

vec1 <- subset4_3$se_Tmin_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_4_elv_w <- subset4_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_tmin_4_elv_w, file='tps_tmin_4_elv_w.rdata')

summary(tps_tmin_4_elv_w$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.8426 -0.5928  0.3477  0.3428  1.4086  3.6820 

tps_tmin_4_elv_w_cut<- tps_tmin_4_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)


summary(tps_tmin_4_elv_w_cut$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-3.6090 -0.1705  0.8925  0.8047  1.8173  3.6820 


#5ka Tmin
subset5<-gradient_env_NA_removed %>% filter(age ==5100)
subset5_2 <- subset5 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset5_3 <- subset5_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset5_3 <- subset5_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left

vec1 <- subset5_3$se_Tmin_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_5_elv_w <- subset5_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_tmin_5_elv_w, file='tps_tmin_5_elv_w.rdata')

summary(tps_tmin_5_elv_w$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-3.9726 -0.1719  0.4402  0.5754  1.2878  4.2928 

tps_tmin_5_elv_w_cut<- tps_tmin_5_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)


summary(tps_tmin_5_elv_w_cut$Tmin_anomaly)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-3.9726 -0.7653  0.3587  0.5280  1.8375  4.2928 

#6-6.2ka Tmin
subset6<-gradient_env_NA_removed %>% filter(age ==6100)
subset6_2 <- subset6 %>%
  dplyr::filter(!is.na(elv))#600 out of 601 obs left
subset6_3 <- subset6_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#272 out of 601 left


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
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_tmin_6_elv_w, file='tps_tmin_6_elv_w.rdata')

summary(tps_tmin_6_elv_w$Tmin_anomaly)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.7289 -1.4124 -0.6841 -0.4164  0.6162  3.7253 


tps_tmin_6_elv_w_cut<- tps_tmin_6_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_tmin_6_elv_w_cut$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.7289 -1.2145 -0.3480 -0.1113  1.0214  3.7253 



#7ka Tmin
subset7<-gradient_env_NA_removed %>% filter(age ==7100)
subset7_2 <- subset7 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset7_3 <- subset7_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset7_3 <- subset7_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left

vec1 <- subset7_3$se_Tmin_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_7_elv_w <- subset7_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_tmin_7_elv_w, file='tps_tmin_7_elv_w.rdata')

summary(tps_tmin_7_elv_w$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.2973 -2.5615 -1.5055 -1.5807 -0.5073  4.2795 

tps_tmin_7_elv_w_cut<- tps_tmin_7_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)


summary(tps_tmin_7_elv_w_cut$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-6.3593 -1.4883 -0.2272 -0.3508  1.0984  4.2795


#8ka Tmin
subset8<-gradient_env_NA_removed %>% filter(age ==8100)
subset8_2 <- subset8 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset8_3 <- subset8_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset8_3 <- subset8_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left

vec1 <- subset8_3$se_Tmin_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_8_elv_w <- subset8_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_tmin_8_elv_w, file='tps_tmin_8_elv_w.rdata')

summary(tps_tmin_8_elv_w$Tmin_anomaly)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.976  -4.666  -2.831  -2.954  -1.641   3.781 

tps_tmin_8_elv_w_cut<- tps_tmin_8_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)


summary(tps_tmin_8_elv_w_cut$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-7.7310 -2.2108 -1.3003 -1.0040  0.5215  3.7812 


#9ka Tmin
subset9<-gradient_env_NA_removed %>% filter(age ==9100)
subset9_2 <- subset9 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset9_3 <- subset9_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset9_3 <- subset9_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left

vec1 <- subset9_3$se_Tmin_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_9_elv_w <- subset9_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_tmin_9_elv_w, file='tps_tmin_9_elv_w.rdata')

summary(tps_tmin_9_elv_w$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-6.020  -4.961  -4.271  -3.383  -2.286   4.263 

tps_tmin_9_elv_w_cut<- tps_tmin_9_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)


summary(tps_tmin_9_elv_w_cut$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.8683 -2.9023 -1.4628 -1.2371 -0.1893  4.2630 


#10ka Tmin
subset10<-gradient_env_NA_removed %>% filter(age ==10100)
subset10_2 <- subset10 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset10_3 <- subset10_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset10_3 <- subset10_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left

vec1 <- subset10_3$se_Tmin_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_10_elv_w <- subset10_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_tmin_10_elv_w, file='tps_tmin_10_elv_w.rdata')

summary(tps_tmin_10_elv_w$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-11.593  -5.856  -4.880  -4.734  -3.942   2.086 

tps_tmin_10_elv_w_cut<- tps_tmin_10_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)


summary(tps_tmin_10_elv_w_cut$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-11.593  -6.626  -4.261  -4.209  -1.373   2.086 

wd=subset10_3  %>%dplyr::filter(site=="Avrig")
write.csv(wd,"weird site for 10ka Tmin.csv")


#11ka Tmin
subset11<-gradient_env_NA_removed %>% filter(age ==11100)
subset11_2 <- subset11 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset11_3 <- subset11_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset11_3 <- subset11_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left

vec1 <- subset11_3$se_Tmin_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_11_elv_w <- subset11_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_tmin_11_elv_w, file='tps_tmin_11_elv_w.rdata')

summary(tps_tmin_11_elv_w$Tmin_anomaly)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-15.336  -9.826  -6.789  -5.507  -1.398   9.045 

tps_tmin_11_elv_w_cut<- tps_tmin_11_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)


summary(tps_tmin_11_elv_w_cut$Tmin_anomaly)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-15.336  -9.662  -6.802  -6.803  -3.974   1.412 




#11.8-12ka Tmin
subset12<-gradient_env_NA_removed %>% filter(age ==11900)
subset12_2 <- subset12 %>%
  dplyr::filter(!is.na(elv))#713 out of 714 obs left
subset12_3 <- subset12_2 %>%
  dplyr::filter(!is.na(se_Tmin_anomaly))#486 out of 714 left
subset12_3 <- subset12_3 %>%
  dplyr::filter(!is.na(Tmin_anomaly))#486 out of 714 left

vec1 <- subset12_3$se_Tmin_anomaly
#vec1 <-1/vec1
vec2 <-1/(vec1*vec1)


#using se_Tmin_anomaly as weighting
tps_tmin_12_elv_w <- subset12_3 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    weights = vec2,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_tmin_12_elv_w, file='tps_tmin_12_elv_w.rdata')

summary(tps_tmin_12_elv_w$Tmin_anomaly)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-21.124 -11.658  -8.826  -8.143  -3.859   5.936 

tps_tmin_12_elv_w_cut<- tps_tmin_12_elv_w  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)


summary(tps_tmin_12_elv_w_cut$Tmin_anomaly)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-20.100 -12.677  -8.767  -8.786  -5.226   2.971 

