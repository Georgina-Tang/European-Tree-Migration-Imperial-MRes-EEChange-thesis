#special-epd Picea calculation

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/special epd comparison.RData")

save.image(file = "special epd comparison.RData")

library(dplyr)
#########################################3DTPS  calculation first

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/TPS calculation Picea new")
#1-1.2ka
special_epd_1<-paleo_compare_2 %>% filter(age ==1100)
#dd<-raw_paleo %>% filter(ID_SAMPLE == 4802)

special_epd_1_2 <- special_epd_1 %>%
  dplyr::filter(!is.na(elv))#1302 out of 1304 obs left
special_epd_1_3 <- special_epd_1_2 %>%
  dplyr::filter(!is.na(Picea_mean))#715 out of 1304 left


str(special_epd_1_3)



tps_special_epd_1_elv <- special_epd_1_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )

#lambda
save(tps_special_epd_1_elv, file='tps_special_epd_1_elv.rdata')


summary(special_epd_1_3$Picea_mean)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.002103 0.081292 0.090909 1.000000 

summary(tps_special_epd_1_elv$Picea_mean)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.38530  0.04736  0.17632  0.25253  0.36816  1.37191 



tps_special_epd_1_elv_cut<- tps_special_epd_1_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_1_elv_cut$Picea_mean)
#       Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.38530 -0.00624  0.05546  0.09453  0.20116  0.97303 


#2-2.2ka
special_epd_2<-paleo_compare_2 %>% filter(age ==2100)
special_epd_2_2 <- special_epd_2 %>%
  dplyr::filter(!is.na(elv))
special_epd_2_3 <- special_epd_2_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_2_3)



tps_special_epd_2_elv <- special_epd_2_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )

# tps_special_epd_2_elv_1 <- special_epd_2_2 %>%
#   smpds::tps(
#     var = "Picea_mean",
#     z_var = "elv",
#     z_mode = "independent",
#     z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
#     cpus = CPUS
#   )


save(tps_special_epd_2_elv, file='tps_special_epd_2_elv.rdata')


summary(special_epd_2_3$Picea_mean)
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000000 0.0000000 0.0009925 0.0704862 0.0658936 1.0000000 

summary(tps_special_epd_2_elv$Picea_mean)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.14312  0.03828  0.18698  0.26848  0.44867  1.17223 


tps_special_epd_2_elv_cut<- tps_special_epd_2_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_2_elv_cut$Picea_mean)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.143121 -0.008886  0.028212  0.084243  0.132744  0.546858


#3-3.2ka
library(dplyr)
special_epd_3<-paleo_compare_2 %>% filter(age ==3100)
special_epd_3_2 <- special_epd_3 %>%
  dplyr::filter(!is.na(elv))
special_epd_3_3 <- special_epd_3_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_3_3)



tps_special_epd_3_elv <- special_epd_3_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_3_elv, file='tps_special_epd_3_elv.rdata')


summary(special_epd_3_3$Picea_mean)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06484 0.05102 1.00000

summary(tps_special_epd_3_elv$Picea_mean)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.15092  0.01570  0.07101  0.19456  0.30020  1.18781



tps_special_epd_3_elv_cut<- tps_special_epd_3_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_3_elv_cut$Picea_mean)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.15092 -0.00554  0.03723  0.07919  0.14908  0.45214 


#4-4.2ka
special_epd_4<-paleo_compare_2 %>% filter(age ==4100)
special_epd_4_2 <- special_epd_4 %>%
  dplyr::filter(!is.na(elv))
special_epd_4_3 <- special_epd_4_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_4_3)



tps_special_epd_4_elv <- special_epd_4_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_4_elv, file='tps_special_epd_4_elv.rdata')


summary(special_epd_4_3$Picea_mean)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000000 0.0000000 0.0004237 0.0816370 0.0683540 0.8476454 

summary(tps_special_epd_4_elv$Picea_mean)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.13958  0.03203  0.13264  0.21187  0.33669  1.03879 



tps_special_epd_4_elv_cut<- tps_special_epd_4_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_4_elv_cut$Picea_mean)
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.139575 -0.005159  0.031005  0.094333  0.171478  0.565835 


#5-5.2ka
special_epd_5<-paleo_compare_2 %>% filter(age ==5100)
special_epd_5_2 <- special_epd_5 %>%
  dplyr::filter(!is.na(elv))
special_epd_5_3 <- special_epd_5_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_5_3)



tps_special_epd_5_elv <- special_epd_5_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_5_elv, file='tps_special_epd_5_elv.rdata')


summary(special_epd_5_3$Picea_mean)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.07685 0.04459 0.99123 

summary(tps_special_epd_5_elv$Picea_mean)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.20915  0.01047  0.10606  0.19872  0.31182  1.21398 



tps_special_epd_5_elv_cut<- tps_special_epd_5_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_5_elv_cut$Picea_mean)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.121624 -0.006841  0.018932  0.102429  0.131627  1.099698 

#6-6.2ka
special_epd_6<-paleo_compare_2 %>% filter(age ==6100)
special_epd_6_2 <- special_epd_6 %>%
  dplyr::filter(!is.na(elv))
special_epd_6_3 <- special_epd_6_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_6_3)



tps_special_epd_6_elv <- special_epd_6_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_6_elv, file='tps_special_epd_6_elv.rdata')


summary(special_epd_6_3$Picea_mean)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06128 0.02770 0.91262 

summary(tps_special_epd_6_elv$Picea_mean)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.343914  0.009358  0.121512  0.288334  0.444038  1.487980 



tps_special_epd_6_elv_cut<- tps_special_epd_6_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683456 left

summary(tps_special_epd_6_elv_cut$Picea_mean)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.107240 -0.005660  0.009184  0.078542  0.094323  0.799869 

#7-7.2ka
special_epd_7<-paleo_compare_2 %>% filter(age ==7100)
special_epd_7_2 <- special_epd_7 %>%
  dplyr::filter(!is.na(elv))
special_epd_7_3 <- special_epd_7_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_7_3)



tps_special_epd_7_elv <- special_epd_7_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_7_elv, file='tps_special_epd_7_elv.rdata')


summary(special_epd_7_3$Picea_mean)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06004 0.02157 0.91489  

summary(tps_special_epd_7_elv$Picea_mean)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.12019  0.01976  0.12448  0.15866  0.24564  0.74081 



tps_special_epd_7_elv_cut<- tps_special_epd_7_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683477 left

summary(tps_special_epd_7_elv_cut$Picea_mean)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.120190 -0.004125  0.010761  0.051364  0.077106  0.607978 

#8-8.2ka
special_epd_8<-paleo_compare_2 %>% filter(age ==8100)
special_epd_8_2 <- special_epd_8 %>%
  dplyr::filter(!is.na(elv))
special_epd_8_3 <- special_epd_8_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_8_3)



tps_special_epd_8_elv <- special_epd_8_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_8_elv, file='tps_special_epd_8_elv.rdata')


summary(special_epd_8_3$Picea_mean)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.060600 0.009154 0.916925 
summary(tps_special_epd_8_elv$Picea_mean)
#        Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.198615  0.007034  0.189959  0.276336  0.502390  0.962299 



tps_special_epd_8_elv_cut<- tps_special_epd_8_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683488 left

summary(tps_special_epd_8_elv_cut$Picea_mean)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.050751 -0.005114  0.002958  0.044656  0.045574  0.569809 


#9-9.2ka
special_epd_9<-paleo_compare_2 %>% filter(age ==9100)
special_epd_9_2 <- special_epd_9 %>%
  dplyr::filter(!is.na(elv))
special_epd_9_3 <- special_epd_9_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_9_3)



tps_special_epd_9_elv <- special_epd_9_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_9_elv, file='tps_special_epd_9_elv.rdata')


summary(special_epd_9_3$Picea_mean)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.039467 0.009501 0.760786

summary(tps_special_epd_9_elv$Picea_mean)
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.054663  0.009126  0.067092  0.121049  0.196803  0.609318



tps_special_epd_9_elv_cut<- tps_special_epd_9_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683499 left

summary(tps_special_epd_9_elv_cut$Picea_mean)
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.054663 -0.003379  0.006735  0.022424  0.026775  0.290055 

#10-10.2ka
special_epd_10<-paleo_compare_2 %>% filter(age ==10100)
special_epd_10_2 <- special_epd_10 %>%
  dplyr::filter(!is.na(elv))
special_epd_10_3 <- special_epd_10_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_10_3)



tps_special_epd_10_elv <- special_epd_10_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_special_epd_10_elv, file='tps_special_epd_10_elv.rdata')


summary(special_epd_10_3$Picea_mean)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.036143 0.006102 0.695652 

summary(tps_special_epd_10_elv$Picea_mean)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03821  0.05494  0.11755  0.14711  0.23346  0.44180 



tps_special_epd_10_elv_cut<- tps_special_epd_10_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 68341010 left

summary(tps_special_epd_10_elv_cut$Picea_mean)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.038215 -0.004255  0.007629  0.050776  0.100013  0.322075 

#11-11.2ka
special_epd_11<-paleo_compare_2 %>% filter(age ==11100)
special_epd_11_2 <- special_epd_11 %>%
  dplyr::filter(!is.na(elv))
special_epd_11_3 <- special_epd_11_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_11_3)



tps_special_epd_11_elv <- special_epd_11_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )




save(tps_special_epd_11_elv, file='tps_special_epd_11_elv.rdata')


summary(special_epd_11_3$Picea_mean)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.021944 0.005556 0.400000  

summary(tps_special_epd_11_elv$Picea_mean)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.05648  0.02372  0.12217  0.12930  0.19744  0.44576 



tps_special_epd_11_elv_cut<- tps_special_epd_11_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 68341111 left

summary(tps_special_epd_11_elv_cut$Picea_mean)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0564796 -0.0043038  0.0006934  0.0289763  0.0435258  0.3114077 

#12-12.2ka
special_epd_12<-paleo_compare_2 %>% filter(age ==11900)
special_epd_12_2 <- special_epd_12 %>%
  dplyr::filter(!is.na(elv))
special_epd_12_3 <- special_epd_12_2 %>%
  dplyr::filter(!is.na(Picea_mean))


str(special_epd_12_3)



tps_special_epd_12_elv <- special_epd_12_3 %>%
  smpds::tps(
    var = "Picea_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_12_elv, file='tps_special_epd_12_elv.rdata')


summary(special_epd_12_3$Picea_mean)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.017921 0.003514 0.585106  

summary(tps_special_epd_12_elv$Picea_mean)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.29529 -0.01248  0.02771  0.03481  0.11295  0.58564 



tps_special_epd_12_elv_cut<- tps_special_epd_12_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 68341212 left

summary(tps_special_epd_12_elv_cut$Picea_mean)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.101587 -0.004441  0.001801  0.031318  0.049640  0.585636 

