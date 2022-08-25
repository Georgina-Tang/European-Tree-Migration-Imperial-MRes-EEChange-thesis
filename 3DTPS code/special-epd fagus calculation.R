#special-epd Fagus calculation

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/special epd comparison.RData")

save.image(file = "special epd comparison.RData")

library(dplyr)


#########################################3DTPS  calculation first
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/TPS paleo calculation Fagus new")
#1-1.2ka
special_epd_1<-paleo_compare_2 %>% filter(age ==1100)
special_epd_1_2 <- special_epd_1 %>%
  dplyr::filter(!is.na(elv))#1302 out of 1304 obs left
special_epd_1_3 <- special_epd_1_2 %>%
  dplyr::filter(!is.na(Fagus_mean))#715 out of 1304 left


str(special_epd_1_3)



tps_special_epd_1_elv <- special_epd_1_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_1_elv, file='tps_special_epd_1_elv.rdata')


summary(special_epd_1_3$Fagus_mean)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.02787 0.10145 0.14691 0.83873

summary(tps_special_epd_1_elv$Fagus_mean)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.086101 -0.003897  0.002146  0.019676  0.016082  0.535286 



tps_special_epd_1_elv_cut<- tps_special_epd_1_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_1_elv_cut$Fagus_mean)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.071807 -0.001391  0.004683  0.068126  0.115093  0.535286 


#2-2.2ka
special_epd_2<-paleo_compare_2 %>% filter(age ==2100)
special_epd_2_2 <- special_epd_2 %>%
  dplyr::filter(!is.na(elv))
special_epd_2_3 <- special_epd_2_2 %>%
  dplyr::filter(!is.na(Fagus_mean))


str(special_epd_2_3)



tps_special_epd_2_elv <- special_epd_2_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_2_elv, file='tps_special_epd_2_elv.rdata')


summary(special_epd_2_3$Fagus_mean)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.01400 0.08791 0.13311 0.91473

summary(tps_special_epd_2_elv$Fagus_mean)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -0.072887 -0.003768  0.001026  0.018152  0.011452  0.511559



tps_special_epd_2_elv_cut<- tps_special_epd_2_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_2_elv_cut$Fagus_mean)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.050190 -0.001671  0.002178  0.056941  0.094352  0.511559 


#3-3.2ka
library(dplyr)
special_epd_3<-paleo_compare_2 %>% filter(age ==3100)
special_epd_3_2 <- special_epd_3 %>%
  dplyr::filter(!is.na(elv))
special_epd_3_3 <- special_epd_3_2 %>%
  dplyr::filter(!is.na(Fagus_mean))


str(special_epd_3_3)



tps_special_epd_3_elv <- special_epd_3_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_3_elv, file='tps_special_epd_3_elv.rdata')


summary(special_epd_3_3$Fagus_mean)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.006025 0.082447 0.103131 0.858624 

summary(tps_special_epd_3_elv$Fagus_mean)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.042626 -0.001697 -0.000170  0.016576  0.001254  0.570060 



tps_special_epd_3_elv_cut<- tps_special_epd_3_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_3_elv_cut$Fagus_mean)
#        Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0325930 -0.0003018  0.0008919  0.0600857  0.0724989  0.5700601 


#4-4.2ka
special_epd_4<-paleo_compare_2 %>% filter(age ==4100)
special_epd_4_2 <- special_epd_4 %>%
  dplyr::filter(!is.na(elv))
special_epd_4_3 <- special_epd_4_2 %>%
  dplyr::filter(!is.na(Fagus_mean))


str(special_epd_4_3)



tps_special_epd_4_elv <- special_epd_4_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_4_elv, file='tps_special_epd_4_elv.rdata')


summary(special_epd_4_3$Fagus_mean)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.000000 0.001961 0.057473 0.048197 1.000000 

summary(tps_special_epd_4_elv$Fagus_mean)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.0615509 -0.0023495 -0.0000326  0.0102644  0.0050773  0.3677068



tps_special_epd_4_elv_cut<- tps_special_epd_4_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_4_elv_cut$Fagus_mean)
#        Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0187543 -0.0003977  0.0011575  0.0368863  0.0491732  0.3677068 


#5-5.2ka
special_epd_5<-paleo_compare_2 %>% filter(age ==5100)
special_epd_5_2 <- special_epd_5 %>%
  dplyr::filter(!is.na(elv))
special_epd_5_3 <- special_epd_5_2 %>%
  dplyr::filter(!is.na(Fagus_mean))


str(special_epd_5_3)



tps_special_epd_5_elv <- special_epd_5_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_5_elv, file='tps_special_epd_5_elv.rdata')


summary(special_epd_5_3$Fagus_mean)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.03406 0.01674 0.67341 

summary(tps_special_epd_5_elv$Fagus_mean)
#        Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0397396 -0.0002854  0.0001267  0.0157742  0.0179377  0.3847547 



tps_special_epd_5_elv_cut<- tps_special_epd_5_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_5_elv_cut$Fagus_mean)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0397396 -0.0001530  0.0000898  0.0212748  0.0183656  0.3847547 

#6-6.2ka
special_epd_6<-paleo_compare_2 %>% filter(age ==6100)
special_epd_6_2 <- special_epd_6 %>%
  dplyr::filter(!is.na(elv))
special_epd_6_3 <- special_epd_6_2 %>%
  dplyr::filter(!is.na(Fagus_mean))


str(special_epd_6_3)



tps_special_epd_6_elv <- special_epd_6_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_6_elv, file='tps_special_epd_6_elv.rdata')


summary(special_epd_6_3$Fagus_mean)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.020532 0.005051 0.663424 

summary(tps_special_epd_6_elv$Fagus_mean)
#        Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0943354 -0.0040757 -0.0006268  0.0011115  0.0018176  0.1537773 



tps_special_epd_6_elv_cut<- tps_special_epd_6_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683456 left

summary(tps_special_epd_6_elv_cut$Fagus_mean)
#       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0264443 -0.0007377  0.0014326  0.0165762  0.0283432  0.1537773 

#7-7.2ka
special_epd_7<-paleo_compare_2 %>% filter(age ==7100)
special_epd_7_2 <- special_epd_7 %>%
  dplyr::filter(!is.na(elv))
special_epd_7_3 <- special_epd_7_2 %>%
  dplyr::filter(!is.na(Fagus_mean))


str(special_epd_7_3)



tps_special_epd_7_elv <- special_epd_7_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_7_elv, file='tps_special_epd_7_elv.rdata')


summary(special_epd_7_3$Fagus_mean)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.012937 0.002126 0.507109 

summary(tps_special_epd_7_elv$Fagus_mean)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-5.392e-02 -1.543e-03 -8.140e-06  1.056e-04  1.420e-03  2.280e-01 



tps_special_epd_7_elv_cut<- tps_special_epd_7_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683477 left

summary(tps_special_epd_7_elv_cut$Fagus_mean)
#       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-2.169e-02 -6.365e-04  1.353e-05  1.100e-02  7.544e-03  2.280e-01  

#8-8.2ka
special_epd_8<-paleo_compare_2 %>% filter(age ==8100)
special_epd_8_2 <- special_epd_8 %>%
  dplyr::filter(!is.na(elv))
special_epd_8_3 <- special_epd_8_2 %>%
  dplyr::filter(!is.na(Fagus_mean))


str(special_epd_8_3)



tps_special_epd_8_elv <- special_epd_8_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_8_elv, file='tps_special_epd_8_elv.rdata')


summary(special_epd_8_3$Fagus_mean)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.009584 0.000000 0.615385 

summary(tps_special_epd_8_elv$Fagus_mean)
#        Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.1430928 -0.0239185 -0.0000963 -0.0133656  0.0021616  0.5413055 



tps_special_epd_8_elv_cut<- tps_special_epd_8_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683488 left

summary(tps_special_epd_8_elv_cut$Fagus_mean)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.1373389 -0.0029583 -0.0000442  0.0130225  0.0012298  0.5413055


#9-9.2ka
special_epd_9<-paleo_compare_2 %>% filter(age ==9100)
special_epd_9_2 <- special_epd_9 %>%
  dplyr::filter(!is.na(elv))
special_epd_9_3 <- special_epd_9_2 %>%
  dplyr::filter(!is.na(Fagus_mean))


str(special_epd_9_3)



tps_special_epd_9_elv <- special_epd_9_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_9_elv, file='tps_special_epd_9_elv.rdata')


summary(special_epd_9_3$Fagus_mean)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.00569 0.00000 0.47561


summary(tps_special_epd_9_elv$Fagus_mean)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0675529 -0.0016251  0.0003391  0.0049489  0.0026734  0.4006151 



tps_special_epd_9_elv_cut<- tps_special_epd_9_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683499 left

summary(tps_special_epd_9_elv_cut$Fagus_mean)
#       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0675529 -0.0009376  0.0000617  0.0055146  0.0015340  0.4006151 

#10-10.2ka
special_epd_10<-paleo_compare_2 %>% filter(age ==10100)
special_epd_10_2 <- special_epd_10 %>%
  dplyr::filter(!is.na(elv))
special_epd_10_3 <- special_epd_10_2 %>%
  dplyr::filter(!is.na(Fagus_mean))


str(special_epd_10_3)



tps_special_epd_10_elv <- special_epd_10_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )

# Hello Georgina! I have seen this warning before, but I have always ignored it :sweat_smile: I believe it is not a problem directly with the code, but something regarding the distribution of observations, something like "there are areas with very few observations, treat the interpolated values with caution"; so, in my opinion, this is something that could be discussed on the manuscript/thesis.


save(tps_special_epd_10_elv, file='tps_special_epd_10_elv.rdata')


summary(special_epd_10_3$Fagus_mean)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.004257 0.000000 0.491041 

summary(tps_special_epd_10_elv$Fagus_mean)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.1039348 -0.0025291  0.0000626  0.0061501  0.0014589  0.4985054 



tps_special_epd_10_elv_cut<- tps_special_epd_10_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 68341010 left

summary(tps_special_epd_10_elv_cut$Fagus_mean)
#       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.1039348 -0.0005662  0.0000331  0.0054948  0.0016309  0.4985054 

#11-11.2ka
special_epd_11<-paleo_compare_2 %>% filter(age ==11100)
special_epd_11_2 <- special_epd_11 %>%
  dplyr::filter(!is.na(elv))
special_epd_11_3 <- special_epd_11_2 %>%
  dplyr::filter(!is.na(Fagus_mean))


str(special_epd_11_3)



tps_special_epd_11_elv <- special_epd_11_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


#same lambda issue

save(tps_special_epd_11_elv, file='tps_special_epd_11_elv.rdata')


summary(special_epd_11_3$Fagus_mean)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.003256 0.000000 0.406250 

summary(tps_special_epd_11_elv$Fagus_mean)
#        Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.1806864 -0.0044729 -0.0001285 -0.0024081  0.0014135  0.4135352 



tps_special_epd_11_elv_cut<- tps_special_epd_11_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 68341111 left

summary(tps_special_epd_11_elv_cut$Fagus_mean)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0394457 -0.0006867 -0.0000146  0.0150563  0.0030829  0.4135352 

#12-12.2ka
special_epd_12<-paleo_compare_2 %>% filter(age ==11900)
special_epd_12_2 <- special_epd_12 %>%
  dplyr::filter(!is.na(elv))
special_epd_12_3 <- special_epd_12_2 %>%
  dplyr::filter(!is.na(Fagus_mean))

# special_epd_12_3 <- special_epd_12_2 %>%
#   dplyr::filter(!is.na(Fagus_mean), Fagus_mean != 0)

str(special_epd_12_3)


tps_special_epd_12_elv <- special_epd_12_3 %>%
  smpds::tps(
    var = "Fagus_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )

#same lambda
save(tps_special_epd_12_elv, file='tps_special_epd_12_elv.rdata')


summary(special_epd_12_3$Fagus_mean)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.002041 0.000000 0.146386

summary(tps_special_epd_12_elv$Fagus_mean)
#        Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0159872 -0.0008369  0.0002999  0.0021719  0.0031486  0.0303373  



tps_special_epd_12_elv_cut<- tps_special_epd_12_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 68341212 left

summary(tps_special_epd_12_elv_cut$Fagus_mean)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0065215  0.0001551  0.0022127  0.0020138  0.0039391  0.0125964 

