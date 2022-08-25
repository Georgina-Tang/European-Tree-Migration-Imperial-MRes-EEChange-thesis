#special-epd Quercus calculation

#special-epd Quercus.deciduous calculation

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/special epd comparison.RData")
#load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/special epd comparison Quercus new.RData")


save.image(file = "special epd comparison.RData")
#save.image(file = "special epd comparison Quercus new.RData")

library(dplyr)
#########################################3DTPS  calculation first
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd")
#1-1.2ka
special_epd_1<-paleo_compare_2 %>% filter(age ==1100)
#dd<-raw_paleo %>% filter(ID_SAMPLE == 4802)

special_epd_1_2 <- special_epd_1 %>%
  dplyr::filter(!is.na(elv))#1302 out of 1304 obs left
special_epd_1_3 <- special_epd_1_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))#715 out of 1304 left

#,Quercus.deciduous_mean != 0
str(special_epd_1_3)



tps_special_epd_1_elv <- special_epd_1_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )

#lambda
save(tps_special_epd_1_elv, file='tps_special_epd_1_elv.rdata')


summary(special_epd_1_3$Quercus.deciduous_mean)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000000 0.000000 0.000000 0.007126 0.000000 0.971154 

summary(tps_special_epd_1_elv$Quercus.deciduous_mean)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.487529 -0.001710  0.004018  0.035517  0.025283  2.717082 



tps_special_epd_1_elv_cut<- tps_special_epd_1_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_1_elv_cut$Quercus.deciduous_mean)
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.487529 -0.006199 -0.000022  0.005205  0.002587  2.717082 


#2-2.2ka
special_epd_2<-paleo_compare_2 %>% filter(age ==2100)
special_epd_2_2 <- special_epd_2 %>%
  dplyr::filter(!is.na(elv))
special_epd_2_3 <- special_epd_2_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_2_3)



tps_special_epd_2_elv <- special_epd_2_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )
#lambda
# tps_special_epd_2_elv_1 <- special_epd_2_2 %>%
#   smpds::tps(
#     var = "Quercus.deciduous_mean",
#     z_var = "elv",
#     z_mode = "independent",
#     z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
#     cpus = CPUS
#   )


save(tps_special_epd_2_elv, file='tps_special_epd_2_elv.rdata')


summary(special_epd_2_3$Quercus.deciduous_mean)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06712 0.00000 1.00000 

summary(tps_special_epd_2_elv$Quercus.deciduous_mean)
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-2.413200 -0.050304 -0.002732  0.031131  0.020475  3.973000 

tps_special_epd_2_elv_cut<- tps_special_epd_2_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_2_elv_cut$Quercus.deciduous_mean)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-2.413200 -0.006862  0.007185  0.248123  0.239711  3.973000 


#3-3.2ka
library(dplyr)
special_epd_3<-paleo_compare_2 %>% filter(age ==3100)
special_epd_3_2 <- special_epd_3 %>%
  dplyr::filter(!is.na(elv))
special_epd_3_3 <- special_epd_3_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_3_3)



tps_special_epd_3_elv <- special_epd_3_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_3_elv, file='tps_special_epd_3_elv.rdata')


summary(special_epd_3_3$Quercus.deciduous_mean)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06217 0.00000 1.00000 

summary(tps_special_epd_3_elv$Quercus.deciduous_mean)
#  Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.4933178 -0.0150437 -0.0005576  0.0299583  0.0339128  1.1721872 


tps_special_epd_3_elv_cut<- tps_special_epd_3_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_3_elv_cut$Quercus.deciduous_mean)
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.355475 -0.002114  0.007629  0.130308  0.218918  1.172187 


#4-4.2ka
special_epd_4<-paleo_compare_2 %>% filter(age ==4100)
special_epd_4_2 <- special_epd_4 %>%
  dplyr::filter(!is.na(elv))
special_epd_4_3 <- special_epd_4_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_4_3)



tps_special_epd_4_elv <- special_epd_4_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_4_elv, file='tps_special_epd_4_elv.rdata')


summary(special_epd_4_3$Quercus.deciduous_mean)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06931 0.00000 1.00000 

summary(tps_special_epd_4_elv$Quercus.deciduous_mean)
#   Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.3848754 -0.0197556 -0.0004437  0.0222736  0.0086211  0.9081778 



tps_special_epd_4_elv_cut<- tps_special_epd_4_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_4_elv_cut$Quercus.deciduous_mean)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.3062680 -0.0008854  0.0033004  0.1114286  0.1776551  0.9077490 


#5-5.2ka
special_epd_5<-paleo_compare_2 %>% filter(age ==5100)
special_epd_5_2 <- special_epd_5 %>%
  dplyr::filter(!is.na(elv))
special_epd_5_3 <- special_epd_5_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_5_3)



tps_special_epd_5_elv <- special_epd_5_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_5_elv, file='tps_special_epd_5_elv.rdata')


summary(special_epd_5_3$Quercus.deciduous_mean)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06423 0.00000 0.93636 

summary(tps_special_epd_5_elv$Quercus.deciduous_mean)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.671868 -0.032758 -0.000062  0.020052  0.006692  3.257660 



tps_special_epd_5_elv_cut<- tps_special_epd_5_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683455 left

summary(tps_special_epd_5_elv_cut$Quercus.deciduous_mean)
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.671868 -0.000533  0.000038  0.051271  0.077229  3.257660 

#6-6.2ka
special_epd_6<-paleo_compare_2 %>% filter(age ==6100)
special_epd_6_2 <- special_epd_6 %>%
  dplyr::filter(!is.na(elv))
special_epd_6_3 <- special_epd_6_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_6_3)



tps_special_epd_6_elv <- special_epd_6_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_6_elv, file='tps_special_epd_6_elv.rdata')


summary(special_epd_6_3$Quercus.deciduous_mean)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06875 0.00000 0.99762 

summary(tps_special_epd_6_elv$Quercus.deciduous_mean)
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.574664 -0.020656 -0.001573  0.017088  0.008440  1.142177 



tps_special_epd_6_elv_cut<- tps_special_epd_6_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683456 left

summary(tps_special_epd_6_elv_cut$Quercus.deciduous_mean)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.341804 -0.003152  0.002749  0.096025  0.135363  1.142177 

#7-7.2ka
special_epd_7<-paleo_compare_2 %>% filter(age ==7100)
special_epd_7_2 <- special_epd_7 %>%
  dplyr::filter(!is.na(elv))
special_epd_7_3 <- special_epd_7_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_7_3)



tps_special_epd_7_elv <- special_epd_7_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_7_elv, file='tps_special_epd_7_elv.rdata')


summary(special_epd_7_3$Quercus.deciduous_mean)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06716 0.00000 0.93578  

summary(tps_special_epd_7_elv$Quercus.deciduous_mean)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.527710 -0.011684  0.001268  0.058112  0.117653  0.734937 



tps_special_epd_7_elv_cut<- tps_special_epd_7_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683477 left

summary(tps_special_epd_7_elv_cut$Quercus.deciduous_mean)
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.130134 -0.000936  0.011062  0.101663  0.168140  0.734937 

#8-8.2ka
special_epd_8<-paleo_compare_2 %>% filter(age ==8100)
special_epd_8_2 <- special_epd_8 %>%
  dplyr::filter(!is.na(elv))
special_epd_8_3 <- special_epd_8_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_8_3)



tps_special_epd_8_elv <- special_epd_8_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_8_elv, file='tps_special_epd_8_elv.rdata')


summary(special_epd_8_3$Quercus.deciduous_mean)
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06308 0.00000 0.85271
summary(tps_special_epd_8_elv$Quercus.deciduous_mean)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.147220 -0.011442  0.003298  0.147112  0.241439  1.077614 



tps_special_epd_8_elv_cut<- tps_special_epd_8_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683488 left

summary(tps_special_epd_8_elv_cut$Quercus.deciduous_mean)
#        Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.1051514 -0.0006432  0.0080139  0.0953874  0.1516075  0.7806476 


#9-9.2ka
special_epd_9<-paleo_compare_2 %>% filter(age ==9100)
special_epd_9_2 <- special_epd_9 %>%
  dplyr::filter(!is.na(elv))
special_epd_9_3 <- special_epd_9_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_9_3)



tps_special_epd_9_elv <- special_epd_9_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_9_elv, file='tps_special_epd_9_elv.rdata')


summary(special_epd_9_3$Quercus.deciduous_mean)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.06504 0.00000 0.90873 

summary(tps_special_epd_9_elv$Quercus.deciduous_mean)
#    Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-1.0612204 -0.0217148  0.0000135  0.0637383  0.1007175  1.0504817 



tps_special_epd_9_elv_cut<- tps_special_epd_9_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 683499 left

summary(tps_special_epd_9_elv_cut$Quercus.deciduous_mean)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.061220 -0.007202  0.003495  0.062331  0.099760  1.050482 

#10-10.2ka
special_epd_10<-paleo_compare_2 %>% filter(age ==10100)
special_epd_10_2 <- special_epd_10 %>%
  dplyr::filter(!is.na(elv))
special_epd_10_3 <- special_epd_10_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_10_3)



tps_special_epd_10_elv <- special_epd_10_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )



save(tps_special_epd_10_elv, file='tps_special_epd_10_elv.rdata')


summary(special_epd_10_3$Quercus.deciduous_mean)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.05388 0.00000 0.92357 

summary(tps_special_epd_10_elv$Quercus.deciduous_mean)
#  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.305480 -0.007798  0.007808  0.090473  0.141580  0.943295  



tps_special_epd_10_elv_cut<- tps_special_epd_10_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 68341010 left

summary(tps_special_epd_10_elv_cut$Quercus.deciduous_mean)
#   Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.3054799 -0.0005532  0.0120823  0.0909401  0.1088901  0.9432950 

#11-11.2ka
special_epd_11<-paleo_compare_2 %>% filter(age ==11100)
special_epd_11_2 <- special_epd_11 %>%
  dplyr::filter(!is.na(elv))
special_epd_11_3 <- special_epd_11_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_11_3)



tps_special_epd_11_elv <- special_epd_11_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )




save(tps_special_epd_11_elv, file='tps_special_epd_11_elv.rdata')


summary(special_epd_11_3$Quercus.deciduous_mean)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.05067 0.00000 0.83333  

summary(tps_special_epd_11_elv$Quercus.deciduous_mean)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.5063641 -0.0133509  0.0006403  0.0656620  0.0104613  1.6525648 



tps_special_epd_11_elv_cut<- tps_special_epd_11_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 68341111 left

summary(tps_special_epd_11_elv_cut$Quercus.deciduous_mean)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.5063641 -0.0002838  0.0012362  0.0532593  0.0300762  1.2931863 

#12-12.2ka
special_epd_12<-paleo_compare_2 %>% filter(age ==11900)
special_epd_12_2 <- special_epd_12 %>%
  dplyr::filter(!is.na(elv))
special_epd_12_3 <- special_epd_12_2 %>%
  dplyr::filter(!is.na(Quercus.deciduous_mean))


str(special_epd_12_3)



tps_special_epd_12_elv <- special_epd_12_3 %>%
  smpds::tps(
    var = "Quercus.deciduous_mean",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = 4,
    grid_boundary = c(-12, 31, 45, 73)
  )


save(tps_special_epd_12_elv, file='tps_special_epd_12_elv.rdata')


summary(special_epd_12_3$Quercus.deciduous_mean)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.04171 0.00000 0.87500 

summary(tps_special_epd_12_elv$Quercus.deciduous_mean)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.5277435 -0.0108765  0.0009007  0.0136394  0.0319521  0.7958480 


tps_special_epd_12_elv_cut<- tps_special_epd_12_elv  %>%dplyr::filter(longitude>-12&longitude<45&latitude<73)
#190994 out of 68341212 left

summary(tps_special_epd_12_elv_cut$Quercus.deciduous_mean)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.1405652  0.0004105  0.0117534  0.0784938  0.0931853  0.7958480 

