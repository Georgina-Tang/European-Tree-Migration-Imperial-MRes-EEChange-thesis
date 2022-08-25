########################################################################################################
####################################  4DTPS - tps calculation only ##################################################
########################################################################################################

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/discrete gridded map/4DTPS with elevation new.RData")

save.image(file = "4DTPS with elevation new.RData")

#This R script is TPS calculation only to avoid technical issues in R when dealing with large datasets



#1-1.2ka
subset1_2 <- subset1 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_1 <- subset1_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_1, file='tps_tmin_1.rdata')
load('tps_tmin_1.rdata')

tps_tmax_1 <- subset1_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_1, file='tps_tmax_1.rdata')
load('tps_tmax_1.rdata')

tps_gdd_1 <- subset1_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_1, file='tps_gdd_1.rdata')
load('tps_gdd_1.rdata')

tps_alpha_1 <- subset1_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_1, file='tps_alpha_1.rdata')
load('tps_alpha_1.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_1_elv <- subset1_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_1_elv, file='tps_tmin_1_elv.rdata')

tps_tmax_1_elv <- subset1_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_1_elv, file='tps_tmax_1_elv.rdata')


tps_gdd_1_elv <- subset1_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_1_elv, file='tps_gdd_1_elv.rdata')


tps_alpha_1_elv <- subset1_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_1_elv, file='tps_alpha_1_elv.rdata')


#2-2.2ka
subset2_2 <- subset2 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_2 <- subset2_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_2, file='tps_tmin_2.rdata')
load('tps_tmin_2.rdata')

tps_tmax_2 <- subset2_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_2, file='tps_tmax_2.rdata')
load('tps_tmax_2.rdata')

tps_gdd_2 <- subset2_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_2, file='tps_gdd_2.rdata')
load('tps_gdd_2.rdata')

tps_alpha_2 <- subset2_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_2, file='tps_alpha_2.rdata')
load('tps_alpha_2.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_2_elv <- subset2_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_2_elv, file='tps_tmin_2_elv.rdata')

tps_tmax_2_elv <- subset2_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_2_elv, file='tps_tmax_2_elv.rdata')


tps_gdd_2_elv <- subset2_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_2_elv, file='tps_gdd_2_elv.rdata')


tps_alpha_2_elv <- subset2_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_2_elv, file='tps_alpha_2_elv.rdata')


#3-3.2ka
subset3_2 <- subset3 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_3 <- subset3_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_3, file='tps_tmin_3.rdata')
load('tps_tmin_3.rdata')

tps_tmax_3 <- subset3_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_3, file='tps_tmax_3.rdata')
load('tps_tmax_3.rdata')

tps_gdd_3 <- subset3_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_3, file='tps_gdd_3.rdata')
load('tps_gdd_3.rdata')

tps_alpha_3 <- subset3_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_3, file='tps_alpha_3.rdata')
load('tps_alpha_3.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_3_elv <- subset3_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_3_elv, file='tps_tmin_3_elv.rdata')

tps_tmax_3_elv <- subset3_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_3_elv, file='tps_tmax_3_elv.rdata')


tps_gdd_3_elv <- subset3_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_3_elv, file='tps_gdd_3_elv.rdata')


tps_alpha_3_elv <- subset3_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_3_elv, file='tps_alpha_3_elv.rdata')


#4-4.2ka
subset4_2 <- subset4 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_4 <- subset4_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_4, file='tps_tmin_4.rdata')
load('tps_tmin_4.rdata')

tps_tmax_4 <- subset4_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_4, file='tps_tmax_4.rdata')
load('tps_tmax_4.rdata')

tps_gdd_4 <- subset4_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_4, file='tps_gdd_4.rdata')
load('tps_gdd_4.rdata')

tps_alpha_4 <- subset4_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_4, file='tps_alpha_4.rdata')
load('tps_alpha_4.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_4_elv <- subset4_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_4_elv, file='tps_tmin_4_elv.rdata')

tps_tmax_4_elv <- subset4_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_4_elv, file='tps_tmax_4_elv.rdata')


tps_gdd_4_elv <- subset4_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_4_elv, file='tps_gdd_4_elv.rdata')


tps_alpha_4_elv <- subset4_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_4_elv, file='tps_alpha_4_elv.rdata')


#5-5.2ka
subset5_2 <- subset5 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_5 <- subset5_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_5, file='tps_tmin_5.rdata')
load('tps_tmin_5.rdata')

tps_tmax_5 <- subset5_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_5, file='tps_tmax_5.rdata')
load('tps_tmax_5.rdata')

tps_gdd_5 <- subset5_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_5, file='tps_gdd_5.rdata')
load('tps_gdd_5.rdata')

tps_alpha_5 <- subset5_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_5, file='tps_alpha_5.rdata')
load('tps_alpha_5.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_5_elv <- subset5_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_5_elv, file='tps_tmin_5_elv.rdata')

tps_tmax_5_elv <- subset5_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_5_elv, file='tps_tmax_5_elv.rdata')


tps_gdd_5_elv <- subset5_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_5_elv, file='tps_gdd_5_elv.rdata')


tps_alpha_5_elv <- subset5_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_5_elv, file='tps_alpha_5_elv.rdata')
#6-6.2ka
subset6_2 <- subset6 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_6 <- subset6_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_6, file='tps_tmin_6.rdata')
load('tps_tmin_6.rdata')

tps_tmax_6 <- subset6_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_6, file='tps_tmax_6.rdata')
load('tps_tmax_6.rdata')

tps_gdd_6 <- subset6_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_6, file='tps_gdd_6.rdata')
load('tps_gdd_6.rdata')

tps_alpha_6 <- subset6_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_6, file='tps_alpha_6.rdata')
load('tps_alpha_6.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_6_elv <- subset6_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_6_elv, file='tps_tmin_6_elv.rdata')

tps_tmax_6_elv <- subset6_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_6_elv, file='tps_tmax_6_elv.rdata')


tps_gdd_6_elv <- subset6_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_6_elv, file='tps_gdd_6_elv.rdata')


tps_alpha_6_elv <- subset6_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_6_elv, file='tps_alpha_6_elv.rdata')

#7-7.2ka
subset7_2 <- subset7 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_7 <- subset7_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_7, file='tps_tmin_7.rdata')
load('tps_tmin_7.rdata')

tps_tmax_7 <- subset7_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_7, file='tps_tmax_7.rdata')
load('tps_tmax_7.rdata')

tps_gdd_7 <- subset7_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_7, file='tps_gdd_7.rdata')
load('tps_gdd_7.rdata')

tps_alpha_7 <- subset7_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_7, file='tps_alpha_7.rdata')
load('tps_alpha_7.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_7_elv <- subset7_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_7_elv, file='tps_tmin_7_elv.rdata')

tps_tmax_7_elv <- subset7_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_7_elv, file='tps_tmax_7_elv.rdata')


tps_gdd_7_elv <- subset7_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_7_elv, file='tps_gdd_7_elv.rdata')


tps_alpha_7_elv <- subset7_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_7_elv, file='tps_alpha_7_elv.rdata')


#8-8.2ka
subset8_2 <- subset8 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_8 <- subset8_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_8, file='tps_tmin_8.rdata')
load('tps_tmin_8.rdata')

tps_tmax_8 <- subset8_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_8, file='tps_tmax_8.rdata')
load('tps_tmax_8.rdata')

tps_gdd_8 <- subset8_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_8, file='tps_gdd_8.rdata')
load('tps_gdd_8.rdata')

tps_alpha_8 <- subset8_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_8, file='tps_alpha_8.rdata')
load('tps_alpha_8.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_8_elv <- subset8_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_8_elv, file='tps_tmin_8_elv.rdata')

tps_tmax_8_elv <- subset8_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_8_elv, file='tps_tmax_8_elv.rdata')


tps_gdd_8_elv <- subset8_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_8_elv, file='tps_gdd_8_elv.rdata')


tps_alpha_8_elv <- subset8_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_8_elv, file='tps_alpha_8_elv.rdata')

#9-9.2ka
subset9_2 <- subset9 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_9 <- subset9_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_9, file='tps_tmin_9.rdata')
load('tps_tmin_9.rdata')

tps_tmax_9 <- subset9_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_9, file='tps_tmax_9.rdata')
load('tps_tmax_9.rdata')

tps_gdd_9 <- subset9_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_9, file='tps_gdd_9.rdata')
load('tps_gdd_9.rdata')

tps_alpha_9 <- subset9_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_9, file='tps_alpha_9.rdata')
load('tps_alpha_9.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_9_elv <- subset9_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_9_elv, file='tps_tmin_9_elv.rdata')

tps_tmax_9_elv <- subset9_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_9_elv, file='tps_tmax_9_elv.rdata')


tps_gdd_9_elv <- subset9_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_9_elv, file='tps_gdd_9_elv.rdata')


tps_alpha_9_elv <- subset9_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_9_elv, file='tps_alpha_9_elv.rdata')

#10-10.2ka
subset10_2 <- subset10 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_10 <- subset10_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_10, file='tps_tmin_10.rdata')
load('tps_tmin_10.rdata')

tps_tmax_10 <- subset10_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_10, file='tps_tmax_10.rdata')
load('tps_tmax_10.rdata')

tps_gdd_10 <- subset10_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_10, file='tps_gdd_10.rdata')
load('tps_gdd_10.rdata')

tps_alpha_10 <- subset10_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_10, file='tps_alpha_10.rdata')
load('tps_alpha_10.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_10_elv <- subset10_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_10_elv, file='tps_tmin_10_elv.rdata')

tps_tmax_10_elv <- subset10_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_10_elv, file='tps_tmax_10_elv.rdata')


tps_gdd_10_elv <- subset10_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_10_elv, file='tps_gdd_10_elv.rdata')


tps_alpha_10_elv <- subset10_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_10_elv, file='tps_alpha_10_elv.rdata')

#11-11.2ka
subset11_2 <- subset11 %>%
  dplyr::filter(!is.na(elv))


save(subset11, file='subset11.rdata')
## Without elevations ----
tps_tmin_11 <- subset11_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
#Warning: 
# Grid searches over lambda (nugget and sill variances) with  minima at the endpoints: 
#   (GCV) Generalized Cross-Validation 
# minimum at  right endpoint  lambda  =  0.0005464086 (eff. df= 108.3 )
save(tps_tmin_11, file='tps_tmin_11.rdata')
load('tps_tmin_11.rdata')

tps_tmax_11 <- subset11_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_11, file='tps_tmax_11.rdata')
load('tps_tmax_11.rdata')

tps_gdd_11 <- subset11_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_11, file='tps_gdd_11.rdata')
load('tps_gdd_11.rdata')

tps_alpha_11 <- subset11_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_11, file='tps_alpha_11.rdata')
load('tps_alpha_11.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_11_elv <- subset11_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_11_elv, file='tps_tmin_11_elv.rdata')
# Warning: 
#   Grid searches over lambda (nugget and sill variances) with  minima at the endpoints: 
#   (GCV) Generalized Cross-Validation 
# minimum at  right endpoint  lambda  =  0.0006128144 (eff. df= 108.3 )

tps_tmax_11_elv <- subset11_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_11_elv, file='tps_tmax_11_elv.rdata')


tps_gdd_11_elv <- subset11_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_11_elv, file='tps_gdd_11_elv.rdata')


tps_alpha_11_elv <- subset11_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_11_elv, file='tps_alpha_11_elv.rdata')

#11.8-12.0ka

subset12_2 <- subset12 %>%
  dplyr::filter(!is.na(elv))

## Without elevations ----
tps_tmin_12 <- subset12_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmin_12, file='tps_tmin_12.rdata')
load('tps_tmin_12.rdata')

tps_tmax_12 <- subset12_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_tmax_12, file='tps_tmax_12.rdata')
load('tps_tmax_12.rdata')

tps_gdd_12 <- subset12_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_gdd_12, file='tps_gdd_12.rdata')
load('tps_gdd_12.rdata')

tps_alpha_12 <- subset12_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
  )
save(tps_alpha_12, file='tps_alpha_12.rdata')
load('tps_alpha_12.rdata')

## With elevation ----
### Elevations as independent variable ----
tps_tmin_12_elv <- subset12_2 %>%
  smpds::tps(
    var = "Tmin_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmin_12_elv, file='tps_tmin_12_elv.rdata')

tps_tmax_12_elv <- subset12_2 %>%
  smpds::tps(
    var = "Tmax_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_tmax_12_elv, file='tps_tmax_12_elv.rdata')


tps_gdd_12_elv <- subset12_2 %>%
  smpds::tps(
    var = "gdd_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )
save(tps_gdd_12_elv, file='tps_gdd_12_elv.rdata')


tps_alpha_12_elv <- subset12_2 %>%
  smpds::tps(
    var = "alpha_anomaly",
    z_var = "elv",
    z_mode = "independent",
    z_ref = "C:/Users/X1 Carbon/Downloads/dem_geotiff/alwdgg.tif",
    cpus = CPUS
  )

save(tps_alpha_12_elv, file='tps_alpha_12_elv.rdata')




