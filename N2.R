#install the N2 package
install.packages("analogue",dependencies = TRUE)
library(analogue)
#sample code
data(swapdiat)
sppN2 <- n2(swapdiat, "species")
head(sppN2)
sampN2 <- n2(swapdiat, "sites")
head(sampN2)


#my own code


sppN2 <- n2(raw_paleo_N2, "species")
sppN2
sampN2 <- n2(raw_paleo_N2, "sites")
sampN2
head(sampN2)


pN2<-n2(raw_paleo_N2, which = c("species", "sites"))

?n2




# NOT RUN {
install.packages("rioja")
require(rioja)
install.packages("palaeoSig")
library(palaeoSig)
data(RLGH)
Hill.N2.core(RLGH$spec)
# }

Hill.N2.core(raw_paleo_N2)
Hill.N2(raw_paleo_N2,margin = 1)


core_N2=core
core_N2[is.na(core_N2)]=0

Hill.N2(core_N2)
Hill.N2(core_N2,margin = 1)



Hill.N2(core,margin = 1)
sampN2 <- n2(core_N2, "sites")
sampN2
sampN2 <- n2(raw_paleo_N2, "sites")
sampN2



#raw paleo: 351 taxa -29
#extract data that only contains species, not other variables
taxaColMin <- which(colnames(raw_paleo) == "Abies")
taxaColMax <- which(colnames(raw_paleo) == "Zygophyllaceae")
raw_paleo_N2=raw_paleo[,taxaColMin:taxaColMax]

#change NA to 0 so that N2 calculation is possible
raw_paleo_N2[is.na(raw_paleo_N2)]=0

#method 1
sampN2_1<-Hill.N2(raw_paleo_N2,margin = 1)

#method 2
sampN2_2 <- n2(raw_paleo_N2, "sites")
sampN2_2 #sampN2_2 is exactly the same as sampN2_1

sampN2_1<-as.data.frame(sampN2_1)


raw_paleo_new<-merge(sampN2_1, raw_paleo)

raw_paleo$N2 <- sampN2_1

raw_paleo_new<-merge(x = sampN2_1, y = raw_paleo,  all.x = TRUE)


#raw_paleo_new %>% select(N2, everything())

require(dplyr)
raw_paleo <- raw_paleo %>% relocate(N2, .before = site_name)
#rm(raw_paleo_new)

raw_paleo_N2_2<-subset(raw_paleo, N2>2)
#73266 left from 76890
#0.9528677, 95% left



raw_paleo_N2_2_removed<-subset(raw_paleo, !N2>2)


#raw_paleo_N2_2_removed %>%
#  select(site_name)

rm(raw_paleo_N2_2)
rm(sampN2_2)

##setting N2=2, still lots of Tmin anomaly being 20
dd <- raw_paleo_N2_2  %>%      group_by(site_name) %>%filter(site_name=="Long Breach")
mean(dd$N2)

# dd <- raw_paleo_N2_2  %>%      group_by(site_name) %>%filter(site_name=="Litzelsee")
# mean(dd$N2)
# [1] 6.293235
# dd <- raw_paleo_N2_2  %>%      group_by(site_name) %>%filter(site_name=="Skomer")
# mean(dd$N2)
# [1] 2.819315
# dd <- raw_paleo_N2_2  %>%      group_by(site_name) %>%filter(site_name=="Long Breach")
# mean(dd$N2)
# [1] 4.090284

dd <- raw_paleo_N2_3  %>%      group_by(site_name) %>%filter(site_name=="Altes Moor")
mean(dd$N2)
# 6.485736

##set N2 boundary=5
raw_paleo_N2_5<-subset(raw_paleo, N2>5)
#38766 left from 76890
#0.5041748, 50% left


raw_paleo_N2_4<-subset(raw_paleo, N2>4)
#51647 left from 76890
#0.6716998, 67% left


raw_paleo_N2_3<-subset(raw_paleo, N2>3)
#63980 left from 76890
#0.8320978, 83% left


##try using 3 as the N2 boundary
raw_paleo_N2_3<-subset(raw_paleo, N2>3)



#checking the anomaly values, using N2 boundary as 3
min(gradient_env$Tmin_anomaly,na.rm = TRUE)
#-29.67161


dd <- gradient_env  %>%      group_by(site) %>%filter(abs(Tmin_anomaly)>5)
site_to_delete<-unique(dd$site)
total_site<-unique(gradient_env$site)
#178 sites need to be deleted from 1278 site to keep Tmin anomaly below 5
#345 sites need to be deleted from 1278 site to keep Tmin anomaly below 2
#502 sites need to be deleted from 1278 site to keep Tmin anomaly between -2 and 2
#368 sites need to be deleted from 1278 site to keep Tmin anomaly between -5 and 5


mean(dd$N2)



##try using 4 as the N2 boundary
raw_paleo_N2_3<-subset(raw_paleo, N2>4)

