#GAM for the 12 time period
#Georgina 21/07/2022

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM.RData")


save.image(file = "GAM.RData")

###################################################################

library(nlme)
library(mgcv)
library(reshape2)
#before running all the GAMs, check if the gam data input file is 7 columns
#after running the GAMs for each time period, do not rerun it again using the same gam input file,
#instead, use the load function to load the original gam input file, as the name for input and output is shared
#load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/input data cleaning/alpha_data_1_pts_gam.rdata")

bookss=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/we_.csv")
taxon=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/we_.csv",header=FALSE,nrow=1)


taxon_1=taxon[,c("V80","V138","V159")] 
bookss_1=bookss[,c("rtmi","Tmin","gdd","Fagus","Picea","Quercus.deciduous")] 

#modern time using CRU
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/input data cleaning")

load("alpha_data_1_pts_gam.rdata")




for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= modern_raster_gam,type="response"),nrow=nrow( modern_raster_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  modern_raster_gam$'taxon abundance'=abundance 
  
  
  write.csv(modern_raster_gam,file = paste(taxon.name," modern CRU.csv",sep=""), row.names=FALSE)
}





#1-1.2ka
#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM output data")

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  #taxon_1=c("Fagus","Quercus.deciduous","Picea")
  # taxon.name=taxon_1
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  
  
  #2. Prepare for contour plot          
  # Create a sequence of incrementally increasing
  # (by 0.02/0.2/1000 units) values for mi, tmin and gdd
  # xgrid<-seq(0,2.4,0.03)
  # ygrid<-seq(-43,15,0.6)   
  # zgrid<-seq(300,8200,100)
  # 
  # Generate a dataframe with every possible combination of rtmi and tmin
  #data.fit<-data.frame(expand.grid(rtmi=xgrid,tmin=ygrid,gdd=zgrid))   
  
  
  # Feed the dataframe into the gam model 
  # and receive a matrix output with estimates of
  # acceleration for each combination of rtmi and tmin
  #surface<-matrix(predict(data.gam,newdata=data.fit,type="response"),nrow=length(xgrid)) 
  # surface<-matrix(predict(data.gam,newdata= alpha_data_1_pts_NA,type="response"),nrow=length( alpha_data_1_pts_NA)) 
  
  #prepare input data
  
  surface<-matrix(predict(data.gam,newdata= alpha_data_1_pts_gam,type="response"),nrow=nrow( alpha_data_1_pts_gam)) 
  
  
  #rownames(surface)<-xgrid
  #colnames(surface)<-rep(ygrid,80) #80=zgrid
  # Abbreviated display of final matrix
  #surface[1:4,1:4]
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  #计算结果可以用一个神奇的function：inv.logit(x)
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  #colnames(data.fit)=c("sqrt(MI)","MTCO","GDD0")
  # colnames(data.fit)=c( "x",  "y","MI",  "gdd", "ID" ,  "elv",  "Tmin","gdd0", "Tmax", "alpha","alpha_anomaly","MI_anomaly", "paleo_MI") 
  #data.fit$'taxon abundance'=abundance #此处必须要分开合并，因为合并后会统一成number格式，然后无法更改科学计数法 
  alpha_data_1_pts_gam$'taxon abundance'=abundance #此处必须要分开合并，因为合并后会统一成number格式，然后无法更改科学计数法 
  
  # taxon$V80="Fagus"
  # taxon$V138="Picea"
  # taxon$V159="Quercus.deciduous"
  #taxon_1=taxon[,c("V80","V138","V159")] 
  
 
  write.csv(alpha_data_1_pts_gam,file = paste(taxon.name," 1-1.2ka.csv",sep=""), row.names=FALSE)
}


#2-2.2ka


#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])

  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_2_pts_gam,type="response"),nrow=nrow( alpha_data_2_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_2_pts_gam$'taxon abundance'=abundance 
   
  
  write.csv(alpha_data_2_pts_gam,file = paste(taxon.name," 2-2.2ka.csv",sep=""), row.names=FALSE)
}


#3-3.2ka

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_3_pts_gam,type="response"),nrow=nrow( alpha_data_3_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_3_pts_gam$'taxon abundance'=abundance 
  
  
  write.csv(alpha_data_3_pts_gam,file = paste(taxon.name," 3-3.2ka.csv",sep=""), row.names=FALSE)
}


#4-4.2ka

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_4_pts_gam,type="response"),nrow=nrow( alpha_data_4_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_4_pts_gam$'taxon abundance'=abundance 
  
  
  write.csv(alpha_data_4_pts_gam,file = paste(taxon.name," 4-4.2ka.csv",sep=""), row.names=FALSE)
}

#5-5.2ka

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_5_pts_gam,type="response"),nrow=nrow( alpha_data_5_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_5_pts_gam$'taxon abundance'=abundance 
  
  
  write.csv(alpha_data_5_pts_gam,file = paste(taxon.name," 5-5.2ka.csv",sep=""), row.names=FALSE)
}

#6-6.2ka

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_6_pts_gam,type="response"),nrow=nrow( alpha_data_6_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_6_pts_gam$'taxon abundance'=abundance 
  
  
  write.csv(alpha_data_6_pts_gam,file = paste(taxon.name," 6-6.2ka.csv",sep=""), row.names=FALSE)
}


#7-7.2ka

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_7_pts_gam,type="response"),nrow=nrow( alpha_data_7_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_7_pts_gam$'taxon abundance'=abundance 
  
  
  write.csv(alpha_data_7_pts_gam,file = paste(taxon.name," 7-7.2ka.csv",sep=""), row.names=FALSE)
}


#8-8.2ka

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_8_pts_gam,type="response"),nrow=nrow( alpha_data_8_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_8_pts_gam$'taxon abundance'=abundance 
  
  
  write.csv(alpha_data_8_pts_gam,file = paste(taxon.name," 8-8.2ka.csv",sep=""), row.names=FALSE)
}

#9-9.2ka

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_9_pts_gam,type="response"),nrow=nrow( alpha_data_9_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_9_pts_gam$'taxon abundance'=abundance 
  
  
  write.csv(alpha_data_9_pts_gam,file = paste(taxon.name," 9-9.2ka.csv",sep=""), row.names=FALSE)
}

#10-10.2ka

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_10_pts_gam,type="response"),nrow=nrow( alpha_data_10_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_10_pts_gam$'taxon abundance'=abundance 
  
  
  write.csv(alpha_data_10_pts_gam,file = paste(taxon.name," 10-10.2ka.csv",sep=""), row.names=FALSE)
}

#11-11.2ka

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_11_pts_gam,type="response"),nrow=nrow( alpha_data_11_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_11_pts_gam$'taxon abundance'=abundance 
  
  
  write.csv(alpha_data_11_pts_gam,file = paste(taxon.name," 11-11.2ka.csv",sep=""), row.names=FALSE)
}

#12-12.2ka

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(1:3)){
  book = bookss_1[,c(1,2,3,i+3)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon_1[1,i])
  
  #1.GAM
  data.gam<-gam(Taxon~s(rtmi)+s(tmin)+s(gdd),data=book,method="REML",
                family=binomial(link="logit"))
  
  #2. Prepare for contour plot          
  surface<-matrix(predict(data.gam,newdata= alpha_data_12_pts_gam,type="response"),nrow=nrow( alpha_data_12_pts_gam)) 
  
  #3. Preparing the Data for Contour Plots in GGPlots
  #3a.Transform data to long form
  mtrx.melt<-melt(surface) 
  abundance<-as.character(format(as.numeric(mtrx.melt[,3]),scientific=TRUE,digits=10))
  
  alpha_data_12_pts_gam$'taxon abundance'=abundance 
  
  
  write.csv(alpha_data_12_pts_gam,file = paste(taxon.name," 12-12.2ka.csv",sep=""), row.names=FALSE)
}


