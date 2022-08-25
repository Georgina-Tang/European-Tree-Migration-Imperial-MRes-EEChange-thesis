#  Georgina 18/07/2022

setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots")
load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM.RData")

#setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/CRU plots")
#load("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/CRU plots/GAM.RData")

save.image(file = "GAM.RData")


library(nlme)
library(mgcv)
library(reshape2)


################################################# training gams using 175 taxa
#0. Extract taxon from "we"

bookss=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/we_.csv")
taxon=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/we_.csv",header=FALSE,nrow=1)
#taxon_1=c("Fagus","Quercus.deciduous","Picea")
#taxon_1=as.character(c("Fagus","Quercus.deciduous","Picea"))

#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

for (i in c(21:195)){
  book = bookss[,c(4,5,6,i+9)]
  colnames(book)=c("rtmi","tmin","gdd","Taxon")
  taxon.name=as.character(taxon[1,i+9])
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


  write.csv(alpha_data_1_pts_gam,file = paste(taxon.name,".csv",sep=""), row.names=FALSE)
}



######################################### training gams using only 3 taxa


bookss=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/we_.csv")
taxon=read.csv("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/data/we_.csv",header=FALSE,nrow=1)


taxon_1=taxon[,c("V80","V138","V159")] 
bookss_1=bookss[,c("rtmi","Tmin","gdd","Fagus","Picea","Quercus.deciduous")] 
#have reassigned the F variable for MI alpha conversion
#Therefore, every F need to be False

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
  
  
  write.csv(alpha_data_1_pts_gam,file = paste(taxon.name,".csv",sep=""), row.names=FALSE)
}
