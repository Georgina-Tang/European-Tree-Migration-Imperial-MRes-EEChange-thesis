#creating combined maps with shared legends

# Install and load patchwork package
# install.packages("patchwork")
# library("patchwork")
# p_total<-p1 + p2 + plot_layout(guides = "collect")
# p_total

install.packages("ggpubr")
library(ggpubr)

#######################alpha



setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with squared weights/alpha")

load("p1.rdata")
load("p2.rdata")
load("p3.rdata")
load("p4.rdata")
load("p5.rdata")
load("p6.rdata")
load("p7.rdata")
load("p8.rdata")
load("p9.rdata")
load("p10.rdata")
load("p11.rdata")
load("p12.rdata")




p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Alpha Anomaly map.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Alpha Anomaly map with anotations.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Alpha Anomaly map without coordinates.jpeg",p_totap,width=13.2,height=22,bg = "white")

####Tmin
p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Tmin Anomaly map.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Tmin Anomaly map with anotations.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Tmin Anomaly map without coordinates.jpeg",p_totap,width=13.2,height=22,bg = "white")


#GDD

p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total GDD Anomaly map.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total GDD Anomaly map with anotations.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total GDD Anomaly map without coordinates.jpeg",p_totap,width=13.2,height=22,bg = "white")



###########Tmax
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd plots/gridded map with squared weights/Tmax/Tmax")
save(p1, file='p1.rdata')
save(p2, file='p2.rdata')
save(p3, file='p3.rdata')
save(p4, file='p4.rdata')
save(p5, file='p5.rdata')
save(p6, file='p6.rdata')
save(p7, file='p7.rdata')
save(p8, file='p8.rdata')
save(p9, file='p9.rdata')
save(p10, file='p10.rdata')
save(p11, file='p11.rdata')
save(p12, file='p12.rdata')






p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Tmax Anomaly map.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Tmax Anomaly map with anotations.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Tmax Anomaly map without coordinates.jpeg",p_totap,width=13.2,height=22,bg = "white")




##########
#Fagus actual Map

#Fagus
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/TPS paleo map Fagus new")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Fagus Abundance map 2.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Fagus Abundance map without coordinates.jpeg",p_totap,width=13.2,height=22,bg = "white")



##Paleo actual map


setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/TPS paleo map Quercus new data")
save(p1, file='p1.rdata')
save(p2, file='p2.rdata')
save(p3, file='p3.rdata')
save(p4, file='p4.rdata')
save(p5, file='p5.rdata')
save(p6, file='p6.rdata')
save(p7, file='p7.rdata')
save(p8, file='p8.rdata')
save(p9, file='p9.rdata')
save(p10, file='p10.rdata')
save(p11, file='p11.rdata')
save(p12, file='p12.rdata')

load("p1.rdata")



#remove lat and long
p1<-p1+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p2<-p2+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p3<-p3+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p4<-p4+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p5<-p5+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p6<-p6+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p7<-p7+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p8<-p8+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p9<-p9+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p10<-p10+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p11<-p11+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))
p12<-p12+theme(axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(0,0,0,0), "cm"))

#annotate
p1<-p1+annotate("text", x=-2, y=65, label= "1 ka",col="black", size=10)
p2<-p2+annotate("text", x=-2, y=65, label= "2 ka",col="black", size=10)
p3<-p3+annotate("text", x=-2, y=65, label= "3 ka",col="black", size=10)
p4<-p4+annotate("text", x=-2, y=65, label= "4 ka",col="black", size=10)
p5<-p5+annotate("text", x=-2, y=65, label= "5 ka",col="black", size=10)
p6<-p6+annotate("text", x=-2, y=65, label= "6 ka",col="black", size=10)
p7<-p7+annotate("text", x=-2, y=65, label= "7 ka",col="black", size=10)
p8<-p8+annotate("text", x=-2, y=65, label= "8 ka",col="black", size=10)
p9<-p9+annotate("text", x=-2, y=65, label= "9 ka",col="black", size=10)
p10<-p10+annotate("text", x=-2, y=65, label= "10 ka",col="black", size=10)
p11<-p11+annotate("text", x=-2, y=65, label= "11 ka",col="black", size=10)
p12<-p12+annotate("text", x=-2, y=65, label= "12 ka",col="black", size=10)


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
  ggsave(file="total Quercus Abundance map.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Quercus Abundance map with anotations.jpeg",p_totap,width=13.2,height=22,bg = "white")



p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Quercus Abundance map without coordinates.jpeg",p_totap,width=13.2,height=22,bg = "white")




#Picea actual map
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/special epd/TPS paleo map Picea new")



p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Picea Abundance map.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Picea Abundance map with anotations.jpeg",p_totap,width=13.2,height=22,bg = "white")



p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total Picea Abundance map without coordinates.jpeg",p_totap,width=13.2,height=22,bg = "white")


#paleo GAM maps
#Quercus
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM paleo maps 2/Quercus")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total reconstructed Quercus Abundance map.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total reconstructed Quercus Abundance map with anotations.jpeg",p_totap,width=13.2,height=22,bg = "white")



p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total reconstructed Quercus Abundance map without coordinates.jpeg",p_totap,width=13.2,height=22,bg = "white")




#Picea
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM paleo maps 2/Picea")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total reconstructed Picea Abundance map.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total reconstructed Picea Abundance map without coordinates.jpeg",p_totap,width=13.2,height=22,bg = "white")

#Fagus
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/GAM paleo maps 2/Fagus")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total reconstructed Fagus Abundance map.jpeg",p_totap,width=13.95,height=22,bg = "white")


p_totap<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
ggsave(file="total reconstructed Fagus Abundance map without coordinates.jpeg",p_totap,width=13.2,height=22,bg = "white")


#site map
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/Figure list")
load("p_site_Special_epd.rdata")
load("p_site.rdata")
library(ggplot2)
library(ggpubr)
p_totap<-ggarrange(p_smpds,p_epd, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
ggsave(file="total site map.jpeg",p_totap,width=13.2,height=14,bg = "white")

p_smpds<-p_smpds+annotate("text", x=-6, y=75, label= "a",col="black", size=10)
p_epd<-p_epd+annotate("text", x=-6, y=75, label= "b",col="black", size=10)

p_totap<-ggarrange(p_smpds,p_epd, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
ggsave(file="total site map 2.jpeg",p_totap,width=13.2,height=14,bg = "white")

#ggsave(file="total site map2.jpeg",p_totap,width=8,height=10,bg = "white")






#modern comparison

#fagus
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/SMPDS modern calculations")
load("p0_f_CRU.rdata")
load("p0_f_smpds.rdata")


p_totap<-ggarrange(p0_f_CRU,p0_f_smpds, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave(file="total Fagus modern comparison map.jpeg",p_totap,width=13,height=10,bg = "white")

p0_f_CRU<-p0_f_CRU+annotate("text", x=-2, y=65, label= "a",col="black", size=10)
p0_f_smpds<-p0_f_smpds+annotate("text", x=-2, y=65, label= "b",col="black", size=10)

p_totap<-ggarrange(p0_f_CRU,p0_f_smpds, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave(file="total Fagus modern comparison map 2.jpeg",p_totap,width=13,height=10,bg = "white")




#picea
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/SMPDS modern calculations")
load("p0_p_CRU.rdata")
load("p0_p_smpds.rdata")


p_totap<-ggarrange(p0_p_CRU,p0_p_smpds, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave(file="total Picea modern comparison map.jpeg",p_totap,width=13,height=10,bg = "white")

p0_p_CRU<-p0_p_CRU+annotate("text", x=-2, y=65, label= "a",col="black", size=10)
p0_p_smpds<-p0_p_smpds+annotate("text", x=-2, y=65, label= "b",col="black", size=10)

p_totap<-ggarrange(p0_p_CRU,p0_p_smpds, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave(file="total Picea modern comparison map 2.jpeg",p_totap,width=13,height=10,bg = "white")




#Quercus
setwd("C:/Users/X1 Carbon/OneDrive - Imperial College London/Tree Migration- Colin/Output data/after new data 05-04/GAM plots/SMPDS modern calculations")
load("p0_q_CRU.rdata")
load("p0_q_smpds.rdata")


p_totap<-ggarrange(p0_q_CRU,p0_q_smpds, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave(file="total Quercus modern comparison map.jpeg",p_totap,width=13,height=10,bg = "white")

p0_q_CRU<-p0_q_CRU+annotate("text", x=-2, y=65, label= "a",col="black", size=10)
p0_q_smpds<-p0_q_smpds+annotate("text", x=-2, y=65, label= "b",col="black", size=10)

p_totap<-ggarrange(p0_q_CRU,p0_q_smpds, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave(file="total Quercus modern comparison map 2.jpeg",p_totap,width=13,height=10,bg = "white")










#libraries:

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
#function to extract the legend of a ggplot; source:
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
#arranging the legend and plots in a grid:

p2_legend <- get_legend(p2)

grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), 
                         p2 + theme(legend.position="none"), nrow=2), 
             p2_legend, 
             nrow=2,heights=c(10, 1))