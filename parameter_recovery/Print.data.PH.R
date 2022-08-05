#------------------------------------------------------------------------------#
# Print graph for parameter recovery for Pearce Hall Model - Instructive - fixed alpha

# Cerated: ""Tue Feb  8 12:00:57 2022""
#------------------------------------------------------------------------------#

rm(list=ls())

library(ggplot2)
library(gridExtra) # for plotting
library(ggpubr)

betalim<-10

model<-"PH"
Ntrial<-48
name<- paste("output_files/parameterRecovery", model, ".", "Ntrial=",  Ntrial,  
             ".initialV=", 0.25 , sep="")

# retrieve the file
parameterRecov<-read.csv(paste0(name, ".csv"))

#parameterRecov<-parameterRecov[parameterRecov$fitBeta<20,]
plotalpha<-ggplot(parameterRecov, aes(x=simAlpha, y=fitAlpha)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("Alpha parameter")

plotbeta<-ggplot(parameterRecov, aes(x=simBeta, y=fitBeta)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("Beta parameter")

plotgamma<-ggplot(parameterRecov, aes(x=simGamma, y=fitGamma)) +
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("Gamma parameter")
#   
g<-grid.arrange(plotalpha, plotbeta, plotgamma, ncol=3)

arrangeGrob(plotalpha, plotbeta,ncol=2)

# save
ggsave(file=paste0("figures/ParameterRecovery_", model, "betalimit=", betalim,  ".jpg"), g)
       