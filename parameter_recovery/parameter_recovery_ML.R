#------------------------------------------------------------------------------#
# simulating trials switch points and character - maximum likelihood
# created: "Tue Oct 26 14:26:25 2021"

# arguments: 1 - simulated model name
#            2 - fitted model name
#------------------------------------------------------------------------------#
rm(list=ls())
#setwd(dirname(getwd()))
library(dplyr)
library(ggplot2)
library(gridExtra) # for plotting
library(ggpubr)
library(doParallel)
library(foreach)


  

#soruce the functions
source("helper_functions/taskSim.R")
source("simulation_functions/simulate_RescorlaWagner_simple.R")
source("simulation_functions/simulate_WSLS.R")
source("simulation_functions/simulate_PearceHall.R")
source("simulation_functions/simulate_HMM.R")

source("likelihood_functions/lik_RescorlaWagner_simple.R")
source("likelihood_functions/lik_WSLS.R")
source("likelihood_functions/lik_PearceHall.R")
source("likelihood_functions/lik_HMM.R")

source("fitting_functions/fit_RescorlaWagner_simple.R")
source("fitting_functions/fit_WSLS.R")
source("fitting_functions/fit_PearceHall.R")
source("fitting_functions/fit_HMM.R")

source("helper_functions/update_RW.R")
source(("helper_functions/softmax.R"))
source(("helper_functions/chooseMultinom.R"))
source(("helper_functions/BICcompute.R"))
source("helper_functions/getCat.R")
source("helper_functions/preSim.R")
source("helper_functions/getcorrCat.R")

#------------------------------------------------------------------------------#
# parameters
#------------------------------------------------------------------------------#
Pcong<-0.75
Ntrial<-24
Ncharacter<-2
switchN<-3
categN<-4
#------------------------------------------------------------------------------#
sims<-30

alphaseq<-seq(0,1, length.out =sims)

mean_alpha<-mean(alphaseq)
sd_alpha<-sd(alphaseq)

betaseq<-seq(1,10, length.out=sims)

mean_beta<-mean(betaseq)
sd_beta<-sd(betaseq)

gammaseq<-seq(0,1, length.out=sims)

mean_gamma<-mean(gammaseq)
sd_gamma<-sd(gammaseq)

kseq<-seq(0,1, length.out=sims)

mean_k<-mean(kseq)
sd_k<-sd(kseq)

cseq<-seq(0,1, length.out=sims)

mean_c<-mean(cseq)
sd_c<-sd(cseq)

# shuffle
alphaseq<-sample(alphaseq)
betaseq<-sample(betaseq)
gammaseq<-sample(gammaseq)
kseq<-sample(kseq)
cseq<-sample(cseq)
#

# boundaries
alphaBound<-c(0,1)
betaBound<-c(0,10)
kBound<-c(0,1)
gammaBound<-c(0,1)
cBound<-c(0,1)


#------------------------------------------------------------------------------#
# get the model
Args<-commandArgs(trailingOnly = T)

Args<-c("simulate_PH", "fit_PearceHall")

simmodel<-get(Args[1])

print(paste0("simulating with model ", Args[1]))

# get the model used to fit the data
fitmodelname<- Args[2]

# fitmodel
fitmodel<-paste0(fitmodelname, ".R")

# get the fit function
fit<-get(fitmodelname)

# initiate a counter
count<-1

# progress bar
pb<-txtProgressBar(min=0, max=sims, style =3)

# export all the functions
functions<-lsf.str()

# detect cores for runnning in parallel
cores=detectCores()

cl <- makeCluster(cores[1]-floor(cores/3), outfile="") # to not overload your computer

registerDoParallel(cl)

# loop through several simuolations
dat<-foreach (nsim=1:sims,.export=c(functions), .packages= c("dplyr"),  .combine = rbind )  %dopar% {
#for (nsim in 1:sims){
  # for (a in 1:length(alphaseq)){
  #   for (b in 1:length(betaseq)){
  
  # generate the task
  Dat<-taskSim(Pcong, Ntrial, Ncharacter, switchN, categN)
  
  # prepare data
  Dat$cuedCharacter<-Dat$character
  
  for (n in 1:nrow(Dat)){
    Dat[n,1:4]<-paste0("stimuli/",Dat[n,1:4], ".png")
  }
  
  # simulate data with the RW obs
  sim<-simmodel(Data=Dat,alpha=alphaseq[nsim], beta =betaseq[nsim], kseq[nsim],
                gammaseq[nsim], cseq[nsim],
             initialV = 0.25)
  
  # delete unnecessary columns
  #sim<-subset(sim, select = -c(V1, V2, V3, V4, P1, P2, P3, P4, accuracy))
  
  # rename variables
  sim$task_resp.keys<-sim$response
  sim$character<-sim$cuedCharacter
  
  # create response category
  #categ<-as.character(Data[1,1:4])
  
  #sim$respCat<-substr(categ[sim$response], 9, nchar(categ[sim$response])-4)
  categ<-levels(as.factor(sim$obj_category))
  
  for (t in 1:nrow(sim)){
  sim$response[t]<-which(unlist(sim[t,c("left_categ","centleft_categ" , 
                                           "centright_categ","right_categ") ])== 
                          paste0("stimuli/", categ[sim$response[t]], ".png"))
  }
  
  # add the participant
  sim$participant<-nsim
  
  sim$Delta<-unlist(sim$Delta)
  
  # append
  #Data_all<-rbind(Data_all, sim)
  
  # fit the model
  fitdata<-fit(data = sim, alphaBound = alphaBound, betaBound = betaBound, 
               kBound = kBound, gammaBound = gammaBound, cBound = cBound, 
               initialV = 0.25)
  
  for (param in c("alpha", "beta", "k", "gamma", "c")){
  if (!is.null(fitdata[[param]])){
  assign(paste0("fit",param),  fitdata[[param]])
  } else{
    assign(paste0("fit",param), NA)
  }
  }
  
  print(paste("completed sim ", nsim))
  
  all_data<- c(alphaseq[nsim], fitalpha, betaseq[nsim], fitbeta, kseq[nsim], fitk,
            gammaseq[nsim], fitgamma, cseq[nsim], fitc)
  
  all_data
}

stopCluster(cl)

# print
#write.csv(Data_all, "output_files/simulated.data.RW.simple.csv", row.names = F)

#------------------------------------------------------------------------------#
#Data_all<-read.csv( "output_files/simulated.data.RW.simple.csv", row.names = F)

parameter_recov<-data.frame(dat)
names(parameter_recov)<-c("alphaseq", "fitalpha", "betaseq", "fitbeta", "kseq", "fitk", 
                   "gammaseq", "fitgamma")

# plot depending on the model
if (fitmodelname ==  "WSLS" ){
  plot_trace_excl_warm_up <- stan_trace(fit_rl, pars = c('beta'), inc_warmup = F)
  plot_dens <- stan_plot(fit_rl, pars=c('beta'), show_density=T, fill_color = 'skyblue')
  
  # get the fitted data
  fitbeta<-get_posterior_mean(dat$model, pars= c("beta"))[, nChains+1]
  
  # create the dataframe
  parameter_recov<-as.data.frame(cbind(betaseq, fitbeta))
  
  plotbeta<-ggplot(parameter_recov, aes(x=betaseq, y=fitbeta)) + 
    geom_point()+
    geom_smooth(method=lm)+
    theme_classic()+
    stat_cor(method="pearson")+
    
    #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
    ggtitle("Beta parameter")
  
  g<-grid.arrange( plotbeta, ncol=1)
  
  arrangeGrob(  plotbeta,ncol=1)
  
  # save
  ggsave(file=paste0("figures/ParameterRecovery.",
                     fitmodelname, ".jpg"), g)
  
  save( list=ls(),file=paste0("output_files/param_rec_", fitmodelname, ".Rdata"))
} else if (fitmodelname  ==  "RescorlaWagner_simple" ){
  plot_trace_excl_warm_up <- stan_trace(fit_rl, pars = c('alpha', 'beta'), inc_warmup = F)
  plot_dens <- stan_plot(fit_rl, pars=c('alpha', 'beta'), show_density=T, fill_color = 'skyblue')
  
  # get the fitted data
  fitalpha<-get_posterior_mean(dat$model, pars= c("alpha"))[, nChains+1]
  fitbeta<-get_posterior_mean(dat$model, pars= c("beta"))[, nChains+1]
  
  # create the dataframe
  parameter_recov<-as.data.frame(cbind(alphaseq, fitalpha, betaseq, fitbeta))
  
  plotalpha<-ggplot(parameter_recov, aes(x=alphaseq, y=fitalpha)) + 
    geom_point()+
    geom_smooth(method=lm)+
    theme_classic()+
    stat_cor(method="pearson")+
    xlim(0,1)+
    ylim(0,1)+
    #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
    ggtitle("Alpha parameter")
  
  plotbeta<-ggplot(parameter_recov, aes(x=betaseq, y=fitbeta)) + 
    geom_point()+
    geom_smooth(method=lm)+
    theme_classic()+
    stat_cor(method="pearson")+
    
    #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
    ggtitle("Beta parameter")
  
  g<-grid.arrange( plotbeta, ncol=2)
  
  arrangeGrob(  plotbeta,ncol=2)
  
  # save
  ggsave(file=paste0("figures/ParameterRecovery.",
                     fitmodelname, ".jpg"), g)
  
  save( list=ls(),file=paste0("output_files/param_rec_", fitmodelname, ".Rdata"))
} else if (fitmodelname == "fit_PearceHall"){

  
  plotalpha<-ggplot(parameter_recov, aes(x=alphaseq, y=fitalpha)) + 
    geom_point()+
    geom_smooth(method=lm)+
    theme_classic()+
    stat_cor(method="pearson")+
    xlim(0,1)+
    ylim(0,1)+
    #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
    ggtitle("Alpha parameter")
  
  plotbeta<-ggplot(parameter_recov, aes(x=betaseq, y=fitbeta)) + 
    geom_point()+
    geom_smooth(method=lm)+
    theme_classic()+
    stat_cor(method="pearson")+
    
    #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
    ggtitle("Beta parameter")
  
  plotk<-ggplot(parameter_recov, aes(x=kseq, y=fitk)) + 
    geom_point()+
    geom_smooth(method=lm)+
    theme_classic()+
    stat_cor(method="pearson")+
    
    #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
    ggtitle("K parameter")
  
  
  plotgamma<-ggplot(parameter_recov, aes(x=gammaseq, y=fitgamma)) + 
    geom_point()+
    geom_smooth(method=lm)+
    theme_classic()+
    stat_cor(method="pearson")+
    
    #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
    ggtitle("Gamma parameter")
  
  g<-grid.arrange(plotalpha, plotbeta, plotk,  plotgamma, ncol=2)
  
  arrangeGrob(  plotalpha, plotbeta, plotk,  plotgamma, ncol=2)
  
  # save
  ggsave(file=paste0("figures/ParameterRecovery.ML.",
                     fitmodelname, ".jpg"), g)
  
  save( list=ls(),file=paste0("output_files/param_rec_", fitmodelname, ".Rdata"))
}else if (fitmodelname == 'HMM_repar'){
  plot_trace_excl_warm_up <- stan_trace(fit_rl, pars = c('c', 'gamma'), inc_warmup = F)
  plot_dens <- stan_plot(fit_rl, pars=c('c', 'gamma'), show_density=T, fill_color = 'skyblue')
  
  # get the fitted data
  fitc<-get_posterior_mean(dat$model, pars= c("c"))[, nChains+1]
  fitgamma<-get_posterior_mean(dat$model, pars= c("gamma"))[, nChains+1]
  
  # create the dataframe
  parameter_recov<-as.data.frame(cbind(cseq, fitc, gammaseq, fitgamma))
  
  plotc<-ggplot(parameter_recov, aes(x=cseq, y=fitc)) + 
    geom_point()+
    geom_smooth(method=lm)+
    theme_classic()+
    stat_cor(method="pearson")+
    xlim(0,1)+
    ylim(0,1)+
    #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
    ggtitle("C parameter")
  
  plotgamma<-ggplot(parameter_recov, aes(x=gammaseq, y=fitgamma)) + 
    geom_point()+
    geom_smooth(method=lm)+
    theme_classic()+
    stat_cor(method="pearson")+
    
    #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
    ggtitle("Gamma parameter")
  
  g<-grid.arrange( plotc, plotgamma, ncol=2)
  
  arrangeGrob(  plotbeta,ncol=2)
  
  # save
  ggsave(file=paste0("figures/ParameterRecovery.",
                     fitmodelname, ".jpg"), g)
  
  save( list=ls(),file=paste0("output_files/param_rec_", fitmodelname, ".Rdata"))
}

# plot_trace_excl_warm_up <- stan_trace(fit_rl, pars = c('beta'), inc_warmup = F)
# plot_dens <- stan_plot(fit_rl, pars=c('beta'), show_density=T, fill_color = 'skyblue')
# 
# # get the fitted data
# fitbeta<-get_posterior_mean(dat$model, pars= c("beta"))[, nChains+1]
# 
# # create the dataframe
# parameter_recov<-as.data.frame(cbind(alphaseq, fitalpha, betaseq, fitbeta))
# 
# 
# 
# #-----------------------------------------------------------------------------#
# # plot simulated vs fitted
# #-----------------------------------------------------------------------------#
# plotalpha<-ggplot(parameter_recov, aes(x=alphaseq, y=fitalpha)) + 
#   geom_point()+
#   geom_smooth(method=lm)+
#   theme_classic()+
#   stat_cor(method="pearson")+
#   xlim(0,1)+
#   ylim(0,1)+
#   #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
#   ggtitle("Alpha parameter")
# 
# 
# plotbeta<-ggplot(parameter_recov, aes(x=betaseq, y=fitbeta)) + 
#   geom_point()+
#  geom_smooth(method=lm)+
#  theme_classic()+
#   stat_cor(method="pearson")+
# 
#   #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
#   ggtitle("Beta parameter")
# 
# g<-grid.arrange(plotalpha, plotbeta, ncol=2)
# 
# arrangeGrob( plotalpha, plotbeta,ncol=1)
# 
# # save
# ggsave(file=paste0("figures/ParameterRecovery.",
#                    fitmodelname, ".jpg"), g)
# 
# save( list=ls(),file=paste0("output_files/param_rec_", fitmodelname, ".Rdata"))
# 
# # extract quantities
# rstan::extract(fit_rl, pars="chosenV")
# 
# rstan::extract(fit_rl, pars="unchosenV")
# 
# 
# as.data.frame(summary(fit_rl, pars = "pe")$summary)$mean
# 
# as.data.frame(summary(fit_rl, pars = "log_lik")$summary)$mean
# 
