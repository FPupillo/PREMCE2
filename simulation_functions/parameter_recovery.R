#------------------------------------------------------------------------------#
# simulating trials switch points and character - RW_IC
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

#soruce the functions
source("helper_functions/taskSim.R")
source("simulation_functions/simulate_RescorlaWagner_simple.R")
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
lengthparam<-30

sims<-30

alphaseq<-seq(0,1, length.out =lengthparam)

mean_alpha<-mean(alphaseq)
sd_alpha<-sd(alphaseq)

betaseq<-seq(1,10, length.out=lengthparam)

mean_beta<-mean(betaseq)
sd_beta<-sd(betaseq)

# shuffle
alphaseq<-sample(alphaseq)
betaseq<-sample(betaseq)

#
data<-matrix(NA, nrow=1,ncol = 5)

df<-data.frame(data)

#------------------------------------------------------------------------------#
# get the model
Args<-commandArgs(trailingOnly = T)

simmodel<-get(Args[1])

print(paste0("simulating with model ", Args[1]))

# get the model used to fit the data
fitmodelname<- Args[2]

# fitmodel
fitmodel<-paste0("stan/", fitmodelname, ".stan")

# initiate a counter
count<-1

# progress bar
pb<-txtProgressBar(min=0, max=(lengthparam)*sims, style =3)

Data_all<-vector()
for (nsim in 1:sims){
  # for (a in 1:length(alphaseq)){
  #   for (b in 1:length(betaseq)){
  
  # generate the task
  Data<-taskSim(Pcong, Ntrial, Ncharacter, switchN, categN)
  
  # prepare data
  Data$cuedCharacter<-Data$character
  
  for (n in 1:nrow(Data)){
    Data[n,1:4]<-paste0("stimuli/",Data[n,1:4], ".png")
  }
  
  # simulate data with the RW obs
  sim<-simmodel(Data=Data,alpha=alphaseq[nsim], beta =betaseq[nsim],
             initialV = 0.25)
  
  # calculate percentage optimal choice
  # rename variables
  sim$task_resp.keys<-sim$response
  sim$character<-sim$cuedCharacter
  
  # create response category
  categ<-as.character(Data[1,1:4])
  
  sim$respCat<-substr(categ[sim$response], 9, nchar(categ[sim$response])-4)
  
  # add the participant
  sim$participant<-nsim
  
  sim$Delta<-unlist(sim$Delta)
  # append
  Data_all<-rbind(Data_all, sim)
  
  print(paste("completed sim ", nsim))
}

# print
#write.csv(Data_all, "output_files/simulated.data.RW.simple.csv", row.names = F)

#------------------------------------------------------------------------------#
#Data_all<-read.csv( "output_files/simulated.data.RW.simple.csv", row.names = F)

# generate an empty container
partAll<-array(NA, dim = c(nsim,Ntrial*4*2, 3))

for (s in 1:nsim){
  
  subs<-Data_all[Data_all$participant==s, ]
  
  choice<-subs$response
  accuracy<-subs$accuracy
  character<-ifelse(subs$character=="stimuli/m5.png", 1, 2 )
  
  # bind them
  df<-cbind(choice, accuracy, character)
  
  # subject
  #sim$subject<-rep(s, nrow(sim))
  
  # assign the choice to the third dimension
  partAll[s,,]<-df
  
}
#------------------------------------------------------------------------------#

run_mp <- function(multiSubj = FALSE) {
  
  # clear workspace
  library(rstan)
  library(ggplot2)
  library(R.matlab)
  
  # locate the script
  modelFile<-fitmodel
  
  print(paste0("fitting ", modelFile))
  
  # how many dimensions?
  sz <- dim(partAll)
  
  nSubjects <- sz[1]
  nTrials   <- sz[2]
  
  
  dataList <- list(nSubjects=nSubjects,
                   nTrials=nTrials, 
                   choice=partAll[,,1], 
                   accuracy=partAll[,,2],
                   character=partAll[,,3]
  )
  
  #------------------------------------------------------------------------------#
  # Running Rstan
  #------------------------------------------------------------------------------#
  rstan_options(auto_write = TRUE)
  options(mc.cores = 4)
  
  nIter     <- 2000
  nChains   <- 4 
  nWarmup   <- floor(nIter/2)
  nThin     <- 1 # warmup
  
  cat("Estimating", modelFile, "model... \n")
  startTime = Sys.time(); print(startTime)
  cat("Calling", nChains, "simulations in Stan... \n")
  
  
  fit_rl <- stan(modelFile, 
                 data    = dataList, 
                 chains  = nChains,
                 iter    = nIter,
                 warmup  = nWarmup,
                 thin    = nThin,
                 init    = "random",
                 seed    = 1450154626)
  
  cat("Finishing", modelFile, "model simulation... \n")
  endTime = Sys.time(); print(endTime)
  cat("It took", as.character.Date(endTime-startTime), "\n")
  
  # =============================================================================
  #### Model Summary and Diagnostics #### 
  # =============================================================================
  print(fit_rl)
  
  plot_trace_excl_warm_up <- stan_trace(fit_rl, pars = c('alpha','beta'), inc_warmup = F)
  plot_dens <- stan_plot(fit_rl, pars=c('alpha','beta'), show_density=T, fill_color = 'skyblue')
  
  return(list ("model" = fit_rl, 
               "trace_plots" = plot_trace_excl_warm_up, 
               "dens_plots"  = plot_dens,
               "true_params" = as.data.frame(cbind(alpha, beta))))
  
}

dat<-run_mp()
save( dat,file="param_rec_RWsimple.Rdata")

# get the fitted data
fitalpha<-get_posterior_mean(dat$model, pars= c("alpha"))[, nChains+1]
fitbeta<-get_posterior_mean(dat$model, pars= c("beta"))[, nChains+1]

# create the dataframe
parameter_recov<-as.data.frame(cbind(alphaseq, fitalpha, betaseq, fitbeta))



#-----------------------------------------------------------------------------#
# plot simulated vs fitted
#-----------------------------------------------------------------------------#
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

g<-grid.arrange(plotalpha, plotbeta, ncol=2)

arrangeGrob( plotalpha, plotbeta,ncol=1)

# save
ggsave(file=paste0("figures/ParameterRecovery.",
                   fitmodelname, ".jpg"), g)
