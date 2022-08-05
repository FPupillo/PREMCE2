#------------------------------------------------------------------------------#
# simulating trials switch points and character - Pearce Hall - with fixed alpha
# created: "Tue Oct 26 14:26:25 2021"
#------------------------------------------------------------------------------#

setwd(dirname(getwd()))

#soruce the functions

source("helper_functions/taskSim.R")
source("simulation_functions/simulate_PearceHall.R")
source(("helper_functions/softmax.R"))
source(("helper_functions/chooseMultinom.R"))
source(("helper_functions/BICcompute.R"))
source("likelihood_functions/lik_PearceHall.R")
source("fitting_functions/fit_PearceHall_fixalpha.R")
source("helper_functions/getCat.R")
source("helper_functions/preSim.R")
source("helper_functions/getcorrCat.R")

#------------------------------------------------------------------------------#
# parameters
#------------------------------------------------------------------------------#
Pcong<-0.75
Ntrial<-48
Ncharacter<-1
switchN<-3
categN<-4
#------------------------------------------------------------------------------#

set.seed(12345)

lengthparam<-10

sims<-100

seq<-seq(0,1, length.out =lengthparam)

alphaseq<-sample(seq, sims,replace = T)
betaseq<-rexp(sims,2/30)
gammaseq<-sample(seq, sims,replace = T)

data<-matrix(NA, nrow=1,ncol = 7)

df<-data.frame(data)

names(df)<-c("simAlpha", "fitAlpha","simBeta", "fitBeta", "simGamma", 
            "fitGamma",  "BIC")

model<-simulate_PearceHall

fit<-fit_PearceHall_fixalpha

modname<-as.character("PH_fixalpha")

name<- paste("output_files/parameterRecovery", modname, ".Ntrial=",Ntrial, 
             ".initialV=", 0.25 , sep="")

write.csv(df, paste0(name, ".csv"), row.names = F)

# initiate a counter
count<-1

# progress bar
pb<-txtProgressBar(min=0, max=sims, style =3)

for(simul in 1:sims){

 #for (p in 1:length(alphaseq)){
#   for (b in 1:length(betaseq)){

    # generate the task
    Data<-taskSim(Pcong, Ntrial, Ncharacter, switchN, categN)
    
    # prepare data
    Data$cuedCharacter<-Data$character
    
    for (n in 1:nrow(Data)){
      Data[n,1:4]<-paste0("stimuli/",Data[n,1:4], ".png")
    }
    
      # simulate data with the RW obs
      sim<-model(Data=Data,alpha=0.5, beta =betaseq[simul], 
                 k=1, gamma = gammaseq[simul],
                 initialV = 0.25)
      

    
    # calculate percentage optimal choice
    # rename variables
    sim$task_resp.keys<-sim$response
    sim$character<-sim$cuedCharacter
    
    # create response category
    categ<-as.character(Data[1,1:4])
    
    sim$respCat<-substr(categ[sim$response], 9, nchar(categ[sim$response])-4)
    
    # now fit the data
    est<-fit(data=sim, alphaBound = c(0,1),  betaBound = c(0,100), kBound =c(0,1), 
             gammaBound = c(0,1),
             initialV =0.25)
    
    estAlpha<-est$alphabetaPAR[1]
    
    estBeta<-est$alphabetaPAR[2]
    
    estGamma<-est$alphabetaPAR[3]
    
    
    temp<-read.csv( paste0(name, ".csv"))
    
    
    #append the data
    temp[nrow(temp)+1, ]<-c(alphaseq[simul], estAlpha, betaseq[simul], 
                            estBeta, gammaseq[simul],estGamma,
                     est$BIC)
    
    #write it
    write.csv(temp, paste0(name, ".csv"), row.names = F)
    
    count <-count+1
    
    setTxtProgressBar(pb, count) 
    
  }
  
