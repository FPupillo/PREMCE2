lik_PearceHall<-function (Data,alpha_0, beta, k, gamma,print,  initialV){
  #----------------------------------------------------------------------------#
  # This function computes the likelihood of the participants'
  # choices conditional on the Rescorla Wagner model = only value of the choice
  # is updated in this model
  #
  # Input
  #   Data: a long dataset where each row represents a trial. 
  #   alpha_0: initial associability parameter 
  #   beta:  beta parameter
  #   k : weight of associability
  #   gamma : weight between associability and PE
  #   print: 1: return only the negative log-likelihood; 
  #          2: return all data
  #   initialV: value of the inital V
  #
  # Output:
  #   Negative Log Likelihood (if print = 1)
  #   All the values (if print = 2)
  #----------------------------------------------------------------------------#
  
  # convert the object category into character variables
  categ<-levels(as.factor(Data$obj_category))
  
  murks<-levels(as.factor(Data$cuedCharacter))
  
  Data$cuedCharacter<-as.character(Data$cuedCharacter)

  # Initialize variables: Qs, the expected values
  Data$V1<-NA; Data$V2<-NA; Data$V3<-NA ; Data$V4<-NA 

  # Ps (probabilities for each category's choice)
  Data$P1<-NA; Data$P2<-NA; Data$P3<-NA ; Data$P4<-NA
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # Delta, prediction error
  Data$Delta<-NA
  
  # participants' response categor
  Data$respCat<-NA
  
  # accuracy
  Data$accuracy2<-NA
  
  # alpha is initialized ad the initial learning rate
  alpha<-alpha_0
  
  # learning rate
  Data$alpha<-NA
  
  # index variables for Q, P, and Delta
  Vindex<-c("V1", "V2", "V3", "V4")
  Pindex<-c("P1", "P2", "P3", "P4") 

  # Counter for indicating which character has to be updated
  count<-rep(0, 2)

  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  count2<-1
  
  # loop over trials
  for (t in 1: nrow(Data) ){
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # The following loop retrieves the Q values of the butterfly that corresponds to the current trial (time t).
    if (count[Murkcounter]==0){
      V<-rep(initialV, 4) # if it is the first time that butterfly is shown, the Qs are at their initial value
      } else{
      V<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Vindex] # if it is not the first time that butterfly is shown, retrieve the Qs of the last trial of that butterfly
      # retrieve the learning rate
      # alpha<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],"alpha"] 
      }
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    p<-softmax(V, beta)
    
    # compute Q, delta, and choice probability for actual choice, only if a choice is computed
    if (Data$response[t]!=0 & !is.na(Data$response[t]) ) {
      
      # which category was the response?
      # extract the order of the categories at trial t
      Data$respCat[t]<-as.character(unlist(Data[t,c("left_categ","centleft_categ" , 
                                                    "centright_categ","right_categ") ][Data$response[t]]))
      # substring
      Data$respCat[t]<-substr(Data$respCat[t], 9, nchar(Data$respCat[t])-4)
      
      #which cat is the corr ans?
      corr_resp<-Data$obj_category[t]
      
      # get accuracy
      if (Data$respCat[t]==corr_resp){
        Data$accuracy2[t]<-1
      }else{
        Data$accuracy2[t]<-0
      }

      # get the observation as 1 if that category is present, and 0 if it is not
      if (Data$accuracy2[t]==1){
        r<-1 
      } else {
        r<-0
      }
      
      # we now need to map the category into our
      # predefined order
      respCounter<-which(categ == Data$respCat[t])
      
      # That is the category to updated
      
      # probability only for the response made by participant
      prob[count2]<-unlist(p[respCounter])
      
      # assign it to the dataset
      Data$Prob[t]<- prob[count2]
      
     # prediction error
      delta <- r - V[respCounter]
      
      delta<-unlist(delta)
      
      # update Q
      V[respCounter]<-V[respCounter]+k*alpha*delta
      
      # update alpha
      alpha<- gamma * abs(delta) + (1-gamma) * alpha
      
      # assign delta to the dataset
      Data$Delta[t]<-delta
    
      # That is the category to updated
      
      # update the counter 
      count2<-count2+1
    }

  # assign values to the dataset
  Data[t, Vindex]<-V
  Data[t, Pindex]<-p
  Data$alpha[t]<-unlist(alpha)      
  
}
# we could take the probability only for the congruent trials, but for now we are taking all the probabilities

NegLL<--sum(log(Data$Prob), na.rm=T)

if (print ==1){
  return(NegLL)
} else if(print==2){
  return(Data)}
}

