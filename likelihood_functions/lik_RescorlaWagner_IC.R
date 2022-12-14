lik_RescorlaWagner_IC<-function (Data,alpha, beta,print,  initialV){
  #----------------------------------------------------------------------------#
  # This function computes the likelihood of the participants'
  # choices conditional on instructive-counterfactual model
  #
  # Input
  #   Data: a long dataset where each row represents a trial. 
  #   alpha: a candidate alpha parameter 
  #   beta: a candidate beta parameter
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
  
  #1  "Electronic device & accessory"  2  "Hand labour tool & accessory" 
  # 3 "Kitchen & utensil"     4  "Outdoor activity & sport item"
  #Data$catNum<-as.numeric((Data$respCat))
  
  ##Data$corrCat<-as.numeric((Data$corrCat))
  
  
  for (n in 1:4){
    
    # Initialize variables: Qs, the expected values
    Data[[paste("Q", n, sep="")]]<-NA
    
    # Ps (probabilities for each category's choice)
    Data[[paste("V", n, sep="")]]<-NA
    
    # Deltas
    Data[[paste("Delta", n, sep="")]]<-NA
    
  }
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # Delta, prediction error
  Data$Delta<-NA
  
  # get the response of the participant
  #Data$response<-NA
  
  # index variables for Q, P, and Delta
  Vindex<-c("V1", "V2", "V3", "V4")
  Pindex<-c("P1", "P2", "P3", "P4") 
  Deltaindex<-c("Delta1", "Delta2", "Delta3", "Delta4")
  
  # uncertainty, using probabilities
  Data$entropy_preP<-NA
  
  # uncertainty, using V
  Data$entropy_preV<-NA
  
  # entropy pre
  Data$surpriseP<-NA
  Data$surpriseV<-NA
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  count2<-1
  
  # loop over trials
  for (t in 1: nrow(Data) ){
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # The following loop retrieves the Q values of the butterfly 
    # that corresponds to the current trial (time t).
    if (count[Murkcounter]==0){
      V<-rep(initialV, 4) # if it is the first time
      # that butterfly is shown, the Vs are at their initial value
    } else{
      V<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],
                                                          Vindex] # if it is
      # not the first time that butterfly is shown, retrieve the Vs of the last 
      # trial of that butterfly
    }
    
    #Data[t, Qindex]<-Q
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    p<-softmax(V, beta)
    
    
    # compute Q, delta, and choice probability for actual choice, only if a choice is computed
    if (Data$response[t]!=0 & !is.na(Data$response[t]) ) {
      
      # placeholder for the category selected by part
      respCounter<-which(categ == as.character(Data$respCat[t]))
      
      # That is the category to updated
      
      # probability only for the response made by participant
      prob[count2]<-unlist(p[respCounter])
      
      # assign it to the dataset
      Data$Prob[t]<- prob[count2]
      
      # update the counter 
      count2<-count2+1
    }
    
    # get the observation as 1 if that chategory is present, and 0 if it is not
    if (Data$obj_category[t]==categ[1]){
      r<-c(1,0,0,0)
    } else if (Data$obj_category[t]==categ[2]){
      r<-c(0,1,0,0)
    } else if (Data$obj_category[t]==categ[3]){
      r<-c(0,0,1,0)
    }else if (Data$obj_category[t]==categ[4]){
      r<-c(0,0,0,1)
    }
    
    # Set value pre
    Ppre<-p
    
    Vpre<-V
    
    # update values
    updateVal<-update_RW(r = r, V = V, alpha = alpha)
    
    # prediction error
    delta <- updateVal$delta
    
    # assign it to the dataset
    Data[t, Deltaindex]<-delta
    
    # update V
    V<-updateVal$V  
    
    Ppost<-softmax(V, beta)
    
    Data$entropy_preP[t]<- -((Ppre[1]*log(Ppre[1]) + Ppre[2]*log(Ppre[2]) +
                               Ppre[3]*log(Ppre[3]) + Ppre[4]*log(Ppre[4]) ))
    
    Data$surpriseP[t]<- Ppost[1] * log(Ppost[1]/Ppre[1]) + Ppost[2] * log(Ppost[2]/Ppre[2]) +
      Ppost[3] * log(Ppost[3]/Ppre[3]) + Ppost[4] * log(Ppost[4]/Ppre[4])  
    
    
    Data$entropy_preV[t]<- -((Vpre[1]*log(Vpre[1]) + Vpre[2]*log(Vpre[2]) +
                                Vpre[3]*log(Vpre[3]) + Vpre[4]*log(Vpre[4]) ))
    
    Data$surpriseV[t]<- V[1] * log(V[1]/Vpre[1]) + V[2] * log(V[2]/Vpre[2]) +
      V[3] * log(V[3]/Vpre[3]) + V[4] * log(V[4]/Vpre[4])  
    
    # assign values to the dataset
    Data[t, Vindex]<-V
    Data[t, Pindex]<-p
    
    # # compute uncertainty as the 1/variance of the probability
    # uncertainty<-1/(var(unlist(Data[t,Pindex]))+1)
    # 
    # # uncertainty as the -log(sum prob * log prob)
    # uncertainty2<- -sum(  unlist(Data[t,Pindex]) *log(unlist(Data[t,Pindex])))
    # 
    # change point probability
    # probability of that observations
    #probobs<-Data[t, paste("P", respCounter, sep="")]
    
    #   if ( count[Murkcounter] ==1 | count[-Murkcounter]==0 ){ # while one of the two character has not been shown
    #     
    #     Data$uncertainty[t]<-uncertainty
    #     Data$uncertainty2[t]<-uncertainty2
    #     #Data$CPP[t]<-probobs
    #     # uncertainty as the -log(sum prob * log prob)
    #     
    #   } else{
    #     # retrieve uncertainty on the last trial of the other character
    #     unctmin1<-Data[Data$cuedCharacter!=Data$cuedCharacter[t],][count[-Murkcounter], "uncertainty"]
    #     
    #     unc2tmin1<-Data[Data$cuedCharacter!=Data$cuedCharacter[t],][count[-Murkcounter], "uncertainty2"]
    #     
    #     # retrieve probability of the observation on the previous trial for the same butterfly
    #     probobstminus1<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter]-1,paste("P", respCounter, sep="") ]
    #     
    #     Data$uncertainty[t]<-mean(uncertainty,unctmin1)
    #     Data$uncertainty2[t]<-mean(uncertainty2,unctmin1)
    #     #Data$CPP[t]<-probobs*probobstminus1
    #     
    #     
    #   }
  }
  # we could take the probability only for the congruent trials, but for now we are taking all the probabilities
  
  NegLL<--sum(log(Data$Prob), na.rm=T)
  
  if (print ==1){
    return(NegLL)
  } else if(print==2){
    return(Data)}
}

