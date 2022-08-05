getPstay<-function(Data){
  #----------------------------------------------------------------------------#
  # Function that calculates the probability of switching
  # It needs the category of the response coded in the "respCat" variable
  # 
  # INPUT: data - dataframe
  # OUTPUT: dataframe with a variable (stayShift) indicating whether the decision 
  #         changed as a function of the previous decision (1 = stay, 0 = change)
  #----------------------------------------------------------------------------#
  
  # initialize the categories
  # convert the object category into numeric variable (in alphabetical order)
  categ<-levels(as.factor(Data$obj_category))
  
  # initialize the murks
  murks<-levels(as.factor(Data$cuedCharacter))
  
  # initialize a variable for the choice
  choice<-vector(mode="numeric", length = 2)
  Data$stayShift<-NA
  for (n in 1: nrow(Data)){
    # which is the current choice?
    currChoice<-Data$respCat[n]
    # which category (number)
    currChoice<-which(categ==currChoice)
   # which murks
    character<-which(Data$cuedCharacter[n]==murks)
    if (n==1){
      choice[character]<-currChoice
      
    } else{ # if this is not the first trial
      if (currChoice==choice[character]){ # if the choice is similar at 
        # the previous choice (for the same character)
         Data$stayShift[n]<-1# stay behaviour
         choice[character]<-currChoice
    }else{
      Data$stayShift[n]<-0 # shift
      choice[character]<-currChoice
      
    }
    }
  }
    
    return(Data)
    
  }
  
