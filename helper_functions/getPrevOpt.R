getPrevOpt<-function(data){
#------------------------------------------------------------------------------#
# function to get the optimal category of the previous contingency
#------------------------------------------------------------------------------#
  
  data$listN<-as.numeric(data$listN)
  data$Character<-as.character(data$character)
  
  data$PrevOptimalChoice<-NA
  for (n in 1:nrow(data)){
    if (data$listN[n]>2){
    
    # sel optimal choice for the previous block ("listN")
    # first, subset the list selecting only the previous block and the congruent trials
    subs<-data[data$SubNum==data$SubNum[n] & data$listN==data$listN[n]-1 & 
                 data$Character == data$Character[n] &
                 data$trialCond==1,]
    
    cat<-c("left_categ", "centleft_categ", "centright_categ", "right_categ")
    
    # optimal choice for one murks (we are taking only the first
    # as all of them should be the same)
    data$PrevOptimalChoice[n]<-as.character(subs[1,"corrCat"])
    }
  }
  
  # create a variable that refers to the previous optimal choice
  data$isPreviousOpt<-ifelse(data$respCat==data$PrevOptimalChoice,1,0)
  
  
  return(data)
}