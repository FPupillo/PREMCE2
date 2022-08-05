
taskSim<-function(
#------------------------------------------------------------------------------#
# function that simulates task
# created: Tue Oct 26 14:15:02 2021"
#  INPUT: Pcong = contingency for the most probably choice
#  Output: a dataset with the task structure 
#------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
#                       Parameters
# -----------------------------------------------------------------------------#
  
  # set the reward probabilities for the contingencies
  Pcong=0.75 ,   # 75 for the conguent ones, the preferred category
  
  # Ntrial per block per character
  Ntrial=24,
  
  # N character
  Ncharacter=1,
  
  # switch point
  switchN=3,
  
  # how many categories? 
  categN=4
  
  
# -----------------------------------------------------------------------------#
){
  
  
  Pincong=(1-Pcong)/(categN-1) 
  
  characters<-c("stimuli/m2.png", "stimuli/m5.png")

  # select the categories 
  selCat<-c( "Outdoor activity & sport item", "Kitchen & utensil",
             "Electronic device & accessory", "Hand labour tool & accessory")
  
  categ<-selCat
  
  if (categN<4){
    categ<-selCat[1:categN]
  }
  
  # create dataframe
  df<-as.data.frame(matrix(NA, ncol = 11, nrow = Ntrial*(switchN+1)))
  
  names(df)<-c("left_categ", "centleft_categ", "centright_categ", "right_categ", 
               "character", "switch_cond", "trial_cond", "trialN","opt_choice",
               "corr_ans", "obj_category")
  
  # put the categories
  for (n in 1:nrow(df)){
    df[n,1:categN]<-categ
  }
  
  # create the switch condition
  df$switch_cond<-rep(c(2:(switchN+2)), each = Ntrial)
  
  
  # bind two characters if necessary
  if (Ncharacter==2){
    df<-rbind(df, df)
    df$character<-rep(c(characters) , each = Ntrial*(switchN+1))
  } else{
    df$character<-rep(characters[1], each = Ntrial*(switchN+1))
  }
  
  # shuffle them
  shuffle_df<-vector()
  for (s in 2:5){
    sub<-df[df$switch_cond==s,]
    
    # shuffle
    sub<-sample_n(sub, nrow(sub))
    
    shuffle_df<-rbind(shuffle_df, sub)
  }
  
  # create correct answer for them
  # the answer is drawn from the categories with the predefinite prob
  # correct category for one character
  if (categN<4){
    corr_cat1<-seq(1:categN-1)
  } else {
  corr_cat1<-seq(1:(switchN+1))
  }
  
  # correct category for the other character
  corr_cat2<-rev(corr_cat1)
  
  # optimal choice for each switch num
  shuffle_df$opt_choice<-NA
  
  if (Ncharacter==1){
    shuffle_df$opt_choice<-rep(corr_cat1, each = Ntrial)
  } else {
  for (n in 1:nrow(shuffle_df)){
    if (shuffle_df$character[n]==characters[1]){
      shuffle_df$opt_choice[n]<-shuffle_df$switch_cond[n]-1  
    }else{
      if (shuffle_df$switch_cond[n]==2){
        shuffle_df$opt_choice[n]<-4
      }else if (shuffle_df$switch_cond[n]==3){
        shuffle_df$opt_choice[n]<-3
      } else if (shuffle_df$switch_cond[n]==4){
        shuffle_df$opt_choice[n]<-2
      }else{
        shuffle_df$opt_choice[n]<-1
      }
      }
  }
    
  }

  
  # create opt_choice_cat
  shuffle_df$opt_choice_cat<-categ[shuffle_df$opt_choice]
  
  # now for each switch sample the most probable category with certain frequency
  shuffle_df$obj_category<-NA
  for (n in 1:nrow(df)){
    shuffle_df$obj_category[n]<-sample(c(categ[shuffle_df$opt_choice[n]], 
                                         categ[-shuffle_df$opt_choice[n]]), 1,
                               prob = c(Pcong, rep(Pincong, categN-1) ))
    if (shuffle_df$obj_category[n]==shuffle_df$opt_choice_cat[n]){
      shuffle_df$trial_cond[n]<-1 }else{ shuffle_df$trial_cond[n]<-0}
  }
  
  # get corr_ans
  for (n in 1:nrow(df)){
    shuffle_df$corr_ans[n]<-which(as.character(shuffle_df[n,1:categN])==
                                    shuffle_df$obj_category[n])
  }
  
  # trial Number
  shuffle_df$trialN<-1:nrow(shuffle_df)
  
  return(shuffle_df)
  
}

