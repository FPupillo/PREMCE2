#library(pracma)

fit_WSLS<-function(data, betaBound, initialV){
  #----------------------------------------------------------------------------#
  # This function finds the parameters that 
  # minimize the negative log-likelihood
  #
  # Input    
  #    Data: a long dataset where each row represents a trial. 
  #    betaBound<- a two-element vector with upper and lower boundaries for
  #                 the beta parameter 
  #    initialQ<- inital Q values
  # Output:
  #   A list with: 
  #   [[1]] "alphabetaPAR" : alpha [1], beta [2]parameters that minimize the 
  #           negative log-likelihood
  #   [[2]] "loglikel": log-likelihood for the model with the parameters 
  #           of best fit
  #   [[3]] "BIC" : Bayesian Information Criterion for the model with the 
  #           parameters of best fit
  #----------------------------------------------------------------------------#  
  
  X0<-c(rexp(1,1))  # rexp generates random numbers from the exponential 
  # distributon with mean 1
  LB<-c(betaBound[1]) # lower boundary
  UB<-c( betaBound[2]) 

  # this function is similar to the MATLAB "handle" function
  obfunc<-function(x) lik_WSLS(data, x[1],1, initialV) 
  
  # Find best-fitting parameters
  NegLL<-optim(X0, obfunc, method = "L-BFGS-B",lower = LB, upper=UB) 
  
  # get log-likelihood
  LL<--NegLL[[2]] # log likelihood
  
  # compute BIC
  BIC <- BICcompute(length(X0), nrow(data), NegLL[[2]])
  
  # Prepare results for output
  data <- list(NegLL[[1]], LL, BIC)
  names(data)<-c("alphabetaPAR", "logLikel", "BIC" )
  
  return(data)
}
