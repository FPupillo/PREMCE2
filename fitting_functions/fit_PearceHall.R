
fit_PearceHall<-function(data,alphaBound = alphaBound, betaBound = betaBound, 
                         kBound = kBound, gammaBound = gammaBound,
                         cBound = NULL, initialV = initialV){
  #----------------------------------------------------------------------------#
  # This function finds the parameters that 
  # minimize the negative log-likelihood
  #
  # Input    
  #    Data: a long dataset where each row represents a trial. 
  #    alphaBound<- a two-element vector with upper and lower boundaries 
  #                 for the alpha parameter     
  #    betaBound<- a two-element vector with upper and lower boundaries 
  #                 for the beta parameter 
  #    kBound<-two element vector with boundaries for parameter k
  #    gammaBound<- gamma parameter's boundaries
  #    initialV<- inital V values
  # Output:
  #   A list with: 
  #   [[1]] "alphabetaPAR" : alpha [1], beta [2]parameters that minimize
  #           the negative log-likelihood
  #   [[2]] "loglikel": log-likelihood for the model with the parameters 
  #           of best fit
  #   [[3]] "BIC" : Bayesian Information Criterion for the model with the 
  #           parameters of best fit
  #----------------------------------------------------------------------------#
  
  X0<-c(runif(1), rexp(1,1), runif(1) , runif(1))  
  # rexp generates random numbers from the exponential distributon with mean 1
  LB<-c(alphaBound[1], betaBound[1], kBound[1], gammaBound[1]) # lower boundary
  UB<-c(alphaBound[2], betaBound[2], kBound[2], gammaBound[2])

  # this function is similar to the MATLAB "handle" function
  obfunc<-function(x) lik_PearceHall(Data = data, alpha_0 = x[1],
                                     beta = x[2], 
                                     k = x[3],
                                     gamma = x[4], 1, initialV) 
  
  # Find best-fitting parameters
  NegLL<-optim(X0, obfunc, method = "L-BFGS-B",lower = LB, upper=UB) 
  
  # get log-likelihood
  LL<--NegLL[[2]] # log likelihood
  
  # compute BIC
  BIC <- BICcompute(length(X0), nrow(data), NegLL[[2]])
  
  # Prepare results for output
  data <- list("alpha"= NegLL[[1]][1], "beta" = NegLL[[1]][2],
               "k" = NegLL[[1]][3], "gamma" = NegLL[[1]][4],"logLikel"= LL,
               "BIC" = BIC)

  return(data)
}
