data {
  int<lower=1> nSubjects;               
  int<lower=1> nTrials;    
  int<lower=1,upper=2> character[nSubjects,nTrials];
  int<lower=1,upper=99> choice[nSubjects,nTrials];   // 99 for missing repsonses trials  
  real<lower=0, upper=1> accuracy[nSubjects,nTrials]; 
}

transformed data {
  vector[4] init_Ps;  // initial prob/belief of the four states
  //vectr[4] initV; 
  init_Ps = rep_vector(0.25, 4); // values are initialized as 0, twice
}

parameters {
/*  real<lower=0,upper=1> alpha_pos;
  real<lower=0,upper=1> alpha_neg;
  */
  // group level parameters
  real gamma_mu_pr; // gamma at the poulation level
   real c_mu_pr;

  real<lower=0> gamma_sd;
  real<lower=0> c_sd;
  // individual-level parameters
  vector[nSubjects] gamma_pr; 
  vector[nSubjects] c_pr; 
}

transformed parameters{
  vector<lower=0, upper=0.33>[nSubjects] gamma;
  vector<lower=0, upper=1>[nSubjects] c;
  
  gamma = Phi_approx(gamma_mu_pr + gamma_sd * gamma_pr);
  c = Phi_approx(c_mu_pr + c_sd * c_pr) ;
}
     
/*  real<lower=0.01,upper=0.32> gamma[nSubjects];  // the limits truncate the distribution: gamma at he subject level
  real<lower=0.34,upper=0.66> c[nSubjects];  // inverse temperature parameter*/
  



model {
  // standard deviation (subjsecg level)
    // prior of group-level parameters
  gamma_mu_pr ~ normal(0,1);
  c_mu_pr ~ normal(0,1);
  
  gamma_sd ~ cauchy(0,1); 
   c_sd ~ cauchy(0,1);
   
/*  // individual ones. they follow normal distribution with mean ond sd of the group level
  gamma ~ normal(gamma_mu, gamma_sd);
  c ~ normal(c_mu, c_sd);*/
  
    // prior of individual-level parameters
  gamma_pr ~ normal(0,1);
  c_pr ~ normal(0,1);
  
  for (s in 1:nSubjects){
    
  // declare the values
  vector[4] Ps1; // values of the categories. It has to be a vector in order to be used by the categorical_logit function. 
  vector[4] Ps2; 
  vector[4] Ps;
  real P_O_S1;   // p(O|S1) 
  real P_O_S2;   // p(O|S2) 
  real P_O_S3;
  real P_O_S4;
  //vector[1] alpha;
  //vector[1] alpha;
  
  // vector[2] p[nTrials];
  
  //v = rep_vector(0.25, 4); // initializing at 0.25
  
  Ps1 = init_Ps;
  Ps2 = init_Ps;

    
  for (t in 1:nTrials) {    
            // State update using the transition matrix 
        // from S[t-1] to S[t], BEFORE observing the outcome
        
     if(choice[s,t]!=99){ // missing response trials are not analysed

    
    if (character[s,t] ==1){
      Ps = Ps1;
    }else{
      Ps = Ps2;
    }
    
        // State update using the transition matrix 
        // from S[t-1] to S[t], BEFORE observing the outcome
        if (t == 1) {
          Ps = init_Ps;
        } else {
      Ps[1]=Ps[1]*(1-gamma[s])+Ps[2]*(gamma[s]/3)+Ps[3]*(gamma[s]/3)+Ps[4]*(gamma[s]/3);
      Ps[2]=Ps[2]*(1-gamma[s])+Ps[3]*(gamma[s]/3)+Ps[4]*(gamma[s]/3)+Ps[1]*(gamma[s]/3);
      Ps[3]=Ps[3]*(1-gamma[s])+Ps[1]*(gamma[s]/3)+Ps[2]*(gamma[s]/3)+Ps[4]*(gamma[s]/3);
      Ps[4]=Ps[4]*(1-gamma[s])+Ps[1]*(gamma[s]/3)+Ps[2]*(gamma[s]/3)+Ps[3]*(gamma[s]/3);
        }
    
            // action selection based on the State probability 
    choice[s,t] ~ categorical_logit( Ps); 

      // renew of emission prob: p(O|S1) p(O|S2); O is a pair between A(ction) and R(eward)
        // --> the probability of actually observing this outcome
        // Conditional Operator (a ? b : c) --> b if a is true, c if a is false
    
      if (accuracy[s,t] == 1) {
       P_O_S1 = 0.25 * ( (choice[s,t] == 1)?c[s]:((1-c[s])/3));
       P_O_S2 = 0.25 * ( (choice[s,t] == 2)?c[s]:((1-c[s])/3));
       P_O_S3 = 0.25 * ( (choice[s,t] == 3)?c[s]:((1-c[s])/3));
       P_O_S4 = 0.25 * ( (choice[s,t] == 4)?c[s]:((1-c[s])/3));
      } else if (accuracy[s,t]==0){
       P_O_S1 = 0.25 * ( (choice[s,t]  == 1)?((1-c[s])/3): c[s]);
       P_O_S2 = 0.25 * ( (choice[s,t]  == 2)?((1-c[s])/3): c[s]);
       P_O_S3 = 0.25* ( (choice[s,t]  == 3)?((1-c[s])/3): c[s]);
       P_O_S4 = 0.25 * ( (choice[s,t] == 4)?((1-c[s])/3): c[s]);
       }
    
    // State belief update using Bayesian rule, after observing the outcome
    # State belief update using Bayesian rule, after observing the outcome
    Ps[1] = (P_O_S1 * Ps[1]) / ( P_O_S1 * Ps[1] + P_O_S2 * Ps[2] + P_O_S3 * Ps[3] + P_O_S4 * Ps[4]);
    Ps[2] = (P_O_S2 * Ps[2]) / ( P_O_S1 * Ps[1] + P_O_S2 * Ps[2] + P_O_S3 * Ps[3] + P_O_S4 * Ps[4]);
    Ps[3] = (P_O_S3 * Ps[3]) / ( P_O_S1 * Ps[1] + P_O_S2 * Ps[2] + P_O_S3 * Ps[3] + P_O_S4 * Ps[4]);
    Ps[4] = (P_O_S4 * Ps[4]) / ( P_O_S1 * Ps[1] + P_O_S2 * Ps[2] + P_O_S3 * Ps[3] + P_O_S4 * Ps[4]);

    if (character[s,t] ==1){
      Ps1 = Ps;
    }else{
      Ps2 = Ps;
    }
    
    
  }
  }
  
  }
}
  
