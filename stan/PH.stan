data {
  int<lower=1> nSubjects;               
  int<lower=1> nTrials;    
  int<lower=1,upper=2> character[nSubjects,nTrials];
  int<lower=1,upper=99> choice[nSubjects,nTrials];   // 99 for missing repsonses trials  
  real<lower=0, upper=1> accuracy[nSubjects,nTrials]; 
}

transformed data {
  vector[4] initV;  // initial values for V
  //vectr[4] initV; 
  initV = rep_vector(0.25, 4); // values are initialized as 0, twice
}

parameters {
  /*  real<lower=0,upper=1> alpha_pos;
  real<lower=0,upper=1> alpha_neg;
  */
  real<lower=0,upper=1> alpha0_mu; // gamma at the poulation level  - uniform
  real<lower=0,upper=10> beta_mu;
  real<lower=0,upper=1> k_mu;
  real<lower=0,upper=1> gamma_mu;

  real<lower=0> alpha0_sd;
  real<lower=0> beta_sd;
  real<lower=0> k_sd;
  real<lower=0> gamma_sd;
  
  real<lower=0,upper=1> alpha0[nSubjects];  // the limits truncate the distribution: gamma at he subject level
  real<lower=0,upper=10> beta[nSubjects];  // inverse temperature parameter
  real<lower=0,upper=1> k[nSubjects];  // associability
  real<lower=0,upper=1> gamma[nSubjects]; 
}

model {
  // standard deviation (subjsecg level)
  alpha0_sd ~ cauchy(0,0.1); 
  beta_sd ~ cauchy(0,0.1);
  k_sd ~ cauchy(0,0.1); 
  gamma_sd ~ cauchy(0,0.1);
  
  // individual ones. they follow normal distribution with mean ond sd of the group level
  alpha0 ~ normal(alpha0_mu, alpha0_sd);
  beta ~ normal(beta_mu, beta_sd);
  k ~ normal(k_mu, k_sd);
  gamma ~ normal(k_mu, k_sd);
  
  for (s in 1:nSubjects){
    
    // declare the values
    vector[4] v1; // values of the categories. It has to be a vector in order to be used by the categorical_logit function. 
    vector[4] v2; 
    vector[4] v;
    real alpha;
    real pe;       // prediction error
    alpha = alpha0[s];
    //vector[1] alpha;
    //vector[1] alpha;
    
    // vector[2] p[nTrials];
    
    //v = rep_vector(0.25, 4); // initializing at 0.25
    
    v1 = initV;
    v2 = initV;
    
    
    for (t in 1:nTrials) {    
      if(choice[s,t]!=99){ // missing response trials are not analysed
      
      
      if (character[s,t] ==1){
        v = v1;
      }else{
        v = v2;
      }
      
      //the choice is distributed multinomially (categorical)
      choice[s, t] ~ categorical(softmax(beta[s]*v)) ;
      //= choice[v] ~ categorical_logit(beta*v)
      // where p = softmax(beta*v) 
      //choice[s,t] ~ categorical_logit(beta[s]*v);
      
      // prediction error 
      pe = accuracy[s,t] - v[choice[s,t]]; // updating only the chosen category
      
      
      // value update (learning) 
      v[choice[s,t]] +=  k[s]*alpha * pe;
      
      alpha = gamma[s] * fabs(pe) + (1-gamma[s]) * alpha;

      
      if (character[s,t] ==1){
        v1 = v;
      }else{
        v2 = v;
      }
      
      }
    }
    
  }
}

generated quantities {
  //real <lower=0,upper=1> alpha_mu;
  //real <lower=0,upper=10> beta_mu;
  real log_lik[nSubjects];
  real pe[nSubjects,nTrials];
  real alpha[nSubjects, nTrials+1];
  real chosenV[nSubjects,nTrials];
  int  y_pred[nSubjects, nTrials];
  real  character2[nSubjects, nTrials];
  real  accuracy2[nSubjects, nTrials];

  y_pred = rep_array(-999,nSubjects ,nTrials);
  
  for (s in 1:nSubjects){
    
    // declare the values
    vector[4] v1; // values of the categories. It has to be a vector in order to be used by the categorical_logit function. 
    vector[4] v2; 
    vector[4] v;
    
    
    // vector[2] p[nTrials];
    
    //v = rep_vector(0.25, 4); // initializing at 0.25
    
    v1 = initV;
    v2 = initV;
    
    log_lik[s]=0;
    alpha[s,1] = alpha0[s];

    for (t in 1:nTrials) {    
      
      if (character[s,t] ==1){
        v = v1;
      }else{
        v = v2;
      }
      
      if(choice[s,t]!=99){ // missing response trials are not analysed
      
      log_lik[s] += categorical_logit_lpmf(choice[s,t] | beta[s]*v );
      y_pred[s,t] = categorical_logit_rng( beta[s] * v); 
      
      character2[s,t] = character[s,t];
      
      accuracy2[s, t] = accuracy [s,t];
      
      // prediction error 
      pe[s,t] = accuracy[s,t] - v[choice[s,t]]; // updating only the chosen category
      
      // update action values
      v[choice[s,t]] +=  k[s]*alpha[s,t] * pe[s,t];
      
      alpha[s, t+1] = gamma[s] * fabs(pe[s,t]) + (1-gamma[s]) * alpha[s,t];      
      
      // record values
      chosenV[s,t] = v[choice[s,t]];
      
      }
      
            
      if (character[s,t] ==1){
        v1 = v;
      }else{
        v2 = v;
      }
      
    }
  }
} //
  