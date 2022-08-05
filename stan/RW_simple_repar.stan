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
  // add group level parameter names:
  real alpha_mu_pr;
  real beta_mu_pr;
  
  real<lower=0> alpha_sd;
  real<lower=0> beta_sd;
  vector[nSubjects] alpha_pr; 
  vector[nSubjects] beta_pr; 

}

transformed parameters{
  // subject level parameters
  vector <lower=0,upper=1>[nSubjects] alpha;
  vector <lower=0,upper=20>[nSubjects] beta; // inverse temp.
  alpha=Phi_approx(alpha_mu_pr+alpha_sd*alpha_pr);
  beta=Phi_approx(beta_mu_pr+beta_sd*beta_pr)*20;
}

model {
  alpha_mu_pr~normal(0,1);
  beta_mu_pr~normal(0,1);
  alpha_sd~cauchy(0,3);
  beta_sd~cauchy(0,3);
  alpha_pr~normal(0,1);
  beta_pr~normal(0,1);
  
  for (s in 1:nSubjects){
    
    // declare the values
    vector[4] v1; // values of the categories. It has to be a vector in order to be used by the categorical_logit function. 
    vector[4] v2; 
    vector[4] v;
    real pe;       // prediction error
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
      v[choice[s,t]] = v[choice[s,t]]+ alpha[s] * pe;
      
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
      v[choice[s,t]] += alpha[s] * pe[s,t];
      
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
  