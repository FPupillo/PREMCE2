data {
  int<lower=1> nSubjects;               
  int<lower=1> nTrials;    
  int<lower=1,upper=2> character[nSubjects,nTrials];
  int<lower=1,upper=99> choice[nSubjects,nTrials];   // 99 for missing repsonses trials  
  int<lower=0, upper=1> accuracy[nSubjects,nTrials]; 
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
  real<lower=0,upper=10> beta_mu;
  
  real<lower=0> beta_sd;
  
  real<lower=0,upper=10> beta[nSubjects];  // inverse temperature parameter
}

model {
  // standard deviation (subjsecg level)
  beta_sd ~ cauchy(0,0.1);
  
  // individual ones. they follow normal distribution with mean ond sd of the group level
  beta ~ normal(beta_mu, beta_sd);
  
  for (s in 1:nSubjects){
    
    // declare the values
    vector[4] v1; // values of the categories. It has to be a vector in order to be used by the categorical_logit function. 
    vector[4] v2; 
    vector[4] v;
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
if (accuracy[s,t]==1){
        if (choice[s,t]==1){
          v[1] = 1; v[2] =0 ; v[3] =0; v[4]=0;
      }else if (choice[s,t]==2){
          v[1] = 0; v[2] =1 ; v[3] =0; v[4]=0;
      }else if (choice[s,t]==3){
          v[1] = 0; v[2] =0 ; v[3] =1; v[4]=0;
      }else if (choice[s,t]==4){
          v[1] = 0; v[2] =0 ; v[3] =0; v[4]=1;
      }
      } else if (accuracy[s,t] ==0){
      if (choice[s,t]==1){
          v[1] = 0; v[2] =0.33; v[3] =0.33; v[4]=0.33;
      }else if (choice[s,t]==2){
          v[1] = 0.33; v[2] =0; v[3] =0.33; v[4]=0.33;
      }else if (choice[s,t]==3){
          v[1] = 0.33; v[2] =0.33; v[3] =0; v[4]=0.33;
      }else if (choice[s,t]==4){
          v[1] = 0.33; v[2] =0.33; v[3] =0.33; v[4]=0;
      }
      }

      // print(v)
      
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
  real chosenV[nSubjects,nTrials];
  real unchosenV[nSubjects,nTrials];
  int  y_pred[nSubjects, nTrials];


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
      
      if (accuracy[s,t]==1){
        if (choice[s,t]==1){
          v[1] = 1; v[2] =0 ; v[3] =0; v[4]=0;
      }else if (choice[s,t]==2){
          v[1] = 0; v[2] =1 ; v[3] =0; v[4]=0;
      }else if (choice[s,t]==3){
          v[1] = 0; v[2] =0 ; v[3] =1; v[4]=0;
      }else if (choice[s,t]==4){
          v[1] = 0; v[2] =0 ; v[3] =0; v[4]=1;
      }
      } else if (accuracy[s,t] ==0){
      if (choice[s,t]==1){
          v[1] = 0; v[2] =0.33; v[3] =0.33; v[4]=0.33;
      }else if (choice[s,t]==2){
          v[1] = 0.33; v[2] =0; v[3] =0.33; v[4]=0.33;
      }else if (choice[s,t]==3){
          v[1] = 0.33; v[2] =0.33; v[3] =0; v[4]=0.33;
      }else if (choice[s,t]==4){
          v[1] = 0.33; v[2] =0.33; v[3] =0.33; v[4]=0;
      }
      }
      }
      

      // record values
      chosenV[s,t] = v[choice[s,t]];
      #print(chosenV)
      unchosenV[s,t] = (accuracy[s,t] == 1)?0:(1);

      
            
      if (character[s,t] ==1){
        v1 = v;
      }else{
        v2 = v;
      }
      
    }
  }
} //