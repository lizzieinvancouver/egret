// here I just reparametrized the Gompertz function in terms of tau1 and tau50 

functions {
  real gompertz(real t, real tau1, real tau50) {
    return exp(-log(2)*(log(2)/log(100))^((t-tau50)/(tau50-tau1)));
  }
}

data {
  int<lower=1> N; // observations, including never germinated seeds
  int<lower=1> Nexps;
  int<lower=1> Nspecies;
  // int<lower=1> Nobs[Nexps]; // number of "observations" per experiment
  
  array[Nexps] int<lower=1, upper=Nspecies> species_idxs;

  // ragged array indexing for experiments
  array[Nexps] int<lower=1, upper=N> exp_start_idxs;
  array[Nexps] int<lower=1, upper=N> exp_end_idxs;

  array[N] int<lower=0> seeds; // number of seeds
  
  vector<lower=0>[N] d; // time of observations (days)
}

transformed data {
  real chill0 = 1;
}

parameters {
  
  real mu_pv;
  real<lower=0> tau_pv;
  vector[Nexps] alpha_pv_tilde;
  
  real<lower=0> mu_tau1;
  real<lower=0> tau_tau1;
  vector[Nspecies] tau1_tilde;
  
  real mu_deltatau;
  real<lower=0> tau_deltatau;
  vector[Nexps] alpha_deltatau_tilde;
}

transformed parameters {
  
  vector[Nexps] alpha_pv = mu_pv + tau_pv*alpha_pv_tilde;
  vector[Nexps] pv = inv_logit(alpha_pv);
  
  vector<lower=0>[Nspecies] tau1 = mu_tau1 + tau_tau1*tau1_tilde;
  
  vector[Nexps] alpha_deltatau = mu_deltatau + tau_deltatau*alpha_deltatau_tilde;
  vector<lower=0>[Nexps] tau50;
  
  vector[N] pg; //germination prob
  
  for(e in 1:Nexps){
    
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    int sp = species_idxs[e];
    
    // if(tau1[sp] > tau50[e]){
    //   print("you should move to a delta");
    // };
    
    tau50[e] =  tau1[sp] + exp(alpha_deltatau[e]);
    
    pg[start] = (1-pv[e])+pv[e]*(1-gompertz(d[end], tau1[sp], tau50[e]));
    
    for (t in (start+1):end) {
      pg[t] = pv[e]*(gompertz(d[t], tau1[sp], tau50[e]) - gompertz(d[t-1], tau1[sp], tau50[e]));
    }
    pg[start:end] =  pg[start:end] + 1e-12;
    pg[start:end] = pg[start:end] / sum(pg[start:end]); //  for stability

  }

}

model {
  
  mu_pv ~ normal(0.5,0.5);
  tau_pv ~ normal(0.5, 0.5); // I expect variation across experiments...
  alpha_pv_tilde ~ normal(0,1);
  
  mu_tau1 ~ normal(7,2);
  tau_tau1 ~ normal(0,2);
  tau1_tilde ~ normal(0,1);

  mu_deltatau ~ normal(0,log(30)/2.57);
  tau_deltatau ~ normal(0,1);
  alpha_deltatau_tilde ~ normal(0,1);
  
  for(e in 1:Nexps){
    
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    int species = species_idxs[e];
  
    // likelihood
    seeds[start:end] ~ multinomial(pg[start:end]);
    
  }
  
}


// generated quantities {
//   int<lower=0> y_pred[N];
//   int<lower=0> cumy_pred[N];
// 
//   for(e in 1:Nexps){
// 
//     int start = exp_start_idxs[e];
//     int end = exp_end_idxs[e];
//     y_pred[start:end] = multinomial_rng(pg[start:end], sum(seeds[start:end]));
//     
//     cumy_pred[start] = 0;
//     cumy_pred[(start+1):end] = cumulative_sum(y_pred[(start+1):end]);
// 
//   }
// 
// }
