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
  // array[N] int<lower=1, upper=Nexps> exps_idxs;

  // ragged array indexing for experiments
  array[Nexps] int<lower=1, upper=N> exp_start_idxs;
  array[Nexps] int<lower=1, upper=N> exp_end_idxs;

  array[N] int<lower=0> seeds; // number of seeds

  vector[Nexps] chill; // chilling treatment 
  // vector[N] germ_temp; // germination temperature
  
  vector<lower=0>[N] d; // time of observations (days)
}

transformed data {
  real chill0 = 1;
}

parameters {
  vector<lower=0, upper=1>[Nspecies] pv_base; // probability of viability
  // vector[Nspecies] beta_pv_chill; // 
  
  vector<lower=0>[Nspecies] tau1_base;
  // vector[Nspecies] beta_tau1_chill; // 
  
  vector<lower=tau1_base>[Nspecies] tau50_base;
  // vector[Nspecies] beta_tau50_chill; // 
}

transformed parameters {
  
  vector[N] pg; //germination prob
  
  for(e in 1:Nexps){
    
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    int species = species_idxs[e];
    
    // real pv = inv_logit(logit(pv_base[species])+beta_pv_chill[species]*(chill[e]-chill0)); 
    real pv = pv_base[species];
    // real tau1 = exp(log(tau1_base[species])+beta_tau1_chill[species]*(chill[e]-chill0));
    real tau1 = tau1_base[species];
    // real tau50 = exp(log(tau50_base[species])+beta_tau50_chill[species]*(chill[e]-chill0));
    real tau50 = tau50_base[species];
    
    // print(pv);
    // print(tau1);
    // print(tau50);
    if(tau1 > tau50){
      print("you should move to a delta");
    };
    
    pg[start] = (1-pv)+pv*(1-gompertz(d[end], tau1, tau50));
    
    for (t in (start+1):end) {
      pg[t] = pv*(gompertz(d[t], tau1, tau50) - gompertz(d[t-1], tau1, tau50));
    }
    pg[start:end] =  pg[start:end] + 1e-12;
    pg[start:end] = pg[start:end] / sum(pg[start:end]); //  for stability
    
    // print(pg);
  }

}

model {
  
  // priors, au pif
  pv_base ~ beta(4,2);
  # beta_pv_chill ~ normal(0,1);
  
  tau1_base ~ normal(7,2);
  # beta_tau1_chill ~ normal(0,1);
  
  tau50_base ~ normal(50,3);
  # beta_tau50_chill ~ normal(0,1);

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
