// here I just reparametrized the Gompertz function in terms of tau1 and delta_tau 

functions {
  real gompertz(real t, real tau1, real delta_tau) {
    return exp(-log(2)*(log(2)/log(100))^((t-delta_tau-tau1)/(delta_tau)));
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

  vector[Nexps] chill; // chilling treatment (in weeks)
  // vector[N] germ_temp; // germination temperature
  
  vector[Nexps] forcing; // chilling treatment (in weeks)
  
  vector<lower=0>[N] d; // time of observations (days)
}

transformed data {
  real chill0 = 1;
  real forcing0 = 2;
}

parameters {
  
  real mu_pv_base;
  real<lower=0> sigma_pv_base;
  vector[Nspecies] alpha_tilde_pv_base; // probability of viability
  
  real mu_beta_pv_chill;
  real<lower=0> sigma_beta_pv_chill;
  vector[Nspecies] beta_tilde_pv_chill; 
  
  real mu_log_tau1_base;
  real<lower=0> sigma_log_tau1_base;
  vector[Nspecies] log_tau1_tilde_base;
  
  real mu_log_delta_tau_base;
  real<lower=0> sigma_log_delta_tau_base;
  vector[Nspecies] log_delta_tau_tilde_base;
  
  // real mu_beta_log_delta_tau_forc;
  // real<lower=0> sigma_beta_log_delta_tau_forc;
  // vector[Nspecies] beta_tilde_log_delta_tau_forc;
}

transformed parameters {
  
  vector[N] pg; //germination prob
  
  vector[Nspecies] alpha_pv_base = mu_pv_base + sigma_pv_base * alpha_tilde_pv_base;
  vector[Nspecies] pv_base = inv_logit(alpha_pv_base);
  
  vector[Nspecies] beta_pv_chill = mu_beta_pv_chill + sigma_beta_pv_chill * beta_tilde_pv_chill;
  
  vector[Nspecies] log_tau1_base = mu_log_tau1_base + sigma_log_tau1_base * log_tau1_tilde_base;
  vector[Nspecies] tau1_base = exp(log_tau1_base);
  
  vector[Nspecies] log_delta_tau_base = mu_log_delta_tau_base + sigma_log_delta_tau_base * log_delta_tau_tilde_base;
  vector[Nspecies] delta_tau_base = exp(log_delta_tau_base);
  
  // vector[Nspecies] beta_log_delta_tau_forc = mu_beta_log_delta_tau_forc + sigma_beta_log_delta_tau_forc * beta_tilde_log_delta_tau_forc;
  // vector[Nspecies] beta_delta_tau_forc = exp(beta_log_delta_tau_forc);
  
  // vector[Nspecies] tau50_base = tau1_base + delta_tau_base;
  
  for(e in 1:Nexps){
    
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    int species = species_idxs[e];
    
    real pv = inv_logit(logit(pv_base[species])+beta_pv_chill[species]*(chill[e]-chill0)); 
    // real tau1 = exp(log(tau1_base[species])+beta_tau1_chill[species]*(chill[e]-chill0));
    real tau1 = tau1_base[species];
    
    // real log_delta_tau = beta_log_delta_tau_forc[species] * (forcing[e]);
    // real tau50 = tau1 + exp(log_delta_tau_base[species]);
    real delta_tau = delta_tau_base[species];
    
    // print(pv);
    // print(tau1);
    // print(tau50);
    // if(tau1 > tau50){
    //   print("you should move to a delta");
    // };
    
    pg[start] = (1-pv)+pv*(1-gompertz(d[end], tau1, delta_tau));
    
    for (t in (start+1):end) {
      pg[t] = pv*(gompertz(d[t], tau1, delta_tau) - gompertz(d[t-1], tau1, delta_tau));
    }
    pg[start:end] =  pg[start:end] + 1e-12;
    pg[start:end] = pg[start:end] / sum(pg[start:end]); //  for stability
    
    // print(pg);
  }

}

model {
  
  // priors, au pif
  mu_pv_base ~ normal(1,1);
  sigma_pv_base ~ normal(0,1); // pif
  alpha_tilde_pv_base ~ normal(0,1);
  // beta_pv_chill ~ normal(0,1);
  
  mu_beta_pv_chill ~ normal(0, 0.15); // -10%  < grand mean < 10% with one more week of chilling
  sigma_beta_pv_chill  ~ normal(0, 0.15); // allows for ~ 20%
  beta_tilde_pv_chill ~ normal(0, 1); 
  
  mu_log_tau1_base ~ normal(0,1);
  sigma_log_tau1_base ~ normal(0,0.2); // pif
  log_tau1_tilde_base ~ normal(0,1);
  // beta_tau1_chill ~ normal(0,1);
  
  mu_log_delta_tau_base ~ normal(2,0.5);
  sigma_log_delta_tau_base ~ normal(0,0.2); // pif
  log_delta_tau_tilde_base ~ normal(0,1);
  // beta_tau50_chill ~ normal(0,1);
  
  // mu_beta_log_delta_tau_forc ~ normal(2, 0.5); // one degree increase would lead to a reduction of 2 days max at delta_tau= 15?
  // sigma_beta_log_delta_tau_forc ~ normal(0 , 0.2);
  // beta_tilde_log_delta_tau_forc ~ normal(0,1);

  for(e in 1:Nexps){
    
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    int species = species_idxs[e];
  
    // likelihood
    seeds[start:end] ~ multinomial(pg[start:end]);
    
  }
  
}


generated quantities {
  int<lower=0> y_pred[N];
  int<lower=0> cumy_pred[N];

  for(e in 1:Nexps){

    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    y_pred[start:end] = multinomial_rng(pg[start:end], sum(seeds[start:end]));

    cumy_pred[start] = 0;
    cumy_pred[(start+1):end] = cumulative_sum(y_pred[(start+1):end]);

  }

}
