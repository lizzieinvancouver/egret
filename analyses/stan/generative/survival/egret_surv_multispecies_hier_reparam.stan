functions {
  real logis_lccdf_s(real x, real m, real s) {
    return -log1p_exp((x - m) / s);
  }
}

data {
  
  int<lower=1> N_exps;
  array[N_exps] real germ_temp;
  
  int<lower=1> N;
  
  array[N] int<lower=1> germ_days;
  
  array[N_exps] int<lower=0> start_exp_idxs;
  array[N_exps] int<lower=0> end_exp_idxs;
  
  array[N_exps] int<lower=0> N_obs;
  array[N_exps] int<lower=0> N_ungerm;
  
  int<lower=1> max_days; 
  
  int<lower=1> N_species;
  array[N_exps] int<lower=1, upper=N_species> species_idxs;
}

parameters {
  
  // forcing function parameters (simple logistic)
  real mu_T0;
  real<lower=0> sigma_T0;
  vector[N_species] T0;
  
  real log_k;
  
  // seed viability
  vector<lower=0, upper=1>[N_species] pv;   
  
  // phenological transition parameters
  real mu_log_Psi0;
  real<lower=0> sigma_log_Psi0;
  vector[N_species] log_Psi0; // log phenology threshold (log forcing units)
  
  real mu_log_cv;
  real<lower=0> sigma_log_cv;
  vector[N_species] log_cv; // log(sigma / Psi0)
  
}

transformed parameters {
  
  real<lower=0> k = exp(log_k);
  vector[N_species] Psi0 = exp(log_Psi0);
  vector[N_species] sigma = exp(log_Psi0 + log_cv);
  
}

model {
  
  mu_T0 ~ normal(1.5, 1.5/2.57);
  sigma_T0 ~ normal(0, 0.3);
  T0 ~ normal(mu_T0, sigma_T0);
  
  log_k ~ normal(log(4), 0.5);
  
  mu_log_Psi0 ~ normal(3, 0.5);
  sigma_log_Psi0 ~ normal(0, 0.5);
  log_Psi0 ~ normal(mu_log_Psi0, sigma_log_Psi0);
  
  mu_log_cv ~ normal(-1, 0.5);
  sigma_log_cv ~ normal(0, 0.3);
  log_cv ~ normal(mu_log_cv, sigma_log_cv);
  
  pv ~ beta(4, 2);
  
  for(e in 1:N_exps){
    
    int sp = species_idxs[e];
    
    int start_idx = start_exp_idxs[e];
    int end_idx = end_exp_idxs[e];
    
    real constant_temp = germ_temp[e];
    
    real log_dPsidt = log_inv_logit(k * (constant_temp - T0[sp]));
    real dPsidt = exp(log_dPsidt);
    
    // germinated seeds
    if (N_obs[e] > 0){
      
      array[N_obs[e]] int local_days = germ_days[start_idx:end_idx];
      
      for(obs in 1:N_obs[e]){
        
        real Psi = (local_days[obs] - 0.5)*dPsidt;
        
        target += log(pv[sp]) + logistic_lpdf(Psi | Psi0[sp], sigma[sp]) + log_dPsidt;
        
      }
    }
    
    // ungerminated seeds
    if (N_ungerm[e] > 0){
      
      real Psi_last = max_days*dPsidt;
      target += N_ungerm[e] * log_sum_exp(log1m(pv[sp]), log(pv[sp]) + logis_lccdf_s(Psi_last, Psi0[sp], sigma[sp]));
    }
    
  }
  
}