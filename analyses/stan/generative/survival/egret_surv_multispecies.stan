// Model inspired by the Survin model (wrote by Mike and Ken)
// currently germ_temp is a constant by experiment
// but the way I wrote the mdoel it should be easy to modify later

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
  // array[N] int<lower=1> exps;
  
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
  vector<lower=0>[N_species] T0;
  vector<lower=0>[N_species] k;  
  
  // seed viability
  vector<lower=0, upper=1>[N_species] pv;   
  
  // phenological transition parameters
  vector[N_species] log_Psi0; // log phenology threshold (log forcing units)
  vector[N_species] log_sigma; // log phenology threshold scale (log forcing units)
  
}


transformed parameters {
  
  vector[N_species] Psi0 = exp(log_Psi0);
  vector[N_species] sigma = exp(log_sigma);
  
}


model {
  
  k ~ normal(0, 1/2.57);
  T0 ~ normal(15, 7/2.57);
  
  log_Psi0 ~ normal(3, 0.5);
  log_sigma ~ normal(2, 0.25);
  
  pv ~ beta(4, 2);
  
  for(e in 1:N_exps){
    
    int sp = species_idxs[e];
    
    int start_idx = start_exp_idxs[e];
    int end_idx = end_exp_idxs[e];
    
    real constant_temp = germ_temp[e];
    
    // compute daily forcings
    int earliest_forcing = 1;
    int latest_forcing = max_days + 1;
    array[latest_forcing] real daily_forcings;
    for(day in earliest_forcing:latest_forcing){
      daily_forcings[day] = inv_logit(k[sp] * (constant_temp - T0[sp]));
    }
    
    // germinated seeds
    if (N_obs[e] > 0){
      for(obs in 1:N_obs[e]){
        array[N_obs[e]] int local_days = germ_days[start_idx:end_idx];
        real Psi = sum(daily_forcings[1:local_days[obs]]);
        real log_dPsidt = log_inv_logit(k[sp] * (constant_temp - T0[sp]));
        
        target += log(pv[sp]) + logistic_lpdf(Psi | Psi0[sp], sigma[sp])  + log_dPsidt;
        
      }
    }
    
    // ungerminated seeds
    if (N_ungerm[e] > 0){
      real Psi_last = sum(daily_forcings[1:latest_forcing]);
      target += N_ungerm[e] * log_sum_exp(log1m(pv[sp]), log(pv[sp]) + logis_lccdf_s(Psi_last, Psi0[sp], sigma[sp]));
    }
    
  }
  
}

generated quantities {

  array[N_exps] int N_obs_pred; // total germianted seeds
  array[N_exps] int N_ungerm_pred;
  
  array[N_exps, max_days] int dgerm_pred; // daily germination
  array[N_exps, max_days] int cumgerm_pred; // cumulative germination

  for (e in 1:N_exps) {
    
    int sp = species_idxs[e];
    
    vector[max_days + 1] fday; // daily forcing
    vector[max_days + 1] cumF; // cumulative sum of forcing

    for (d in 1:(max_days + 1))
      fday[d] = inv_logit(k[sp] * (germ_temp[e] - T0[sp]));
    cumF = cumulative_sum(fday);

    N_obs_pred[e] = 0;
    N_ungerm_pred[e] = 0;
    for (day in 1:max_days)
      dgerm_pred[e, day] = 0;

    // for each seed in the experiment...
    for (s in 1:(N_obs[e] + N_ungerm[e])) {
      
      if (bernoulli_rng(pv[sp]) == 0) {
        N_ungerm_pred[e] += 1;
      } else {
        
        real l = logistic_rng(Psi0[sp], sigma[sp]); // threshold to reach
        
        if (l < cumF[1] || l >= cumF[max_days + 1]) {
          N_ungerm_pred[e] += 1;  
        } else {
          
          // latest d with cumPsi[d] < psi
          int day = 1;                        
          while (day < max_days && cumF[day + 1] < l)
            day += 1;
          dgerm_pred[e, day] += 1;
          N_obs_pred[e] += 1;
        }
      }
      
    }
    
    cumgerm_pred[e,1:max_days] = cumulative_sum(dgerm_pred[e,1:max_days]);
  }
}
