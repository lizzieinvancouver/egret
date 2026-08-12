// Model inspired by the Survin model (wrote by Mike and Ken)
// currently germ_temp is a constant by experiment
// but the way I wrote the model it should be easy to modify later

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
}

parameters {
  
  // forcing function parameters (simple logistic)
  real<lower=0> T0;
  real<lower=0> k;  
  
  // seed viability
  real<lower=0, upper=1> pv;   
  
  // ceiling temp
  real mu_c;
  real<lower=0> sigma_c; 
  
  // phenological transition parameters
  real log_Psi0; // log phenology threshold (log forcing units)
  real log_sigma; // log phenology threshold scale (log forcing units)
  
}


transformed parameters {
  real Psi0 = exp(log_Psi0);
  real sigma = exp(log_sigma);
}


model {
  
  k ~ normal(0, 1/2.57);
  T0 ~ normal(15, 7/2.57);
  
  log_Psi0 ~ normal(3, 0.5);
  log_sigma ~ normal(2, 0.25);
  
  pv ~ beta(4, 2);
  
  mu_c ~ normal(19, 3);
  sigma_c ~ normal(0, 2);
  
  for(e in 1:N_exps){
    
    int start_idx = start_exp_idxs[e];
    int end_idx = end_exp_idxs[e];
    
    real constant_temp = germ_temp[e];
    
    real log_pi   = log_inv_logit((mu_c - constant_temp) / sigma_c);
    real log1m_pi = log1m_inv_logit((mu_c - constant_temp) / sigma_c);
    
    // compute daily forcings
    int earliest_forcing = 1;
    int latest_forcing = max_days + 1;
    array[latest_forcing] real daily_forcings;
    
    real dPsidt = inv_logit(k * (constant_temp - T0));
    real log_dPsidt = log_inv_logit(k * (constant_temp - T0));
    
    for(day in earliest_forcing:latest_forcing){
      daily_forcings[day] = dPsidt;
    }
    
    // germinated seeds
    if (N_obs[e] > 0){
      for(obs in 1:N_obs[e]){
        array[N_obs[e]] int local_days = germ_days[start_idx:end_idx];
        real Psi = sum(daily_forcings[1:local_days[obs]]);
        // real log_dPsidt = log_inv_logit(k * (constant_temp - T0));
        
        target += log(pv) + log_pi + logistic_lpdf(Psi | Psi0, sigma)  + log_dPsidt;
        
      }
    }
    
    // ungerminated seeds
    if (N_ungerm[e] > 0){
      real Psi_last = sum(daily_forcings[1:latest_forcing]);
      
      vector[3] lp;
      lp[1] = log1m(pv);
      lp[2] = log(pv) + log1m_pi;
      lp[3] = log(pv) + log_pi + logis_lccdf_s(Psi_last, Psi0, sigma);
      
      target += N_ungerm[e] * log_sum_exp(lp);
    }
    
  }
  
}

generated quantities {
  
  array[N_exps] int N_obs_pred;
  array[N_exps] int N_ungerm_pred;
  array[N_exps, max_days] int dgerm_pred;
  array[N_exps, max_days] int cumgerm_pred;
  
  for (e in 1:N_exps) {
    
    vector[max_days + 1] fday;
    vector[max_days + 1] cumF;
    real pi_e = inv_logit((mu_c - germ_temp[e]) / sigma_c);
    
    for (d in 1:(max_days + 1))
    {
      fday[d] = inv_logit(k * (germ_temp[e] - T0));
    }
    
    cumF = cumulative_sum(fday);
    N_obs_pred[e] = 0;
    N_ungerm_pred[e] = 0;
    
    for (day in 1:max_days)
      dgerm_pred[e, day] = 0;
      
    // for each seed in the experiment...
    for (s in 1:(N_obs[e] + N_ungerm[e])) {
      if (bernoulli_rng(pv) == 0) {
        N_ungerm_pred[e] += 1; // not viable
      } else if (bernoulli_rng(pi_e) == 0) {
        N_ungerm_pred[e] += 1; // viable but somehow thermoinhibited
      } else {
        real l = logistic_rng(Psi0, sigma);
        if (l >= cumF[max_days + 1]) {
          N_ungerm_pred[e] += 1; // threshold not reached before the end of exp
        } else if (l < cumF[1]) {
          dgerm_pred[e, 1] += 1; // already past threshold on day 1..?
          N_obs_pred[e] += 1;
        } else {
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
