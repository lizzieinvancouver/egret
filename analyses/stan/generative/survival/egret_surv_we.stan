// Model inspired by the Survin model (wrote by Mike and Ken)
// currently germ_temp is a constant by experiment
// but the way I wrote the mdoel it should be easy to modify later

functions {
  real logis_lccdf_s(real x, real m, real s) {
    return -log1p_exp((x - m) / s);
  }
  
  // Forcing Function
  real forcing(real T, real T_min, real T_opt, real T_max, real delta) {
    if (T < T_min)
      return 0.001;
    if (T > T_max)
      return 0.001;
    
    if (T_opt > 0.5 * (T_min + T_max)) {
      real phi = (T_max - T_opt) / (T_opt - T_min);
      real gamma =   (delta * T_max + T_opt - (1 + delta) * T_min) 
      / (T_max - T_opt);
      real a = (T - T_min) / (T_opt - T_min);
      
      if (T > T_opt) {
        real b = (T_max - T) / (T_max - T_opt);
        real c = pow(b, phi);
        return (pow(a * c, gamma) + 0.001);
      } else {
        real b = (T_max - T_opt) / (T_max - T);
        real c = pow(b, -phi);
        return (pow(a * c, gamma) + 0.001);
      }
    } else {
      real phi = (T_opt - T_min) / (T_max - T_opt);
      real gamma =   ((1 + delta) * T_max - T_opt - delta * T_min) 
      / (T_opt - T_min);
      real b = (T_max - T) / (T_max - T_opt);
      
      if (T < T_opt) {
        real a = (T - T_min) / (T_opt - T_min);
        real c = pow(a, phi);
        return (pow(c * b, gamma) + 0.001);
      } else {
        real a = (T_opt - T_min) / (T - T_min);
        real c = pow(a, -phi);
        return (pow(c * b, gamma) + 0.001);
      }
    }
  }
  
  // Log Forcing Function
  real log_forcing(real T, real T_min, real T_opt, real T_max, real delta) {
    if (T < T_min)
      return log(0.001);
    if (T > T_max)
      return log(0.001);
    
    if (T_opt > 0.5 * (T_min + T_max)) {
      real gamma =   (delta * T_max + T_opt - (1 + delta) * T_min) 
      / (T_max - T_opt);
      real a = (T - T_min) / (T_opt - T_min);
      real b = (T_max - T_opt) / (T_opt - T_min);
      real c = (T_max - T) / (T_max - T_opt);
      return log(pow(a * pow(c, b), gamma) + 0.001);
    } else {
      real gamma =   ((1 + delta) * T_max - T_opt - delta * T_min) 
      / (T_opt - T_min);
      real a = (T - T_min) / (T_opt - T_min);
      real b = (T_opt - T_min) / (T_max - T_opt);
      real c = (T_max - T) / (T_max - T_opt);
      return log(pow(pow(a, b) * c, gamma) + 0.001);
    }
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

transformed data {
  
   real<lower=0> delta = 0.4;
  
}

parameters {
  
  // forcing function parameters (WE curve)
  real T_min;
  real<lower=T_min> T_max; 
  // real<lower=0> delta;
  real logit_x_opt; // logit relative temperature with maximal forcing mean (unitless)
  
  // seed viability
  real<lower=0, upper=1> pv;   
  
  // phenological transition parameters
  real log_Psi0; // log phenology threshold (log forcing units)
  real log_sigma; // log phenology threshold scale (log forcing units)
  
}


transformed parameters {
  real Psi0 = exp(log_Psi0);
  real sigma = exp(log_sigma);
  
  
  real T_opt = (T_max - T_min) * inv_logit(logit_x_opt) + T_min;
}


model {
  
  T_min ~ normal(0, 2.5/2.32);
  T_max ~ normal(40, 2.5/2.32);
  logit_x_opt ~ normal(0.89, 0.94/2.32);
  
  log_Psi0 ~ normal(3, 0.5);
  log_sigma ~ normal(2, 0.25);
  
  pv ~ beta(4, 2);
  
  for(e in 1:N_exps){
    
    int start_idx = start_exp_idxs[e];
    int end_idx = end_exp_idxs[e];
    
    real constant_temp = germ_temp[e];
    
    // compute daily forcings
    int earliest_forcing = 1;
    int latest_forcing = max_days + 1;
    array[latest_forcing] real daily_forcings;
    for(day in earliest_forcing:latest_forcing){
      daily_forcings[day] = forcing(constant_temp, T_min, T_opt, T_max, delta);
    }
    
    // germinated seeds
    if (N_obs[e] > 0){
      for(obs in 1:N_obs[e]){
        array[N_obs[e]] int local_days = germ_days[start_idx:end_idx];
        real Psi = sum(daily_forcings[1:local_days[obs]]);
        real log_dPsidt = log_forcing(constant_temp, T_min, T_opt, T_max, delta);
        
        target += log(pv) + logistic_lpdf(Psi | Psi0, sigma)  + log_dPsidt;
        
      }
    }
    
    // ungerminated seeds
    if (N_ungerm[e] > 0){
      real Psi_last = sum(daily_forcings[1:latest_forcing]);
      target += N_ungerm[e] * log_sum_exp(log1m(pv), log(pv) + logis_lccdf_s(Psi_last, Psi0, sigma));
    }
    
  }
  
}

generated quantities {

  array[N_exps] int N_obs_pred; // total germianted seeds
  array[N_exps] int N_ungerm_pred;
  
  array[N_exps, max_days] int dgerm_pred; // daily germination
  array[N_exps, max_days] int cumgerm_pred; // cumulative germination

  for (e in 1:N_exps) {
    
    vector[max_days + 1] fday; // daily forcing
    vector[max_days + 1] cumF; // cumulative sum of forcing

    for (d in 1:(max_days + 1))
      fday[d] = forcing(germ_temp[e], T_min, T_opt, T_max, delta);
    cumF = cumulative_sum(fday);

    N_obs_pred[e] = 0;
    N_ungerm_pred[e] = 0;
    for (day in 1:max_days)
      dgerm_pred[e, day] = 0;

    // for each seed in the experiment...
    for (s in 1:(N_obs[e] + N_ungerm[e])) {
      
      if (bernoulli_rng(pv) == 0) {
        N_ungerm_pred[e] += 1;
      } else {
        
        real l = logistic_rng(Psi0, sigma); // threshold to reach
        
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
