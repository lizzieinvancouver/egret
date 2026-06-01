functions {
  real gompertz(real t, real tau25, real tau75) {
    real log_b = (log(-log(0.75)) - log(-log(0.25))) / (tau75 - tau25);
    real log_a = log(-log(0.75)) - log_b * tau25;
    return exp(-exp(log_a + log_b * t));
  }
}

data {
  int<lower=1> N;
  int<lower=1> Nexps;
  
  int<lower=1> Nspecies;
  array[Nexps] int<lower=1, upper=Nspecies> species_idxs;

  array[Nexps] int<lower=1, upper=N> exp_start_idxs;
  array[Nexps] int<lower=1, upper=N> exp_end_idxs;
  array[N] int<lower=0> seeds;

  vector<lower=0>[N] d;
  
  int<lower=1> N_newdays;
}

transformed data {
  vector<lower=0>[N] dsf = d;

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    dsf[(start+1):end] = dsf[(start+1):end] + 50;
  }
}

parameters {
  real mu_pv;
  real<lower=0> sigma_pv_species;
  vector[Nspecies] logit_pv_species_tilde;
  real<lower=0> sigma_pv_exp;
  vector[Nexps] logit_pv_tilde;

  real mu_log_tau25;
  real<lower=0> sigma_log_tau25_species;
  vector[Nspecies] log_tau25_species;
  real<lower=0> sigma_log_tau25_exp;
  vector[Nexps] log_tau25_tilde;

  real mu_log_spread;
  real<lower=0> sigma_log_spread_species;
  vector[Nspecies] log_spread_species_tilde;
  real<lower=0> sigma_log_spread_exp;
  vector[Nexps] log_spread_tilde;
}

transformed parameters {
  vector[Nexps] logit_pv;
  vector[Nexps] pv;
  vector[Nexps] log_tau25;
  vector[Nexps] log_spread;
  vector[Nexps] tau25;
  vector[Nexps] tau75;
  vector[N] pg;

  for (e in 1:Nexps) {
    logit_pv[e] = mu_pv + sigma_pv_species * logit_pv_species_tilde[species_idxs[e]] 
      + sigma_pv_exp * logit_pv_tilde[e];
    log_tau25[e] = mu_log_tau25 + log_tau25_species[species_idxs[e]] 
      + sigma_log_tau25_exp * log_tau25_tilde[e];
    log_spread[e] = mu_log_spread + sigma_log_spread_species * log_spread_species_tilde[species_idxs[e]] 
      + sigma_log_spread_exp * log_spread_tilde[e];
  }

  pv = inv_logit(logit_pv);
  tau25 = exp(log_tau25);
  tau75 = tau25 + 1 + exp(log_spread);

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];

    pg[start] = (1 - pv[e]) + pv[e] * (1 - gompertz(dsf[end], tau25[e], tau75[e]));

    for (t in (start+1):end) {
      pg[t] = pv[e] * (gompertz(dsf[t], tau25[e], tau75[e]) - gompertz(dsf[t-1], tau25[e], tau75[e]));
    }
    pg[start:end] = pg[start:end] + 1e-12;
    pg[start:end] = pg[start:end] / sum(pg[start:end]);
  }
}

model {
  mu_pv ~ normal(1, 2);
  sigma_pv_species ~ normal(0, 3);
  sigma_pv_exp ~ normal(0, 3);
  logit_pv_species_tilde ~ normal(0, 1);
  logit_pv_tilde ~ normal(0, 1);

  mu_log_tau25 ~ normal(log(50), log(20)/2.57);
  sigma_log_tau25_species ~ normal(0, 3);
  sigma_log_tau25_exp ~ normal(0, 3);
  log_tau25_species ~ normal(0, sigma_log_tau25_species);
  log_tau25_tilde ~ normal(0, 1);

  mu_log_spread ~ normal(log(10), log(200)/2.57);
  sigma_log_spread_species ~ normal(0, 3);
  sigma_log_spread_exp ~ normal(0, 3);
  log_spread_species_tilde ~ normal(0, 1);
  log_spread_tilde ~ normal(0, 1);

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    seeds[start:end] ~ multinomial(pg[start:end]);
  }
}


generated quantities {
  
  array[N] int<lower=0> y_pred;
  array[N] int<lower=0> cumy_pred;

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    y_pred[start:end] = multinomial_rng(pg[start:end], sum(seeds[start:end]));

    cumy_pred[start] = 0;
    cumy_pred[(start+1):end] = cumulative_sum(y_pred[(start+1):end]);
  }
  
  array[Nexps,(N_newdays+1)] int y_pred_new;      
  array[Nexps,(N_newdays+1)] int cumy_pred_new;
  vector[(N_newdays+1)] grid_new = linspaced_vector(N_newdays+1, 0, N_newdays);

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end   = exp_end_idxs[e];

    vector[(N_newdays+1)] g;
    for (t in 1:(N_newdays+1)) g[t] = gompertz(grid_new[t], tau25[e], tau75[e]);

    vector[(N_newdays+1)] pg_new;
    pg_new[1] = (1 - pv[e]) + pv[e] * (1 - g[(N_newdays+1)]); 
    
    for (t in 1:N_newdays)
      pg_new[t+1] = pv[e] * (g[t+1] - g[t]);

    pg_new = pg_new + 1e-12;
    pg_new = pg_new / sum(pg_new);

    y_pred_new[e,] = multinomial_rng(pg_new, sum(seeds[start:end]));

    cumy_pred_new[e,1] = 0;                                    
    cumy_pred_new[e,2:(N_newdays+1)] = cumulative_sum(y_pred_new[e,2:(N_newdays+1)]);  
  }

}


