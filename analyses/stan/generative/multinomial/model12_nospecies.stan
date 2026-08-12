functions {
  real gompertz(real t, real tau25, real tau75) {
    real log_b = (log(-log(0.75)) - log(-log(0.25))) / (tau75 - tau25);
    real log_a = log(-log(0.25)) - log_b * tau25;
    return exp(-exp(log_a + log_b * t));
  }
}

data {
  int<lower=1> N;
  int<lower=1> Nexps;

  array[Nexps] int<lower=1, upper=N> exp_start_idxs;
  array[Nexps] int<lower=1, upper=N> exp_end_idxs;
  array[N] int<lower=0> seeds;

  vector<lower=0>[N] d;
  
  vector[Nexps] chill;
  vector[Nexps] forcing;
  
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
  real beta_chill_pv;
  real beta_chill2_pv;
  real beta_forc_pv;
  real beta_forc2_pv;
  real beta_chillforc_pv;
  
  real mu_log_tau25;
  real beta_chill_tau25;
  real beta_chill2_tau25;
  real beta_forc_tau25;
  real beta_forc2_tau25;
  real beta_chillforc_tau25;

  real mu_log_spread;
  real beta_chill_spread;
  real beta_chill2_spread;
  real beta_forc_spread;
  real beta_forc2_spread;
  real beta_chillforc_spread;

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
    logit_pv[e] = mu_pv + beta_chill_pv * (chill[e]-7) + beta_forc_pv * (forcing[e]-1.5) 
    + beta_chill2_pv* (chill[e]-7)^2  + beta_forc2_pv * (forcing[e]-1.5)^2 
    + beta_chillforc_pv * (forcing[e]-1.5) * (chill[e]-7);
    
    log_tau25[e] = mu_log_tau25 + beta_chill_tau25 * (chill[e]-7) + beta_forc_tau25 * (forcing[e]-1.5) 
    + beta_chill2_tau25 * (chill[e]-7)^2  + beta_forc2_tau25 * (forcing[e]-1.5)^2 
    + beta_chillforc_tau25 * (forcing[e]-1.5) * (chill[e]-7);
    
    
    log_spread[e] = mu_log_spread + beta_chill_spread * (chill[e]-7) + beta_forc_spread * (forcing[e]-1.5)
    + beta_chill2_spread * (chill[e]-7)^2  + beta_forc2_spread * (forcing[e]-1.5)^2 
    + beta_chillforc_spread * (forcing[e]-1.5) * (chill[e]-7);
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
  
  mu_pv ~ normal(2.5, 1.28); // between 50% and 99% 
  beta_chill_pv      ~ normal(0, 0.15);
  beta_forc_pv       ~ normal(0, 0.15);
  beta_chillforc_pv  ~ normal(0, 0.05);
  beta_chill2_pv     ~ normal(0, 0.03);
  beta_forc2_pv      ~ normal(0, 0.03);

  mu_log_tau25 ~ normal(4.35, 0.13); // between log(50+10) and log(50+50) (time is shifted by 50 days)
  beta_chill_tau25      ~ normal(0, 0.07);
  beta_forc_tau25       ~ normal(0, 0.07);
  beta_chillforc_tau25  ~ normal(0, 0.03);
  beta_chill2_tau25     ~ normal(0, 0.02);
  beta_forc2_tau25      ~ normal(0, 0.02);

  mu_log_spread ~ normal(3.10, 0.41); // between log(10) and log(50)
  beta_chill_spread      ~ normal(0, 0.07);
  beta_forc_spread       ~ normal(0, 0.07);
  beta_chillforc_spread  ~ normal(0, 0.03);
  beta_chill2_spread     ~ normal(0, 0.02);
  beta_forc2_spread      ~ normal(0, 0.02);
  
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

