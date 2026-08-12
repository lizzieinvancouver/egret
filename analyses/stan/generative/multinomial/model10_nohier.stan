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
  vector<lower = 0, upper = 1>[Nexps] pv;
  vector[Nexps] log_tau25;
  vector[Nexps] log_spread;
}

transformed parameters {

  // vector[Nexps] pv = inv_logit(logit_pv);
  vector[Nexps] tau25 = exp(log_tau25);
  vector[Nexps] tau75 = tau25 + 1 + exp(log_spread);

  vector[N] pg;

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
  
  // logit_pv ~ normal(1, 2);
  log_tau25 ~ normal(log(50), log(20)/2.57);
  log_spread ~ normal(log(10), log(200)/2.57);

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    seeds[start:end] ~ multinomial(pg[start:end]);
  }
}

// 
// generated quantities {
//   
//   array[N] int<lower=0> y_pred;
//   array[N] int<lower=0> cumy_pred;
// 
//   for (e in 1:Nexps) {
//     int start = exp_start_idxs[e];
//     int end = exp_end_idxs[e];
//     y_pred[start:end] = multinomial_rng(pg[start:end], sum(seeds[start:end]));
// 
//     cumy_pred[start] = 0;
//     cumy_pred[(start+1):end] = cumulative_sum(y_pred[(start+1):end]);
//   }
//   
//   array[Nexps,(N_newdays+1)] int y_pred_new;      
//   array[Nexps,(N_newdays+1)] int cumy_pred_new;
//   vector[(N_newdays+1)] grid_new = linspaced_vector(N_newdays+1, 0, N_newdays);
// 
//   for (e in 1:Nexps) {
//     int start = exp_start_idxs[e];
//     int end   = exp_end_idxs[e];
// 
//     vector[(N_newdays+1)] g;
//     for (t in 1:(N_newdays+1)) g[t] = gompertz(grid_new[t], tau25[e], tau75[e]);
// 
//     vector[(N_newdays+1)] pg_new;
//     pg_new[1] = (1 - pv[e]) + pv[e] * (1 - g[(N_newdays+1)]); 
//     
//     for (t in 1:N_newdays)
//       pg_new[t+1] = pv[e] * (g[t+1] - g[t]);
// 
//     pg_new = pg_new + 1e-12;
//     pg_new = pg_new / sum(pg_new);
// 
//     y_pred_new[e,] = multinomial_rng(pg_new, sum(seeds[start:end]));
// 
//     cumy_pred_new[e,1] = 0;                                    
//     cumy_pred_new[e,2:(N_newdays+1)] = cumulative_sum(y_pred_new[e,2:(N_newdays+1)]);  
//   }
// 
// }


