functions {
  real gompertz(real t, real tau50, real delta_595) {
    real log_b = (log(-log(0.95)) - log(-log(0.05))) / delta_595;
    real a = log(2) * exp(-log_b * tau50);
    return exp(-a * exp(log_b * t));
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
}
parameters {
  real mu_pv;
  real<lower=0> tau_pv;
  vector[Nexps] alpha_pv_tilde;
  
  real mu_tau50;
  real<lower=0> tau_tau50;
  vector[Nexps] alpha_tau50_tilde;
  
  real mu_logratio;
  real<lower=0> tau_logratio;
  vector[Nexps] alpha_logratio_tilde;
}
transformed parameters {
  vector[Nexps] alpha_pv = mu_pv + tau_pv * alpha_pv_tilde;
  vector[Nexps] pv = inv_logit(alpha_pv);
  
  vector[Nexps] log_tau50 = mu_tau50 + tau_tau50 * alpha_tau50_tilde;
  vector[Nexps] tau50 = exp(log_tau50);
  
  vector[Nexps] log_delta = log_tau50 + mu_logratio + tau_logratio * alpha_logratio_tilde;
  vector[Nexps] delta_595 = exp(log_delta);
  
  vector[N] pg;
  
  for(e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    
    pg[start] = (1 - pv[e]) + pv[e] * (1 - gompertz(d[end], tau50[e], delta_595[e]));
    
    for (t in (start+1):end) {
      pg[t] = pv[e] * (gompertz(d[t], tau50[e], delta_595[e]) - gompertz(d[t-1], tau50[e], delta_595[e]));
    }
    pg[start:end] = pg[start:end] + 1e-12;
    pg[start:end] = pg[start:end] / sum(pg[start:end]);
  }
}
model {
  mu_pv ~ normal(0, 1);
  tau_pv ~ normal(0.5, 0.5);
  alpha_pv_tilde ~ normal(0, 1);
  
  mu_tau50 ~ normal(log(20), 1);
  tau_tau50 ~ normal(0, 1);
  alpha_tau50_tilde ~ normal(0, 1);
  
  mu_logratio ~ normal(log(0.4), 0.5);
  tau_logratio ~ normal(0, 0.5);
  alpha_logratio_tilde ~ normal(0, 1);
  
  for(e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    seeds[start:end] ~ multinomial(pg[start:end]);
  }
}
