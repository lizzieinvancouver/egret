functions {
  real gompertz(real t, real tau50, real delta_595) {
    real log_b = (log(-log(0.95)) - log(-log(0.05))) / delta_595;
    real log_a = log(log(2)) - log_b * tau50;
    real p = exp(-exp(log_a + log_b * t));
    return fmin(p, 1e-6);
  }
}
data {
  int<lower=1> N;
  int<lower=1> Nexps;

  array[Nexps] int<lower=1, upper=N> exp_start_idxs;
  array[Nexps] int<lower=1, upper=N> exp_end_idxs;
  array[N] int<lower=0> seeds;
  
  vector<lower=0>[N] d;
}

transformed data{
  
  vector<lower=0>[N] dsf = d;
  
  for(e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    
    dsf[(start+1):end] = dsf[(start+1):end] + 20; // shift by 20 days (except the start id, which correspond to 0)
    
    // if(e == 447){
    //   print(dsf[start:end]);
    // }
  }
  
}

parameters {
  real mu_pv;
  real<lower=0> tau_pv_exp;
  vector[Nexps] alpha_pv_tilde;
  
  real mu_tau50;
  real<lower=0> tau_tau50_exp;
  vector[Nexps] alpha_tau50_tilde;
  
  real mu_logratio;
  real<lower=0> tau_logratio_exp;
  vector[Nexps] alpha_logratio_tilde;
}

transformed parameters {
  vector[Nexps] log_tau50;
  vector[Nexps] tau50;
  vector[Nexps] log_delta;
  vector[Nexps] delta_595;
  vector[Nexps] alpha_pv;
  vector[Nexps] pv;
  vector[N] pg;
  
  for(e in 1:Nexps) {
    alpha_pv[e] = mu_pv + tau_pv_exp * alpha_pv_tilde[e];
    log_tau50[e] = mu_tau50 + tau_tau50_exp * alpha_tau50_tilde[e];
    log_delta[e] = log_tau50[e] + mu_logratio + tau_logratio_exp * alpha_logratio_tilde[e];
  }
  
  pv = inv_logit(alpha_pv);
  tau50 = exp(log_tau50);
  delta_595 = exp(log_delta);
  
  for(e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    
    pg[start] = (1 - pv[e]) + pv[e] * (1 - gompertz(dsf[end], tau50[e], delta_595[e]));
    
    for (t in (start+1):end) {
      pg[t] = pv[e] * (gompertz(dsf[t], tau50[e], delta_595[e]) - gompertz(dsf[t-1], tau50[e], delta_595[e]));
    }
    pg[start:end] = pg[start:end] + 1e-6;
    pg[start:end] = pg[start:end] / sum(pg[start:end]);
  }
}
model {
  mu_pv ~ normal(0, 1);
  tau_pv_exp ~ normal(0, 1); // I was expecting something < tau_pv_species before, but...
  alpha_pv_tilde ~ normal(0, 1);
  
  mu_tau50 ~ normal(log(60), log(20)/2.57); 
  tau_tau50_exp ~ normal(0, 1); // idem, normal(0,0.5) before
  alpha_tau50_tilde ~ normal(0, 1);
  
  mu_logratio ~ normal(log(0.4), 0.5);
  tau_logratio_exp ~ normal(0, 1); // normal(0,0.3) before
  alpha_logratio_tilde ~ normal(0, 1);
  
  for(e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
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
