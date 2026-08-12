functions {
  real gompertz(real t, real tau50, real delta_595) {
    real log_b = (log(-log(0.95)) - log(-log(0.05))) / delta_595;
    real log_a = log(log(2)) - log_b * tau50;
    real p = exp(-exp(log_a + log_b * t));
    return p;
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
  
  corr_matrix[Nspecies] Vphy; // phylogenetic relationship matrix (fixed)
}

transformed data{
  
  vector<lower=0>[N] dsf = d;
  
  for(e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    
    dsf[(start+1):end] = dsf[(start+1):end] + 10; // shift by 10 days (except the start id, which correspond to 0)
    
    // if(e == 447){
    //   print(dsf[start:end]);
    // }
  }
  
}

parameters {
  real mu_pv;
  real<lower=0> tau_pv_species;
  real<lower=0, upper=1> lambda_pv_species;
  vector[Nspecies] mu_pv_species;
  real<lower=0> tau_pv_exp;
  vector[Nexps] alpha_pv_tilde;
  
  real mu_tau50;
  real<lower=0> tau_tau50_species;
  real<lower=0, upper=1> lambda_tau50_species;
  vector[Nspecies] mu_tau50_species_tilde;
  real<lower=0> tau_tau50_exp;
  vector[Nexps] alpha_tau50_tilde;
  
  real mu_logratio;
  real<lower=0> tau_logratio_species;
  real<lower=0, upper=1> lambda_logratio_species;
  vector[Nspecies] mu_logratio_species_tilde;
  real<lower=0> tau_logratio_exp;
  vector[Nexps] alpha_logratio_tilde;
}

transformed parameters {
  // vector[Nspecies] mu_pv_species;
  vector[Nspecies] mu_tau50_species;
  vector[Nspecies] mu_logratio_species;
  vector[Nexps] log_tau50;
  vector[Nexps] tau50;
  vector[Nexps] log_delta;
  vector[Nexps] delta_595;
  vector[Nexps] alpha_pv;
  vector[Nexps] pv;
  vector[N] pg;
  
  // mu_pv_species = mu_pv + tau_pv_species * mu_pv_species_tilde;
  matrix[Nspecies,Nspecies] C_tau50_species = lambda_tau50_species * Vphy;
  C_tau50_species = C_tau50_species - diag_matrix(diagonal(C_tau50_species)) + diag_matrix(diagonal(Vphy));
  matrix[Nspecies,Nspecies] L_tau50_species = cholesky_decompose(tau_tau50_species^2*C_tau50_species);
  mu_tau50_species = mu_tau50 + L_tau50_species * mu_tau50_species_tilde;
  
  matrix[Nspecies,Nspecies] C_logratio_species = lambda_logratio_species * Vphy;
  C_logratio_species = C_logratio_species - diag_matrix(diagonal(C_logratio_species)) + diag_matrix(diagonal(Vphy));
  matrix[Nspecies,Nspecies] L_logratio_species = cholesky_decompose(tau_logratio_species^2*C_logratio_species);
  mu_logratio_species = mu_logratio + L_logratio_species * mu_logratio_species_tilde;
  
  for(e in 1:Nexps) {
    alpha_pv[e] = mu_pv_species[species_idxs[e]] + tau_pv_exp * alpha_pv_tilde[e];
    log_tau50[e] = mu_tau50_species[species_idxs[e]] + tau_tau50_exp * alpha_tau50_tilde[e];
    log_delta[e] = log_tau50[e] + mu_logratio_species[species_idxs[e]] + tau_logratio_exp * alpha_logratio_tilde[e];
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
    pg[start:end] = pg[start:end] + 1e-12;
    pg[start:end] = pg[start:end] / sum(pg[start:end]);
  }
}
model {
  mu_pv ~ normal(0, 1);
  tau_pv_species ~ normal(0, 2);
  lambda_pv_species ~ beta(1.5, 1.5);
  matrix[Nspecies,Nspecies] C_pv_species = lambda_pv_species * Vphy;
  C_pv_species = C_pv_species - diag_matrix(diagonal(C_pv_species)) + diag_matrix(diagonal(Vphy));
  matrix[Nspecies,Nspecies] L_pv_species = cholesky_decompose(tau_pv_species^2*C_pv_species);
  mu_pv_species ~ multi_normal_cholesky(rep_vector(mu_pv,Nspecies), L_pv_species); 
  tau_pv_exp ~ normal(0, 1); 
  alpha_pv_tilde ~ normal(0, 1);
  
  mu_tau50 ~ normal(log(50), log(10)/2.57); // was log(20) before, but with the 10-day shift...
  tau_tau50_species ~ normal(0, 2);
  lambda_pv_species ~ beta(1.5, 1.5);
  mu_tau50_species_tilde ~ normal(0,1);
  tau_tau50_exp ~ normal(0, 1); // idem, normal(0,0.5) before
  alpha_tau50_tilde ~ normal(0, 1);
  
  mu_logratio ~ normal(log(0.4), 0.5);
  tau_logratio_species ~ normal(0, 2); // normal(0,0.5) before
  lambda_logratio_species ~ beta(1.5, 1.5);
  mu_logratio_species_tilde ~ normal(0,1);
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
