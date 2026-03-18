// here I just reparametrized the Gompertz function in terms of tau1 and tau50 

functions {
  real gompertz(real t, real tau1, real tau50) {
    return exp(-log(2)*(log(2)/log(100))^((t-tau50)/(tau50-tau1)));
  }
}

data {
  int<lower=1> N; // observations, including never germinated seeds
  int<lower=1> Nexps;
  int<lower=1> Nspecies;
  // int<lower=1> Nobs[Nexps]; // number of "observations" per experiment
  
  array[Nexps] int<lower=1, upper=Nspecies> species_idxs;

  // ragged array indexing for experiments
  array[Nexps] int<lower=1, upper=N> exp_start_idxs;
  array[Nexps] int<lower=1, upper=N> exp_end_idxs;

  array[N] int<lower=0> seeds; // number of seeds
  
  vector<lower=0>[N] d; // time of observations (days)
}

transformed data {
  real chill0 = 1;
}

parameters {
  
  real mu_pv;
  real<lower=0> tau_pv;
  vector[Nexps] alpha_pv_tilde;
  
  real mu_tau50;
  real<lower=0> tau_tau50;
  vector[Nexps] tau50_tilde;
  
  real mu_deltatau; // tau50-tau1
  real<lower=0> tau_deltatau;
  vector[Nexps] alpha_deltatau_tilde;
}

transformed parameters {
  
  vector[Nexps] alpha_pv = mu_pv + tau_pv*alpha_pv_tilde;
  vector[Nexps] pv = inv_logit(alpha_pv);
  
  vector[Nexps] alpha_tau50 = mu_tau50 + tau_tau50*tau50_tilde;
  vector[Nexps] tau50 = exp(alpha_tau50);
  
  vector[Nexps] alpha_deltatau = mu_deltatau + tau_deltatau*alpha_deltatau_tilde;
  vector[Nexps] tau1;
  
  vector[N] pg; //germination prob
  
  // for(sp in 1:Nspecies){
  //   tau1[sp] = tau50[sp] - exp(alpha_deltatau[sp]);
  // }
  
  for(e in 1:Nexps){
    
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    int sp = species_idxs[e];
    
    tau1[e] = tau50[e] - exp(alpha_deltatau[e]);
    
    pg[start] = (1-pv[e])+pv[e]*(1-gompertz(d[end], tau1[e], tau50[e]));
    
    for (t in (start+1):end) {
      pg[t] = pv[e]*(gompertz(d[t], tau1[e], tau50[e]) - gompertz(d[t-1], tau1[e], tau50[e]));
    }
    pg[start:end] =  pg[start:end] + 1e-12;
    pg[start:end] = pg[start:end] / sum(pg[start:end]); //  for stability

  }

}

model {
  
  mu_pv ~ normal(0.5,0.5);
  tau_pv ~ normal(0.5, 0.5); // I expect variation across experiments...
  alpha_pv_tilde ~ normal(0,1);
  
  mu_tau50 ~ normal(log(20), 1);
  tau_tau50 ~ normal(0, 0.5);
  tau50_tilde ~ normal(0,1);

  mu_deltatau ~ normal(log(7), 0.7);  
  tau_deltatau ~ normal(0, 0.5);
  alpha_deltatau_tilde ~ normal(0,1);
  
  for(e in 1:Nexps){
    
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    int species = species_idxs[e];
  
    // likelihood
    seeds[start:end] ~ multinomial(pg[start:end]);
    
  }
  
}

