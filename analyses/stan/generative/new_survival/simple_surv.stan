
functions{
  
  // Forcing stimulus
  real forcing_stim(real T, real T_ref_forc, real beta, real beta2){
    return(exp(beta * (T - T_ref_forc) + beta2 * (T - T_ref_forc)^2));
  }
  
  // Chilling stimulus
  real chilling_stim(real T, real T_ref_chill){
    return(1 / (1 + exp((T - T_ref_chill) / 1.5)));
  }
  
  // Germination logCDF function
  real G_lcdf(
    real t, real d_chill, 
    real T_chill, real T_ref_chill, real kappa,
    real T_incub, real T_ref_forc, real beta, real beta2,
    real mu, real sigma){
      
      real chilling = chilling_stim(T_chill, T_ref_chill) * fmin(t, d_chill) 
      + chilling_stim(T_incub, T_ref_chill) * fmax(0, t-d_chill);
      real forcing = forcing_stim(T_chill, T_ref_forc, beta, beta2) * fmin(t, d_chill) 
      + forcing_stim(T_incub, T_ref_forc, beta, beta2) * fmax(0, t-d_chill);
                
      return(normal_lcdf(log(forcing) + kappa * chilling | mu, sigma));
  }
  
}


data {
  
  int<lower=1> N_exps;
  
  
  array[N_exps] int<lower=1> N_seeds;
  vector<lower=0>[N_exps] dxs_chill;
  vector[N_exps] Txs_chill;
  vector[N_exps] Txs_incub;
  
  int<lower=1> N_census;
  array[N_census] int<lower=1, upper=N_exps> exp_idxs;
  vector<lower=0>[N_census] txs_start;
  vector<lower=0>[N_census] txs_ends;
  array[N_census] int<lower=0> N_germ;
}


transformed data {
  
  // End of each experiment (i.e. we stop observation), and seeds still ungerminated at this moment
  array[N_exps] int last_obs_days = rep_array(0, N_exps);
  array[N_exps] int N_ungerm = N_seeds;
  
  for (i in 1:N_census) {
    int exp_id = exp_idxs[i];
    last_obs_days[exp_id] = i;
    N_ungerm[exp_id] -= N_germ[i];
  }
  
  real T_ref_chill = 10;
  real T_ref_forc = 15;
  
}


parameters {
  
  
  real mu; //  log-requirement (forcing days at T_baseline)
  real<lower=0> sigma; // spread across seeds (different requirements)
  
  // Forcing stimulus
  real beta;
  real<upper=0> beta2;
  
  // Chilling stimulus
  real<lower=0> kappa; //  how much one day of chilling reduces the requirement
  
  // Viability
  real<lower=0, upper=1>pv;

}


transformed parameters {
  
  // germination CDF at start and end of census
  vector[N_census] logGxs_start;
  vector[N_census] logGxs_end;

  
  for (i in 1:N_census) {

    int exp_id = exp_idxs[i];

    real T_chill = Txs_chill[exp_id];
    real T_incub = Txs_incub[exp_id];
    real d_chill = dxs_chill[exp_id];

    logGxs_start[i] = G_lcdf(txs_start[i] | d_chill, T_chill, T_ref_chill, kappa,
      T_incub, T_ref_forc, beta, beta2, mu, sigma);

    logGxs_end[i] = G_lcdf(txs_ends[i] | d_chill, T_chill, T_ref_chill, kappa,
      T_incub, T_ref_forc, beta, beta2, mu, sigma);
      
  }
  
}

model {

  mu ~ normal(6, 2);
  sigma ~ normal(0, 1);
  
  beta ~ normal(0, 0.5/2.57);
  beta2 ~ normal(0, 0.05/2.57);
  
  kappa ~ normal(0, 0.25/2.57);
  
   pv ~ beta(4, 2);

  // Germination observed in each census
  for (i in 1:N_census) {
    if (N_germ[i] > 0) {
      real logpg = log(pv) + log_diff_exp(logGxs_end[i], logGxs_start[i]);
      target += N_germ[i] * logpg;
    }
  }

  // Seeds not germinated, either not viable and right-censored
  for (e in 1:N_exps) {
    if (N_ungerm[e] > 0) {
      target += N_ungerm[e] * log_sum_exp(log1m(pv), log(pv) + log1m_exp(logGxs_end[last_obs_days[e]]));
    }
  }
}
