
functions{
  
  // Forcing stimulus
  real forcing_stim(real T, real T_ref_forc, real beta, real beta2){
    real T_opt = T_ref_forc - beta / (2 * beta2);
  
    return(exp(beta * (T - T_ref_forc) + beta2 * (T - T_ref_forc)^2 
    -beta * (T_opt - T_ref_forc) - beta2 * (T_opt - T_ref_forc)^2));
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
      
      if (t <= 0){
        return(negative_infinity()); // we assume germ. before start of exp. is impossible
      }
      
      real chilling = chilling_stim(T_chill, T_ref_chill) * fmin(t, d_chill) 
      + chilling_stim(T_incub, T_ref_chill) * fmax(0, t-d_chill);
      real forcing = forcing_stim(T_chill, T_ref_forc, beta, beta2) * fmin(t, d_chill) 
      + forcing_stim(T_incub, T_ref_forc, beta, beta2) * fmax(0, t-d_chill);
                
      return(normal_lcdf(log(forcing) + kappa * chilling | mu, sigma));
  }
  
}


data{
  
  int<lower=1> N_species;
  int<lower=1> N_exps;
  array[N_exps] int<lower=1, upper=N_species> species_idxs;
  int<lower=1> N_census;
  array[N_exps] int<lower=1, upper = N_census> N_census_perexp;
  array[N_exps] int<lower=1, upper = N_census> start_census_idxs;
  array[N_exps] int<lower=1, upper = N_census> end_census_idxs;
  
  array[N_exps] int<lower=1> N_seeds;
  vector<lower=0>[N_exps] dxs_chill;
  vector[N_exps] Txs_chill;
  vector[N_exps] Txs_incub;
  
  
  array[N_census] int<lower=1, upper=N_exps> exp_idxs;
  vector<lower=0>[N_census] txs_start;
  vector<lower=0>[N_census] txs_ends;
  array[N_census] int<lower=0> N_germ;
}


transformed data{
  
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
  
  
  vector[N_species] mu; //  log-requirement (forcing days at T_baseline)
  vector<lower=0>[N_species] sigma; // spread across seeds (different requirements)
  
  // Forcing stimulus
  vector[N_species] beta;
  vector<upper=0>[N_species] beta2;
  
  // Chilling stimulus
  vector<lower=0>[N_species] kappa; // how much one day of chilling reduces the log-requirement
  
  // Viability
  vector<lower=0, upper=1>[N_species]pv;

}


transformed parameters{
  
  // germination CDF at start and end of census
  vector[N_census] logGxs_start;
  vector[N_census] logGxs_end;

  
  for (i in 1:N_census) {
    
    int exp_id = exp_idxs[i];
    int sp_id = species_idxs[exp_id];

    real T_chill = Txs_chill[exp_id];
    real T_incub = Txs_incub[exp_id];
    real d_chill = dxs_chill[exp_id];

    logGxs_start[i] = G_lcdf(txs_start[i] | d_chill, T_chill, T_ref_chill, kappa[sp_id],
      T_incub, T_ref_forc, beta[sp_id], beta2[sp_id], mu[sp_id], sigma[sp_id]);

    logGxs_end[i] = G_lcdf(txs_ends[i] | d_chill, T_chill, T_ref_chill, kappa[sp_id],
      T_incub, T_ref_forc, beta[sp_id], beta2[sp_id], mu[sp_id], sigma[sp_id]);
      
  }
  
}

model{

  mu ~ normal(6, 2);
  sigma ~ normal(0, 1);
  
  beta ~ normal(0, 0.5/2.57);
  beta2 ~ normal(0, 0.05/2.57);
  
  kappa ~ normal(0, 0.1/2.57);
  
   pv ~ beta(4, 2);

  // Germination observed in each census
  for(i in 1:N_census){
    int exp_id = exp_idxs[i];
    int sp_id = species_idxs[exp_id];
    if (N_germ[i] > 0) {
      real logpg = log(pv[sp_id]) + log_diff_exp(logGxs_end[i], logGxs_start[i]);
      target += N_germ[i] * logpg;
    }
  }

  // Seeds not germinated, either not viable and right-censored
  for(e in 1:N_exps){
    int sp_id = species_idxs[e];
    if (N_ungerm[e] > 0) {
      target += N_ungerm[e] * log_sum_exp(log1m(pv[sp_id]), log(pv[sp_id]) + log1m_exp(logGxs_end[last_obs_days[e]]));
    }
  }
  
}

generated quantities{
  
  vector[N_census] pG;
  array[N_census] int<lower=0> N_germ_pred;
  
  for (i in 1:N_census) {
    real logpg = log_diff_exp(logGxs_end[i], logGxs_start[i]);
    pG[i] = exp(logpg);
  }
  
  for(e in 1:N_exps){
    
    int start = start_census_idxs[e];
    int end = end_census_idxs[e];
    
    int sp_id = species_idxs[e];
    
    array[N_census_perexp[e] + 1] real theta;
    theta[1] = 1 - pv[sp_id] * sum(pG[start:end]); // "ungerminated" probability
    for (i in 1:N_census_perexp[e]) {
      theta[i + 1] = pv[sp_id] * pG[start + i - 1];
    }
    
    array[N_census_perexp[e] + 1] int N_ungerm_germ = multinomial_rng(to_vector(theta), N_seeds[e]);
    N_germ_pred[start:end] = N_ungerm_germ[2:(N_census_perexp[e] + 1)];
    
  }
  
}



