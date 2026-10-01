
functions{
  
  // Forcing stimulus
  real log_forcing_stim(real T, real T_opt, real width) {
    return (-(T - T_opt)^2 / (2 * width^2));
  }
  
  // Chilling stimulus
  real chilling_stim(real T, real T_ref_chill){
    return(1 / (1 + exp((T - T_ref_chill) / 1.5)));
  }
  
  // Germination logCDF function
  real G_lcdf(
    real t, real d_chill, 
    real T_chill, real T_ref_chill, real kappa,
    real T_incub, real T_opt, real width,
    real mu, real sigma){
      
      if (t <= 0){
        return(negative_infinity()); // we assume germ. before start of exp. is impossible
      }
      
      real chilling = chilling_stim(T_chill, T_ref_chill) * fmin(t, d_chill) 
      + chilling_stim(T_incub, T_ref_chill) * fmax(0, t-d_chill);
      real log_forcing;
      
      if(t <= d_chill){
        log_forcing = log_forcing_stim(T_chill, T_opt, width) + log(t);
      }else{
        log_forcing = log_sum_exp(log_forcing_stim(T_chill, T_opt, width) + log(d_chill), 
          log_forcing_stim(T_incub, T_opt, width) + log(t - d_chill));
      }
                
      return(normal_lcdf(log_forcing + kappa * chilling | mu, sigma));
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
  
  corr_matrix[N_species] C_phy; // phylogenetic relationship matrix (fixed)
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
  
}


parameters {
  
  //  log-requirement (forcing days at T_baseline)
  real mu_psi; // root value
  real<lower=0, upper=1> lambda_psi;  // phylogenetic structure        
  real<lower=0> sigma_psi; // overall "rate" of change (not a rate per se)
  vector[N_species] psi;
  
  // spread across seeds (different requirements)
  real mu_log_tau_psi; // root value
  real<lower=0, upper=1> lambda_log_tau_psi;  // phylogenetic structure        
  real<lower=0> sigma_log_tau_psi; // overall "rate" of change (not a rate per se)
  vector[N_species] log_tau_psi;
  
  // Forcing stimulus
  real mu_T_opt; // root value
  real<lower=0, upper=1> lambda_T_opt;  // phylogenetic structure        
  real<lower=0> sigma_T_opt; // overall "rate" of change (not a rate per se)
  vector[N_species] T_opt;
  
  real mu_log_width; // root value
  real<lower=0, upper=1> lambda_log_width;  // phylogenetic structure        
  real<lower=0> sigma_log_width; // overall "rate" of change (not a rate per se)
  vector[N_species] log_width;
  
  // Chilling stimulus
  real mu_log_kappa; // root value
  real<lower=0, upper=1> lambda_log_kappa;  // phylogenetic structure        
  real<lower=0> sigma_log_kappa; // overall "rate" of change (not a rate per se)
  vector[N_species] log_kappa;
  
  // Viability
  real mu_logit_pv; // root value
  real<lower=0, upper=1> lambda_logit_pv;  // phylogenetic structure        
  real<lower=0> sigma_logit_pv; // overall "rate" of change (not a rate per se)
  vector[N_species] logit_pv;
}


transformed parameters{
  
  vector[N_species] width = exp(log_width);
  vector[N_species] kappa = exp(log_kappa);
  vector[N_species] pv = inv_logit(logit_pv);
  vector[N_species] tau_psi = exp(log_tau_psi);
  
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
      T_incub, T_opt[sp_id], width[sp_id], psi[sp_id], tau_psi[sp_id]);

    logGxs_end[i] = G_lcdf(txs_ends[i] | d_chill, T_chill, T_ref_chill, kappa[sp_id],
      T_incub, T_opt[sp_id], width[sp_id], psi[sp_id], tau_psi[sp_id]);
      
  }
  
}

model{
  
  matrix[N_species, N_species] C_psi = lambda_psi * C_phy;
  C_psi = C_psi - diag_matrix(diagonal(C_psi)) + diag_matrix(diagonal(C_phy));
  matrix[N_species, N_species] L_psi = cholesky_decompose(sigma_psi^2*C_psi);
  
  matrix[N_species, N_species] C_log_tau_psi = lambda_log_tau_psi * C_phy;
  C_log_tau_psi = C_log_tau_psi - diag_matrix(diagonal(C_log_tau_psi)) + diag_matrix(diagonal(C_phy));
  matrix[N_species, N_species] L_log_tau_psi = cholesky_decompose(sigma_log_tau_psi^2*C_log_tau_psi);
  
  matrix[N_species, N_species] C_T_opt = lambda_T_opt * C_phy;
  C_T_opt = C_T_opt - diag_matrix(diagonal(C_T_opt)) + diag_matrix(diagonal(C_phy));
  matrix[N_species, N_species] L_T_opt = cholesky_decompose(sigma_T_opt^2*C_T_opt);
  
  matrix[N_species, N_species] C_log_width = lambda_log_width * C_phy;
  C_log_width = C_log_width - diag_matrix(diagonal(C_log_width)) + diag_matrix(diagonal(C_phy));
  matrix[N_species, N_species] L_log_width = cholesky_decompose(sigma_log_width^2*C_log_width);
  
  matrix[N_species, N_species] C_log_kappa = lambda_log_kappa * C_phy;
  C_log_kappa = C_log_kappa - diag_matrix(diagonal(C_log_kappa)) + diag_matrix(diagonal(C_phy));
  matrix[N_species, N_species] L_log_kappa = cholesky_decompose(sigma_log_kappa^2*C_log_kappa);
  
  matrix[N_species, N_species] C_logit_pv = lambda_logit_pv * C_phy;
  C_logit_pv = C_logit_pv - diag_matrix(diagonal(C_logit_pv)) + diag_matrix(diagonal(C_phy));
  matrix[N_species, N_species] L_logit_pv = cholesky_decompose(sigma_logit_pv^2*C_logit_pv);

  mu_psi ~ normal(3.68, 0.49);
  sigma_psi ~ normal(0, 0.84);
  lambda_psi ~ beta(1.5, 1.5);
  psi ~ multi_normal_cholesky(rep_vector(mu_psi, N_species), L_psi); 
  
  mu_log_tau_psi ~ normal(0.29, 0.47);
  sigma_log_tau_psi ~ normal(0, 0.1);
  lambda_log_tau_psi ~ beta(1.5, 1.5);
  log_tau_psi ~ multi_normal_cholesky(rep_vector(mu_log_tau_psi, N_species), L_log_tau_psi); 
  
  mu_T_opt ~ normal(30, 15/2.57);
  sigma_T_opt ~ normal(5, 5/2.57);
  lambda_T_opt ~ beta(1.5, 1.5);
  T_opt ~ multi_normal_cholesky(rep_vector(mu_T_opt, N_species), L_T_opt); 
  
  mu_log_width ~ normal(log(20), 0.25);
  sigma_log_width ~ normal(0, 0.4);
  lambda_log_width ~ beta(1.5, 1.5);
  log_width ~ multi_normal_cholesky(rep_vector(mu_log_width, N_species), L_log_width); 
  
  mu_log_kappa ~ normal(0, log(0.25)/2.57);
  sigma_log_kappa ~ normal(0, log(0.25)/2.57);
  lambda_log_kappa ~ beta(1.5, 1.5);
  log_kappa ~ multi_normal_cholesky(rep_vector(mu_log_kappa, N_species), L_log_kappa); 
  
  mu_logit_pv ~ normal(1.065, 1.186);
  sigma_logit_pv ~ normal(0, 0.5);
  lambda_logit_pv ~ beta(1.5, 1.5);
  logit_pv ~ multi_normal_cholesky(rep_vector(mu_logit_pv, N_species), L_logit_pv); 

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



