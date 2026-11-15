// Started 14 September 2026
// By Ken

// test code

functions {
  // Forcing Function
  real forcing(real T, real T_min, real T_opt, real T_max, real delta) {
    if (T < T_min)
      return 0.001;
    if (T > T_max)
      return 0.001;
    
    if (T_opt > 0.5 * (T_min + T_max)) {
      real phi = (T_max - T_opt) / (T_opt - T_min);
      real gamma =   (delta * T_max + T_opt - (1 + delta) * T_min) 
      / (T_max - T_opt);
      real a = (T - T_min) / (T_opt - T_min);
      
      if (T > T_opt) {
        real b = (T_max - T) / (T_max - T_opt);
        real c = pow(b, phi);
        return (pow(a * c, gamma) + 0.001);
      } else {
        real b = (T_max - T_opt) / (T_max - T);
        real c = pow(b, -phi);
        return (pow(a * c, gamma) + 0.001);
      }
    } else {
      real phi = (T_opt - T_min) / (T_max - T_opt);
      real gamma =   ((1 + delta) * T_max - T_opt - delta * T_min) 
      / (T_opt - T_min);
      real b = (T_max - T) / (T_max - T_opt);
      
      if (T < T_opt) {
        real a = (T - T_min) / (T_opt - T_min);
        real c = pow(a, phi);
        return (pow(c * b, gamma) + 0.001);
      } else {
        real a = (T_opt - T_min) / (T - T_min);
        real c = pow(a, -phi);
        return (pow(c * b, gamma) + 0.001);
      }
    }
  }
  
  real survival_function(real psi, real psi0, real sigma) {
    return 1 / (1 + exp(sigma * (psi - psi0)));
  }
}

data {
  int<lower=1> N_species;
  int<lower=1> N_seedlots;
  int<lower=1> N_condition_sets;
  
  array[N_condition_sets] int<lower=1, upper=N_species> species;
  array[N_condition_sets] int<lower=1, upper=N_seedlots> seedlots;
  
  int<lower=1> N_phases;
  array[N_condition_sets] int<lower=1> num_phases;
  array[N_condition_sets] int<lower=1, upper=N_phases> phase_start_idxs;
  
  array[N_phases] int<lower=1> durations;
  // array[N_phases] int<lower=1> phase_start_idxs;
  array[N_phases] real temps;
  array[N_phases] int<lower=0, upper=1> moisture;
  
  int<lower=1> N_observations;
  array[N_condition_sets] int<lower=1> num_seeds;
  array[N_condition_sets] int<lower=1> num_observations;
  array[N_condition_sets] int<lower=1, upper=N_observations> obs_start_idxs;
  array[N_observations] int<lower=1> observations;
  array[N_observations] int<lower=0> germinated_seeds;
}

parameters {
  // Germinability parameters
  array[N_seedlots] real logit_viability; // Logit viability (unitless)
  array[N_species] real xi_logit_viability;
  array[N_species] real upsilon_logit_viability;
  real nu_xi_logit_viability;
  real<lower=0> tau_xi_logit_viability;
  
  array[N_seedlots] real log_decay; // Log decay (unitless)
  array[N_species] real xi_log_decay;
  array[N_species] real upsilon_log_decay;
  real nu_xi_log_decay;
  real<lower=0> tau_xi_log_decay;
  
  array[N_species] real logit_phi_decay_d; // Logit germinability change scaling factor by dry conditions (unitless)
  real nu_logit_phi_decay_d;
  real<lower=0> tau_logit_phi_decay_d;
  array[N_species] real logit_phi_decay_m; // Logit germinability change scaling factor by moist conditions (unitless)
  real nu_logit_phi_decay_m;
  real<lower=0> tau_logit_phi_decay_m;
  
  // Dormancy breaking parameters
  array[N_species] real logit_B_0; // Logit initial dormancy breaking progress (unitless)
  real nu_logit_B_0;
  real<lower=0> tau_logit_B_0;
  
  array[N_species] real breaking; // Breaking progress with temperature (K-1)
  real nu_breaking;
  real<lower=0> tau_breaking;
  
  array[N_species] real breaking_midtemp; // Temperature at which dormancy progression response switches between breaking and forming (C)
  real nu_breaking_midtemp;
  real<lower=0> tau_breaking_midtemp;
  
  array[N_species] real logit_phi_break_d; // Dormancy breaking scaling factor by dry conditions (unitless)
  real nu_logit_phi_break_d;
  real<lower=0> tau_logit_phi_break_d;
  
  array[N_species] real logit_phi_break_m; // Dormancy breaking scaling factor by moist conditions (untiless)
  real nu_logit_phi_break_m;
  real<lower=0> tau_logit_phi_break_m;
  
  // Forcing parameters
  real T_min; // Minimum temperature for non-zero forcing (C)
  real T_max; // Maximum temperature for non-zero forcing (C)
  
  array[N_species] real<lower=0> log_delta; // Shape parameter controlling breadth of forcing function across temperature (unitless)
  real nu_log_delta;
  real<lower=0> tau_log_delta;
  
  array[N_species] real logit_x_opt; // Logit of optimal temperature along range of non-zero forcing (unitless)
  real nu_logit_x_opt;
  real<lower=0> tau_logit_x_opt;
  
  array[N_species] real logit_phi_force_d; // Forcing scaling factor by dry conditions (unitless)
  real nu_logit_phi_force_d;
  real<lower=0> tau_logit_phi_force_d;
  
  array[N_species] real logit_phi_force_m; // Forcing scaling factor by moist conditions (untiless)
  real nu_logit_phi_force_m;
  real<lower=0> tau_logit_phi_force_m;
  
  // // Phenological transition parameters
  // array[N_groups] real log_Psi0;     // Log phenology threshold (Log Forcing Units)
  // real nu_log_Psi0;            // Log phenology threshold mean (Log Forcing Units)
  // real<lower=0> tau_log_Psi0;  // Log phenology threshold sd (Log Forcing Units)
  // 
  // array[N_groups] real log_sigma;    // Log phenology threshold scale (Log Forcing Units)
  // real nu_log_sigma;           // Log phenology threshold scale mean (Log Forcing Units)
  // real<lower=0> tau_log_sigma; // Log phenology threshold scale sd (Log Forcing Units)
  // 
  // Phenological transition parameters
  array[N_species] real log_Psi0; // t50 dependent on daily forcing (log forcing units)
  array[N_species] real log_sigma; // spread, related to t75 - t25, dependent on daily forcing (log forcing units)
}

transformed parameters {
  // Germinability parameters
  array[N_seedlots] real viability = inv_logit(logit_viability);
  array[N_seedlots] real decay = exp(log_decay);
  array[N_species] real phi_decay_d = inv_logit(logit_phi_decay_d);
  array[N_species] real phi_decay_m = inv_logit(logit_phi_decay_m);
  
  // Dormancy breaking parameters
  array[N_species] real B_0 = inv_logit(logit_B_0);
  array[N_species] real phi_break_d = inv_logit(logit_phi_break_d);
  array[N_species] real phi_break_m = inv_logit(logit_phi_break_m);
  
  // Forcing parameters
  array[N_species] real delta = exp(log_delta);
  array[N_species] real x_opt = inv_logit(logit_x_opt);
  array[N_species] real phi_force_d = inv_logit(logit_phi_force_d);
  array[N_species] real phi_force_m = inv_logit(logit_phi_force_m);
  
  // Phenological transition parameters
  array[N_species] real Psi0 = exp(log_Psi0);
  array[N_species] real sigma = exp(log_sigma);
}

model {
  for(cond_set in 1:N_condition_sets){
    // Subset condition environment data
    int phase_start_idx = phase_start_idxs[cond_set];
    int phase_end_idx = phase_start_idx + num_phases[cond_set] - 1;
    
    array[num_phases[cond_set]] real phase_temps = temps[phase_start_idx:phase_end_idx];
    array[num_phases[cond_set]] int phase_durs = durations[phase_start_idx:phase_end_idx];
    array[num_phases[cond_set]] int phase_moist = moisture[phase_start_idx:phase_end_idx];
    
    // Subset condition observation data
    int obs_start_idx = obs_start_idxs[cond_set];
    int obs_end_idx = obs_start_idx + num_observations[cond_set] - 1;
    
    array[num_observations[cond_set]] int obs = observations[obs_start_idx:obs_end_idx];
    array[num_observations[cond_set]] int germ = germinated_seeds[obs_start_idx:obs_end_idx];
    
    // Make additive germinability, dormancy breaking, and forcing time series
    vector[sum(phase_durs)] log_germinability;
    vector[sum(phase_durs)] logit_dorm_break;
    vector[sum(phase_durs)] temp_forcing;
    vector[sum(phase_durs)] moisture_series;
    
    int cond_start_idx = 0;
    int cond_end_idx = 0;
    for(phase in 1:num_phases[cond_set]){
      cond_start_idx = cond_end_idx + 1;
      cond_end_idx = cond_start_idx + phase_durs[phase] - 1;
      
      real change_germ = - log(2) * (phi_decay_d[species[cond_set]] * (1 - phase_moist[phase]) + phi_decay_m[species[cond_set]] * phase_moist[phase]) / decay[seedlots[cond_set]];
      real change_dorm_break = (phi_break_d[species[cond_set]] * (1 - phase_moist[phase]) + phi_break_m[species[cond_set]] * phase_moist[phase]) * breaking[species[cond_set]] * (phase_temps[phase] - breaking_midtemp[species[cond_set]]);
      real change_temp_forcing = forcing(phase_temps[phase], T_min, x_opt[species[cond_set]] * (T_max - T_min), T_max, delta[species[cond_set]]);
      
      moisture_series[cond_start_idx:cond_end_idx] = rep_vector(phase_moist[phase], phase_durs[phase]);
      log_germinability[cond_start_idx:cond_end_idx] = rep_vector(change_germ, phase_durs[phase]);
      logit_dorm_break[cond_start_idx:cond_end_idx] = rep_vector(change_dorm_break, phase_durs[phase]);
      temp_forcing[cond_start_idx:cond_end_idx] = rep_vector(change_temp_forcing, phase_durs[phase]);
    }
    
    logit_dorm_break[1] = logit_dorm_break[1] + logit_B_0[species[cond_set]];
    
    vector[sum(phase_durs)] accum_germinability = viability[seedlots[cond_set]] * exp(cumulative_sum(log_germinability));
    vector[sum(phase_durs)] accum_dorm_break = inv_logit(cumulative_sum(logit_dorm_break));
    // vector[sum(cond_durs)] real accum_forcing = cumulative_sum(forcing);
    vector[sum(phase_durs)] hazard = (phi_force_d[species[cond_set]] .* (1 - moisture_series) + phi_force_m[species[cond_set]] .* moisture_series) .* accum_dorm_break .* temp_forcing;
    vector[sum(phase_durs)] accum_hazard = cumulative_sum(hazard);
    
    target += germ[1] * (log(accum_germinability[obs[1]]) + log1m(survival_function(accum_hazard[obs[1]], Psi0[species[cond_set]], sigma[species[cond_set]])));
    
    if(num_observations[cond_set] > 1){
      for(ob in 2:num_observations[cond_set]){
        target += germ[ob] * (log(accum_germinability[obs[ob]]) + log(survival_function(accum_hazard[obs[ob - 1]], Psi0[species[cond_set]], sigma[species[cond_set]]) - survival_function(accum_hazard[obs[ob]], Psi0[species[cond_set]], sigma[species[cond_set]])));
      }
    }
    
    target += (num_seeds[cond_set] - sum(germ)) * log_sum_exp(log1m(accum_germinability[obs[num_observations[cond_set]]]), log(accum_germinability[obs[num_observations[cond_set]]]) + log(survival_function(accum_hazard[obs[num_observations[cond_set]]], Psi0[species[cond_set]], sigma[species[cond_set]])));
  }
}
