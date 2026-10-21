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
  int<lower=1> N_conditions;
  
  array[N_conditions] int<lower=1, upper=N_species> species;
  array[N_conditions] int<lower=1, upper=N_seedlots> seedlots;
  
  int<lower=1> N_phases;
  array[N_conditions] int<lower=1> num_phases;
  array[N_conditions] int<lower=1, upper=N_phases> cond_start_idxs;
  
  array[N_phases] int<lower=1> durations;
  array[N_phases] int<lower=1> phase_start_idxs;
  array[N_phases] real temps;
  array[N_phases] int<lower=0, upper=1> moisture;
  
  int<lower=1> N_observations;
  array[N_conditions] int<lower=1> num_seeds;
  array[N_conditions] int<lower=1> num_observations;
  array[N_conditions] int<lower=1, upper=N_observations> obs_start_idxs;
  array[N_observations] int<lower=1> observations;
  array[N_observations] int<lower=0> germinated_seeds;
}

parameters {
  
  // forcing function parameters (simple logistic)
  vector<lower=0>[N_species] T0; // temperature at which the forcing function is half the optimum
  vector<lower=0>[N_species] k;  // steepness of forcing function
  
  // seed viability
  // vector<lower=0, upper=1>[N_species] pv;   
  
  // phenological transition parameters
  vector[N_species] log_Psi0; // t50 dependent on daily forcing (log forcing units)
  vector[N_species] log_sigma; // spread, related to t75 - t25, dependent on daily forcing (log forcing units)
}


transformed parameters {
  vector[N_species] Psi0 = exp(log_Psi0);
  vector[N_species] sigma = exp(log_sigma);
}


model {
  for(cond in 1:N_conditions){
    // Subset condition environment data
    int cond_start_idx = phase_start_idxs[cond];
    int cond_end_idx = phase_start_idx + num_phases[cond] - 1;
    
    array[num_phases[cond]] real cond_temps = temps[cond_start_idx:cond_end_idx];
    array[num_phases[cond]] int cond_durs = durations[cond_start_idx:cond_end_idx];
    array[num_phases[cond]] int cond_moist = moisture[cond_start_idx:cond_end_idx];
    
    // Subset condition observation data
    int obs_start_idx = obs_start_idxs[cond];
    int obs_end_idx = obs_start_idx + num_observations[cond] - 1;
    
    array[num_observations[cond]] int cond_obs = observations[obs_start_idx:obs_end_idx];
    array[num_observations[cond]] int cond_germ = germinated_seeds[obs_start_idx:obs_end_idx];
    
    // Make additive germinability, dormancy breaking, and forcing time series
    vector[sum(cond_durs)] real log_germinability;
    vector[sum(cond_durs)] real logit_dorm_break;
    vector[sum(cond_durs)] real forcing;
    vector[sum(cond_durs)] real moisture_series;
    
    int phase_start_idx = 0;
    int phase_end_idx = 0;
    for(phase in 1:num_observations[cond]){
      phase_start_idx = phase_end_idx + 1;
      phase_end_idx = phase_start_idx + cond_durs[phase] - 1;
      
      real change_germ = - log(2) * cond_moist[phase] / dcy_sl[seedlots[cond]];
      real change_dorm_break = (drc[species[cond]] * (1 - cond_moist[phase]) + wtc[species[cond]] * cond_moist[phase]) * dbr[species[cond]] * (cond_temps[phase] - dth[species[cond]]);
      real change_forcing = forcing(cond_temps[phase], T_min, x_opt[species[cond]] * (T_max - T_min), T_max, delta[cond]);
      
      moisture_series[phase_start_idx:phase_end_idx] = rep_vector(cond_moist[phase], cond_durs[phase]);
      log_germinability[phase_start_idx:phase_end_idx] = rep_vector(change_germ, cond_durs[phase]);
      logit_dorm_break[phase_start_idx:phase_end_idx] = rep_vector(change_dorm_break, cond_durs[phase]);
      forcing[phase_start_idx:phase_end_idx] = rep_array(change_forcing, cond_durs[phase]);
    }
    
    logit_dorm_break[1] = logit_dorm_break[1] + logitdrm[i];
    
    vector[sum(cond_durs)] real accum_germinability = vbl_sl[seedlots[cond]] * exp(cumsum(log_germinability));
    vector[sum(cond_durs)] real accum_dorm_break = invlogit(cumsum(logit_dorm_break));
    // vector[sum(cond_durs)] real accum_forcing = cumsum(forcing);
    vector[sum(cond_durs)] real hazard = (drg[species[cond]] .* (1 - moisture_series) + wtg[species[cond]] .* moisture_series) .* accum_dorm_break .* forcing;
    vector[sum(cond_durs)] real accum_hazard = cumsum(hazard);
    
    target += cond_germ[1] * (log(accum_germinability[cond_obs[1]]) + log1m(survival_function(accum_hazard[cond_obs[1]]), Psi_0[species[cond]], sigma[species[cond]]))
    if(num_observations[cond] > 1){
      for(obs in 2:num_observations[cond]){
        target += cond_germ[obs] * (log(accum_germinability[cond_obs[obs]]) + log(survival_function(accum_hazard[cond_obs[obs - 1]]), Psi_0[species[cond]], sigma[species[cond]] - survival_function(accum_hazard[cond_obs[obs]]), Psi_0[species[cond]], sigma[species[cond]]));
      }
    }
    
    target += (num_seeds[cond] - sum(cond_germ)) * log_sum_exp(log1m(accum_germinability[cond_obs[obs]]), log(accum_germinability[cond_obs[obs]]) + log1m(survival_function(accum_hazard[cond_obs[obs]]), Psi_0[species[cond]], sigma[species[cond]]));
    num_seeds[cond] - sum(cond_germ)
  }
}

// generated quantities {
// 
//   array[N_exps] int N_obs_pred; // total germianted seeds
//   array[N_exps] int N_ungerm_pred;
//   
//   array[N_exps, max_days] int dgerm_pred; // daily germination
//   array[N_exps, max_days] int cumgerm_pred; // cumulative germination
// 
//   for (e in 1:N_exps) {
//     
//     int sp = species_idxs[e];
//     
//     vector[max_days + 1] fday; // daily forcing
//     vector[max_days + 1] cumF; // cumulative sum of forcing
// 
//     for (d in 1:(max_days + 1))
//       fday[d] = inv_logit(k[sp] * (germ_temp[e] - T0[sp]));
//     cumF = cumulative_sum(fday);
// 
//     N_obs_pred[e] = 0;
//     N_ungerm_pred[e] = 0;
//     for (day in 1:max_days)
//       dgerm_pred[e, day] = 0;
// 
//     // for each seed in the experiment...
//     for (s in 1:(N_obs[e] + N_ungerm[e])) {
//       
//       if (bernoulli_rng(pv[sp]) == 0) {
//         N_ungerm_pred[e] += 1;
//       } else {
//         
//         real l = logistic_rng(Psi0[sp], sigma[sp]); // threshold to reach
//         
//         if (l < cumF[1] || l >= cumF[max_days + 1]) {
//           N_ungerm_pred[e] += 1;  
//         } else {
//           
//           // latest d with cumPsi[d] < psi
//           int day = 1;                        
//           while (day < max_days && cumF[day + 1] < l)
//             day += 1;
//           dgerm_pred[e, day] += 1;
//           N_obs_pred[e] += 1;
//         }
//       }
//       
//     }
//     
//     cumgerm_pred[e,1:max_days] = cumulative_sum(dgerm_pred[e,1:max_days]);
//   }
// }
