// Feb 3, 2025
// Started by V Van der Meersch, CRD commented the provenance lines
// Inspired by previous works on PPMs in the lab

// Beta regression with proportion and degenerate responses (i.e. 0 and 1)
// Original code from R. Kubinec, https://github.com/saudiwin/ordbetareg

// Modified Nov. 18 to add provenance effect

functions {
  
  // prior from Michael Betancourt for ordered cutpoints
  // see: https://betanalpha.github.io/assets/case_studies/ordinal_regression.html
    real induced_dirichlet_lpdf(vector c, vector alpha, real phi) {
    int K = num_elements(c) + 1;
    vector[K - 1] sigma = inv_logit(phi - c);
    vector[K] p;
    matrix[K, K] J = rep_matrix(0, K, K);
    
    // Induced ordinal probabilities
    p[1] = 1 - sigma[1];
    for (k in 2:(K - 1))
      p[k] = sigma[k - 1] - sigma[k];
    p[K] = sigma[K - 1];
    
    // Baseline column of Jacobian
    for (k in 1:K) J[k, 1] = 1;
    
    // Diagonal entries of Jacobian
    for (k in 2:K) {
      real rho = sigma[k - 1] * (1 - sigma[k - 1]);
      J[k, k] = - rho;
      J[k - 1, k] = rho;
    }
    
    return   dirichlet_lpdf(p | alpha)
           + log_determinant(J);
  }
}

data {
  
  int<lower=0> N_prop; // number of proportion observations (0,1)
  int<lower=0> N_degen; // number of 0/1 observations
  
  int<lower=1> Nsp; // number of species
  array[N_prop] int<lower=1, upper=Nsp> sp_prop; // species ID for prop. observations
  array[N_degen] int<lower=1, upper=Nsp> sp_degen; // species ID for degen. observations
  
  // int<lower=1> Nprov; // number of species
  // array[N_prop] int<lower=1, upper=Nprov> prov_prop; // species ID for prop. observations
  // array[N_degen] int<lower=1, upper=Nprov> prov_degen; // species ID for degen. observations
  
  array[N_prop] real y_prop; // Y in (0,1)
  array[N_degen] int<lower=0, upper=1> y_degen; // Y in {0,1}
  
  vector[N_prop] t_prop; // covariate time for proportion outcome
  vector[N_degen] t_degen; // covariate time for degenerate (0,1) outcome
  
  vector[N_prop] f_prop; // covariate forcing for proportion outcome
  vector[N_degen] f_degen; // covariate forcing for degenerate (0,1) outcome
  
  vector[N_prop] cs_prop; // covariate chilling for proportion outcome
  vector[N_degen] cs_degen; // covariate chilling for degenerate (0,1) outcome
  
  // corr_matrix[Nsp] Vphy; // phylogenetic relationship matrix (fixed)
}

parameters {
  
  // intercept (+ the portion of phenotypes, not predicted by x, which still covary between related species, right?)
  vector[Nsp] a; 
  real a_z; // root value
  real<lower=0> sigma_a; 
  // vector[Nprov] a_prov; 
  // real<lower=0> sigma_a_prov;
  
  // slope of time effect
  vector[Nsp] bt_tilde; 
  real bt_z; // root value
  real<lower=0> sigma_bt; 
  // vector[Nprov] bt_tilde_prov; 
  // real<lower=0> sigma_bt_prov;
  
  // slope of forcing effect
  vector[Nsp] bf_tilde; 
  real bf_z; // root value
  real<lower=0> sigma_bf; 
  // vector[Nprov] bf_tilde_prov; 
  // real<lower=0> sigma_bf_prov;
  
  // slope of chilling effect
  vector[Nsp] bcs_tilde; 
  real bcs_z; // root value
  real<lower=0> sigma_bcs; 
  // vector[Nprov] bcs_tilde_prov; 
  // real<lower=0> sigma_bcs_prov;
  
  ordered[2] cutpoints; // cutpoints on ordered (latent) variable (also stand in as intercepts)
  real<lower=0> kappa; // scale parameter for beta regression
}

transformed parameters {
  
  vector[Nsp] bt = bt_z + sigma_bt * bt_tilde;
  vector[Nsp] bf = bf_z + sigma_bf * bf_tilde;
  vector[Nsp] bcs = bcs_z + sigma_bcs * bcs_tilde;
  
  // vector[Nprov] bt_prov = 0 + sigma_bt_prov * bt_tilde_prov;
  // vector[Nprov] bf_prov = 0 + sigma_bf_prov * bf_tilde_prov;
  // vector[Nprov] bcs_prov = 0 + sigma_bcs_prov * bcs_tilde_prov;
  
  array[N_degen] real calc_degen;
  array[N_prop] real calc_prop;
  
  if(N_degen>0) {
    for(i in 1:N_degen){
      calc_degen[i] = a[sp_degen[i]] + 
      bt[sp_degen[i]] * t_degen[i] +
      bf[sp_degen[i]] * f_degen[i] +
      bcs[sp_degen[i]] * cs_degen[i];
    }
  }
  
  for(i in 1:N_prop){
    calc_prop[i] = a[sp_prop[i]] + 
    bt[sp_prop[i]] * t_prop[i] + 
    bf[sp_prop[i]] * f_prop[i] + 
    bcs[sp_prop[i]] * cs_prop[i];
  }
}

model {
  
  a ~ normal(a_z, sigma_a); 
  bt_tilde ~ normal(0, 1); 
  bf_tilde ~ normal(0, 1); 
  bcs_tilde ~ normal(0, 1); 
  
  // a_prov ~ normal(0, sigma_a_prov); 
  // bt_tilde_prov ~ normal(0, 1); 
  // bf_tilde_prov ~ normal(0, 1); 
  // bcs_tilde_prov ~ normal(0, 1); 
  
  target += induced_dirichlet_lpdf(cutpoints | rep_vector(1, 3), 0);
  
  // need separate loops for logit (0/1) and beta regression
  if(N_degen>0) {
    for(n in 1:N_degen) {
      if(y_degen[n]==0) {
        // Pr(Y==0)
        target += log1m_inv_logit(calc_degen[n] - cutpoints[1]);
      } else {
        //Pr(Y==1)
        target += log_inv_logit(calc_degen[n] - cutpoints[2]);
      }
    }
  }
  
  for(n in 1:N_prop) {
    // Pr(Y in (0,1))
    target += log(inv_logit(calc_prop[n] - cutpoints[1]) - inv_logit(calc_prop[n] - cutpoints[2]));
    // Pr(Y==x where x in (0,1))
    // y_prop[n] ~ beta_proportion(inv_logit(calc_prop[n]),kappa);
    target += beta_proportion_lpdf(y_prop[n] | inv_logit(calc_prop[n]),kappa);
  }
  
  // priors
  a_z ~ normal(0, 1.5); 
  bt_z ~ normal(0.5, 1); 
  bf_z ~ normal(0.5, 1); 
  bcs_z ~ normal(0.5, 1); 
  
  sigma_a ~ normal(0, 1);
  sigma_bt ~ normal(0, 1);
  sigma_bf ~ normal(0, 1);
  sigma_bcs ~ normal(0, 1);
  
  // sigma_a_prov ~ normal(0, 1);
  // sigma_bt_prov ~ normal(0, 1);
  // sigma_bf_prov ~ normal(0, 1);
  // sigma_bcs_prov ~ normal(0, 1);
  
  
  kappa ~ exponential(.1); // vague?
  
}

generated quantities {
  
  array[N_prop] real y_prop_gen;
  array[N_degen] real y_degen_gen;
  
  real logisticvariate;
  
  if(N_degen>0) {
    for(i in 1:N_degen) {
      
      logisticvariate = ordered_logistic_rng(calc_degen[i] ,cutpoints);
       
      if(logisticvariate==1) {
        y_degen_gen[i] = 0;
      } else if(logisticvariate==3) {
        y_degen_gen[i] = 1;
      } else {
        y_degen_gen[i] = beta_proportion_rng(inv_logit(calc_degen[i]),kappa);
      }
    
    }
  }
  for(i in 1:N_prop) {
      
    logisticvariate = ordered_logistic_rng(calc_prop[i] ,cutpoints);
       
    if(logisticvariate==1) {
        y_prop_gen[i] = 0;
    } else if(logisticvariate==3) {
        y_prop_gen[i] = 1;
    } else {
        y_prop_gen[i] = beta_proportion_rng(inv_logit(calc_prop[i]),kappa);
    }
  }
  
  
  
} 
  
  
  


