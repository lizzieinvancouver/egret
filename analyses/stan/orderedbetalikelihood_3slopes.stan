// Feb 3, 2025
// Started by V Van der Meersch
// Inspired by previous works on PPMs in the lab

// Beta regression with proportion and degenerate responses (i.e. 0 and 1)
// Original code from R. Kubinec, https://github.com/saudiwin/ordbetareg

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
  
  array[N_prop] real y_prop; // Y in (0,1)
  array[N_degen] int<lower=0, upper=1> y_degen; // Y in {0,1}
  
  vector[N_prop] t_prop; // covariate time for proportion outcome
  vector[N_degen] t_degen; // covariate time for degenerate (0,1) outcome
  
  vector[N_prop] f_prop; // covariate forcing for proportion outcome
  vector[N_degen] f_degen; // covariate forcing for degenerate (0,1) outcome
  
  vector[N_prop] c_prop; // covariate chilling for proportion outcome
  vector[N_degen] c_degen; // covariate chilling for degenerate (0,1) outcome
  
  corr_matrix[Nsp] Vphy; // phylogenetic relationship matrix (fixed)
}

parameters {
  
  // intercept (+ the portion of phenotypes, not predicted by x, which still covary between related species, right?)
  vector[Nsp] a; 
  real a_z; // root value
  real<lower=0, upper=1> lambda_a; // phylogenetic structure      
  real<lower=0> sigma_a; // overall rate of change (brownian motion?)
  
  // slope of time effect
  vector[Nsp] bt; 
  real bt_z; // root value
  real<lower=0, upper=1> lambda_bt;  // phylogenetic structure        
  real<lower=0> sigma_bt; // overall rate of change (brownian motion?)
  
  // slope of forcing effect
  vector[Nsp] bf; 
  real bf_z; // root value
  real<lower=0, upper=1> lambda_bf;  // phylogenetic structure        
  real<lower=0> sigma_bf; // overall rate of change (brownian motion?)
  
  // slope of chilling effect
  vector[Nsp] bc; 
  real bc_z; // root value
  real<lower=0, upper=1> lambda_bc;  // phylogenetic structure        
  real<lower=0> sigma_bc; // overall rate of change (brownian motion?)
  
  ordered[2] cutpoints; // cutpoints on ordered (latent) variable (also stand in as intercepts)
  real<lower=0> kappa; // scale parameter for beta regression
}

transformed parameters {
  
  array[N_degen] real calc_degen;
  array[N_prop] real calc_prop;
  
  if(N_degen>0) {
    for(i in 1:N_degen){
      calc_degen[i] = a[sp_degen[i]] + bt[sp_degen[i]] * t_degen[i] + bf[sp_degen[i]] * f_degen[i] + bc[sp_degen[i]] * c_degen[i];
    }
  }
  
  for(i in 1:N_prop){
    
    calc_prop[i] = a[sp_prop[i]] + bt[sp_prop[i]] * t_prop[i] + bf[sp_prop[i]] * f_prop[i] + bc[sp_prop[i]] * c_prop[i];
    
  }

}

model {
  
  // move all corr/cov matrix to model block, to save RAM
  matrix[Nsp,Nsp] C_a = lambda_a * Vphy; // previously defined as corr_matrix, but not working in model block?
  C_a = C_a - diag_matrix(diagonal(C_a)) + diag_matrix(diagonal(Vphy));
  matrix[Nsp,Nsp]  C_bt = lambda_bt * Vphy;
  C_bt = C_bt - diag_matrix(diagonal(C_bt)) + diag_matrix(diagonal(Vphy));
  matrix[Nsp,Nsp]  C_bf = lambda_bf * Vphy;
  C_bf = C_bf - diag_matrix(diagonal(C_bf)) + diag_matrix(diagonal(Vphy));
  matrix[Nsp,Nsp]  C_bc = lambda_bc * Vphy;
  C_bc = C_bc - diag_matrix(diagonal(C_bc)) + diag_matrix(diagonal(Vphy));
  
  // more numerically stable and more efficient to use pre-factored covariance matrices (i.e. multi_normal_cholesky in the following
  matrix[Nsp,Nsp] L_a = cholesky_decompose(sigma_a^2*C_a);
  matrix[Nsp,Nsp] L_bt =  cholesky_decompose(sigma_bt^2*C_bt); 
  matrix[Nsp,Nsp] L_bf =  cholesky_decompose(sigma_bf^2*C_bf); 
  matrix[Nsp,Nsp] L_bc =  cholesky_decompose(sigma_bc^2*C_bc); 
  
  a ~ multi_normal_cholesky(rep_vector(a_z,Nsp), L_a); 
  bt ~ multi_normal_cholesky(rep_vector(bt_z,Nsp), L_bt); 
  bf ~ multi_normal_cholesky(rep_vector(bf_z,Nsp), L_bf); 
  bc ~ multi_normal_cholesky(rep_vector(bc_z,Nsp), L_bc); 
  
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
  bc_z ~ normal(0.5, 1); 
  
  lambda_a ~ beta(1.5, 1.5);
  sigma_a ~ normal(0, 1);
  
  lambda_bt ~ beta(1.5, 1.5);
  sigma_bt ~ normal(0, 1);
  
  lambda_bf ~ beta(1.5, 1.5);
  sigma_bf ~ normal(0, 1);
  
  lambda_bc ~ beta(1.5, 1.5);
  sigma_bc ~ normal(0, 1);
  
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
  
  
  


