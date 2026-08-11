//Started Aug 11, 2026 by D. Loughnan
//aim of this model is to model both ospree and egret+USDA in a single Stan model: 

// The input data is a vector 'y' of length 'N'.
functions {
  matrix lambda_vcv(matrix vcv, real lambda, real sigma){
    matrix[rows(vcv),cols(vcv)] local_vcv;
   // matrix[rows(vcv),cols(vcv)] sigma_mat;  
    local_vcv = vcv * lambda;
    for(i in 1:rows(local_vcv))
      local_vcv[i,i] = vcv[i,i];
      return(quad_form_diag(local_vcv, rep_vector(sigma, rows(vcv))));
    //sigma_mat = diag_matrix(rep_vector(sigma, rows(vcv)));
    //return(sigma_mat * local_vcv * sigma_mat);
    
    
  }
  
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
  //egret data:
  int<lower=0> N_prop; // number of proportion observations (0,1)
  int<lower=0> N_degen; // number of 0/1 observations
  
  int<lower=1> N_spEgret; // number of species
  array[N_prop] int<lower=1, upper=N_spEgret> sp_prop;
  array[N_degen] int<lower=1, upper=N_spEgret> sp_degen;

  vector[N_prop] y_prop; // Y in (0,1)
  array[N_degen] int<lower=0, upper=1> y_degen;

  vector[N_prop] c_prop; // covariate time for proportion outcome
  vector[N_degen] c_degen; // covariate time for degenerate (0,1) outcome
  
  vector[N_prop] f_prop; // covariate forcing for proportion outcome
  vector[N_degen] f_degen; // covariate forcing for degenerate (0,1) outcome
  
  corr_matrix[N_spEgret] Vphy_egret; // phylogenetic relationship matrix (fixed)
  
  //ospree data:
  int<lower=1> N_ospree;
  int<lower=1> N_ospreeSp;
  array[N_ospree] int<lower=1, upper=N_ospreeSp> spOspree;
  vector[N_ospree] y_ospree; 		// response
  vector[N_ospree] x1_ospree; 	// predictor (forcing)
  vector[N_ospree] x2_ospree; 	// predictor (chilling)
  vector[N_ospree] x3_ospree; 	// predictor (photoperiod)
  matrix[N_ospreeSp,N_ospreeSp]Vphy_ospree;     // phylogeny
  
  //combined slope model:
  int<lower=0> N_shared;
  array[N_shared] int shared_sp_ospree;
  array[N_shared] int shared_sp_egret;

}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //egret+usda
  vector[N_spEgret] a_egret; 
  real a_z_egret; // root value
  real<lower=0, upper=1> lambda_a_egret; // phylogenetic structure      
  real<lower=0> sigma_a_egret; // overall rate of change (brownian motion?)
  
  // slope of time effect
  vector[N_spEgret] bc_egret; 
  real bc_z_egret; // root value
  real<lower=0, upper=1> lambda_bc_egret;  // phylogenetic structure        
  real<lower=0> sigma_bc_egret; // overall rate of change (brownian motion?)
  
  // slope of forcing effect
  vector[N_spEgret] bf_egret; 
  real bf_z_egret; // root value
  real<lower=0, upper=1> lambda_bf_egret;  // phylogenetic structure        
  real<lower=0> sigma_bf_egret; // overall rate of change (brownian motion?)
  
  ordered[2] cutpoints; // cutpoints on ordered (latent) variable (also stand in as intercepts)
  real<lower=0> kappa; // scale parameter for beta regression
  
  //ospree:
  real<lower=0> sigma_y_ospree;    
  real<lower=0, upper=1> lam_interceptsa_ospree;       
  real<lower=0> sigma_interceptsa_ospree;
  real<lower=0, upper=1> lam_interceptsbf_ospree;       
  real<lower=0> sigma_interceptsbf_ospree;   
  real<lower=0, upper=1> lam_interceptsbc_ospree;       
  real<lower=0> sigma_interceptsbc_ospree; 
  real<lower=0, upper=1> lam_interceptsbp_ospree;       
  real<lower=0> sigma_interceptsbp_ospree; 
  vector[N_ospreeSp] b_force_ospree; // slope of forcing effect
  real b_zf_ospree;
  vector[N_ospreeSp] b_chill_ospree; // slope of chilling effect
  real b_zc_ospree;
  vector[N_ospreeSp] b_photo_ospree; // slope of photo effect
  real b_zp_ospree;
  vector[N_ospreeSp] a_ospree; // intercept
  real a_z_ospree;
  
  // for the combined 
  
  real b_both;
  real a_both;
  real<lower=0> sigma_y_both; 

}

transformed parameters{
  //ospree
   array[N_ospree] real y_ospree_hat;
       
       	for(i in 1:N_ospree){
            y_ospree_hat[i] = 
	      a_ospree[spOspree[i]] 
	      + b_force_ospree[spOspree[i]] * x1_ospree[i] 
	      + b_chill_ospree[spOspree[i]] * x2_ospree[i] 
	      + b_photo_ospree[spOspree[i]] * x3_ospree[i];
			     	}
			     	
  //egret + usda
  array[N_degen] real calc_degen;
  array[N_prop] real calc_prop;

  if(N_degen>0) {
    for(i in 1:N_degen){
      calc_degen[i] = a_egret[sp_degen[i]] + bc_egret[sp_degen[i]] * c_degen[i] + bf_egret[sp_degen[i]] * f_degen[i];
    }
  }
  
  for(i in 1:N_prop){
    
    calc_prop[i] = a_egret[sp_prop[i]] + bc_egret[sp_prop[i]] * c_prop[i] + bf_egret[sp_prop[i]] * f_prop[i];
  

}

}

model {
  
  //egret + usda
  matrix[N_spEgret, N_spEgret] C_a = lambda_a_egret * Vphy_egret;
  C_a = C_a - diag_matrix(diagonal(C_a)) + diag_matrix(diagonal(Vphy_egret));
  
  matrix[N_spEgret, N_spEgret] C_bc = lambda_bc_egret * Vphy_egret;
  C_bc = C_bc - diag_matrix(diagonal(C_bc)) + diag_matrix(diagonal(Vphy_egret));
  
  matrix[N_spEgret, N_spEgret] C_bf = lambda_bf_egret * Vphy_egret;
  C_bf = C_bf - diag_matrix(diagonal(C_bf)) + diag_matrix(diagonal(Vphy_egret));
  
  // more numerically stable and more efficient to use pre-factored covariance matrices (i.e. multi_normal_cholesky in the following
  matrix[N_spEgret, N_spEgret] L_a = cholesky_decompose(sigma_a_egret^2*C_a);
  matrix[N_spEgret, N_spEgret] L_bc =  cholesky_decompose(sigma_bc_egret^2*C_bc); 
  matrix[N_spEgret, N_spEgret] L_bf =  cholesky_decompose(sigma_bf_egret^2*C_bf); 
  
  a_egret ~ multi_normal_cholesky(rep_vector(a_z_egret, N_spEgret), L_a); 
  bc_egret ~ multi_normal_cholesky(rep_vector(bc_z_egret, N_spEgret), L_bc); 
  bf_egret ~ multi_normal_cholesky(rep_vector(bf_z_egret, N_spEgret), L_bf); 
  
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
    y_prop[n] ~ beta_proportion(inv_logit(calc_prop[n]),kappa);
  }
  
  // priors
  a_z_egret ~ normal(0, 1.5); 
  bc_z_egret ~ normal(0.5, 1); 
  bf_z_egret ~ normal(0.5, 1); 
  
  lambda_a_egret ~ beta(1.5, 1.5);
  sigma_a_egret ~ normal(0, 1);
  
  lambda_bc_egret ~ beta(1.5, 1.5);
  sigma_bc_egret ~ normal(0, 1);
  
  lambda_bf_egret ~ beta(1.5, 1.5);
  sigma_bf_egret ~ normal(0, 1);
  
  kappa ~ exponential(.1); // 
  
  //ospree
//  array[N_ospree] real y_ospree_hat;
//        
        matrix[N_ospreeSp,N_ospreeSp] vcv_a_ospree;     // phylogeny
        matrix[N_ospreeSp,N_ospreeSp] vcv_bf_ospree;     // phylogeny
        matrix[N_ospreeSp,N_ospreeSp] vcv_bc_ospree;     // phylogeny
        matrix[N_ospreeSp,N_ospreeSp] vcv_bp_ospree;     // phylogeny
// 
//        
//        	for(i in 1:N_ospree){
//             y_ospree_hat[i] = 
// 	      a_ospree[spOspree[i]] 
// 	      + b_force_ospree[spOspree[i]] * x1_ospree[i] 
// 	      + b_chill_ospree[spOspree[i]] * x2_ospree[i] 
// 	      + b_photo_ospree[spOspree[i]] * x3_ospree[i];
// 			     	}
  
	vcv_a_ospree = cholesky_decompose(lambda_vcv(Vphy_ospree, lam_interceptsa_ospree, sigma_interceptsa_ospree));
  vcv_bf_ospree = cholesky_decompose(lambda_vcv(Vphy_ospree, lam_interceptsbf_ospree, sigma_interceptsbf_ospree));
  vcv_bc_ospree = cholesky_decompose(lambda_vcv(Vphy_ospree, lam_interceptsbc_ospree, sigma_interceptsbc_ospree));
  vcv_bp_ospree = cholesky_decompose(lambda_vcv(Vphy_ospree, lam_interceptsbp_ospree, sigma_interceptsbp_ospree));


  a_ospree ~ multi_normal_cholesky(rep_vector(a_z_ospree,N_ospreeSp), vcv_a_ospree); 
  b_force_ospree ~ multi_normal_cholesky(rep_vector(b_zf_ospree, N_ospreeSp), vcv_bf_ospree); 
  b_chill_ospree ~ multi_normal_cholesky(rep_vector(b_zc_ospree, N_ospreeSp), vcv_bc_ospree);
  b_photo_ospree ~ multi_normal_cholesky(rep_vector(b_zp_ospree, N_ospreeSp),vcv_bp_ospree);
  
  y_ospree ~ normal(y_ospree_hat, sigma_y_ospree);

 // Priors -- keep in Stan code, better for reproducibility and runs faster
    a_z_ospree ~ normal(30, 10); // Same as before, seems okay
    b_zf_ospree ~ normal(-2, 10); // updated prior ... I think we should also try 0, 10
    b_zc_ospree ~ normal(-2, 10); // updated prior
    b_zp_ospree ~ normal(0, 5); // updated prior

    // All below: same as before, seems okay
    lam_interceptsa_ospree ~ beta(1, 1);
    lam_interceptsbf_ospree ~ beta(1, 1);
    lam_interceptsbc_ospree~ beta(1, 1);
    lam_interceptsbp_ospree ~ beta(1, 1);

    // I don't have a good sense of how to set these, so keeping a little wide
    sigma_interceptsa_ospree ~ normal(30, 20);
    sigma_interceptsbf_ospree ~ normal(1, 5);
    sigma_interceptsbc_ospree ~ normal(1, 5);
    sigma_interceptsbp_ospree ~ normal(1, 5);
    
    sigma_y_ospree ~ normal(10, 10); // updated prior
    
    // New combined model:
    
    //zero one of whether they agree and then index on that to subset down
    // in transformed data build the spp set the is the zero and 1
//     for (i in 1:N_degen) {
//   if (sp_shared_egret == 1) {
//     // use species_effect[species_id[i]]
//   }
// }
   vector[N_shared] mu;
   
    mu = a_both + b_both * b_force_ospree[shared_sp_ospree];
    
    bf_egret[shared_sp_egret] ~ normal(mu, sigma_y_both);

}

