// Jan 24, 2025
// Started by V Van der Meersch
// Inspired by previous works on PPMs in the lab (Deirdre!)

data {
  
  int<lower=1> N;
  int<lower=1> Nsp;
  int<lower=1, upper=Nsp> sp[N];
  int<lower=1> Ntrials[N];
  int<lower=0> y[N]; // response, number of successes!
  vector[N] x; // predictor
  corr_matrix[Nsp] Vphy; // phylogenetic relationship matrix (correlation matrix based on distance, fixed)
  
}

parameters {
  
  // intercept (including the portion of phenotypes, not predicted by x, which still covary between related species?)
  vector[Nsp] a; 
  real a_z; // root value
  real<lower=0, upper=1> lambda_a; // phylogenetic structure      
  real<lower=0> sigma_a; // overall rate of change (brownian motion)
  
  // slope of x effect
  vector[Nsp] b; 
  real b_z; // root value
  real<lower=0, upper=1> lambda_b;  // phylogenetic structure        
  real<lower=0> sigma_b; // overall rate of change (brownian motion)
  
}

transformed parameters{
  
  real<lower=0, upper = 1> mu[N];
                                                                                         
  for(i in 1:N){
    mu[i] = inv_logit(a[sp[i]] + b[sp[i]] * x[i]);
  }
                                                                                         
}

model {
  
  // move all corr/cov matrix to model block, to save RAM (initially put them in transformed parameters)
  matrix[Nsp,Nsp] C_a = lambda_a * Vphy;
  C_a = C_a - diag_matrix(diagonal(C_a)) + diag_matrix(diagonal(Vphy)); // lambda only on off-diag elements
  matrix[Nsp,Nsp] C_b = lambda_b * Vphy;
  C_b = C_b - diag_matrix(diagonal(C_b)) + diag_matrix(diagonal(Vphy)); // lambda only on off-diag elements
  
  // more numerically stable and more efficient to use pre-factored covariance matrices (we will thus use multi_normal_cholesky in the following)
  matrix[Nsp,Nsp] L_a = cholesky_decompose(sigma_a^2*C_a);
  matrix[Nsp,Nsp] L_b =  cholesky_decompose(sigma_b^2*C_b);
  
  a ~ multi_normal_cholesky(rep_vector(a_z,Nsp), L_a); 
  b ~ multi_normal_cholesky(rep_vector(b_z,Nsp), L_b); 
  
  y ~ binomial(Ntrials, mu);
  
  // priors (from phyloMdlLambdaIntSlope.stan)
  a_z ~ normal(0, 1.5); 
  b_z ~ normal(0.5, 1); 
  
  lambda_a ~ beta(1.5, 1.5);
  sigma_a ~ normal(0, 1);
  
  lambda_b ~ beta(1.5, 1.5);
  sigma_b ~ normal(0, 1);
  
}

