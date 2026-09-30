## Started 27 Sep 2026 ##
## Started by Mao ##
## Understand the effect of scarification ##

functions {

  // prior from Michael Betancourt for ordered cutpoints
  // see: https://betanalpha.github.io/assets/case_studies/ordinal_regression.html
  real induced_dirichlet_lpdf(vector c, vector alpha, real phi) {
    int K = num_elements(c) + 1;
    vector[K - 1] sigma = inv_logit(phi - c);
    vector[K] p;
    matrix[K, K] J = rep_matrix(0, K, K);

    p[1] = 1 - sigma[1];
    for (k in 2:(K - 1))
      p[k] = sigma[k - 1] - sigma[k];
    p[K] = sigma[K - 1];

    for (k in 1:K) J[k, 1] = 1;

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

  int<lower=0> N_prop;   // number of proportion observations (0,1)
  int<lower=0> N_degen;  // number of 0/1 (degenerate) observations

  int<lower=1> Nsp; // number of species
  array[N_prop]  int<lower=1, upper=Nsp> sp_prop;
  array[N_degen] int<lower=1, upper=Nsp> sp_degen;

  vector[N_prop] y_prop; // Y in (0,1)
  array[N_degen] int<lower=0, upper=1> y_degen;

  vector[N_prop]  scar_prop;  // scarification for proportion outcome
  vector[N_degen] scar_degen; // scarification for degenerate (0,1) outcome

  corr_matrix[Nsp] Vphy; // phylogenetic relationship matrix (fixed)
}

parameters {

  vector[Nsp] a; 
  real a_z; // root value
  real<lower=0, upper=1> lambda_a;  // phylogenetic structure
  real<lower=0> sigma_a; // overall rate of change (brownian motion?)

  real b_scar; // single global effect of scarification

  ordered[2] cutpoints; // cutpoints on ordered (latent) variable (also stand in as intercepts)
  real<lower=0> kappa; // scale parameter for beta regression
}

transformed parameters {

  array[N_degen] real calc_degen;
  array[N_prop]  real calc_prop;

  if (N_degen > 0) {
    for (i in 1:N_degen) {
      calc_degen[i] = a[sp_degen[i]] + b_scar * scar_degen[i];
    }
  }

  for (i in 1:N_prop) {
    calc_prop[i] = a[sp_prop[i]] + b_scar * scar_prop[i];
  }
}

model {

  matrix[Nsp, Nsp] C_a = lambda_a * Vphy;
  C_a = C_a - diag_matrix(diagonal(C_a)) + diag_matrix(diagonal(Vphy));

  matrix[Nsp, Nsp] L_a = cholesky_decompose(sigma_a^2 * C_a);

  a ~ multi_normal_cholesky(rep_vector(a_z, Nsp), L_a);

  target += induced_dirichlet_lpdf(cutpoints | rep_vector(1, 3), 0);

  if (N_degen > 0) {
    for (n in 1:N_degen) {
      if (y_degen[n] == 0) {
        // Pr(Y == 0)
        target += log1m_inv_logit(calc_degen[n] - cutpoints[1]);
      } else {
        // Pr(Y == 1)
        target += log_inv_logit(calc_degen[n] - cutpoints[2]);
      }
    }
  }

  for (n in 1:N_prop) {
    // Pr(Y in (0,1))
    target += log(inv_logit(calc_prop[n] - cutpoints[1]) - inv_logit(calc_prop[n] - cutpoints[2])); 
    // Pr(Y == x, x in (0,1))
    y_prop[n] ~ beta_proportion(inv_logit(calc_prop[n]), kappa); 
  }

  // priors
  a_z ~ normal(0, 1.5);
  lambda_a ~ beta(1.5, 1.5);
  sigma_a ~ normal(0, 1);
  b_scar ~ normal(0, 1);
  kappa ~ exponential(.1);
}
