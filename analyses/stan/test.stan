functions {
  matrix lambda_vcv(matrix vcv, real lambda, real sigma){
    matrix[rows(vcv),rows(vcv)] C = lambda * vcv;
    C = C - diag_matrix(diagonal(C)) + diag_matrix(diagonal(vcv));
    return(sigma^2*C);
  }
}

data {
  int<lower=0> nspecies;
  real<lower=0> sigma;
  real<lower=0> lambda;
  matrix[nspecies,nspecies] Vphy;     // phylogeny
}

generated quantities {
  
   matrix[nspecies, nspecies] Cov = lambda_vcv(Vphy, lambda, sigma);
  
}

