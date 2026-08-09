data {
  int<lower=0> N; //No. obs
  vector[N] cue;
//response
  vector[N] ypred; //germ respons
}

parameters {

  real b;
  real a;

  real<lower=0> sigma_y; 
}

transformed parameters {
  vector[N] mu;

  mu = a + b * cue;
}

model {
  // priors
  a ~ normal(0, 10);
  b ~ normal(0, 10);
  sigma_y ~ normal(0,10); 
  // likelihood
  ypred ~ normal(mu, sigma_y);
}

