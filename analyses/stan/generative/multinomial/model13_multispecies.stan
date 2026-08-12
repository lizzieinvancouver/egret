functions {
  real gompertz(real t, real tau25, real tau75) {
    real log_b = (log(-log(0.75)) - log(-log(0.25))) / (tau75 - tau25);
    real log_a = log(-log(0.25)) - log_b * tau25;
    return exp(-exp(log_a + log_b * t));
  }
}

data {
  int<lower=1> N;
  int<lower=1> Nexps;
  int<lower=1> Nspecies;

  array[Nexps] int<lower=1, upper=N> exp_start_idxs;
  array[Nexps] int<lower=1, upper=N> exp_end_idxs;
  array[N] int<lower=0> seeds;

  array[Nexps] int<lower=1, upper=Nspecies> species_idxs;

  vector<lower=0>[N] d;

  vector[Nexps] chill;
  vector[Nexps] forcing;

  int<lower=1> N_newdays;
}

transformed data {
  vector<lower=0>[N] dsf = d;

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    dsf[(start+1):end] = dsf[(start+1):end] + 50;
  }
}

parameters {

  real mu_pv;
  vector[Nspecies] alpha_pv;
  real<lower=0> sigma_pv;

  real beta_chill_pv;
  vector[Nspecies] z_beta_chill_pv;
  real<lower=0> sigma_beta_chill_pv;

  // real beta_chill2_pv;
  vector[Nspecies] z_beta_chill2_pv;
  real<lower=0> sigma_beta_chill2_pv;

  real beta_forc_pv;
  vector[Nspecies] z_beta_forc_pv;
  real<lower=0> sigma_beta_forc_pv;

  // real beta_forc2_pv;
  vector[Nspecies] z_beta_forc2_pv;
  real<lower=0> sigma_beta_forc2_pv;

  // real beta_chillforc_pv;
  vector[Nspecies] z_beta_chillforc_pv;
  real<lower=0> sigma_beta_chillforc_pv;

  real mu_log_tau25;
  vector[Nspecies] alpha_log_tau25;
  real<lower=0> sigma_tau25;

  real mu_beta_chill_tau25;
  vector[Nspecies] beta_chill_tau25;
  real<lower=0> sigma_beta_chill_tau25;

  // real beta_chill2_tau25;
  vector[Nspecies] z_beta_chill2_tau25;
  real<lower=0> sigma_beta_chill2_tau25;

  real beta_forc_tau25;
  vector[Nspecies] z_beta_forc_tau25;
  real<lower=0> sigma_beta_forc_tau25;

  // real beta_forc2_tau25;
  vector[Nspecies] z_beta_forc2_tau25;
  real<lower=0> sigma_beta_forc2_tau25;

  // real beta_chillforc_tau25;
  vector[Nspecies] z_beta_chillforc_tau25;
  real<lower=0> sigma_beta_chillforc_tau25;

  real mu_log_spread;
  vector[Nspecies] alpha_log_spread;
  real<lower=0> sigma_spread;

  real beta_chill_spread;
  vector[Nspecies] z_beta_chill_spread;
  real<lower=0> sigma_beta_chill_spread;

  // real beta_chill2_spread;
  vector[Nspecies] z_beta_chill2_spread;
  real<lower=0> sigma_beta_chill2_spread;

  real beta_forc_spread;
  vector[Nspecies] z_beta_forc_spread;
  real<lower=0> sigma_beta_forc_spread;

  // real beta_forc2_spread;
  vector[Nspecies] z_beta_forc2_spread;
  real<lower=0> sigma_beta_forc2_spread;

  // real beta_chillforc_spread;
  vector[Nspecies] z_beta_chillforc_spread;
  real<lower=0> sigma_beta_chillforc_spread;
}

transformed parameters {

  vector[Nexps] logit_pv;
  vector[Nexps] pv;
  vector[Nexps] log_tau25;
  vector[Nexps] log_spread;
  vector[Nexps] tau25;
  vector[Nexps] tau75;
  vector[N] pg;

  for (e in 1:Nexps) {
    
    int sp = species_idxs[e];

    real bcp = beta_chill_pv + sigma_beta_chill_pv * z_beta_chill_pv[sp];
    real bcp2 = sigma_beta_chill2_pv * z_beta_chill2_pv[sp];
    real bfp = beta_forc_pv + sigma_beta_forc_pv * z_beta_forc_pv[sp];
    real bfp2 = sigma_beta_forc2_pv * z_beta_forc2_pv[sp];
    real bcfp = sigma_beta_chillforc_pv * z_beta_chillforc_pv[sp];

    logit_pv[e] = alpha_pv[sp]
      + bcp * (chill[e]-7)
      + bfp * (forcing[e]-1.5)
      + bcp2 * square(chill[e]-7)
      + bfp2 * square(forcing[e]-1.5)
      + bcfp * (forcing[e]-1.5) * (chill[e]-7);

    real bct2 = sigma_beta_chill2_tau25 * z_beta_chill2_tau25[sp];
    real bft = beta_forc_tau25 + sigma_beta_forc_tau25 * z_beta_forc_tau25[sp];
    real bft2 = sigma_beta_forc2_tau25 * z_beta_forc2_tau25[sp];
    real bctf = sigma_beta_chillforc_tau25 * z_beta_chillforc_tau25[sp];

    log_tau25[e] = alpha_log_tau25[sp]
      + beta_chill_tau25[sp] * (chill[e]-7)
      + bft * (forcing[e]-1.5)
      + bct2 * square(chill[e]-7)
      + bft2 * square(forcing[e]-1.5)
      + bctf * (forcing[e]-1.5) * (chill[e]-7);

    real bcs = beta_chill_spread + sigma_beta_chill_spread * z_beta_chill_spread[sp];
    real bcs2 = sigma_beta_chill2_spread * z_beta_chill2_spread[sp];
    real bfs = beta_forc_spread + sigma_beta_forc_spread * z_beta_forc_spread[sp];
    real bfs2 = sigma_beta_forc2_spread * z_beta_forc2_spread[sp];
    real bcsf = sigma_beta_chillforc_spread * z_beta_chillforc_spread[sp];

    log_spread[e] = alpha_log_spread[sp]
      + bcs * (chill[e]-7)
      + bfs * (forcing[e]-1.5)
      + bcs2 * square(chill[e]-7)
      + bfs2 * square(forcing[e]-1.5)
      + bcsf * (forcing[e]-1.5) * (chill[e]-7);
  }

  pv = inv_logit(logit_pv);
  tau25 = exp(log_tau25);
  tau75 = tau25 + 1 + exp(log_spread);

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];

    pg[start] = (1 - pv[e]) + pv[e] * (1 - gompertz(dsf[end], tau25[e], tau75[e]));

    for (t in (start+1):end) {
      pg[t] = pv[e] * (gompertz(dsf[t], tau25[e], tau75[e]) - gompertz(dsf[t-1], tau25[e], tau75[e]));
    }

    pg[start:end] = pg[start:end] + 1e-12;
    pg[start:end] = pg[start:end] / sum(pg[start:end]);
  }
}

model {

  mu_pv ~ normal(0.5, 1);
  sigma_pv ~ normal(0, 1);
  alpha_pv ~ normal(mu_pv, sigma_pv);

  beta_chill_pv ~ normal(0, 0.15);
  sigma_beta_chill_pv ~ normal(0, 0.1);
  z_beta_chill_pv ~ normal(0, 1);

  // beta_chill2_pv ~ normal(0, 0.03);
  sigma_beta_chill2_pv ~ normal(0, 0.05);
  z_beta_chill2_pv ~ normal(0, 1);

  beta_forc_pv ~ normal(0, 0.15);
  sigma_beta_forc_pv ~ normal(0, 0.1);
  z_beta_forc_pv ~ normal(0, 1);

  // beta_forc2_pv ~ normal(0, 0.03);
  sigma_beta_forc2_pv ~ normal(0, 0.05);
  z_beta_forc2_pv ~ normal(0, 1);

  // beta_chillforc_pv ~ normal(0, 0.05);
  sigma_beta_chillforc_pv ~ normal(0, 0.05);
  z_beta_chillforc_pv ~ normal(0, 1);

  mu_log_tau25 ~ normal(4.35, 0.13);
  sigma_tau25 ~ normal(0, 0.5);
  alpha_log_tau25 ~ normal(mu_log_tau25, sigma_tau25);

  mu_beta_chill_tau25 ~ normal(0, 0.07);
  sigma_beta_chill_tau25 ~ normal(0, 0.05);
  beta_chill_tau25 ~ normal(mu_beta_chill_tau25, sigma_beta_chill_tau25);

  // beta_chill2_tau25 ~ normal(0, 0.02);
  sigma_beta_chill2_tau25 ~ normal(0, 0.03);
  z_beta_chill2_tau25 ~ normal(0, 1);

  beta_forc_tau25 ~ normal(0, 0.07);
  sigma_beta_forc_tau25 ~ normal(0, 0.05);
  z_beta_forc_tau25 ~ normal(0, 1);

  // beta_forc2_tau25 ~ normal(0, 0.02);
  sigma_beta_forc2_tau25 ~ normal(0, 0.03);
  z_beta_forc2_tau25 ~ normal(0, 1);

  // beta_chillforc_tau25 ~ normal(0, 0.03);
  sigma_beta_chillforc_tau25 ~ normal(0, 0.03);
  z_beta_chillforc_tau25 ~ normal(0, 1);

  mu_log_spread ~ normal(3.10, 0.41);
  sigma_spread ~ normal(0, 0.5);
  alpha_log_spread ~ normal(mu_log_spread, sigma_spread);

  beta_chill_spread ~ normal(0, 0.07);
  sigma_beta_chill_spread ~ normal(0, 0.05);
  z_beta_chill_spread ~ normal(0, 1);

  // beta_chill2_spread ~ normal(0, 0.02);
  sigma_beta_chill2_spread ~ normal(0, 0.03);
  z_beta_chill2_spread ~ normal(0, 1);

  beta_forc_spread ~ normal(0, 0.07);
  sigma_beta_forc_spread ~ normal(0, 0.05);
  z_beta_forc_spread ~ normal(0, 1);

  // beta_forc2_spread ~ normal(0, 0.02);
  sigma_beta_forc2_spread ~ normal(0, 0.03);
  z_beta_forc2_spread ~ normal(0, 1);

  // beta_chillforc_spread ~ normal(0, 0.03);
  sigma_beta_chillforc_spread ~ normal(0, 0.03);
  z_beta_chillforc_spread ~ normal(0, 1);

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    seeds[start:end] ~ multinomial(pg[start:end]);
  }
}

generated quantities {

  array[N] int<lower=0> y_pred;
  array[N] int<lower=0> cumy_pred;

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];
    y_pred[start:end] = multinomial_rng(pg[start:end], sum(seeds[start:end]));
    cumy_pred[start] = 0;
    cumy_pred[(start+1):end] = cumulative_sum(y_pred[(start+1):end]);
  }

  array[Nexps,(N_newdays+1)] int y_pred_new;
  array[Nexps,(N_newdays+1)] int cumy_pred_new;
  vector[(N_newdays+1)] grid_new = linspaced_vector(N_newdays+1, 0, N_newdays);

  for (e in 1:Nexps) {
    int start = exp_start_idxs[e];
    int end = exp_end_idxs[e];

    vector[(N_newdays+1)] g;
    for (t in 1:(N_newdays+1)) g[t] = gompertz(grid_new[t], tau25[e], tau75[e]);

    vector[(N_newdays+1)] pg_new;
    pg_new[1] = (1 - pv[e]) + pv[e] * (1 - g[(N_newdays+1)]);

    for (t in 1:N_newdays)
      pg_new[t+1] = pv[e] * (g[t+1] - g[t]);

    pg_new = pg_new + 1e-12;
    pg_new = pg_new / sum(pg_new);

    y_pred_new[e,] = multinomial_rng(pg_new, sum(seeds[start:end]));

    cumy_pred_new[e,1] = 0;
    cumy_pred_new[e,2:(N_newdays+1)] = cumulative_sum(y_pred_new[e,2:(N_newdays+1)]);
  }

}
