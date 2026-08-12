

data <- mdl.data


exps <- which(data$species_idxs == 17)
offset <- data$exp_end_idxs[exps[1]-1]

new_starts <- c()
new_ends <- c()
new_seeds <- c()
new_d <- c()
new_chill <- c()
new_forcing <- c()
for(i in 1:length(exps)){
  e <- exps[i]
  
  if(e == 444){
    offset <- data$exp_end_idxs[exps[i]-1] - new_ends[i-1]
  }
  
  start <- data$exp_start_idxs[e]
  end <- data$exp_end_idxs[e]
  
  new_starts <- c(new_starts, start-offset)
  new_ends <- c(new_ends, end-offset)

  new_seeds <- c(new_seeds, data$seeds[start:end])
  new_d <- c(new_d, data$d[start:end])
  
  new_chill <- c(new_chill, data$chill[e])
  new_forcing <- c(new_forcing, data$forcing[e])
}
Nexps <- length(exps)
N <- length(new_seeds)

newdata <- list(
  N = N,
  Nexps = Nexps, 
  exp_start_idxs = new_starts,
  exp_end_idxs = new_ends,
  seeds = new_seeds,
  chill = new_chill,
  forcing = new_forcing,
  d = new_d
)

init_fn <- function() list(
  mu_log_tau25 = rnorm(1, log(50), log(20)/2.57),
  mu_log_spread = rnorm(1, log(10), log(10)/2.57)
)

newdata$N_newdays <- 150
modelstan <- stan_model("~/projects/egret/analyses/stan/generative/model11_nospecies.stan")
fit <- sampling(modelstan, newdata, chains = 4, cores = 4, 
                seed = 123456, iter = 2000, warmup = 1000,
                init = init_fn)

modelstan <- stan_model("~/projects/egret/analyses/stan/generative/model10_nospecies.stan")
fit2 <- sampling(modelstan, newdata, chains = 4, cores = 4, 
                seed = 12345, iter = 2000, warmup = 1000,
                init = init_fn)
samples2 <- util$extract_expectand_vals(fit2)

diag <- util$extract_hmc_diagnostics(fit)
util$check_all_hmc_diagnostics(diag)

samples <- util$extract_expectand_vals(fit)
base_samples <- util$filter_expectands(samples,
                                         c('mu_log_tau25', 'beta_chill_tau25', 'beta_chill2_tau25',
                                         'beta_forc_tau25', 'beta_forc2_tau25', 'beta_chillforc_tau25',
                                         'mu_log_spread', 'beta_chill_spread', 'beta_chill2_spread',
                                         'beta_forc_spread', 'beta_forc2_spread', 'beta_chillforc_spread'),
                                       check_arrays = T)
util$check_all_expectand_diagnostics(base_samples)

util$plot_div_pairs('mu_pv', 'sigma_pv_exp', samples, diag,
                    transforms =  list('sigma_pv_exp' = 1))

util$plot_div_pairs('mu_log_tau25', 'sigma_log_tau25_exp', samples, diag,
                    transforms =  list('sigma_log_tau25_exp' = 1))

util$plot_div_pairs('mu_log_spread', 'sigma_log_spread_exp', samples, diag,
                    transforms =  list('sigma_log_spread_exp' = 1))

par(mfrow = c(5,6))
for(e in 1:newdata$Nexps){
  util$plot_div_pairs(paste0('log_tau25[',e,']'), 
                      paste0('log_spread[',e,']'), samples, diag,
                      ylim = c(-10, 6), xlim = c(-1, 6))
  abline(h = util$ensemble_mcmc_quantile_est(samples[['mu_log_spread']], c(0.5)), lty = 2)
  abline(v = util$ensemble_mcmc_quantile_est(samples[['mu_log_tau25']], c(0.5)), lty = 2)
}

for(e in 1:newdata$Nexps){
  util$plot_div_pairs(paste0('log_tau25[',e,']'), 
                      'sigma_log_tau25_exp', samples, diag,
                      transforms = list('sigma_log_tau25_exp' = 1),
                      ylim = c(-10, 2), xlim = c(2, 4))
  # abline(h = util$ensemble_mcmc_quantile_est(samples[['mu_log_spread']], c(0.5)), lty = 2)
  abline(v = util$ensemble_mcmc_quantile_est(samples[['mu_log_tau25']], c(0.5)), lty = 2)
}


for(e in 1:newdata$Nexps){
  util$plot_div_pairs(paste0('log_spread[',e,']'), 
                      'sigma_log_spread_exp', samples, diag,
                      transforms = list('sigma_log_spread_exp' = 1),
                      ylim = c(-5, 1), xlim = c(-5, 5))
  # abline(h = util$ensemble_mcmc_quantile_est(samples[['mu_log_spread']], c(0.5)), lty = 2)
  # abline(v = util$ensemble_mcmc_quantile_est(samples[['mu_log_tau25']], c(0.5)), lty = 2)
}


for(e in 1:newdata$Nexps){
  util$plot_div_pairs(paste0('logit_pv[',e,']'), 
                      'sigma_pv_exp', samples, diag,
                      transforms = list('sigma_pv_exp' = 1),
                      ylim = c(-2, 1), xlim = c(-1, 5))
  # abline(h = util$ensemble_mcmc_quantile_est(samples[['mu_log_spread']], c(0.5)), lty = 2)
  abline(v = util$ensemble_mcmc_quantile_est(samples[['mu_pv']], c(0.5)), lty = 2)
}

for(e in 1:newdata$Nexps){
  trans <- list('sigma_log_tau25_exp' = 1, 'tau75' = 1)
  names(trans)[2] <- paste0('tau75[',e,']')
  util$plot_div_pairs(paste0('tau75[',e,']'), 
                      'sigma_log_tau25_exp', samples, diag,
                      transforms = trans,
                      ylim = c(-3, 1), xlim = c(2, 7))
}





util$plot_expectand_pushforward(samples[['mu_pv']], 50, flim = c(-5, 5))
prior <- rnorm(1e6, 0, 3)
lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)

util$plot_expectand_pushforward(samples[['sigma_pv_exp']], 50, flim = c(0, 10))
prior <- rnorm(1e6, 0, 3)
lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)

util$plot_expectand_pushforward(samples[['mu_log_tau25']], 50, flim = c(0, 7))
prior <- rnorm(1e6, log(50), log(20)/2.57)
lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)

util$plot_expectand_pushforward(samples[['sigma_log_tau25_exp']], 50, flim = c(0, 5))
prior <- rnorm(1e6, 0, 1)
lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)

util$plot_expectand_pushforward(samples[['mu_log_spread']], 50, flim = c(-7, 7))
prior <- rnorm(1e6, log(10), log(200)/2.57)
lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)

util$plot_expectand_pushforward(samples[['sigma_log_spread_exp']], 50, flim = c(0, 10))
prior <- rnorm(1e6, 0, 3)
lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)



util$plot_pairs_by_chain(samples[['mu_log_tau25']], 'mu_log_tau25',
                         (samples[['mu_log_spread']]), 'mu_log_spread')





for(e in 1:newdata$Nexps){
  util$plot_div_pairs(paste0('log_tau25_tilde[',e,']'), 
                      'sigma_log_tau25_exp', samples, diag,
                      transforms = list('sigma_log_tau25_exp' = 1),
                      ylim = c(-11, 2), xlim = c(-2, 2))
  # abline(h = util$ensemble_mcmc_quantile_est(samples[['mu_log_spread']], c(0.5)), lty = 2)
  abline(v = util$ensemble_mcmc_quantile_est(samples[['mu_log_tau25']], c(0.5)), lty = 2)
}
