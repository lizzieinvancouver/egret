beta_names <- c("beta_chill_pv", "beta_forc_pv",
                "beta_forc_tau25",
                "beta_chill_spread", "beta_forc_spread")

init_fn <- function() {
  betas       <- setNames(as.list(rnorm(length(beta_names), 0, 0.01)), beta_names)
  zs_beta     <- setNames(rep(list(rep(0, data$Nspecies)), length(beta_names)), paste0("z_", beta_names))
  sigmas_beta <- setNames(as.list(abs(rnorm(length(beta_names), 0, 0.05)) + 0.01), paste0("sigma_", beta_names))
  c(
    list(
      mu_pv = rnorm(1, 0.5, 0.5), alpha_pv = rnorm(data$Nspecies, 0.5, 0.3), sigma_pv = abs(runif(1, -2, 2)),
      mu_log_tau25  = rnorm(1, 4.35, 0.3), alpha_log_tau25 = rnorm(data$Nspecies, 4.35, 0.2), sigma_tau25 = abs(runif(1, -2, 2)),
      mu_beta_chill_tau25 = rnorm(1, 0, 0.05), beta_chill_tau25 = rnorm(data$Nspecies, 0, 0.01), sigma_beta_chill_tau25 = abs(rnorm(1, 0, 0.05)) + 0.01,
      mu_log_spread = rnorm(1, 3.10, 0.3), alpha_log_spread = rnorm(data$Nspecies, 3.10, 0.2), sigma_spread = abs(runif(1, -2, 2))
    ),
    betas, zs_beta, sigmas_beta
  )
}
data$N_newdays <- 150
modelstan <- stan_model("~/projects/egret/analyses/stan/generative/model13_1storder_multispecies.stan")
fit <- sampling(modelstan, data, chains = 4, cores = 4, 
                seed = 12345, iter = 2000, warmup = 1000, init = init_fn)

diagnostics <- util$extract_hmc_diagnostics(fit)
util$check_all_hmc_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
params <- c("mu_pv", "alpha_pv", "sigma_pv",
            "beta_chill_pv", "z_beta_chill_pv", "sigma_beta_chill_pv",
            "beta_forc_pv", "z_beta_forc_pv", "sigma_beta_forc_pv",
            "mu_log_tau25", "alpha_log_tau25", "sigma_tau25",
            "mu_beta_chill_tau25", "beta_chill_tau25", "sigma_beta_chill_tau25",
            "beta_forc_tau25", "z_beta_forc_tau25", "sigma_beta_forc_tau25",
            "mu_log_spread", "alpha_log_spread", "sigma_spread",
            "beta_chill_spread", "z_beta_chill_spread", "sigma_beta_chill_spread",
            "beta_forc_spread", "z_beta_forc_spread", "sigma_beta_forc_spread")
base_samples <- util$filter_expectands(samples,
                                       params,
                                       check_arrays = T)
util$check_all_expectand_diagnostics(base_samples)

# probability of 'germinability' pv
util$plot_pairs_by_chain(samples[['mu_pv']], 'mu_pv',
                         log(samples[['sigma_pv']]), 'log(sigma_pv)')
util$plot_div_pairs(paste0('alpha_pv[',1:data$Nspecies,']'), 'sigma_pv', samples, diagnostics,
                    transforms = list('sigma_pv' = 1))

util$plot_pairs_by_chain(samples[['beta_chill_pv']], 'beta_chill_pv',
                         log(samples[['sigma_beta_chill_pv']]), 'log(sigma_beta_chill_pv)')
util$plot_div_pairs(paste0('z_beta_chill_pv[',1:data$Nspecies,']'), 'sigma_beta_chill_pv', samples, diagnostics,
                    transforms = list('sigma_beta_chill_pv' = 1))

util$plot_pairs_by_chain(samples[['beta_forc_pv']], 'beta_forc_pv',
                         log(samples[['sigma_beta_forc_pv']]), 'log(sigma_beta_forc_pv)')
util$plot_div_pairs(paste0('z_beta_forc_pv[',1:data$Nspecies,']'), 'sigma_beta_forc_pv', samples, diagnostics,
                    transforms = list('sigma_beta_forc_pv' = 1))

# timing tau25
util$plot_pairs_by_chain(samples[['mu_log_tau25']], 'mu_log_tau25',
                         log(samples[['sigma_tau25']]), 'log(sigma_tau25)')
util$plot_div_pairs(paste0('alpha_log_tau25[',1:data$Nspecies,']'), 'sigma_tau25', samples, diagnostics,
                    transforms = list('sigma_tau25' = 1))

util$plot_pairs_by_chain(samples[['mu_beta_chill_tau25']], 'mu_beta_chill_tau25',
                         log(samples[['sigma_beta_chill_tau25']]), 'log(sigma_beta_chill_tau25)')
util$plot_div_pairs(paste0('beta_chill_tau25[',1:data$Nspecies,']'), 'sigma_beta_chill_tau25', samples, diagnostics,
                    transforms = list('sigma_beta_chill_tau25' = 1))

util$plot_pairs_by_chain(samples[['beta_forc_tau25']], 'beta_forc_tau25',
                         log(samples[['sigma_beta_forc_tau25']]), 'log(sigma_beta_forc_tau25)')
util$plot_div_pairs(paste0('z_beta_forc_tau25[',1:data$Nspecies,']'), 'sigma_beta_forc_tau25', samples, diagnostics,
                    transforms = list('sigma_beta_forc_tau25' = 1))


par(mfrow = c(2,2), mar = c(4,4,1,1))
util$plot_expectand_pushforward(samples[['beta_chill_pv']], 50,
                                display_name = 'beta_chill_pv',
                                flim = c(-0.75,0.75))
prior <- rnorm(1e6, 0, 0.15)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)
util$plot_expectand_pushforward(samples[['sigma_beta_chill_pv']], 50,
                                display_name = 'sigma_beta_chill_pv',
                                flim = c(0,1.5))
prior <- rnorm(1e6, 0, 0.1)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)

util$plot_expectand_pushforward(samples[['beta_forc_pv']], 50,
                                display_name = 'beta_forc_pv',
                                flim = c(-0.75,0.75))
prior <- rnorm(1e6, 0, 0.15)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)
util$plot_expectand_pushforward(samples[['sigma_beta_forc_pv']], 50,
                                display_name = 'sigma_beta_forc_pv',
                                flim = c(0,1.5))
prior <- rnorm(1e6, 0, 0.1)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)

par(mfrow = c(2,2), mar = c(4,4,1,1))
util$plot_expectand_pushforward(samples[['mu_beta_chill_tau25']], 50,
                                display_name = 'mu_beta_chill_tau25',
                                flim = c(-0.3,0.3))
prior <- rnorm(1e6, 0, 0.07)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)
util$plot_expectand_pushforward(samples[['sigma_beta_chill_tau25']], 50,
                                display_name = 'sigma_beta_chill_tau25',
                                flim = c(0,0.2))
prior <- rnorm(1e6, 0, 0.05)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)

util$plot_expectand_pushforward(samples[['beta_forc_tau25']], 50,
                                display_name = 'beta_forc_tau25',
                                flim = c(-0.3,0.3))
prior <- rnorm(1e6, 0, 0.07)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)
util$plot_expectand_pushforward(samples[['sigma_beta_forc_tau25']], 50,
                                display_name = 'sigma_beta_forc_tau25',
                                flim = c(0,0.4))
prior <- rnorm(1e6, 0, 0.05)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)

par(mfrow = c(2,2), mar = c(4,4,1,1))
util$plot_expectand_pushforward(samples[['beta_chill_spread']], 50,
                                display_name = 'beta_chill_spread',
                                flim = c(-0.3,0.3))
prior <- rnorm(1e6, 0, 0.07)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)
util$plot_expectand_pushforward(samples[['sigma_beta_chill_spread']], 50,
                                display_name = 'sigma_beta_chill_spread',
                                flim = c(0,0.4))
prior <- rnorm(1e6, 0, 0.05)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)

util$plot_expectand_pushforward(samples[['beta_forc_spread']], 50,
                                display_name = 'beta_forc_spread',
                                flim = c(-0.4,0.4))
prior <- rnorm(1e6, 0, 0.07)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)
util$plot_expectand_pushforward(samples[['sigma_beta_forc_spread']], 50,
                                display_name = 'sigma_beta_forc_spread',
                                flim = c(0,1))
prior <- rnorm(1e6, 0, 0.05)
lines(density(prior), col = util$c_light_teal, lwd = 2, lty = 2)