wd <- '/home/victor/projects/egret/analyses'
setwd(wd)
util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

data <- mdl.data

selected_species <- c(4, 5, 17, 25, 26,  31, 32, 48, 49, 55, 56, 76, 77, 78)
selected_species <- c(48) # this is the one with supra-optimal temp
exps <- which(data$species_idxs %in% selected_species)
offset <- data$exp_end_idxs[exps[1]-1]

new_starts <- c()
new_ends <- c()
new_seeds <- c()
new_d <- c()
new_chill <- c()
new_forcing <- c()
new_species <- c()
idx <- 1
for(i in 1:length(exps)){
  e <- exps[i]
  
  new_species <- c(new_species, which(selected_species == data$species_idxs[e]))
  
  start <- data$exp_start_idxs[e]
  end <- data$exp_end_idxs[e]
  
  new_seeds <- c(new_seeds, data$seeds[start:end])
  new_d <- c(new_d, data$d[start:end])
  
  new_starts <- c(new_starts, idx)
  idx <- idx + length(start:end)
  new_ends <- c(new_ends, idx - 1)
  
  
  new_chill <- c(new_chill, data$chill[e])
  new_forcing <- c(new_forcing, data$forcing[e])
}
Nspecies <- length(selected_species)
Nexps <- length(exps)
N <- length(new_seeds)

newdata <- list(
  N = N,
  Nexps = Nexps, 
  Nspecies = Nspecies,
  species_idxs = new_species,
  exp_start_idxs = new_starts,
  exp_end_idxs = new_ends,
  seeds = new_seeds,
  chill = new_chill,
  forcing = new_forcing,
  d = new_d
)

saveRDS(newdata, '~/projects/egret/analyses/analyseSeedCues/survival/newdata_species48.rds')

init_fn <- function() list(
  log_tau25 = rnorm(newdata$Nexps, log(50), log(20)/2.57),
  log_spread = rnorm(newdata$Nexps, log(10), log(20)/2.57)
)

newdata$N_newdays <- 150
modelstan <- stan_model("~/projects/egret/analyses/stan/generative/model10_nohier.stan")
fit <- sampling(modelstan, newdata, chains = 4, cores = 4,
                seed = 123456, iter = 1500, warmup = 1000,
                init = init_fn)


init_fn <- function() list(
  gamma = 0.053 * exp(rnorm(newdata$Nexps, 0, 0.1)),
  r     = 0.02  * exp(rnorm(newdata$Nexps, 0, 0.1)),
  pv    = rep(0.7, newdata$Nexps)
)
modelstan <- stan_model("~/projects/egret/analyses/stan/generative/model10_reparam_nohier.stan")
fit <- sampling(modelstan, newdata, chains = 4, cores = 4,
                seed = 123456, iter = 1500, warmup = 1000,
                init = init_fn)



diag <- util$extract_hmc_diagnostics(fit)
util$check_all_hmc_diagnostics(diag)

samples <- util$extract_expectand_vals(fit)
base_samples <- util$filter_expectands(samples,
                                       c('pv', 'gamma', 'r'),
                                       check_arrays = T)
util$check_all_expectand_diagnostics(samples)

for(e in 1:newdata$Nexps){
  print(e)
  util$plot_div_pairs(paste0('gamma[',e,']'), paste0('r[',e,']'), samples, diag)
}






# 
# util$plot_div_pairs('mu_pv', 'sigma_pv_exp', samples, diag,
#                     transforms =  list('sigma_pv_exp' = 1))
# 
# util$plot_div_pairs('mu_log_tau25', 'sigma_log_tau25_exp', samples, diag,
#                     transforms =  list('sigma_log_tau25_exp' = 1))
# 
# util$plot_div_pairs('mu_log_tau25', 'sigma_log_tau25_species', samples, diag,
#                     transforms =  list('sigma_log_tau25_species' = 1))
# 
# util$plot_div_pairs('mu_log_spread', 'sigma_log_spread_exp', samples, diag,
#                     transforms =  list('sigma_log_spread_exp' = 1))
# 
# util$plot_div_pairs('mu_log_spread', 'sigma_log_spread_species', samples, diag,
#                     transforms =  list('sigma_log_spread_species' = 1))
# 
# for(s in 1:newdata$Nspecies){
#   util$plot_div_pairs(paste0('log_spread_species_tilde[',s,']'), 
#                       'sigma_log_spread_species', samples, diag,
#                       transforms = list('sigma_log_spread_species' = 1),
#                       ylim = c(-10, 6), xlim = c(-5, 5))
# }
# 
# for(s in 1:newdata$Nspecies){
#   util$plot_div_pairs(paste0('log_tau25_species[',s,']'), 
#                       'sigma_log_tau25_species', samples, diag,
#                       transforms = list('sigma_log_tau25_species' = 1),
#                       ylim = c(-10, 6), xlim = c(-5, 5))
# }
# 
# for(s in 1:newdata$Nspecies){
#   util$plot_div_pairs(paste0('logit_pv_species_tilde[',s,']'), 
#                       'sigma_pv_species', samples, diag,
#                       transforms = list('sigma_pv_species' = 1),
#                       ylim = c(-10, 6), xlim = c(-5, 5))
# }
# 
# 
# 
# 
# 
# par(mfrow= c(1,3))
# util$plot_expectand_pushforward(samples[['mu_pv']], 30, flim = c(-5, 5))
# # prior <- rnorm(1e6, 0, 1)
# # lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)
# prior <- rnorm(1e6, 1, 2)
# lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)
# 
# util$plot_expectand_pushforward(samples[['sigma_pv_species']], 30, flim = c(0, 7))
# prior <- rnorm(1e6, 0, 3)
# lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)
# 
# util$plot_expectand_pushforward(samples[['sigma_pv_exp']], 50, flim = c(0, 7))
# prior <- rnorm(1e6, 0, 3)
# lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)
# 
# util$plot_expectand_pushforward(samples[['mu_log_tau25']], 50, flim = c(0, 7))
# prior <- rnorm(1e6, log(50), log(20)/2.57)
# lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)
# 
# util$plot_expectand_pushforward(samples[['sigma_log_tau25_species']], 50, flim = c(0, 5))
# prior <- rnorm(1e6, 0, 3)
# lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)
# 
# util$plot_expectand_pushforward(samples[['sigma_log_tau25_exp']], 50, flim = c(0, 5))
# prior <- rnorm(1e6, 0, 3)
# lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)
# 
# util$plot_expectand_pushforward(samples[['mu_log_spread']], 50, flim = c(-7, 7))
# prior <- rnorm(1e6, log(10), log(200)/2.57)
# lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)
# 
# util$plot_expectand_pushforward(samples[['sigma_log_spread_species']], 50, flim = c(0, 10))
# prior <- rnorm(1e6, 0, 3)
# lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)
# 
# util$plot_expectand_pushforward(samples[['sigma_log_spread_exp']], 50, flim = c(0, 10))
# prior <- rnorm(1e6, 0, 3)
# lines(density(prior), col = util$c_light_teal, lwd = 1.5, lty = 2)
# 
# 
# pdf('hmm.pdf')
# for(e in 1:newdata$Nexps){
#   util$plot_pairs_by_chain(samples[[paste0('log_spread[',e,']')]], paste0('log_spread[',e,']'),
#                            samples[[paste0('log_tau25[',e,']')]], paste0('log_tau25[',e,']'))
# }
# dev.off()
# 
# 
# 
# 
