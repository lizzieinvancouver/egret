# Fit the model on some subset of real data
# For the new excting survival model
## Started 12 Aug. 2026
## Last modified: 8 Sept. 2026

setwd('~/projects/egret/analyses/modeling')
util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)
setwd('~/projects/egret/analyses')


library(rstan)
# data <- readRDS('~/projects/egret/analyses/analyseSeedCues/survival/newdata.rds')
data <- mdl.data
data <- newdata

germ_days <- c()
germ_days_prev <- c() 
N_ungerm <- c()
N_obs <- c()
start_exp_idxs <- c()
end_exp_idxs <- c()
germ_temps <- c()
chill_cond <- c()
exp_idxs <- c()
uniq_species <- unique(data$species_idxs)
species_idxs <- c()
last_day <- c() 
cens_day <- c()                                    
cens_start_idxs <- c()                             
cens_end_idxs <- c()
germ_obs <- c()
idx <- 1
jdx <- 1                                           
for(e in 1:data$Nexps){
  
  start <- data$exp_start_idxs[e]
  end <- data$exp_end_idxs[e]
  
  nungerm_here <- data$seeds[start]
  germ_temp <- data$forcing[e]
  germ_temps <- c(germ_temps, germ_temp)
  
  chill <- data$chill[e]
  chill_cond <- c(chill_cond, chill)
  
  cens_start_idxs <- c(cens_start_idxs, jdx)  
  
  count <- 0
  prev <- 0
  for(i in c(start+1):end){
    
    seeds <- data$seeds[i]
    day_here <- data$d[i] + 1
    
    germ_obs <- c(germ_obs, seeds)
    
    germ_days <- c(germ_days, rep(day_here, seeds))
    germ_days_prev <- c(germ_days_prev, rep(prev, seeds)) 
    
    cens_day <- c(cens_day, day_here) 
    jdx <- jdx + 1
    
    prev <- day_here  
    count <- count + seeds
    
  }
  N_obs <- c(N_obs, count)
  N_ungerm <- c(N_ungerm, nungerm_here)
  last_day <- c(last_day, prev)
  
  cens_end_idxs <- c(cens_end_idxs, jdx - 1)  
  
  start_exp_idxs <- c(start_exp_idxs, idx)
  idx <- idx + count
  end_exp_idxs <- c(end_exp_idxs, idx - 1)
}

N <- sum(N_obs)
N_cens <- length(cens_day)

stopifnot(all(germ_days_prev < germ_days))
stopifnot(all(cens_day[cens_end_idxs] == last_day))

newdata <- list(
  N_exps = data$Nexps,
  germ_temp = as.array(germ_temps*10),
  chill_cond = chill_cond, 
  N = N,
  germ_days = germ_days,
  germ_days_prev = germ_days_prev,
  last_day = last_day,  
  start_exp_idxs = start_exp_idxs,
  end_exp_idxs = end_exp_idxs,
  N_obs = N_obs,
  N_ungerm = N_ungerm,
  N_cens = N_cens,
  cens_day = cens_day,
  cens_start_idxs = cens_start_idxs,
  cens_end_idxs = cens_end_idxs,
  N_species = length(uniq_species),
  species_idxs = data$species_idxs
)

# ----------------------------------------------------------#
# First model: no covariates, free parameters by experiment #
# ----------------------------------------------------------#
modelstan <- stan_model("~/projects/egret/analyses/stan/generative/survival/egret_surv_accs.stan")
# fit <- sampling(modelstan, newdata, chains = 4, cores = 4,
#                 seed = 12345, iter = 2000, warmup = 1000)
# saveRDS(fit, file = "/home/victor/projects/egret/analyses/analyseSeedCues/output/model/survival/fit_nocovar.rds")
fit <- readRDS("/home/victor/projects/egret/analyses/analyseSeedCues/output/model/survival/fit_nocovar.rds")

get_elapsed_time(fit)
diagnostics <- util$extract_hmc_diagnostics(fit)
util$check_all_expectand_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
base_samples <- util$filter_expectands(samples,
                                       c('log_b', "log_c", "pv"), 
                                       check_arrays = T)
util$check_all_expectand_diagnostics(base_samples)



# ------------------------------------#
# Second model: germ. temp. on log(c) #
# ------------------------------------#
modelstan <- stan_model("~/projects/egret/analyses/stan/generative/survival/egret_surv_forcinglogc.stan")
fit2 <- sampling(modelstan, newdata, chains = 4, cores = 4,
                seed = 12345, iter = 1500, warmup = 1000, refresh = 10)
saveRDS(fit2, file = "/home/victor/projects/egret/analyses/analyseSeedCues/output/model/survival/fit2_forcinglogc.rds")
diagnostics <- util$extract_hmc_diagnostics(fit2)
util$check_all_expectand_diagnostics(diagnostics)
get_elapsed_time(fit2)

samples <- util$extract_expectand_vals(fit2)
base_samples <- util$filter_expectands(samples, 
                                       c('mu_a_c', 'sigma_a_c', paste0('bT_c[',1:newdata$N_species,']'),
                                         'mu_bT_c', 'sigma_bT_c', paste0('a_c[',1:newdata$N_species,']'),
                                         paste0('log_b[',1:newdata$N_species,']'),
                                         paste0('pv[',1:newdata$N_species,']')),
                                       check_arrays = T)
util$check_all_expectand_diagnostics(base_samples)


util$plot_pairs_by_chain(samples[['log_b[13]']], 'log_b[13]',
                         samples[['pv[13]']], 'pv[13]')


par(mfrow = c(1,1), cex.main = 1, mar = c(4,5,1,1))
util$plot_hist_quantiles(samples, 'germ_pred', baseline_values = germ_obs)

par(mfrow = c(1,1), cex.main = 1, mar = c(4,5,1,1))
util$plot_disc_pushforward_quantiles(samples,  paste0('a_c[',1:newdata$N_species,']'))
util$plot_disc_pushforward_quantiles(samples,  paste0('bT_c[',1:newdata$N_species,']'),
                                     ylab = 'beta_forc_logc', xlab = 'Species')
# util$plot_disc_pushforward_quantiles(samples,  paste0('bchill_pv[',1:newdata$N_species,']'))


par(mfrow = c(2,2), cex.main = 1, mar = c(4,5,1,1))
prior <- rnorm(1e6, log(0.1), 1)
util$plot_expectand_pushforward(samples[['mu_a_c']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)
prior <- rnorm(1e6, 0, 0.5)
util$plot_expectand_pushforward(samples[['sigma_a_c']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)
prior <- rnorm(1e6, 0.1, 0.1)
util$plot_expectand_pushforward(samples[['mu_bT_c']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)
prior <- rnorm(1e6, 0, 0.05)
util$plot_expectand_pushforward(samples[['sigma_bT_c']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)


# ----------------------------------------------#
# Third model: germ. temp. on log(c) AND log(b) #
# ----------------------------------------------#
modelstan <- stan_model("~/projects/egret/analyses/stan/generative/survival/egret_surv_forcinglogc_forcinglogb.stan")
fit3 <- sampling(modelstan, newdata, chains = 4, cores = 4,
                 seed = 123456, iter = 1500, warmup = 1000, refresh = 10,
                 control = list(max_treedepth = 8))
saveRDS(fit3, file = "/home/victor/projects/egret/analyses/analyseSeedCues/output/model/survival/fit3_forcinglogc_forcinglogb.rds")
diagnostics <- util$extract_hmc_diagnostics(fit3)
util$check_all_expectand_diagnostics(diagnostics)
get_elapsed_time(fit3)

samples <- util$extract_expectand_vals(fit3)
base_samples <- util$filter_expectands(samples, 
                                       c('mu_a_c', 'sigma_a_c', paste0('bT_c[',1:newdata$N_species,']'),
                                         'mu_bT_c', 'sigma_bT_c', paste0('a_c[',1:newdata$N_species,']'),
                                         'mu_a_b', 'sigma_a_b', paste0('bT_b[',1:newdata$N_species,']'),
                                         'mu_bT_b', 'sigma_bT_b', paste0('a_b[',1:newdata$N_species,']'),
                                         paste0('log_b[',1:newdata$N_species,']'),
                                         paste0('pv[',1:newdata$N_species,']')),
                                       check_arrays = T)
util$check_all_expectand_diagnostics(base_samples, min_ess_hat_per_chain = 50)

util$plot_pairs_by_chain(samples[['a_c[19]']], 'a_c[19]',
                         samples[['a_b[19]']], 'a_b[19]')


par(mfrow = c(2,1), cex.main = 1, mar = c(4,5,1,1))
util$plot_disc_pushforward_quantiles(samples,  paste0('bT_b[',1:newdata$N_species,']'))
util$plot_disc_pushforward_quantiles(samples,  paste0('bT_c[',1:newdata$N_species,']'),
                                     ylab = 'beta_forc_logc', xlab = 'Species')


par(mfrow = c(2,2), cex.main = 1, mar = c(4,5,1,1))
prior <- rnorm(1e6, log(0.1), 1)
util$plot_expectand_pushforward(samples[['mu_a_c']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)
prior <- rnorm(1e6, 0, 0.5)
util$plot_expectand_pushforward(samples[['sigma_a_c']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)
prior <- rnorm(1e6, 0, 0.1)
util$plot_expectand_pushforward(samples[['mu_bT_c']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)
prior <- rnorm(1e6, 0, 0.05)
util$plot_expectand_pushforward(samples[['sigma_bT_c']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)

prior <- rnorm(1e6, 4, 2)
util$plot_expectand_pushforward(samples[['mu_a_b']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)
prior <- rnorm(1e6, 0, 1)
util$plot_expectand_pushforward(samples[['sigma_a_b']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)
prior <- rnorm(1e6, -0.2, 0.2)
util$plot_expectand_pushforward(samples[['mu_bT_b']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)
prior <- rnorm(1e6, 0, 0.1)
util$plot_expectand_pushforward(samples[['sigma_bT_b']], 30, flim = range(prior))
lines(density(prior), lwd = 2, col = util$c_light_teal)



# -------------------------------------------------------------------#
# Fourth model: germ. temp. on log(c) AND log(b), chilling on log(b) #
# -------------------------------------------------------------------#
modelstan <- stan_model("~/projects/egret/analyses/stan/generative/survival/egret_surv_forcinglogc_forcinglogb_chillinglogb.stan")
fit4 <- sampling(modelstan, newdata, chains = 4, cores = 4,
                 seed = 123456, iter = 1500, warmup = 1000, refresh = 10)
saveRDS(fit4, file = "/home/victor/projects/egret/analyses/analyseSeedCues/output/model/survival/fit4_forcinglogc_forcinglogb_chillinglogb.rds")
diagnostics <- util$extract_hmc_diagnostics(fit4)
util$check_all_expectand_diagnostics(diagnostics)
get_elapsed_time(fit4)


samples <- util$extract_expectand_vals(fit4)
base_samples <- util$filter_expectands(samples, 
                                       c('mu_a_c', 'sigma_a_c', paste0('bT_c[',1:newdata$N_species,']'),
                                         'mu_bT_c', 'sigma_bT_c', paste0('a_c[',1:newdata$N_species,']'),
                                         'mu_a_b', 'sigma_a_b', paste0('a_b[',1:newdata$N_species,']'),
                                         'mu_bT_b', 'sigma_bT_b', paste0('bT_b[',1:newdata$N_species,']'),
                                         'mu_bC_b', 'sigma_bC_b', paste0('bC_b[',1:newdata$N_species,']'),
                                         paste0('log_b[',1:newdata$N_species,']'),
                                         paste0('pv[',1:newdata$N_species,']')),
                                       check_arrays = T)
util$check_all_expectand_diagnostics(base_samples, min_ess_hat_per_chain = 50)

util$plot_pairs_by_chain(samples[['a_b[3]']], 'a_b[3]',
                         samples[['bC_b[3]']], 'bC_b[3]')
util$plot_pairs_by_chain(samples[['bT_b[3]']], 'bT_b[3]',
                         samples[['bC_b[3]']], 'bC_b[3]')
util$plot_pairs_by_chain(samples[['a_b[3]']], 'a_b[3]',
                         samples[['bT_b[3]']], 'bT_b[3]')

util$plot_pairs_by_chain(samples[['a_b[10]']], 'a_b[10]',
                         samples[['bC_b[10]']], 'bC_b[10]')
util$plot_pairs_by_chain(samples[['bT_b[10]']], 'bT_b[10]',
                         samples[['bC_b[10]']], 'bC_b[10]')
util$plot_pairs_by_chain(samples[['a_b[10]']], 'a_b[10]',
                         samples[['bT_b[10]']], 'bT_b[10]')


# -------------------------------------------------------------------#
# Fourth model: germ. temp. on log(c) AND log(b), chilling on log(b) #
# -------------------------------------------------------------------#
modelstan <- stan_model("~/projects/egret/analyses/stan/generative/survival/egret_surv_forcinglogc_forcinglogb_chillinglogb_chillingpv.stan")
fit4 <- sampling(modelstan, newdata, chains = 4, cores = 4,
                 seed = 123456, iter = 15, warmup = 10, refresh = 10)
saveRDS(fit4, file = "/home/victor/projects/egret/analyses/analyseSeedCues/output/model/survival/fit4_forcinglogc_forcinglogb_chillinglogb.rds")
diagnostics <- util$extract_hmc_diagnostics(fit4)
util$check_all_expectand_diagnostics(diagnostics)
get_elapsed_time(fit4)


# 
# par(mfrow = c(7,4), cex.main = 1, mar = c(4,5,1,1))
# for(e in 1:newdata$N_exps){
#   
#   cs <- newdata$cens_start_idxs[e]
#   ce <- newdata$cens_end_idxs[e]
#   
#   max <- 120
#   util$plot_disc_pushforward_quantiles(samples, paste0('germ_pred[',cs:ce,']'), 
#                                        display_ylim = c(0, max*1.2), ylab = 'Observed germination\n(#seeds, at each census)',
#                                        baseline_values = germ_obs[cs:ce])
# }
# 
# 
# 
# 
# 
# 
# par(mfrow = c(7,4), cex.main = 1, mar = c(4,5,1,1))
# for(e in 1:newdata$N_exps){
#   
#   cs <- newdata$cens_start_idxs[e]
#   ce <- newdata$cens_end_idxs[e]
#   
#   # max <- max(germ_obs[cs:ce])
#   max <- 150
#   util$plot_conn_pushforward_quantiles(samples, paste0('germ_pred[',cs:ce,']'), plot_xs = newdata$cens_day[cs:ce],
#                                        display_ylim = c(0, max*1.2), ylab = 'Observed germination\n(#seeds, at each census)')
#   points(x = newdata$cens_day[cs:ce], y = germ_obs[cs:ce], pch = 20, cex = 1, col = 'white')
#   points(x = newdata$cens_day[cs:ce], y = germ_obs[cs:ce], pch = 20, cex = 0.5, col = 'black')
# }
# 
# par(mfrow = c(7,4), cex.main = 1, mar = c(4,5,1,1))
# for(e in 1:newdata$N_exps){
#   
#   cs <- newdata$cens_start_idxs[e]
#   ce <- newdata$cens_end_idxs[e]
#   
#   max <- 120
#   util$plot_disc_pushforward_quantiles(samples, paste0('germ_pred[',cs:ce,']'), 
#                                        display_ylim = c(0, max*1.2), ylab = 'Observed germination\n(#seeds, at each census)',
#                                        baseline_values = germ_obs[cs:ce])
# }
# 
# 
# par(mfrow = c(3,4), cex.main = 1, mar = c(4,5,1,1))
# for(e in 1:12){
#   
#   cs <- newdata$cens_start_idxs[e]
#   ce <- newdata$cens_end_idxs[e]
#   
#   print(newdata$species_idxs[e])
#   
#   # max <- max(germ_obs[cs:ce])
#   max <- 150
#   util$plot_conn_pushforward_quantiles(samples, paste0('germ_pred[',cs:ce,']'), plot_xs = newdata$cens_day[cs:ce],
#                                        display_ylim = c(0, max*1.2), ylab = 'Observed germination\n(#seeds, at each census)')
#   points(x = newdata$cens_day[cs:ce], y = germ_obs[cs:ce], pch = 20, cex = 1, col = 'white')
#   points(x = newdata$cens_day[cs:ce], y = germ_obs[cs:ce], pch = 20, cex = 0.5, col = 'black')
#   text(x = 0, y = 160, adj = 0,
#        labels = paste0('Forcing=', newdata$germ_temp[e], ', chilling=', newdata$chill_cond[e]))
# }
# 
# 
# 
# for(e in 1:newdata$N_exps){
#   
#   samples[[paste0('t50[',e,']')]] <-
#     log(samples[[paste0('b[',e,']')]]/log(2))/samples[[paste0('c[',e,']')]]
#   
# }
# 
# par(mfrow = c(2,2), cex.main = 1, mar = c(4,5,1,1))
# for(s in 1){
#   idxs <- which(newdata$species_idxs == s)
#   idxs <- idxs[order(newdata$germ_temp[idxs])]
#   
#   chill <- newdata$chill_cond[idxs]
#   germtemp <- newdata$germ_temp[idxs]
#   for(c in unique(chill)){
#     
#     idxs_here <- which(chill == c)
#     util$plot_conn_pushforward_quantiles(samples, paste0('c[', idxs[idxs_here],']'),
#                                          plot_xs = germtemp[idxs_here])
#     
#   }
#   
#   
# }
# 
# 
# par(mfrow = c(1,1))
# util$plot_disc_pushforward_quantiles(samples,  paste0('k[',1:newdata$N_species,']'))
# 
# par(mfrow = c(1,1))
# util$plot_disc_pushforward_quantiles(samples,  paste0('a_pv[',1:newdata$N_species,']'))
