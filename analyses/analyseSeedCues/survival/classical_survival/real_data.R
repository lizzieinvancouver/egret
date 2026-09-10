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
data <- readRDS('~/projects/egret/analyses/analyseSeedCues/survival/newdata.rds')


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


modelstan <- stan_model("~/projects/egret/analyses/stan/generative/survival/egret_surv_forc_chill_chillpv.stan")
fit <- sampling(modelstan, newdata, chains = 4, cores = 4,
                seed = 12345, iter = 2000, warmup = 1000)
diagnostics <- util$extract_hmc_diagnostics(fit)
util$check_all_expectand_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
base_samples <- util$filter_expectands(samples,
                                       c('mu_log_c0', 'sigma_log_c0', 'log_c0',
                                         'mu_log_k', 'sigma_log_k', 'log_k',
                                         'mu_beta_chill', 'sigma_beta_chill', 'beta_chill',
                                         'mu_T0', 'sigma_T0', 'T0',
                                         'mu_log_b', 'sigma_log_b', 'log_b_sp',
                                         "mu_a_pv", "sigma_a_pv", "a_pv",
                                         "mu_bchill_pv", "sigma_bchill_pv", "bchill_pv"), check_arrays = T)
util$check_all_expectand_diagnostics(base_samples)

util$plot_pairs_by_chain(samples[['bchill_pv[13]']], 'bchill_pv[13]',
                         samples[['sigma_bchill_pv']], 'sigma_bchill_pv')

util$plot_pairs_by_chain(samples[['bchill_pv[5]']], 'bchill_pv[5]',
                         samples[['sigma_bchill_pv']], 'sigma_bchill_pv')


par(mfrow = c(7,4), cex.main = 1, mar = c(4,5,1,1))
for(e in 1:28){
  
  cs <- newdata$cens_start_idxs[e]
  ce <- newdata$cens_end_idxs[e]
  
  # max <- max(germ_obs[cs:ce])
  max <- 100
  util$plot_conn_pushforward_quantiles(samples, paste0('germ_pred[',cs:ce,']'), plot_xs = newdata$cens_day[cs:ce],
                                       display_ylim = c(0, max*1.2), ylab = 'Observed germination\n(#seeds, at each census)')
  points(x = newdata$cens_day[cs:ce], y = germ_obs[cs:ce], pch = 20, cex = 1, col = 'white')
  points(x = newdata$cens_day[cs:ce], y = germ_obs[cs:ce], pch = 20, cex = 0.5, col = 'black')
}

par(mfrow = c(7,4), cex.main = 1, mar = c(4,5,1,1))
for(e in 1:28){
  
  cs <- newdata$cens_start_idxs[e]
  ce <- newdata$cens_end_idxs[e]
  
  max <- 120
  util$plot_disc_pushforward_quantiles(samples, paste0('germ_pred[',cs:ce,']'), 
                                       display_ylim = c(0, max*1.2), ylab = 'Observed germination\n(#seeds, at each census)',
                                       baseline_values = germ_obs[cs:ce])
}


par(mfrow = c(3,4), cex.main = 1, mar = c(4,5,1,1))
for(e in 1:12){
  
  cs <- newdata$cens_start_idxs[e]
  ce <- newdata$cens_end_idxs[e]
  
  print(newdata$species_idxs[e])
  
  # max <- max(germ_obs[cs:ce])
  max <- 150
  util$plot_conn_pushforward_quantiles(samples, paste0('germ_pred[',cs:ce,']'), plot_xs = newdata$cens_day[cs:ce],
                                       display_ylim = c(0, max*1.2), ylab = 'Observed germination\n(#seeds, at each census)')
  points(x = newdata$cens_day[cs:ce], y = germ_obs[cs:ce], pch = 20, cex = 1, col = 'white')
  points(x = newdata$cens_day[cs:ce], y = germ_obs[cs:ce], pch = 20, cex = 0.5, col = 'black')
  text(x = 0, y = 160, adj = 0,
       labels = paste0('Forcing=', newdata$germ_temp[e], ', chilling=', newdata$chill_cond[e]))
}



for(e in 1:newdata$N_exps){
  
  samples[[paste0('t50[',e,']')]] <-
    log(samples[[paste0('b[',e,']')]]/log(2))/samples[[paste0('c[',e,']')]]
  
}

par(mfrow = c(2,2), cex.main = 1, mar = c(4,5,1,1))
for(s in 1){
  idxs <- which(newdata$species_idxs == s)
  idxs <- idxs[order(newdata$germ_temp[idxs])]
  
  chill <- newdata$chill_cond[idxs]
  germtemp <- newdata$germ_temp[idxs]
  for(c in unique(chill)){
    
    idxs_here <- which(chill == c)
    util$plot_conn_pushforward_quantiles(samples, paste0('c[', idxs[idxs_here],']'),
                                         plot_xs = germtemp[idxs_here])
    
  }
  
  
}




