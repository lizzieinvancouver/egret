
# Fit the model on some subset of real data
# For the new excting survival model

# 12 Aug. 2026

data <- readRDS('~/projects/egret/analyses/analyseSeedCues/survival/newdata.rds')


germ_days <- c()
N_ungerm <- c()
N_obs <- c()
start_exp_idxs <- c()
end_exp_idxs <- c()
germ_temps <- c()
chill_cond <- c()
exp_idxs <- c()
uniq_species <- unique(data$species_idxs)
species_idxs <- c()
idx <- 1
for(e in 1:data$Nexps){
  
  start <- data$exp_start_idxs[e]
  end <- data$exp_end_idxs[e]
  
  nungerm_here <- data$seeds[start]
  germ_temp <- data$forcing[e]
  germ_temps <- c(germ_temps, germ_temp)
  
  chill <- data$chill[e]
  chill_cond <- c(chill_cond, chill)
  
  count <- 0
  
  for(i in c(start+1):end){
    
    seeds <- data$seeds[i]
    
    # if( data$d[i] == 0){stop()}
    days_here <- rep(data$d[i]+1, seeds)
    germ_days <- c(germ_days, days_here)
    count <- count + seeds
    
  }
  N_obs <- c(N_obs, count)
  N_ungerm <- c(N_ungerm, nungerm_here)

  start_exp_idxs <- c(start_exp_idxs, idx)
  idx <- idx + count
  end_exp_idxs <- c(end_exp_idxs, idx - 1)
}

max_days <- max(floor(germ_days))
N <- sum(N_obs)

newdata <- list(
  N_exps = data$Nexps,
  germ_temp = as.array(germ_temps*10),
  chill_cond = chill_cond, 
  N = N,
  germ_days = floor(germ_days),
  start_exp_idxs = start_exp_idxs,
  end_exp_idxs = end_exp_idxs,
  N_obs = N_obs,
  N_ungerm = N_ungerm,
  max_days = max_days,
  N_species = length(uniq_species),
  species_idxs = data$species_idxs
)


modelstan <- stan_model("~/projects/egret/analyses/stan/generative/survival/egret_surv_multispecies.stan")
fit <- sampling(modelstan, newdata, chains = 4, cores = 4,
                seed = 123456, iter = 2000, warmup = 1000)
diagnostics <- util$extract_hmc_diagnostics(fit)
util$check_all_expectand_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
base_samples <- util$filter_expectands(samples,
                                       c('T0', 'k', 'pv', 'log_Psi0', 'log_sigma'), check_arrays = T)
util$check_all_expectand_diagnostics(base_samples)


par(mfrow = c(3,2), mar = c(4,4,1,1))

util$plot_expectand_pushforward(samples[['T0[1]']], 30, flim = c(0, 40),
                                display_name = 'T0')
for(s in 2:newdata$N_species){
  util$plot_expectand_pushforward(samples[[paste0('T0[',s,']')]], 30, flim = c(0, 40),
                                  display_name =  paste0('T0[',s,']'), add = T)
}

util$plot_expectand_pushforward(samples[['k[1]']], 30, flim = c(0, 2),
                                display_name = 'k')
for(s in 2:newdata$N_species){
  util$plot_expectand_pushforward(samples[[paste0('k[',s,']')]], 30, flim = c(0, 2),
                                  display_name =  paste0('k[',s,']'), add = T)
}

util$plot_expectand_pushforward(samples[['Psi0[1]']], 30, flim = c(0, 30),
                                display_name = 'Psi0')
for(s in 2:newdata$N_species){
  util$plot_expectand_pushforward(samples[[paste0('Psi0[',s,']')]], 30, flim = c(0, 30),
                                  display_name =  paste0('Psi0[',s,']'), add = T)
}

util$plot_expectand_pushforward(samples[['sigma[1]']], 30, flim = c(0, 20),
                                display_name = 'sigma')
for(s in 2:newdata$N_species){
  util$plot_expectand_pushforward(samples[[paste0('sigma[',s,']')]], 30, flim = c(0, 20),
                                  display_name =  paste0('sigma[',s,']'), add = T)
}

util$plot_expectand_pushforward(samples[['pv[1]']], 30, flim = c(0, 1),
                                display_name = 'pv')
for(s in 2:newdata$N_species){
  util$plot_expectand_pushforward(samples[[paste0('pv[',s,']')]], 30, flim = c(0, 1),
                                  display_name =  paste0('pv[',s,']'), add = T)
}

util$plot_expectand_pushforward(samples[['Psi0[1]']]/samples[['sigma[1]']], 30, flim = c(0, 10),
                                display_name = 'Psi0/sigma')
for(s in 2:newdata$N_species){
  util$plot_expectand_pushforward(samples[[paste0('Psi0[',s,']')]]/samples[[paste0('sigma[',s,']')]], 30, flim = c(0, 10),
                                  display_name =  paste0('Psi0[',s,']'), add = T)
}


for(s in 1:newdata$N_species){
  util$plot_pairs_by_chain(samples[[paste0('T0[',s,']')]], paste0('T0[',s,']'),
                           samples[[paste0('k[',s,']')]], paste0('k[',s,']'))
}


for(s in 1:newdata$N_species){
  util$plot_pairs_by_chain(samples[[paste0('Psi0[',s,']')]], paste0('Psi0[',s,']'),
                           samples[[paste0('sigma[',s,']')]], paste0('sigma[',s,']'))
}



dgerm_obs <- array(0, dim = c(newdata$N_exps, newdata$max_days))
cumgerm_obs <- array(0, dim = c(newdata$N_exps, newdata$max_days))
for(e in 1:newdata$N_exps){
  
  idxs <- newdata$start_exp_idxs[e]:newdata$end_exp_idxs[e]
  
  for(i in idxs){
    
    dgerm_obs[e, newdata$germ_days[i]] <- dgerm_obs[e, newdata$germ_days[i]] + 1
    
  }
  
  cumgerm_obs[e,] <- cumsum(dgerm_obs[e, ])
  
}

par(mfrow = c(3,3), cex.main = 1)
for(e in 1:newdata$N_exps){
  util$plot_conn_pushforward_quantiles(samples, paste0('cumgerm_pred[',e,',',1:newdata$max_days,']'), 1:newdata$max_days,
                                       display_ylim = c(0 ,300),
                                       main = paste0('exp ', e,
                                                     ' - forcing temp. = ', round(newdata$germ_temp[e],1), 'degC',
                                                     ' - chilling = ', round(newdata$chill_cond[e],1), 'units'),
                                       ylab = 'Cumulative germination')
  idxs <- newdata$start_exp_idxs[e]:newdata$end_exp_idxs[e]
  days <- unique(newdata$germ_days[idxs])
  points(x = days, y = cumgerm_obs[e,days], pch = 20, cex = 2, col = 'white')
  points(x = days, y = cumgerm_obs[e,days], pch = 20, cex = 1, col = 'black')
}

par(mfrow = c(1,1))
constant_temp <- seq(-10, 50, 1)
qy <- sapply(constant_temp, function(t){
  k <- samples[['k[4]']]
  T0 <- samples[['T0[4]']]
  y <- boot::inv.logit(k * (t - T0))
  util$ensemble_mcmc_quantile_est(y, c(0.05, 0.5, 0.95))
})
plot(qy['50%',] ~ constant_temp, type = 'l', lwd = 2, col = util$c_mid_highlight)
lines(qy['5%',] ~ constant_temp, col = util$c_mid, lwd = 1, lty = 2)
lines(qy['95%',] ~ constant_temp, col = util$c_mid, lwd = 1, lty = 2)
