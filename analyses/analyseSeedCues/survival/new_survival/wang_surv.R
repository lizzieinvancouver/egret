rm(list = ls())
setwd('~/projects/egret/analyses/modeling')
util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)
setwd('~/projects/egret/analyses')

subset_input <- readRDS("/home/victor/projects/egret/analyses/stan/generative/sequential_survival/subset_input_10species.rds")

# Process data
olddata <- subset_input
exps_tokeep <- 1:olddata$N_exps
N_species <- olddata$N_species
N_exps <- length(exps_tokeep)

# Format data
species_idxs <- c()
N_seeds <- c()
dxs_chill <- c()
Txs_chill <- c()
Txs_incub <- c()
exp_idxs <- c()
txs_start <- c()
txs_ends <- c()
N_germ <- c()
N_census_perexp <- c()
start_census_idxs <- c()
end_census_idxs <- c()
start_census_id <- 1
for(i in 1:N_exps){
  
  e <- exps_tokeep[i]
  
  species_idxs <- c(species_idxs, olddata$species_idxs[e])
  eidxs <- olddata$start_exp_idxs[e]:olddata$end_exp_idxs[e]
  
  N_seeds_e <- length(olddata$germ_days[eidxs]) + olddata$N_ungerm[e]
  N_seeds <- c(N_seeds, N_seeds_e)
  
  d_chill <- olddata$chill_days[e]
  dxs_chill <- c(dxs_chill, d_chill)
  T_chill <- olddata$chill_temp[e]
  Txs_chill <- c(Txs_chill, T_chill)
  T_incub <- olddata$germ_temp[e]
  Txs_incub <- c(Txs_incub, T_incub)
  
  idxs <- olddata$cens_start_idxs[e]:olddata$cens_end_idxs[e]
  census_ends <- olddata$cens_day[idxs]
  census_starts <- c(0, census_ends[-length(census_ends)])
  
  germ_days <- olddata$germ_days[olddata$start_exp_idxs[e]:olddata$end_exp_idxs[e]]
  seeds_counts <- table(germ_days)
  
  N_cens_exp <- length(census_ends)
  N_census_perexp <- c(N_census_perexp, N_cens_exp)
  
  start_census_idxs <- c(start_census_idxs,start_census_id)
  end_census_id <- start_census_id + N_cens_exp - 1 
  end_census_idxs <- c(end_census_idxs,end_census_id)
  start_census_id <- end_census_id + 1 
  
  for(j in 1:N_cens_exp){
    
    exp_idxs <- c(exp_idxs, i)
    t_start <- census_starts[j]
    txs_start <- c(txs_start, t_start)
    t_end <- census_ends[j]
    txs_ends <- c(txs_ends, t_end)
    
    seed_count <- seeds_counts[as.character(t_end)]
    if(is.na(seed_count)){
      seed_count <- 0
    }
    
    N_germ <- c(N_germ, seed_count)
  }
  
}

N_germ <- as.numeric(N_germ)
N_census <- sum(N_census_perexp)

stan_data <- list(
  N_species = N_species,
  N_exps = N_exps,
  species_idxs = species_idxs,
  N_census_perexp = N_census_perexp,
  start_census_idxs = start_census_idxs,
  end_census_idxs = end_census_idxs,
  
  N_seeds = N_seeds,
  
  dxs_chill = dxs_chill,
  Txs_chill = Txs_chill,
  Txs_incub = Txs_incub,
  
  N_census = N_census,
  exp_idxs = exp_idxs, 
  txs_start = txs_start,
  txs_ends = txs_ends,
  
  N_germ = N_germ
)

# Run model
init_fn <- function() {
  
  # mu_logit_x_opt <- rnorm(1, 1.11, 1.41)
  # sigma_logit_x_opt <- abs(rnorm(1, 1.75, 1.41))
  
  list(
    psi = rnorm(stan_data$N_species, 4.5, 0.5),
    
    sigma = abs(rnorm(stan_data$N_species, 1.5, 0.25)),
    
    T_min = rnorm(1, 5, 0.5),
    delta = abs(rnorm(1, 0.2, 0.02)),
    log_delta_opt = rnorm(stan_data$N_species, log(12), 0.1),
    log_delta_max = rnorm(stan_data$N_species, log(25), 0.1),
    
    kappa = abs(rnorm(stan_data$N_species, 0, 0.1/2.57)), # normal(0, 0.25/2.57) in the model
    pv = rbeta(stan_data$N_species, 4, 2)
  )
}


T_ref_chill <- 10

model <- rstan::stan_model("~/projects/egret/analyses/stan/generative/new_survival/wang_surv_multispecies_nohier.stan")
fit <- rstan::sampling(model, stan_data, iter = 1500, warmup = 1000, cores = 4, init = init_fn, seed = 280926)
saveRDS(fit, '/home/victor/projects/egret/analyses/analyseSeedCues/output/model/new_survival/wang_surv_10species_nohier.rds')

diagnostics <- util$extract_hmc_diagnostics(fit)
util$check_all_expectand_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
# params <- c('psi', 'sigma', 'T_min', 'log_delta_T_max', 'delta', 'logit_x_opt', 'kappa', 'pv')
params <- c('psi', 'sigma', 'T_min', 'log_delta_opt', 'log_delta_max', 'delta', 'kappa', 'pv')
base_samples <- util$filter_expectands(samples, params, check_arrays = T)
util$check_all_expectand_diagnostics(base_samples, min_ess_hat_per_chain = 50)

util$plot_pairs_by_chain(samples[['log_delta_max[6]']], 'log_delta_max[6]',
                         samples[['log_delta_opt[6]']], 'log_delta_opt[6]')

util$plot_pairs_by_chain(samples[['T_min']], 'T_min',
                         samples[['T_max']], 'T_max')



for(sp in 1:stan_data$N_species){
  util$plot_pairs_by_chain(samples[[paste0('log_delta_max[',sp,']')]], paste0('log_delta_max[',sp,']'),
                           samples[[paste0('log_delta_opt[',sp,']')]], paste0('log_delta_opt[',sp,']'))
}



for(sp in 1:stan_data$N_species){
  util$plot_div_pairs(paste0('log_delta_max[',sp,']'), paste0('log_delta_opt[',sp,']'), samples, diagnostics)
}


par(mfrow = c(2,2))
for(chain in 1:4){
  subsamples <- lapply(base_samples, function(m) m[chain,, drop = FALSE])
  util$plot_disc_pushforward_quantiles(subsamples, paste0('logit_x_opt[',1:stan_data$N_species,']'))
}

par(mfrow = c(1,1))
util$plot_disc_pushforward_quantiles(samples, paste0('T_opt[',1:stan_data$N_species,']'), ylab = 'T_opt')



f_stim <- function(temp, tmin, topt, tmax, delta){
  forcing <- rep(NA, length(temp))
  
  for(i in 1:length(temp)){
    if(temp[i] < tmin){
      forcing[i] <- 0.001
    } else if(temp[i] > tmax){
      forcing[i] <- 0.001
    } else if(topt > (tmin + tmax) / 2){
      phi <- (tmax - topt) / (topt - tmin)
      gamma <- (delta * tmax + topt - (delta + 1) * tmin) / (tmax - topt)
      
      a <- (temp[i] - tmin) / (topt - tmin)
      b <- (tmax - temp[i]) / (tmax - topt)
      forcing[i] <- (a * b ^ phi) ^ gamma + 0.001
    } else if(topt <= (tmin + tmax) / 2){
      phi <- (topt - tmin) / (tmax - topt)
      gamma <- ((delta + 1) * tmax - topt - delta * tmin) / (topt - tmin)
      
      a <- (tmax - temp[i]) / (tmax - topt)
      b <- (temp[i] - tmin) / (topt - tmin)
      forcing[i] <- (a * b ^ phi) ^ gamma + 0.001
    }
  }
  
  return(forcing)
}





par(mfrow = c(5,2))
Tx <- seq(-20,80,1)
cols <- colorRampPalette(c("#B97C7C", "#7C9EB9", "#9EB97C"))(stan_data$N_species)
for(sp in 1:stan_data$N_species){
  plot(NULL, xlim = range(Tx), ylim = c(0,1.01),
       xlab = "Temperature", ylab = "Forcing stimulus",
       bty = "n", type = 'l', col = util$c_light, lwd = 2,
       main = paste0('Species ', sp))
  fx <- lapply(Tx, function(T){
    
    N <- length(samples[['T_min']])
    fmat <- matrix(nrow = nrow(samples[['T_min']]), ncol = ncol(samples[['T_min']]))
    for(i in 1:N){
      f <- f_stim(T, 
                  tmin = samples[['T_min']][i], 
                  topt = samples[[paste0('T_opt[', sp,']')]][i], 
                  tmax = samples[[paste0('T_max[', sp,']')]][i], 
                  delta = samples[['delta']][i])
      
      fmat[i] <- f
    }
    
    util$ensemble_mcmc_quantile_est(fmat, c(0.1, 0.5, 0.9))
  })
  fx <- do.call(rbind, fx)
  polygon(x = c(Tx, rev(Tx)), y = c(fx[,'10%'], rev(fx[,'90%'])),
          col = paste0(cols[sp],50), border = NA)
  lines(x = Tx, y = fx[,'50%'], col = cols[sp], lwd = 2)
  text(x = -20, y = 0.95, adj = 0, 
       labels = paste0('T°rng:\n', paste(round(range(stan_data$Txs_incub[stan_data$species_idxs == sp]),1), collapse = '-')), cex = 0.65)
}
