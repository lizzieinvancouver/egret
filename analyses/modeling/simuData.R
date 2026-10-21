## Started 03 October 2026
## By Ken

## Try simulating egret data

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdre", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses/modeling')
  util <- new.env()
  source('mcmc_analysis_tools_rstan.R', local=util)
  source('mcmc_visualization_tools.R', local=util)
  setwd('~/projects/egret/analyses')
} 

library(logitnorm)

#stan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

# Calculate daily forcing according to Smoothed Wang & Engel model
swe_model <- function(temp, tmin, topt, tmax, delta){
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

# simulate conditions
N_species <- 50
N_seedlots <- ceiling(3 * rbeta(N_species, 2, 5))
N_conditions <- ceiling(3 * rbeta(sum(N_seedlots), 5, 5))
N_phases <- ceiling(3 * rbeta(sum(N_conditions), 5, 5) + 2.01)
N_seeds <- round(runif(sum(N_conditions), 30, 300))
N_obs <- round(10 * rbeta(sum(N_conditions), 1, 2)) + 1

sp <- 1:50
sp_start <- rep(NA, N_species)
sp_end <- rep(NA, N_species)
sl <- rep(NA, sum(N_seedlots))
cond <- rep(NA, sum(N_conditions))
phase <- rep(NA, sum(N_phases))
temp <- rep(NA, sum(N_phases))
dur <- rep(NA, sum(N_phases))
moist <- rep(NA, sum(N_phases))
obs <- rep(NA, sum(N_obs))

for(i in 1:N_species){
  sl[(sum(N_seedlots[0:(i-1)]) + 1):(sum(N_seedlots[0:(i - 1)]) + N_seedlots[i])] <- 1:N_seedlots[i]
}

for(i in 1:sum(N_seedlots)){
  cond[(sum(N_conditions[0:(i-1)]) + 1):(sum(N_conditions[0:(i - 1)]) + N_conditions[i])] <- 1:N_conditions[i]
}

for(i in 1:sum(N_conditions)){
  phase[(sum(N_phases[0:(i-1)]) + 1):(sum(N_phases[0:(i - 1)]) + N_phases[i])] <- 1:N_phases[i]
  
  temp[(sum(N_phases[0:(i-1)]) + 1):(sum(N_phases[0:(i - 1)]) + N_phases[i])] <- round(runif(N_phases[i], 0, 30), 1)
  dur[(sum(N_phases[0:(i-1)]) + 1):(sum(N_phases[0:(i - 1)]) + N_phases[i])] <- ceiling(720 * rbeta(N_phases[i], 10, 100) + 0.01)
  moist[(sum(N_phases[0:(i-1)]) + 1)] <- 0
  moist[(sum(N_phases[0:(i-1)]) + 2):(sum(N_phases[0:(i - 1)]) + N_phases[i])] <- 1
  
  is_equal <- FALSE
  while(!is_equal){
    cond_obs <- sort(unique(c(ceiling(runif(N_obs[i], 0, sum(dur[(sum(N_phases[0:(i-1)]) + 2):(sum(N_phases[0:(i - 1)]) + N_phases[i])]))), sum(dur[(sum(N_phases[0:(i-1)]) + 2):(sum(N_phases[0:(i - 1)]) + N_phases[i])]))))
    if(length(cond_obs) == N_obs[i]){
      obs[(sum(N_obs[0:(i - 1)]) + 1):(sum(N_obs[0:(i - 1)]) + N_obs[i])] <- cond_obs + dur[(sum(N_phases[0:(i-1)]) + 1)]
      break
    }
  }
  
  ceiling(runif(N_obs[i], 0, sum(dur[(sum(N_phases[0:(i - 1)]) + 2):(sum(N_phases[0:(i - 1)]) + N_phases[i])])))
  
  phase[(sum(N_phases[0:(i-1)]) + 1):(sum(N_phases[0:(i - 1)]) + N_phases[i])] <- 1:N_phases[i]
}

# simulate germination data
nlv <- 2
tlv <- 2
nld <- 3
tld <- 0.5
nlx <- 1.05
tlx <- 2.06 / 2.32
nlp <- 2
tlp <- 0.49 / 2.32
nls <- 1.16
tls <- 1.08 / 2.32

tmin <- 0
tmax <- 40

logit_vbl <- rnorm(N_species, nlv, tlv)
log_dcy <- rnorm(N_species, nld, tld)
vbl <- rlogitnorm(N_species, nlv, tlv)
dcy <- rlnorm(N_species, nld, tld)
olv <- rlnorm(N_species, tlv / 10, tlv / 100)
old <- rlnorm(N_species, tld / 10, tld / 100)

drc <- rlogitnorm(N_species, -2, 1)
wtc <- rlogitnorm(N_species, 2, 1)
logitdrm <- rnorm(N_species, 2, 1)
dbr <- rlnorm(N_species, 0.1, 0.01)
dth <- rnorm(N_species, 10, 3)

drg <- rlogitnorm(N_species, -2, 1)
wtg <- rlogitnorm(N_species, 2, 1)
delta <- rlnorm(N_species, 0.8, 0.8)
xopt <- rlogitnorm(N_species, nlx, tlx)
psi0 <- rlnorm(N_species, nlp, tlp)
sigma <- rlnorm(N_species, nls, tls)

vbl_sl <- rep(NA, sum(N_seedlots))
dcy_sl <- rep(NA, sum(N_seedlots))
for(i in 1:N_species){
  vbl_sl[(sum(N_seedlots[0:(i-1)]) + 1):(sum(N_seedlots[0:(i - 1)]) + N_seedlots[i])] <-
    rlogitnorm(N_seedlots[i], logit_vbl[i], tlv)
  dcy_sl[(sum(N_seedlots[0:(i-1)]) + 1):(sum(N_seedlots[0:(i - 1)]) + N_seedlots[i])] <-
    rlnorm(N_seedlots[i], log_dcy[i], tld)
}

germ <- rep(0, sum(N_obs))

for(i in 1:N_species){
  for(j in (sum(N_seedlots[0:(i-1)]) + 1):(sum(N_seedlots[0:(i - 1)]) + N_seedlots[i])){
    for(k in (sum(N_conditions[0:(j-1)]) + 1):(sum(N_conditions[0:(j - 1)]) + N_conditions[j])){
      cond_phase <- phase[(sum(N_phases[0:(k-1)]) + 1):(sum(N_phases[0:(k - 1)]) + N_phases[k])]
      cond_temp <- temp[(sum(N_phases[0:(k-1)]) + 1):(sum(N_phases[0:(k - 1)]) + N_phases[k])]
      cond_dur <- dur[(sum(N_phases[0:(k-1)]) + 1):(sum(N_phases[0:(k - 1)]) + N_phases[k])]
      cond_moist <- moist[(sum(N_phases[0:(k-1)]) + 1):(sum(N_phases[0:(k - 1)]) + N_phases[k])]
      cond_obs <- obs[(sum(N_obs[0:(k-1)]) + 1):(sum(N_obs[0:(k - 1)]) + N_obs[k])]
      
      temp_series <- rep(cond_temp, times = cond_dur)
      moist_series <- rep(cond_moist, time = cond_dur)
      
      log_germinability <- - log(2) * moist_series / dcy_sl[j]
      accum_germinability <- vbl_sl[j] * exp(cumsum(log_germinability))
      
      logit_dorm_break <- ( drc[i] * (1 - moist_series) + wtc[i] * moist_series ) * dbr[i] * (temp_series - dth[i])
      logit_dorm_break[1] <- logit_dorm_break[1] + logitdrm[i]
      accum_dorm_break <- invlogit(cumsum(logit_dorm_break))
      
      forcing <- swe_model(temp_series, tmin, xopt[i] * (tmax - tmin), tmax, delta[i])
      accum_forcing <- cumsum(forcing)
      
      cond_germinability <- runif(N_seeds[k], 0, 1)
      cond_threshold <- rlogis(N_seeds[k], psi0[i], sigma[i])
      
      for(s in 1:N_seeds[k]){
        if(cond_germinability[s] <= accum_germinability[length(accum_germinability)]){
          next
        }
        if(cond_threshold[s] > accum_forcing[length(accum_forcing)]){
          next
        }
        day <- which(cond_threshold[s] <= accum_forcing)[1]
        germ[sum(N_obs[0:(k-1)]) + which((cond_obs - day) > 0)[1]] <- 
          germ[sum(N_obs[0:(k-1)]) + which((cond_obs - day) > 0)[1]] + 1
      }
    }
  }
}

for(i in 1:N_species){
  for(j in (sum(N_seedlots[0:(i-1)]) + 1):(sum(N_seedlots[0:(i - 1)]) + N_seedlots[i])){
    for(k in (sum(N_conditions[0:(j-1)]) + 1):(sum(N_conditions[0:(j - 1)]) + N_conditions[j])){
      cond_phase <- phase[(sum(N_phases[0:(k-1)]) + 1):(sum(N_phases[0:(k - 1)]) + N_phases[k])]
      cond_temp <- temp[(sum(N_phases[0:(k-1)]) + 1):(sum(N_phases[0:(k - 1)]) + N_phases[k])]
      cond_dur <- dur[(sum(N_phases[0:(k-1)]) + 1):(sum(N_phases[0:(k - 1)]) + N_phases[k])]
      cond_moist <- moist[(sum(N_phases[0:(k-1)]) + 1):(sum(N_phases[0:(k - 1)]) + N_phases[k])]
      cond_obs <- obs[(sum(N_obs[0:(k-1)]) + 1):(sum(N_obs[0:(k - 1)]) + N_obs[k])]
      cond_germ <- germ[(sum(N_obs[0:(k-1)]) + 1):(sum(N_obs[0:(k - 1)]) + N_obs[k])]
      
      cum_per_germ <- cumsum(cond_germ) / N_seeds[k] * 100
      
      plot(x = NULL,
           y = NULL,
           xlim = c(0, sum(cond_dur)),
           ylim = c(0, 100),
           xlab = "Duration (Day)",
           ylab = "Cumulative percent generation (%)")
      
      abline(v = cumsum(cond_dur))
      points(cond_obs, cum_per_germ)
    }
  }
}

N_species <- 50
N_seedlots <- ceiling(3 * rbeta(N_species, 2, 5))
N_conditions <- ceiling(3 * rbeta(sum(N_seedlots), 5, 5))
N_phases <- ceiling(3 * rbeta(sum(N_conditions), 5, 5) + 2.01)
N_seeds <- round(runif(sum(N_conditions), 30, 300))
N_obs <- round(10 * rbeta(sum(N_conditions), 1, 2)) + 1

data <- list("N_species" = N_species,
             "N_seedlots" = N_seedlots,
             "N_conditions" = N_conditions,
             "N_phases" = N_phases,
             "N_seeds" = N_seeds,
             "N_obs" = N_obs,
             "temp" = temp,
             "dur" = dur,
             "moist" = moist,
             "obs" = obs,
             "germ" = germ)
