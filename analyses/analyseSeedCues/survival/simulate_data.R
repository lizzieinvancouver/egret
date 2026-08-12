
wd <- '/home/victor/projects/egret/analyses'
setwd(wd)
util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)



library(rstan)

inv_logit <- function(x) 1 / (1 + exp(-x))


T0_true <- 18.0  
k_true <- 0.35      
Psi0_true <- exp(2.0) 
sigma_true <- 1.0 
pv_true <- 0.8 

temps <- c(10, 12, 15, 17, 20, 23, 25)   
N_exps <- length(temps)

n_seeds <- 100  
max_days <- 30   



sim_one_exp <- function(e) {
  temp <- temps[e]
  f <- inv_logit(k_true * (temp - T0_true))
  viable <- runif(n_seeds) < pv_true                  
  psi <- rlogis(n_seeds, location = Psi0_true, scale = sigma_true)
  t <- ifelse(viable, psi / f, Inf) 
  day <- floor(t)
  data.frame(exp = e, germ_temp = temp, t_germ = t, germ_day = day)
}


sim_raw <- bind_rows(lapply(1:N_exps, sim_one_exp))

sim <- sim_raw %>%
  filter(germ_day >= 1, germ_day <= max_days) %>%
  arrange(exp, germ_day)

counts <- as.integer(table(factor(sim$exp, levels = 1:N_exps)))
N_ungerm <- n_seeds - counts

end_idx <- cumsum(counts)
start_idx <- end_idx - counts + 1L

data <- list(
  N_exps = N_exps,
  germ_temp = as.array(temps),
  N = nrow(sim),
  germ_days = as.array(as.integer(sim$germ_day)),
  exps = as.array(as.integer(sim$exp)),      
  start_exp_idxs = as.array(as.integer(start_idx)),
  start_end_idxs = as.array(as.integer(end_idx)),
  N_obs = as.array(counts),
  N_ungerm = as.array(as.numeric(N_ungerm)),
  max_days = max_days
)

modelstan <- stan_model("~/projects/egret/analyses/stan/generative/survival/egret_surv.stan")
fit <- sampling(modelstan, data, chains = 4, cores = 4,
                seed = 123456, iter = 2000, warmup = 1000)

samples <- util$extract_expectand_vals(fit)

par(mfrow = c(3,2), mar = c(4,4,1,1))

util$plot_expectand_pushforward(samples[['T0']], 30, flim = c(16, 20),
                                display_name = 'T0')
abline(v = T0_true, lty = 2, lwd = 2, col = util$c_light_teal)

util$plot_expectand_pushforward(samples[['k']], 30, flim = c(0.1, 0.6),
                                display_name = 'k')
abline(v = k_true, lty = 2, lwd = 2, col = util$c_light_teal)

util$plot_expectand_pushforward(samples[['Psi0']], 30, flim = c(5, 10),
                                display_name = 'Psi0')
abline(v = Psi0_true, lty = 2, lwd = 2, col = util$c_light_teal)

util$plot_expectand_pushforward(samples[['sigma']], 30, flim = c(0, 3),
                                display_name = 'sigma')
abline(v = sigma_true, lty = 2, lwd = 2, col = util$c_light_teal)

util$plot_expectand_pushforward(samples[['pv']], 30, flim = c(0, 1),
                                display_name = 'pv')
abline(v = pv_true, lty = 2, lwd = 2, col = util$c_light_teal)




