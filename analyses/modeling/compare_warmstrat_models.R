
fit_ab0 <- readRDS(file.path('modeling/output/abundant0model.rds'))
samples_ab0 <- util$extract_expectand_vals(fit_ab0)

fit_dr <- readRDS(file.path('modeling/output/warmStratCollapse.R'))
samples_dr <- util$extract_expectand_vals(fit_dr)


names <- sapply(1:mdl.data$Nsp, function(sp) paste0('bt[', sp, ']'))
q10_ab0 <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_ab0[[n]], c(0.1))
})
q50_ab0 <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_ab0[[n]], c(0.5))
})
q90_ab0 <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_ab0[[n]], c(0.9))
})
q10_dr <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_dr[[n]], c(0.1))
})
q50_dr <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_dr[[n]], c(0.5))
})
q90_dr <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_dr[[n]], c(0.9))
})
bt <- data.frame(
  sp = 1:mdl.data$Nsp, 
  q10_ab0, q50_ab0, q90_ab0, 
  q10_dr, q50_dr, q90_dr, 
  parameter = 'Germ. duration'
)

names <- sapply(1:mdl.data$Nsp, function(sp) paste0('bf[', sp, ']'))
q10_ab0 <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_ab0[[n]], c(0.1))
})
q50_ab0 <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_ab0[[n]], c(0.5))
})
q90_ab0 <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_ab0[[n]], c(0.9))
})
q10_dr <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_dr[[n]], c(0.1))
})
q50_dr <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_dr[[n]], c(0.5))
})
q90_dr <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_dr[[n]], c(0.9))
})
bf <- data.frame(
  sp = 1:mdl.data$Nsp, 
  q10_ab0, q50_ab0, q90_ab0, 
  q10_dr, q50_dr, q90_dr, 
  parameter = 'Germ. temp.'
)

names <- sapply(1:mdl.data$Nsp, function(sp) paste0('bcs[', sp, ']'))
q10_ab0 <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_ab0[[n]], c(0.1))
})
q50_ab0 <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_ab0[[n]], c(0.5))
})
q90_ab0 <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_ab0[[n]], c(0.9))
})
q10_dr <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_dr[[n]], c(0.1))
})
q50_dr <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_dr[[n]], c(0.5))
})
q90_dr <- sapply(names, function(n){
  util$ensemble_mcmc_quantile_est(samples_dr[[n]], c(0.9))
})
bcs <- data.frame(
  sp = 1:mdl.data$Nsp, 
  q10_ab0, q50_ab0, q90_ab0, 
  q10_dr, q50_dr, q90_dr, 
  parameter = 'Cold stratification'
)

estimate_2models <- rbind(bt, bf, bcs)

ggplot(estimate_2models) +
  facet_wrap(~ parameter, scales = 'free') +
  geom_abline(intercept = 0, slope = 1, color = 'grey90') +
  geom_pointrange(aes(ymin = q10_ab0, ymax = q90_ab0, y = q50_ab0, xmin = q10_dr, xmax = q90_dr, x = q50_dr),
                  size = 0.1, linewidth = 0.3, alpha = 0.5) +
  geom_smooth(aes(y = q50_ab0, x = q50_dr), method = "lm", color = 'red') +
  theme_classic() +
  labs(x = 'Decision-rule model', y = 'Abundant-0s model')
  


