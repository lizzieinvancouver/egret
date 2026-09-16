

par(mfrow = c(2,1), cex.lab = 0.9, cex.axis = 0.85)
temps <- seq(0, 50, 0.5)

plot(NULL, xlim = range(temps), ylim = c(-30, 400),
     xlab = "Temperature (degC)", ylab = "Species-specific T50 (days)",
     bty = 'n')

for(sp in 1:newdata$N_species){
  
  temps_sp <- newdata$germ_temp[newdata$species_idxs == sp]
  temps_sp <- seq(range(temps_sp)[1], range(temps_sp)[2], length.out = 100)
  
  alpha_logc <- base_samples[[paste0('a_c[',sp,']')]]
  beta_logc <- base_samples[[paste0('bT_c[',sp,']')]]
  
  alpha_logb <- base_samples[[paste0('a_b[',sp,']')]]
  beta_logb <- base_samples[[paste0('bT_b[',sp,']')]]
  
  t50_q <- sapply(temps_sp, function(t){
    
    logb <- alpha_logb + beta_logb * (t - temp0)
    c <- exp(alpha_logc + beta_logc * (t - temp0))
    t50 <- (logb - log(log(2)))/c
    
    util$ensemble_mcmc_quantile_est(t50, c(0.1, 0.5, 0.9))
  })
  
  lines(temps_sp, t50_q['50%',], lwd = 1.5, col = 'grey40')
  polygon(x = c(temps_sp, rev(temps_sp)), y = c(t50_q['10%',], rev(t50_q['90%',])),
          col = adjustcolor('grey80', alpha.f = 0.3), border = NA)
}

mtext("Individual parameters", side = 2, line = 2.2, las = 0, cex = 0.8,
      col = 'grey40', font = 3)

plot(NULL, xlim = range(temps), ylim = c(0, 100),
     xlab = "Temperature (degC)", ylab = "Across-species T50 (days)",
     bty = 'n')

temps_sp <- newdata$germ_temp
temps_sp <- seq(range(temps_sp)[1], range(temps_sp)[2], length.out = 100)

alpha_logc <- base_samples[[paste0('mu_a_c')]]
beta_logc <- base_samples[[paste0('mu_bT_c')]]

alpha_logb <- base_samples[[paste0('mu_a_b')]]
beta_logb <- base_samples[[paste0('mu_bT_b')]]

t50_q <- sapply(temps_sp, function(t){
  
  logb <- alpha_logb + beta_logb * (t - temp0)
  c <- exp(alpha_logc + beta_logc * (t - temp0))
  t50 <- (logb - log(log(2)))/c
  
  util$ensemble_mcmc_quantile_est(t50, c(0.1, 0.5, 0.9))
})

lines(temps_sp, t50_q['50%',], lwd = 1.5, col = 'grey40')
polygon(x = c(temps_sp, rev(temps_sp)), y = c(t50_q['10%',], rev(t50_q['90%',])),
        col = adjustcolor('grey80', alpha.f = 0.3), border = NA)

mtext("Population parameters", side = 2, line = 2.2, las = 0, cex = 0.8,
      col = 'grey40', font = 3)
