

par(mfrow = c(1,2), cex.main = 1, mar = c(4,5,1,1))
util$plot_hist_quantiles(samples, 'germ_pred', baseline_values = germ_obs,
                         xlab = 'Germination (#seeds per census bin)')
util$plot_hist_quantiles(samples, 'germ_pred', baseline_values = germ_obs,
                         xlab = 'Germination (#seeds per census bin)', 
                         bin_min=10, bin_max=100, bin_delta=10)


par(mfrow = c(7,4), cex.main = 1, mar = c(4,5,1,1))
exps <- sample(1:newdata$N_exps, 28)
# exps <- list_exps
for(e in exps){
  
  cs <- newdata$cens_start_idxs[e]
  ce <- newdata$cens_end_idxs[e]
  
  names <- paste0('germ_pred[',cs:ce,']')
  baseline_values <- germ_obs[cs:ce]
  x <- newdata$cens_day[cs:ce]
  plot_xs <- c(0, rep(x, each = 2)[-(2 * length(x))])
  
  N <- length(names)
  bin_min <- 0.5
  bin_max <- N + 0.5
  bin_delta <- 1
  breaks <- seq(bin_min, bin_max, bin_delta)
  
  plot_config <- util$configure_bin_plotting(breaks)
  plot_idxs <- plot_config[[1]]
  
  # Construct marginal quantiles
  probs <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  
  
  calc <- function(n) {
    util$ensemble_mcmc_quantile_est(samples[[names[n]]], probs)
  }
  quantiles <- sapply(1:N, calc)
  
  
  plot_quantiles <- do.call(cbind, lapply(plot_idxs,
                                          function(n) quantiles[1:9, n]))
  
  
  plot(1, type="n", main=e,
       xlim=c(0, max(x)), xlab='Day',
       ylim=c(0, 160), ylab='Germination\n(#seeds per census bin)')
  
  
  
  
  polygon(c(plot_xs, rev(plot_xs)),
          c(plot_quantiles[1,], rev(plot_quantiles[9,])),
          col = util$c_light, border = NA)
  polygon(c(plot_xs, rev(plot_xs)),
          c(plot_quantiles[2,], rev(plot_quantiles[8,])),
          col = util$c_light_highlight, border = NA)
  polygon(c(plot_xs, rev(plot_xs)),
          c(plot_quantiles[3,], rev(plot_quantiles[7,])),
          col = util$c_mid, border = NA)
  polygon(c(plot_xs, rev(plot_xs)),
          c(plot_quantiles[4,], rev(plot_quantiles[6,])),
          col = util$c_mid_highlight, border = NA)
  
  for (n in 1:N) {
    idx1 <- 2 * n - 1
    idx2 <- 2 * n
    lines(plot_xs[idx1:idx2], plot_quantiles[5, idx1:idx2],
          col=util$c_dark, lwd=2)
  }
  
  for (n in 1:N) {
    idx1 <- 2 * n - 1
    idx2 <- 2 * n
    lines(plot_xs[idx1:idx2], rep(baseline_values[n], 2),
          col="white", lwd=4)
    lines(plot_xs[idx1:idx2], rep(baseline_values[n], 2),
          col='black', lwd=2)
  }
  
  abline(h = c(55,65), lty = 2)
}

par(mfrow = c(5,3), cex.main = 1, mar = c(4,5,1,1))
for(s in 1:newdata$N_species){
  
  plot(1, type="n", main='',
       xlim=c(0, 100), xlab='Germ temp. (degC)',
       ylim=c(-5, 5), ylab='log(b)')
  
  exps_sp <- which(newdata$species_idxs == s)
  
  for(e in exps_sp){
    log_b <- util$ensemble_mcmc_quantile_est(samples[[paste0('log_b[',e,']')]], c(0.05,0.5,0.95))
    segments(x0 = newdata$germ_temp[e], y0 = log_b['5%'], y1 = log_b['95%'], col = util$c_light)
  }
  for(e in exps_sp){
    log_b <- util$ensemble_mcmc_quantile_est(samples[[paste0('log_b[',e,']')]], c(0.05,0.5,0.95))
    points(y = log_b['50%'], x = newdata$germ_temp[e], pch = 20, col = util$c_mid )
  }
}

par(mfrow = c(5,3), cex.main = 1, mar = c(4,5,1,1))
for(s in 1:newdata$N_species){
  
  plot(1, type="n", main='',
       xlim=c(0, 22), xlab='Chilling',
       ylim=c(-5, 5), ylab='log(b)')
  
  exps_sp <- which(newdata$species_idxs == s)
  
  for(e in exps_sp){
    log_b <- util$ensemble_mcmc_quantile_est(samples[[paste0('log_b[',e,']')]], c(0.05,0.5,0.95))
    segments(x0 = newdata$chill_cond[e], y0 = log_b['5%'], y1 = log_b['95%'], col = util$c_light)
  }
  for(e in exps_sp){
    log_b <- util$ensemble_mcmc_quantile_est(samples[[paste0('log_b[',e,']')]], c(0.05,0.5,0.95))
    points(y = log_b['50%'], x = newdata$chill_cond[e], pch = 20, col = util$c_mid )
  }
}


par(mfrow = c(5,3), cex.main = 1, mar = c(4,5,1,1))
for(s in 1:newdata$N_species){
  
  plot(1, type="n", main='',
       xlim=c(0, 100), xlab='Germ temp. (degC)',
       ylim=c(-5, 1), ylab='log(c)')
  
  exps_sp <- which(newdata$species_idxs == s)
  
  for(e in exps_sp){
    log_c <- util$ensemble_mcmc_quantile_est(samples[[paste0('log_c[',e,']')]], c(0.05,0.5,0.95))
    segments(x0 = newdata$germ_temp[e], y0 = log_c['5%'], y1 = log_c['95%'], col = util$c_light)
  }
  for(e in exps_sp){
    log_c <- util$ensemble_mcmc_quantile_est(samples[[paste0('log_c[',e,']')]], c(0.05,0.5,0.95))
    points(y = log_c['50%'], x = newdata$germ_temp[e], pch = 20, col = util$c_mid )
  }
  
}

par(mfrow = c(5,3), cex.main = 1, mar = c(4,5,1,1))
for(s in 1:newdata$N_species){
  
  plot(1, type="n", main='',
       xlim=c(0, 22), xlab='Chilling',
       ylim=c(-5, 2), ylab='log(c)')
  
  exps_sp <- which(newdata$species_idxs == s)
  
  for(e in exps_sp){
    log_c <- util$ensemble_mcmc_quantile_est(samples[[paste0('log_c[',e,']')]], c(0.05,0.5,0.95))
    segments(x0 = newdata$chill_cond[e], y0 = log_c['5%'], y1 = log_c['95%'], col = util$c_light)
  }
  for(e in exps_sp){
    log_c <- util$ensemble_mcmc_quantile_est(samples[[paste0('log_c[',e,']')]], c(0.05,0.5,0.95))
    points(y = log_c['50%'], x = newdata$chill_cond[e], pch = 20, col = util$c_mid )
  }
  
  text(y = 1.7, x = 0, labels = paste0('Species ', s), adj = 0, cex = 0.9)
  
}


list_exps <- c()
for(e in 1:newdata$N_exps){
  
  cs <- newdata$cens_start_idxs[e]
  ce <- newdata$cens_end_idxs[e]
  
  if(any(germ_obs[cs:ce] %in% c(50:60))){
    list_exps <- c(list_exps, e)
  }
}




par(mfrow = c(5,3), cex.main = 1, mar = c(4,5,1,1))
for(s in 1:newdata$N_species){
  
  plot(1, type="n", main='',
       xlim=c(0, 22), xlab='Chilling',
       ylim=c(0, 1), ylab='pv')
  
  exps_sp <- which(newdata$species_idxs == s)
  
  for(e in exps_sp){
    pv <- util$ensemble_mcmc_quantile_est(samples[[paste0('pv[',e,']')]], c(0.05,0.5,0.95))
    segments(x0 = newdata$chill_cond[e], y0 = pv['5%'], y1 = pv['95%'], col = util$c_light)
  }
  for(e in exps_sp){
    pv <- util$ensemble_mcmc_quantile_est(samples[[paste0('pv[',e,']')]], c(0.05,0.5,0.95))
    points(y = pv['50%'], x = newdata$chill_cond[e], pch = 20, col = util$c_mid )
  }
  
  text(y = 1.7, x = 0, labels = paste0('Species ', s), adj = 0, cex = 0.9)
  
}

