
base_samples <- util$filter_expectands(samples,
                                       c('mu_pv', 'tau_pv_species', 'mu_pv_species_tilde'),
                                       check_arrays = T)
util$check_all_expectand_diagnostics(base_samples)

base_samples <- util$filter_expectands(samples,
                                       c('mu_tau50', 'tau_tau50_species', 'mu_tau50_species_tilde'),
                                       check_arrays = T)
util$check_all_expectand_diagnostics(base_samples)

# the species 4 seems quite problematic
exps <- which(mdl.data$species_idxs == 4)
# interestingly, it seems to be shared by two different studies?




par(mfrow = c(1,1))
util$plot_disc_pushforward_quantiles(samples , paste0('tau50[',exps,']'), ylab = 'tau50',
                                     xticklabs = exps)

par(mfrow = c(4,4))
for(e in exps){
  
  start <- mdl.data$exp_start_idxs[e] 
  end <- mdl.data$exp_end_idxs[e]
  
  max <- sum(mdl.data$seeds[start:end])
  
  plot(x = mdl.data$d[c(start+1):end],
       y = cumsum(mdl.data$seeds[c(start+1):end]),
       xlab = 'Days', ylab = 'Cumulative\ngermination',
       type = 'l', main = e, bty = 'n', col = util$c_mid,
       ylim = c(0,  max))
  abline(h = max, col = util$c_mid_teal, lty = 2)
  
}

exps <- which(mdl.data$species_idxs == 28)
exps <- c(200,198,199)
par(mfrow = c(1,3))
for(e in exps){
  
  start <- mdl.data$exp_start_idxs[e]
  end <- mdl.data$exp_end_idxs[e]
  max <- sum(mdl.data$seeds[start:end])
  
  start <- start
  
  days <- mdl.data$d[start:end]
  days[2:length(days)] <- days[2:length(days)]  + 10
  
  
  namesy <- paste0('cumy_pred[', c(start+1):end,']')
  util$plot_conn_pushforward_quantiles(samples, namesy, days[2:length(days)],
                                       display_xlim =  c(0, 100),
                                       display_ylim = c(0,  max*1.1))
  points(x = days[2:length(days)], y = cumsum(mdl.data$seeds[c(start+1):end]),
         pch = 20, col = "white", cex = 2)
  points(x = days[2:length(days)], y = cumsum(mdl.data$seeds[c(start+1):end]),
         pch = 20, col = "black", cex = 1)
  abline(h = max, col = util$c_mid_teal, lty = 2)
  
  abline(v= util$ensemble_mcmc_quantile_est(samples[[paste0('tau50[', e,']')]], c(0.5)), 
         col = util$c_mid_teal, lty = 1, lwd = 2)
  
  text(x = 0, y = 80, labels = paste0('Chilling: ', round(mdl.data$chill[e],1), ' weeks'), adj = 0, col = util$c_mid_teal)
  text(x = 0, y = 70, labels = paste0('Forcing: ', round(mdl.data$forcing[e],1), ' x10degC'), adj = 0, col = util$c_mid_teal)
  
}


pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/retrodictive_checks.pdf', width = 14, height = 14)
par(mfrow = c(7,6), mar = c(4,4,1,1))
for(e in 1:mdl.data$Nexps){
  
  start <- mdl.data$exp_start_idxs[e]
  end <- mdl.data$exp_end_idxs[e]
  max <- sum(mdl.data$seeds[start:end])
  
  start <- start
  
  days <- mdl.data$d[start:end]
  days[2:length(days)] <- days[2:length(days)]  + 10
  
  
  namesy <- paste0('cumy_pred[', c(start+1):end,']')
  util$plot_conn_pushforward_quantiles(samples, namesy, days[2:length(days)],
                                       display_xlim =  c(0, mdl.data$d[end]+10),
                                       display_ylim = c(0,  max*1.2))
  points(x = days[2:length(days)], y = cumsum(mdl.data$seeds[c(start+1):end]),
         pch = 20, col = "white", cex = 1)
  points(x = days[2:length(days)], y = cumsum(mdl.data$seeds[c(start+1):end]),
         pch = 20, col = "black", cex = 0.5)
  abline(h = max, col = util$c_mid_teal, lty = 2)
  
  tau50 <- util$ensemble_mcmc_quantile_est(samples[[paste0('tau50[', e,']')]], c(0.5))
  segments(x0 = tau50, y0 =0, y1 = max,
           col = util$c_mid_teal, lty = 1, lwd = 2)
  text(x =  tau50 - (mdl.data$d[end]+10)*0.05, 
       y = max*0.8, labels = bquote(tau[50]), srt = 90, col = util$c_mid_teal)
  
  pv <- as.numeric(util$ensemble_mcmc_quantile_est(samples[[paste0('pv[', e,']')]], c(0.5)))
  text(x = 0,  y = max*1.1, labels =  bquote(p[v]~'='~.(format(pv, digits = 2))), 
       srt = 0, col = util$c_mid_teal, adj = 0)
  
 
  
  # namesy <- paste0('y_pred[', c(start+1):end,']')
  # util$plot_conn_pushforward_quantiles(samples, namesy, days[2:length(days)],
  #                                      display_xlim =  c(0, mdl.data$d[end]+10),
  #                                      display_ylim = c(0,  max))
  # points(x = days[2:length(days)], y = mdl.data$seeds[c(start+1):end],
  #        pch = 20, col = "white", cex = 2)
  # points(x = days[2:length(days)], y = mdl.data$seeds[c(start+1):end],
  #        pch = 20, col = "black", cex = 1)
  # 
  # namesy <- paste0('pg[', start:end,']')
  # util$plot_disc_pushforward_quantiles(samples, namesy)
  
}
dev.off()

pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/pv_chill.pdf', width = 14, height = 14)
par(mfrow = c(7,6), mar = c(4,4,1,1), cex.main = 0.8)
for(s in 1:mdl.data$Nspecies){
  
  exps <- which(mdl.data$species == s)
  ordchill <- order(mdl.data$chill[exps])
  ordforc <- order(mdl.data$forcing[exps])
  
  dataplot <- 
    data.frame(chill = mdl.data$chill[exps], forcing = mdl.data$forcing[exps],
               pv_q5 = as.numeric( sapply(paste0('pv[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.05)))),
               pv_q50 = as.numeric( sapply(paste0('pv[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.5)))),
               pv_q95 = as.numeric( sapply(paste0('pv[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.95)))))
  
  plot(x = NULL, y = NULL,
       xlim = range(dataplot$chill), ylim = c(0,1),
       bty = 'n',
       xlab = "Chill (weeks)", ylab = bquote(p[v]),
       main = paste0('Species ', s))
  
  if(length(unique(dataplot$chill)) > 1){
    model <- lm(pv_q50 ~ chill, data = dataplot)
    abline(model, col = util$c_light_teal, lwd = 1.5, lty = 2)
  }
  
  for(i in 1:nrow(dataplot)){
    segments(x0 = dataplot$chill[i], y0 = dataplot$pv_q5[i], y1 = dataplot$pv_q95[i], col = util$c_mid)
  }
  points(x = dataplot$chill, y = dataplot$pv_q50, pch = 20, col = util$c_dark)
  
  
  
}
dev.off()


pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/pv_forcing.pdf', width = 14, height = 14)
par(mfrow = c(7,6), mar = c(4,4,1,1), cex.main = 0.8)
for(s in 1:mdl.data$Nspecies){
  
  exps <- which(mdl.data$species == s)
  ordchill <- order(mdl.data$chill[exps])
  ordforc <- order(mdl.data$forcing[exps])
  
  dataplot <- 
    data.frame(chill = mdl.data$chill[exps], forcing = mdl.data$forcing[exps],
               pv_q5 = as.numeric( sapply(paste0('pv[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.05)))),
               pv_q50 = as.numeric( sapply(paste0('pv[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.5)))),
               pv_q95 = as.numeric( sapply(paste0('pv[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.95)))))
  
  plot(x = NULL, y = NULL,
       xlim = range(dataplot$forcing), ylim = c(0,1),
       bty = 'n',
       xlab = "Germ. temp. (x10degC)", ylab = bquote(p[v]),
       main = paste0('Species ', s))
  
  if(length(unique(dataplot$forcing)) > 1){
    model <- lm(pv_q50 ~ forcing, data = dataplot)
    abline(model, col = util$c_light_teal, lwd = 1.5, lty = 2)
  }
  
  for(i in 1:nrow(dataplot)){
    segments(x0 = dataplot$forcing[i], y0 = dataplot$pv_q5[i], y1 = dataplot$pv_q95[i], col = util$c_mid)
  }
  points(x = dataplot$forcing, y = dataplot$pv_q50, pch = 20, col = util$c_dark)
  
  
  
}
dev.off()

pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/tau50_chilling.pdf', width = 14, height = 14)
par(mfrow = c(7,6), mar = c(4,4,1,1), cex.main = 0.8)
for(s in 1:mdl.data$Nspecies){
  
  exps <- which(mdl.data$species == s)
  ordchill <- order(mdl.data$chill[exps])
  ordforc <- order(mdl.data$forcing[exps])
  
  dataplot <- 
    data.frame(chill = mdl.data$chill[exps], forcing = mdl.data$forcing[exps],
               tau50_q5 = as.numeric( sapply(paste0('tau50[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.05)))),
               tau50_q50 = as.numeric( sapply(paste0('tau50[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.5)))),
               tau50_q95 = as.numeric( sapply(paste0('tau50[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.95)))))
  
  plot(x = NULL, y = NULL,
       xlim = range(dataplot$chill), ylim = range(min(dataplot$tau50_q5), max(dataplot$tau50_q95)),
       bty = 'n',
       xlab = "Chill (weeks)", ylab = bquote(tau[50]),
       main = paste0('Species ', s))
  
  if(length(unique(dataplot$chill)) > 1){
    model <- lm(tau50_q50 ~ chill, data = dataplot)
    abline(model, col = util$c_light_teal, lwd = 1.5, lty = 2)
  }
  
  for(i in 1:nrow(dataplot)){
    segments(x0 = dataplot$chill[i], y0 = dataplot$tau50_q5[i], y1 = dataplot$tau50_q95[i], col = util$c_mid)
  }
  points(x = dataplot$chill, y = dataplot$tau50_q50, pch = 20, col = util$c_dark)
  
  
  
}
dev.off()

pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/tau50_forcing.pdf', width = 14, height = 14)
par(mfrow = c(7,6), mar = c(4,4,1,1), cex.main = 0.8)
for(s in 1:mdl.data$Nspecies){
  
  exps <- which(mdl.data$species == s)
  ordchill <- order(mdl.data$chill[exps])
  ordforc <- order(mdl.data$forcing[exps])
  
  dataplot <- 
    data.frame(chill = mdl.data$chill[exps], forcing = mdl.data$forcing[exps],
               tau50_q5 = as.numeric( sapply(paste0('tau50[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.05)))),
               tau50_q50 = as.numeric( sapply(paste0('tau50[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.5)))),
               tau50_q95 = as.numeric( sapply(paste0('tau50[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.95)))))
  
  plot(x = NULL, y = NULL,
       xlim = range(dataplot$forcing), ylim = range(min(dataplot$tau50_q5), max(dataplot$tau50_q95)),
       bty = 'n',
       xlab = "Germ. temp. (x10degC)", ylab = bquote(tau[50]),
       main = paste0('Species ', s))
  
  if(length(unique(dataplot$forcing)) > 1){
    model <- lm(tau50_q50 ~ forcing, data = dataplot)
    abline(model, col = util$c_light_teal, lwd = 1.5, lty = 2)
  }
  
  for(i in 1:nrow(dataplot)){
    segments(x0 = dataplot$forcing[i], y0 = dataplot$tau50_q5[i], y1 = dataplot$tau50_q95[i], col = util$c_mid)
  }
  points(x = dataplot$forcing, y = dataplot$tau50_q50, pch = 20, col = util$c_dark)
  
  
  
}
dev.off()

pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/delta_595_forcing.pdf', width = 14, height = 14)
par(mfrow = c(7,6), mar = c(4,4,1,1), cex.main = 0.8)
for(s in 1:mdl.data$Nspecies){
  
  exps <- which(mdl.data$species == s)
  
  dataplot <- 
    data.frame(chill = mdl.data$chill[exps], forcing = mdl.data$forcing[exps],
               delta_595_q5 = as.numeric( sapply(paste0('delta_595[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.05)))),
               delta_595_q50 = as.numeric( sapply(paste0('delta_595[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.5)))),
               delta_595_q95 = as.numeric( sapply(paste0('delta_595[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.95)))))
  
  plot(x = NULL, y = NULL,
       xlim = range(dataplot$forcing), ylim = range(min(dataplot$delta_595_q5), max(dataplot$delta_595_q95)),
       bty = 'n',
       xlab = "Germ. temp. (x10degC)", ylab = bquote(delta[5-95]),
       main = paste0('Species ', s))
  
  if(length(unique(dataplot$forcing)) > 1){
    model <- lm(delta_595_q50 ~ forcing, data = dataplot)
    abline(model, col = util$c_light_teal, lwd = 1.5, lty = 2)
  }
  
  for(i in 1:nrow(dataplot)){
    segments(x0 = dataplot$forcing[i], y0 = dataplot$delta_595_q5[i], y1 = dataplot$delta_595_q95[i], col = util$c_mid)
  }
  points(x = dataplot$forcing, y = dataplot$delta_595_q50, pch = 20, col = util$c_dark)
  
  
  
}
dev.off()


pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/delta_595_chilling.pdf', width = 14, height = 14)
par(mfrow = c(7,6), mar = c(4,4,1,1), cex.main = 0.8)
for(s in 1:mdl.data$Nspecies){
  
  exps <- which(mdl.data$species == s)
  ordchill <- order(mdl.data$chill[exps])
  ordforc <- order(mdl.data$forcing[exps])
  
  dataplot <- 
    data.frame(chill = mdl.data$chill[exps], forcing = mdl.data$forcing[exps],
               delta_595_q5 = as.numeric( sapply(paste0('delta_595[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.05)))),
               delta_595_q50 = as.numeric( sapply(paste0('delta_595[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.5)))),
               delta_595_q95 = as.numeric( sapply(paste0('delta_595[', exps, ']'), function(i) util$ensemble_mcmc_quantile_est(samples[[i]], c(0.95)))))
  
  plot(x = NULL, y = NULL,
       xlim = range(dataplot$chill), ylim = range(min(dataplot$delta_595_q5), max(dataplot$delta_595_q95)),
       bty = 'n',
       xlab = "Chill (weeks)", ylab = bquote(delta[5-95]),
       main = paste0('Species ', s))
  
  if(length(unique(dataplot$chill)) > 1){
    model <- lm(delta_595_q50 ~ chill, data = dataplot)
    abline(model, col = util$c_light_teal, lwd = 1.5, lty = 2)
  }
  
  for(i in 1:nrow(dataplot)){
    segments(x0 = dataplot$chill[i], y0 = dataplot$delta_595_q5[i], y1 = dataplot$delta_595_q95[i], col = util$c_mid)
  }
  points(x = dataplot$chill, y = dataplot$delta_595_q50, pch = 20, col = util$c_dark)
  
  
  
}
dev.off()
