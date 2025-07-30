
save <- modeld
modeld <- modeld[modeld$datasetID %in% sample(unique(modeld$datasetID), 50),]


# Trim the phylo tree with species present in the dataset
spp <-  unique(modeld$genusspecies)
length(spp)
length(phylo$node.label)
phylo2 <- keep.tip(phylo, spp)
cphy <- vcv.phylo(phylo2,corr=TRUE)

modeld$numspp = as.integer(factor(modeld$genusspecies, levels = colnames(cphy)))

mdl.data <- list(N_degen = sum(modeld$responseValue %in% c(0,1)),
                 N_prop = sum(modeld$responseValue>0 & modeld$responseValue<1),
                 
                 Nsp =  length(unique(modeld$numspp)),
                 sp_degen = array(modeld$numspp[modeld$responseValue %in% c(0,1)],
                                  dim = sum(modeld$responseValue%in% c(0,1))),
                 sp_prop = array(modeld$numspp[modeld$responseValue>0 & modeld$responseValue<1],
                                 dim = sum(modeld$responseValue>0 & modeld$responseValue<1)),
                 
                 y_degen = array(modeld$responseValue[modeld$responseValue %in% c(0,1)],
                                 dim = sum(modeld$responseValue%in% c(0,1))),
                 y_prop = array(modeld$responseValue[modeld$responseValue>0 & modeld$responseValue<1],
                                dim = sum(modeld$responseValue>0 & modeld$responseValue<1)),
                 
                 t_degen = array(modeld$time[modeld$responseValue %in% c(0,1)],
                                 dim = sum(modeld$responseValue%in% c(0,1))),
                 t_prop = array(modeld$time[modeld$responseValue>0 & modeld$responseValue<1],
                                dim = sum(modeld$responseValue>0 & modeld$responseValue<1)),
                 
                 f_degen = array(modeld$forcing[modeld$responseValue %in% c(0,1)],
                                 dim = sum(modeld$responseValue%in% c(0,1))),
                 f_prop = array(modeld$forcing[modeld$responseValue>0 & modeld$responseValue<1],
                                dim = sum(modeld$responseValue>0 & modeld$responseValue<1)),
                 
                 c_degen = array(modeld$chillh10[modeld$responseValue %in% c(0,1)],
                                 dim = sum(modeld$responseValue%in% c(0,1))),
                 c_prop = array(modeld$chillh10[modeld$responseValue>0 & modeld$responseValue<1],
                                dim = sum(modeld$responseValue>0 & modeld$responseValue<1)),
                 
                 Vphy = cphy)

# Compile and run model
smordbeta <- stan_model("stan/orderedbetalikelihood_3slopes_cutpoints.stan")
fit <- sampling(smordbeta, mdl.data, 
                iter = 4000, warmup = 3000,
                chains = 4)
summ <- data.frame(summary(fit)[["summary"]])
sampler_params  <- get_sampler_params(fit, inc_warmup = FALSE)
diagnostics <- list(
  max_treedepth= max(sapply(sampler_params, function(x) max(x[, "treedepth__"]))),
  max_divergence = max(sapply(sampler_params, function(x) sum(x[, "divergent__"]))),
  max_rhat = max(summ$Rhat, na.rm = TRUE),
  min_ess = min(summ$n_eff, na.rm = TRUE)
)


sp <- 30
idxs_degen <- which(mdl.data$sp_degen == sp)
idxs_prop <- which(mdl.data$sp_prop == sp)
y_prop_names <- sapply(idxs_prop, function(n) paste0('y_prop_gen[', n, ']'))

par(mfrow=c(1, 1))
plot.new()
xlim=c(0, 1)
ylim=c(0, 1)
abline(a=0, b=1, col = 'white', lwd = 4)
abline(a=0, b=1, col = 'black', lwd = 1)
points(x= mdl.data$y_prop[idxs_prop], y =  summ[y_prop_names, 'mean'], pch=16, cex=1.0, col="white")
points(x= mdl.data$y_prop[idxs_prop], y =  summ[y_prop_names, 'mean'], pch=16, cex=0.8, col="black")



plot(mdl.data$y_prop[idxs_prop] ~ mdl.data$f_prop[idxs_prop], pch = 16, cex = 0.8,
     col = mdl.data$t_prop[idxs_prop]+10)

plot(summ[y_prop_names, 'mean'] ~ mdl.data$c_prop[idxs_prop], pch = 16, cex = 0.8,
     col = mdl.data$c_prop[idxs_prop]+10)

plotdf <-
  data.frame(
    yobs = mdl.data$y_prop[idxs_prop],
    ypred_median = summ[y_prop_names, 'X50.'],
    ypred_q2.5 = summ[y_prop_names, 'X25.'],
    ypred_q97.5 = summ[y_prop_names, 'X75.'],
    chilling = mdl.data$c_prop[idxs_prop],
    forcing = mdl.data$f_prop[idxs_prop],
    time = mdl.data$t_prop[idxs_prop]
  )

ggplot(data = plotdf) +
  # geom_pointrange(aes(y = ypred_median, ymin = ypred_q2.5, ymax = ypred_q97.5, x = forcing, color = time),
  #                 size = 0.3) +
  geom_point(aes(y = ypred_median, x = forcing, color = time)) +
  geom_line(aes(y = ypred_median, x = forcing, color = time, group = time)) +
  geom_point(aes(y = yobs, x = forcing, color = time), color = 'black', fill = 'white', shape = 21, size = 2) +
  geom_point(aes(y = yobs, x = forcing, color = time), color = 'black', shape = 20, size = 0.5) +
  scale_color_viridis_c() +
  theme_bw() +
  labs(y = 'ypred') +
  theme(panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 1))

ggplot(data = plotdf) +
  geom_pointrange(aes(y = ypred_median, ymin = ypred_q2.5, ymax = ypred_q97.5, x = time, color = forcing),
                  size = 0.3) +
  geom_point(aes(y = yobs, x = time, color = forcing), color = 'black', fill = 'white', shape = 21, size = 2) +
  geom_point(aes(y = yobs, x = time, color = forcing), color = 'black', shape = 20, size = 0.5) +
  scale_color_viridis_c() +
  theme_bw() +
  labs(y = 'ypred') +
  theme(panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 1))

ggplot(data = plotdf) +
  geom_point(aes(y = yobs, x = time, color = forcing)) +
  scale_color_viridis_c() +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 1))

ggplot(data = plotdf) +
  geom_point(aes(y = yobs, x = forcing, color = time)) +
  geom_line(aes(y = yobs, x = forcing, color = time, group = time)) +
  scale_color_viridis_c() +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 1))


sp
y_prop_names <- sapply(idxs_prop, function(n) paste0('y_prop_gen[', n, ']'))


bfsp <- data.frame(samples = unlist(extract(fit, paste0('bf[', sp, ']'))),
                   prior = rnorm(4000, 0.5, 1))

ggplot(data = bfsp) +
  geom_histogram(aes(x= samples, y = after_stat(density)), alpha = 0.8, bins = 600) +
  geom_density(aes(x= prior), color = 'darkred') +
  coord_cartesian(xlim = c(-2,2)) +
  theme_bw() +
  theme(panel.grid = element_blank())

