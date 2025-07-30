
samples <- util$extract_expectand_vals(fit)

f_names <- sapply(1:mdl.data$N_degen,
                  function(n) paste0('y_degen_gen[', n, ']'))
modeld$y_gen_mean <- NA
modeld[modeld$responseValue %in% c(0,1), 'y_gen_mean'] <-
  sapply(f_names, function(n){
    mean(rowMeans(samples[[n]]))})


f_names <- sapply(1:mdl.data$N_prop,
                  function(n) paste0('y_prop_gen[', n, ']'))
modeld[modeld$responseValue>0 & modeld$responseValue<1, 'y_gen_mean'] <-
  sapply(f_names, function(n){
    mean(rowMeans(samples[[n]]))})

library(paletteer)
nColor <- 100
colors = paletteer_c("viridis::inferno", n=nColor)
rank <- as.factor( as.numeric( cut(ppcheck$numspp, nColor)))

ppcheck <- modeld[c('numspp', 'responseValue', 'y_gen_mean', 'forcing', 'time')]
plot(ppcheck$responseValue ~ ppcheck$y_gen_mean, 
     col = colors[ rank ], cex = 0.5)
abline(a=0, b=1, col = 'white', lwd = 4)
abline(a=0, b=1, col = 'black', lwd = 1)



s <- sample(unique(ppcheck$numspp), 1)
ppchecks <- ppcheck[ppcheck$numspp ==s, ]
plot(ppchecks$responseValue ~ ppchecks$time, 
     col = 'grey', cex = 1)
points(ppchecks$y_gen_mean ~ ppchecks$time, cex = 0.5)



sp <- 94
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

sub <- modeld[modeld$numspp == sp, ]
sub <- d[d$genusspecies == 'Clematis_vitalba',]

par(mfrow=c(1, 3))
mdl.data$dataID_prop <- as.integer(factor(modeld$datasetID[modeld$responseValue>0 & modeld$responseValue<1]))
idxs_prop <- which(mdl.data$sp_prop == sp)

for(id in unique(mdl.data$dataID_prop[idxs_prop])){
  
  idxs_prop_sp <- which(mdl.data$dataID_prop[idxs_prop] == id)
  plot(mdl.data$y_prop[idxs_prop_sp] ~ mdl.data$t_prop[idxs_prop_sp], pch = 16, cex = 0.5,
       col = unique(mdl.data$dataID_prop[idxs_prop_sp]))
  
  
}

length(unique(modeld$numspp))


