rm(list = ls())
setwd('~/projects/egret/analyses/modeling')
util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)
setwd('~/projects/egret/analyses')

subset_input <- readRDS("/home/victor/projects/egret/analyses/stan/generative/sequential_survival/subset_input_53species.rds")

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
  
  N_germ = N_germ,
  
  uniq_species = subset_input$uniq_species
)

# Prepare phylogeny
phylo <- ape::read.tree("/home/victor/projects/egret/analyses/output/egretPhylogenyFull.tre")
gymno <- c('Pseudotsuga_menziesii', 'Pinus_roxburghii','Pinus_sylvestris','Pinus_halepensis',
           'Pinus_brutia','Pinus_canariensis','Pinus_bungeana','Pinus_koraiensis','Pinus_wallichiana',
           'Pinus_strobus','Picea_orientalis','Picea_abies','Picea_sitchensis','Picea_glauca',
           'Abies_amabilis','Abies_procera','Abies_grandis','Abies_nordmanniana','Abies_chensiensis',
           'Abies_lasiocarpa','Tsuga_heterophylla','Tsuga_mertensiana','Ginkgo_biloba', 'Juniperus_oxycedrus',
           'Juniperus_communis')
namesphy <- phylo$tip.label
phylo <- phytools::force.ultrametric(phylo, method="extend")
phylo$node.label <- seq(1,length(phylo$node.label),1)
ape::is.ultrametric(phylo)
# plot(phylo, cex=0.7)
# phylo <- ape::drop.tip(phylo, gymno) # exclude gymnosperms
# plot(phylo, cex=0.7)
cphy <- ape::vcv.phylo(phylo,corr=TRUE)
# rm(gymno)


spp <-  unique(stan_data$uniq_species)
length(spp)
length(phylo$node.label)
phylo2 <- ape::keep.tip(phylo, spp)
cphy <- ape::vcv(phylo2,corr=TRUE)

fit <- readRDS('/home/victor/projects/egret/analyses/analyseSeedCues/output/model/new_survival/fit_53species_hierToptsigma_reparam.rds')
samples <- util$extract_expectand_vals(fit)

pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/57species/all_parameters.pdf',
    height = 10, width = 10)
# pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/57species/kappa.pdf',
#     height = 10, width = 10)
par(mfrow = c(1,2))
par(mar = c(3,0,0,0), mgp=c(3,0.5,0))
plot(phylo2, cex = 0.7, x.lim = c(0, 450), y.lim = c(0, stan_data$N_species+1),
     label.offset = 2)
# segments(x0 = 600, y0 = 1, y1 = stan_data$N_species)
# segments(x0 = 500, x1 = 700, y0 = 1, y1 = 1)
plot(NULL, xlim = c(0,0.2), ylim = c(0, stan_data$N_species+1), bty = 'n',
     yaxt="n", xaxt = 'n')
for(s in phylo2$tip.label){
  snum <- which(phylo2$tip.label == s)
  pq <- util$ensemble_mcmc_quantile_est(samples[[paste0('kappa[',which(stan_data$uniq_species == s),']')]],
                                        c(0.1, 0.25, 0.5, 0.75, 0.9))
  segments(x0 = pq['10%'], x1 = pq['90%'], y0 = snum, col = util$c_light_teal)
  points(x = pq['50%'], y = snum, cex = 0.8, pch = 20, col = util$c_mid_teal)
}
axis(side = 1, at = seq(0, 0.2, 0.05), 
     line = -0.75, cex.axis = 0.75)
mtext(side = 1, text = expression(kappa~'(chilling reduction factor)'), line = 1, cex = 0.9)
# dev.off()

# pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/57species/psi.pdf',
#     height = 10, width = 10)
par(mfrow = c(1,2))
par(mar = c(3,0,0,0), mgp=c(3,0.5,0))
plot(phylo2, cex = 0.7, x.lim = c(0, 450), y.lim = c(0, stan_data$N_species+1),
     label.offset = 2)
# segments(x0 = 600, y0 = 1, y1 = stan_data$N_species)
# segments(x0 = 500, x1 = 700, y0 = 1, y1 = 1)
plot(NULL, xlim = c(-2,7), ylim = c(0, stan_data$N_species+1), bty = 'n',
     yaxt="n", xaxt = 'n')
for(s in phylo2$tip.label){
  snum <- which(phylo2$tip.label == s)
  pq <- util$ensemble_mcmc_quantile_est(samples[[paste0('psi[',which(stan_data$uniq_species == s),']')]],
                                        c(0.1, 0.25, 0.5, 0.75, 0.9))
  segments(x0 = pq['10%'], x1 = pq['90%'], y0 = snum, col = util$c_light_teal)
  points(x = pq['50%'], y = snum, cex = 0.8, pch = 20, col = util$c_mid_teal)
}
axis(side = 1, at = seq(-2, 7, 1), 
     line = -0.75, cex.axis = 0.75)
mtext(side = 1, text = expression(psi~'(log-forcing requirement)'), line = 1, cex = 0.9)
# dev.off()

# pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/57species/sigma.pdf',
#     height = 10, width = 10)
par(mfrow = c(1,2))
par(mar = c(3,0,0,0), mgp=c(3,0.5,0))
plot(phylo2, cex = 0.7, x.lim = c(0, 450), y.lim = c(0, stan_data$N_species+1),
     label.offset = 2)
# segments(x0 = 600, y0 = 1, y1 = stan_data$N_species)
# segments(x0 = 500, x1 = 700, y0 = 1, y1 = 1)
plot(NULL, xlim = c(0,5), ylim = c(0, stan_data$N_species+1), bty = 'n',
     yaxt="n", xaxt = 'n')
for(s in phylo2$tip.label){
  snum <- which(phylo2$tip.label == s)
  pq <- util$ensemble_mcmc_quantile_est(samples[[paste0('sigma[',which(stan_data$uniq_species == s),']')]],
                                        c(0.1, 0.25, 0.5, 0.75, 0.9))
  segments(x0 = pq['10%'], x1 = pq['90%'], y0 = snum, col = util$c_light_teal)
  points(x = pq['50%'], y = snum, cex = 0.8, pch = 20, col = util$c_mid_teal)
}
axis(side = 1, at = seq(0, 5, 1), 
     line = -0.75, cex.axis = 0.75)
mtext(side = 1, text = expression(sigma~'(log-forcing requirement spread)'), line = 1, cex = 0.9)
# dev.off()

# pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/57species/topt.pdf',
#     height = 10, width = 10)
par(mfrow = c(1,2))
par(mar = c(3,0,0,0), mgp=c(3,0.5,0))
plot(phylo2, cex = 0.7, x.lim = c(0, 450), y.lim = c(0, stan_data$N_species+1),
     label.offset = 2)
# segments(x0 = 600, y0 = 1, y1 = stan_data$N_species)
# segments(x0 = 500, x1 = 700, y0 = 1, y1 = 1)
plot(NULL, xlim = c(10,60), ylim = c(0, stan_data$N_species+1), bty = 'n',
     yaxt="n", xaxt = 'n')
newspecies <- rnorm(length(samples[['mu_T_opt']]), c(samples[['mu_T_opt']]), c(samples[['sigma_T_opt']]))
newspecies <- matrix(newspecies, nrow = nrow(samples[['mu_T_opt']]), ncol = ncol(samples[['mu_T_opt']]))
pq <- util$ensemble_mcmc_quantile_est(newspecies, c(0.1, 0.25, 0.5, 0.75, 0.9))
rect(xleft = pq['10%'], xright = pq['90%'], 
     ybottom = -1, ytop = stan_data$N_species+1,
     border = NA, col = paste0(util$c_light_teal, '10'))
pq <- util$ensemble_mcmc_quantile_est(samples[['mu_T_opt']],
                                      c(0.1, 0.25, 0.5, 0.75, 0.9))
segments(x0 = pq['50%'], y0 = -1, y1 = stan_data$N_species+1,
         col = util$c_light_teal, lty = 2)
rect(xleft = pq['10%'], xright = pq['90%'], 
     ybottom = -1, ytop = stan_data$N_species+1,
     border = NA, col = paste0(util$c_light_teal, '20'))
for(s in phylo2$tip.label){
  snum <- which(phylo2$tip.label == s)
  pq <- util$ensemble_mcmc_quantile_est(samples[[paste0('T_opt[',which(stan_data$uniq_species == s),']')]],
                                        c(0.1, 0.25, 0.5, 0.75, 0.9))
  segments(x0 = pq['10%'], x1 = pq['90%'], y0 = snum, col = util$c_light_teal)
  points(x = pq['50%'], y = snum, cex = 0.8, pch = 20, col = util$c_mid_teal)
}
axis(side = 1, at = seq(10, 60, 10), 
     line = -0.75, cex.axis = 0.75)
mtext(side = 1, text = expression(T[opt]~'(optimal forcing temperature)'), line = 1, cex = 0.9)
# dev.off()

# pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/57species/width.pdf',
#     height = 10, width = 10)
par(mfrow = c(1,2))
par(mar = c(3,0,0,0), mgp=c(3,0.5,0))
plot(phylo2, cex = 0.7, x.lim = c(0, 450), y.lim = c(0, stan_data$N_species+1),
     label.offset = 2)
# segments(x0 = 600, y0 = 1, y1 = stan_data$N_species)
# segments(x0 = 500, x1 = 700, y0 = 1, y1 = 1)
plot(NULL, xlim = c(0,35), ylim = c(0, stan_data$N_species+1), bty = 'n',
     yaxt="n", xaxt = 'n')
newspecies <- rnorm(length(samples[['mu_log_width']]), c(samples[['mu_log_width']]), c(samples[['sigma_log_width']]))
newspecies <- matrix(exp(newspecies), nrow = nrow(samples[['mu_log_width']]), ncol = ncol(samples[['mu_log_width']]))
pq <- util$ensemble_mcmc_quantile_est(newspecies, c(0.1, 0.25, 0.5, 0.75, 0.9))
rect(xleft = pq['10%'], xright = pq['90%'], 
     ybottom = -1, ytop = stan_data$N_species+1,
     border = NA, col = paste0(util$c_light_teal, '10'))
pq <- util$ensemble_mcmc_quantile_est(exp(samples[['mu_log_width']]),
                                      c(0.1, 0.25, 0.5, 0.75, 0.9))
segments(x0 = pq['50%'], y0 = -1, y1 = stan_data$N_species+1,
         col = util$c_light_teal, lty = 2)
rect(xleft = pq['10%'], xright = pq['90%'], 
     ybottom = -1, ytop = stan_data$N_species+1,
     border = NA, col = paste0(util$c_light_teal, '20'))
for(s in phylo2$tip.label){
  snum <- which(phylo2$tip.label == s)
  pq <- util$ensemble_mcmc_quantile_est(samples[[paste0('width[',which(stan_data$uniq_species == s),']')]],
                                        c(0.1, 0.25, 0.5, 0.75, 0.9))
  segments(x0 = pq['10%'], x1 = pq['90%'], y0 = snum, col = util$c_light_teal)
  points(x = pq['50%'], y = snum, cex = 0.8, pch = 20, col = util$c_mid_teal)
}
axis(side = 1, at = seq(0, 35, 5), 
     line = -0.75, cex.axis = 0.75)
mtext(side = 1, text = expression(width~'(optimal forcing temperature)'), line = 1, cex = 0.9)
# dev.off()

# pdf('/home/victor/projects/egret/analyses/analyseSeedCues/figures/57species/pv.pdf',
#     height = 10, width = 10)
par(mfrow = c(1,2))
par(mar = c(3,0,0,0), mgp=c(3,0.5,0))
plot(phylo2, cex = 0.7, x.lim = c(0, 450), y.lim = c(0, stan_data$N_species+1),
     label.offset = 2)
# segments(x0 = 600, y0 = 1, y1 = stan_data$N_species)
# segments(x0 = 500, x1 = 700, y0 = 1, y1 = 1)
plot(NULL, xlim = c(0,1), ylim = c(0, stan_data$N_species+1), bty = 'n',
     yaxt="n", xaxt = 'n')
for(s in phylo2$tip.label){
  snum <- which(phylo2$tip.label == s)
  pq <- util$ensemble_mcmc_quantile_est(samples[[paste0('pv[',which(stan_data$uniq_species == s),']')]],
                                        c(0.1, 0.25, 0.5, 0.75, 0.9))
  segments(x0 = pq['10%'], x1 = pq['90%'], y0 = snum, col = util$c_light_teal)
  points(x = pq['50%'], y = snum, cex = 0.8, pch = 20, col = util$c_mid_teal)
}
axis(side = 1, at = seq(0, 1, 0.25), 
     line = -0.75, cex.axis = 0.75)
mtext(side = 1, text = expression(p[v]~'(viability)'), line = 1, cex = 0.9)
dev.off()
