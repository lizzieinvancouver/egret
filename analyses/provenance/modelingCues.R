# Started 18 Nov 2025
# by the provenance model subgroup!

library(stringr)
library(ape)
library(phytools)
library(rstan)
options(mc.cores = parallel::detectCores())

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
  setwd("/Users/christophe_rouleau-desrochers/github/egret/analyses")
} else if(length(grep("crouleau", getwd())) > 0){
  setwd("/home/crouleau/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# load Mike's diagnostic tools
util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

# Load data, discard some experiments following various decision rules
source('provenance/decisionRules.R')
# source('studyDesign/decisionRules_abundant0s_Deirdre.R')

# Prepare phylogeny
phylo <- ape::read.tree("output/egretPhylogenyFull.tre")
phylo$tip.label <- sapply(phylo$tip.label, function(i) paste0(unlist(stringr::str_split(i, '_'))[1:2], collapse = '_')) # remove subspecies or whatever
names(phylo$tip.label) <- unname(phylo$tip.label)
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
phylo <- ape::drop.tip(phylo, gymno) # exclude gymnosperms
# plot(phylo, cex=0.7)
cphy <- ape::vcv.phylo(phylo,corr=TRUE)
rm(gymno)

# Process data 
# (1) - removing rows where we do not have any info on forcing) 
modeld <- newd[!is.na(newd$germDuration) & !is.na(newd$germTempGen) & newd$germDuration != 'unknown' & newd$germTempGen != "ambient",] 
# (2) - separating warm and cold strat. durations
modeld$warmStratDur <- as.numeric(sapply(1:nrow(modeld), function(i){
  seq <-  unlist(stringr::str_split(modeld$stratSequence_condensed[i], ' then '))
  temp <-  unlist(stringr::str_split(modeld$stratDur_condensed[i], ' then '))
  id <- which(seq == 'warm')
  return(ifelse(is.null(id), NA, temp[id]))
}))
modeld$coldStratDur <- as.numeric(sapply(1:nrow(modeld), function(i){
  seq <-  unlist(stringr::str_split(modeld$stratSequence_condensed[i], ' then '))
  temp <-  unlist(stringr::str_split(modeld$stratDur_condensed[i], ' then '))
  id <- which(seq == 'cold')
  return(ifelse(is.null(id), NA, temp[id]))
}))
# (3) - assuming NA strat. mean 0
modeld$warmStratDur <- ifelse(is.na(modeld$warmStratDur), 0, modeld$warmStratDur)
modeld$coldStratDur <- ifelse(is.na(modeld$coldStratDur), 0, modeld$coldStratDur)
# (4) - removing species not present in the phylo tree
modeld$genusspecies <- sapply(modeld$genusspecies, function(i) stringr::str_split_i(i, ' ', 1))
# test <- modeld[!(modeld$genusspecies %in% phylo$tip.label), ]
# unique(test$genusspecies) # check only Gymno!
modeld <- modeld[(modeld$genusspecies %in% phylo$tip.label), ]
# (4) - transform response to proportion and germ. covariates to numeric
modeld$responseValueNum <- as.numeric(modeld$responseValueNum)/100
modeld$germDuration <- as.numeric(modeld$germDuration)
modeld$germTempGen <- as.numeric(modeld$germTempGen)
# temporary - need to check whether odd values (>>> scrapping uncertainty) have been corrected
# modeld[modeld$responseValueNum < 1.05,] # not needed anymore!
# (5) - transform values a bit above or below 0 (due to scrapping uncertainty)
modeld$responseValueNum <- ifelse(modeld$responseValueNum > 1, 1, modeld$responseValueNum)
modeld$responseValueNum <- ifelse(modeld$responseValueNum < 0, 0, modeld$responseValueNum)
modeld$germDuration <- ifelse(modeld$germDuration < 0, 0, modeld$germDuration)

modeld <- modeld[, c('uniqueID','datasetID', 'study', 'genusspecies', 'provLatLonAlt', 'responseValueNum', 'warmStratDur', 'coldStratDur', 'germTempGen', 'germDuration')]
modeld <- na.omit(modeld) 

# Removing potential duplicates
modeld_wodup <- modeld[!duplicated(modeld),]
message(paste0("Removing ", nrow(modeld)-nrow(modeld_wodup), ' potential duplicates!'))# 137 rows 
# Other test for duplicate removal
modeld$responseValueRounded <- round(modeld$responseValueNum,3) # rounded to 3 digits, ie percentage with 1 digits (data scraping uncertainty...?)
modeld_wodup <- modeld[!duplicated(modeld[c('datasetID', 'study', 'genusspecies', 'responseValueRounded', 'warmStratDur', 'coldStratDur', 'germTempGen', 'germDuration')]),]
nrow(modeld)-nrow(modeld_wodup) # 14() when responseValue rounded to 3 digits (XX.X%)
modeld <- modeld_wodup 
rm(modeld_wodup)

# I hate doing this, but I want to go swimmmmmiiiiing
modeld$warmStratDur <- scale(modeld$warmStratDur)[,1]
modeld$coldStratDur <- scale(modeld$coldStratDur)[,1]
modeld$germDuration <- scale(modeld$germDuration)[,1]
modeld$germTempGen <- scale(modeld$germTempGen)[,1]

# Trim the phylo tree with species present in the dataset
spp <-  unique(modeld$genusspecies)
length(spp)
length(phylo$node.label)
phylo2 <- ape::keep.tip(phylo, spp)
cphy <- ape::vcv.phylo(phylo2,corr=TRUE)

# Prepare data for Stan
modeld$numspp = as.integer(factor(modeld$genusspecies, levels = colnames(cphy)))
modeld$numprov = as.integer(factor(modeld$provLatLonAlt))
mdl.data <- list(N_degen = sum(modeld$responseValueNum %in% c(0,1)),
                 N_prop = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1),
                 
                 Nsp =  length(unique(modeld$numspp)),
                 sp_degen = array(modeld$numspp[modeld$responseValueNum %in% c(0,1)],
                                  dim = sum(modeld$responseValueNum%in% c(0,1))),
                 sp_prop = array(modeld$numspp[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                 dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 Nprov =  length(unique(modeld$numprov)),
                 prov_degen = array(modeld$numprov[modeld$responseValueNum %in% c(0,1)],
                                  dim = sum(modeld$responseValueNum%in% c(0,1))),
                 prov_prop = array(modeld$numprov[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                 dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 y_degen = array(modeld$responseValueNum[modeld$responseValueNum %in% c(0,1)],
                                 dim = sum(modeld$responseValueNum%in% c(0,1))),
                 y_prop = array(modeld$responseValueNum[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 t_degen = array(modeld$germDuration[modeld$responseValueNum %in% c(0,1)],
                                 dim = sum(modeld$responseValueNum%in% c(0,1))),
                 t_prop = array(modeld$germDuration[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 f_degen = array(modeld$germTempGen[modeld$responseValueNum %in% c(0,1)],
                                 dim = sum(modeld$responseValueNum%in% c(0,1))),
                 f_prop = array(modeld$germTempGen[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 cs_degen = array(modeld$coldStratDur[modeld$responseValueNum %in% c(0,1)],
                                  dim = sum(modeld$responseValueNum%in% c(0,1))),
                 cs_prop = array(modeld$coldStratDur[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                 dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 Vphy = cphy)

# Posterior quantification
smordbeta <- stan_model("stan/orderedbetalikelihood_3slopes_provenance.stan")
fit <- sampling(smordbeta, mdl.data,
                iter = 2024, warmup = 1000,
                chains = 4)

smordbeta_nophy <- stan_model("stan/orderedbetalikelihood_3slopes_provenance_nophylo.stan")
fit_nophy <- sampling(smordbeta_nophy, mdl.data,
                iter = 2024, warmup = 1000,
                chains = 4)

smordbeta_nophy_noprov <- stan_model("stan/orderedbetalikelihood_3slopes_noprovenance_nophylo.stan")
fit_nophy_noprov <- sampling(smordbeta_nophy_noprov, mdl.data,
                      iter = 2024, warmup = 1000,
                      chains = 4)

# Diagnostics
diagnostics <- util$extract_hmc_diagnostics(fit_nophy)
util$check_all_hmc_diagnostics(diagnostics)
samples <- util$extract_expectand_vals(fit_nophy)

# for(prov in 1:mdl.data$Nprov){
#   util$plot_div_pairs(paste0('a_prov[',prov,']'), 'sigma_a_prov', samples, diagnostics, transforms=list('sigma_a_prov' = 1))
# }
# for(prov in 1:mdl.data$Nprov){
#   util$plot_div_pairs(paste0('bf_prov[',prov,']'), 'sigma_bf_prov', samples, diagnostics, transforms=list('sigma_bf_prov' = 1))
# }
# for(prov in 1:mdl.data$Nprov){
#   util$plot_div_pairs(paste0('bt_prov[',prov,']'), 'sigma_bt_prov', samples, diagnostics, transforms=list('sigma_bt_prov' = 1))
# }

base_samples <- util$filter_expectands(samples,
                                       c('a_z', 'lambda_a', 'sigma_a', 'a',
                                         'bt_z', 'lambda_bt', 'sigma_bt', 'bt',
                                         'bf_z', 'lambda_bf', 'sigma_bf', 'bf',
                                         'bcs_z', 'lambda_bcs', 'sigma_bcs', 'bcs',
                                         'sigma_a_prov', 'a_prov',
                                         'sigma_bt_prov', 'bt_tilde_prov',
                                         'sigma_bf_prov', 'bf_tilde_prov',
                                         'sigma_bcs_prov', 'bcs_tilde_prov',
                                         'cutpoints', 'kappa'),
                                       check_arrays=TRUE)
util$check_all_expectand_diagnostics(base_samples)

# Retrodictive check
par(mfrow=c(1, 1), mar = c(4,4,2,2))
names <- c(sapply(1:mdl.data$N_prop, function(n) paste0('y_prop_gen[',n,']')),
           sapply(1:mdl.data$N_degen, function(n) paste0('y_degen_gen[',n,']')))
preds <- samples[names]
names(preds) <- sapply(1:length(preds), function(n) paste0('y_gen[',n,']'))
util$plot_hist_quantiles(preds, 'y_gen', 0, 1, 0.1,
                         baseline_values=c(mdl.data$y_prop, mdl.data$y_degen), 
                         xlab="Germination perc.")

# Posterior inference
par(mfrow=c(4, 2), mar = c(4,4,1,1))
util$plot_expectand_pushforward(samples[['sigma_a']], 20,
                                flim = c(0,6),
                                display_name="sigma_a")
util$plot_expectand_pushforward(samples[['sigma_a_prov']], 20,
                                flim = c(0,6),
                                display_name="sigma_a_prov")
util$plot_expectand_pushforward(samples[['sigma_bt']], 20,
                                flim = c(0,6),
                                display_name="sigma_bt")
util$plot_expectand_pushforward(samples[['sigma_bt_prov']], 20,
                                flim = c(0,6),
                                display_name="sigma_bt_prov")
util$plot_expectand_pushforward(samples[['sigma_bf']], 20,
                                flim = c(0,6),
                                display_name="sigma_bf")
util$plot_expectand_pushforward(samples[['sigma_bf_prov']], 20,
                                flim = c(0,6),
                                display_name="sigma_bf_prov")
util$plot_expectand_pushforward(samples[['sigma_bcs']], 20,
                                flim = c(0,6),
                                display_name="sigma_bcs")
util$plot_expectand_pushforward(samples[['sigma_bcs_prov']], 20,
                                flim = c(0,6),
                                display_name="sigma_bcs_prov")

#  

sp <- 7
provs <- c(mdl.data$prov_prop[which(mdl.data$sp_prop == sp)], mdl.data$prov_degen[which(mdl.data$sp_degen == sp)])
par(mfrow=c(2, 2), mar = c(4,4,4,1), cex.main = 1.1)
for(p in unique(provs)){
  idx_prop <- which(mdl.data$sp_prop == sp & mdl.data$prov_prop == p)
  idx_degen <- which(mdl.data$sp_degen == sp & mdl.data$prov_degen == p)
  names <- unlist(c(sapply(idx_prop, function(n) paste0('y_prop_gen[',n,']')),
             sapply(idx_degen, function(n) paste0('y_degen_gen[',n,']'))))
  c <- c(mdl.data$cs_prop[idx_prop], mdl.data$cs_degen[idx_degen])
  orderx <- order(c)
  names <- names[orderx]
  c <- c[orderx]
  util$plot_conn_pushforward_quantiles(samples, names, plot_xs = c, main = paste0('Provenance: ', levels(factor(modeld$provLatLonAlt))[p]),
                                       xlab = 'Chilling (scaled)', ylab = 'Germination perc.(marginal quantiles)',
                                       display_xlim = c(-0.5,1.1), display_ylim = c(0,1))
  y <- c(mdl.data$y_prop[idx_prop], mdl.data$y_degen[idx_degen])
  y <- y[orderx]
  points(c, y, pch=16, cex=1.2, col="white")
  points(c, y, pch=16, cex=0.8, col="black")
}

sp <- 7
provs <- c(mdl.data$prov_prop[which(mdl.data$sp_prop == sp)], mdl.data$prov_degen[which(mdl.data$sp_degen == sp)])
par(mfrow=c(2, 2), mar = c(4,4,4,1), cex.main = 1.1)
for(p in unique(provs)){
  idx_prop <- which(mdl.data$sp_prop == sp & mdl.data$prov_prop == p)
  idx_degen <- which(mdl.data$sp_degen == sp & mdl.data$prov_degen == p)
  names <- unlist(c(sapply(idx_prop, function(n) paste0('y_prop_gen[',n,']')),
                    sapply(idx_degen, function(n) paste0('y_degen_gen[',n,']'))))
  t <- c(mdl.data$t_prop[idx_prop], mdl.data$t_degen[idx_degen])
  cs <- c(mdl.data$cs_prop[idx_prop], mdl.data$cs_degen[idx_degen])
  f <- c(mdl.data$f_prop[idx_prop], mdl.data$f_degen[idx_degen])
  
  orderx <- order(t)
  names <- names[orderx]
  t <- t[orderx]
  util$plot_conn_pushforward_quantiles(samples, names, plot_xs = t, main = paste0('Provenance: ', levels(factor(modeld$provLatLonAlt))[p]),
                                       xlab = 'Time (scaled)', ylab = 'Germination perc.(marginal quantiles)',
                                       display_xlim = c(-0.6, -0.3), display_ylim = c(0,1))
  y <- c(mdl.data$y_prop[idx_prop], mdl.data$y_degen[idx_degen])
  y <- y[orderx]
  points(t, y, pch=16, cex=1.2, col="white")
  points(t, y, pch=16, cex=0.8, col="black")
}




sp <- 7
provs <- c(mdl.data$prov_prop[which(mdl.data$sp_prop == sp)], mdl.data$prov_degen[which(mdl.data$sp_degen == sp)])
par(mfrow=c(2, 3), mar = c(4,4,4,1), cex.main = 1.1)
p <- 1

idx_prop <- which(mdl.data$sp_prop == sp & mdl.data$prov_prop == p)
idx_degen <- which(mdl.data$sp_degen == sp & mdl.data$prov_degen == p)
names <- unlist(c(sapply(idx_prop, function(n) paste0('y_prop_gen[',n,']')),
                  sapply(idx_degen, function(n) paste0('y_degen_gen[',n,']'))))
t <- c(mdl.data$t_prop[idx_prop], mdl.data$t_degen[idx_degen])
cs <- c(mdl.data$cs_prop[idx_prop], mdl.data$cs_degen[idx_degen])
f <- c(mdl.data$f_prop[idx_prop], mdl.data$f_degen[idx_degen])
y <- c(mdl.data$y_prop[idx_prop], mdl.data$y_degen[idx_degen])

for(c in unique(cs)){

  names_c <- names[which(cs == c)]
  t_c <- t[which(cs == c)]

  orderx <- order(t_c)
  names_c <- names_c[orderx]
  t_c <- t_c[orderx]
  util$plot_conn_pushforward_quantiles(samples, names_c, plot_xs = t_c, main = paste0('Chilling (scaled): ', round(c,2)),
                                       xlab = 'Time (scaled)', ylab = 'Germination perc.(marginal quantiles)',
                                       display_xlim = c(-0.6, -0.3), display_ylim = c(0,1))
  y_c <- y[which(cs == c)]
  y_c <- y_c[orderx]
  points(t_c, y_c, pch=16, cex=1.2, col="white")
  points(t_c, y_c, pch=16, cex=0.8, col="black")

}

