# January 7, 2026
# started by D. Loughnan with the aim to model the USDA data using the main egret model
# but USDA data just has stratification and germination temps
library(cmdstanr)
library(stringr)
library(ape)
library(phytools)
# library(rstan)
options(mc.cores = parallel::detectCores())
library(dplyr) # oops
# this preliminary version uses dplyr, will soon take the time to move to a full base R version

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
  setwd('~/projects/egret/analyses')
} 

phylo <- ape::read.tree("output/ospreeUsdaTree.tre")
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

# Trim the phylo tree with species present in the dataset
spp <-  unique(modeld$genusspecies)
length(spp)
length(phylo$node.label)
phylo2 <- keep.tip(phylo, spp)
cphy <- vcv.phylo(phylo2,corr=TRUE)

# Prepare data for Stan - chilling hours between -20 and 10
modeld$numspp = as.integer(factor(modeld$genusspecies, levels = colnames(cphy)))
mdl.data <- list(N_degen = sum(modeld$responseValueNum %in% c(0,1)),
                 N_prop = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1),
                 
                 Nsp =  length(unique(modeld$numspp)),
                 sp_degen = array(modeld$numspp[modeld$responseValueNum %in% c(0,1)],
                                  dim = sum(modeld$responseValueNum%in% c(0,1))),
                 sp_prop = array(modeld$numspp[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                 dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 y_degen = array(modeld$responseValueNum[modeld$responseValueNum %in% c(0,1)],
                                 dim = sum(modeld$responseValueNum%in% c(0,1))),
                 y_prop = array(modeld$responseValueNum[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 t_degen = array(modeld$time[modeld$responseValueNum %in% c(0,1)],
                                 dim = sum(modeld$responseValueNum%in% c(0,1))),
                 t_prop = array(modeld$time[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 f_degen = array(modeld$forcing[modeld$responseValueNum %in% c(0,1)],
                                 dim = sum(modeld$responseValueNum%in% c(0,1))),
                 f_prop = array(modeld$forcing[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 c_degen = array(modeld$chillh10[modeld$responseValueNum %in% c(0,1)],
                                 dim = sum(modeld$responseValueNum%in% c(0,1))),
                 c_prop = array(modeld$chillh10[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 Vphy = cphy)

# Compile and run model
smordbeta <- cmdstan_model("stan/orderedbetalikelihood_3slopes.stan")
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
