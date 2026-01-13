# January 7, 2026
# started by D. Loughnan with the aim to model the USDA data using the main egret model
# but USDA data just has stratification and germination temps
library(cmdstanr)
library(stringr)
library(ape)
library(phytools)
library(pez)
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

d <- read.csv("output/usdaChillGermTemp.csv")

d$latbi[which(d$latbi == "Aronia_x prunifolia")] <-"Aronia_x_prunifolia"


phylo <- ape::read.tree("output/usdaPhylogenyFull.tre")
missing <- c("Quercus_falcata","Quercus_nigra","Quercus_chrysolepis", "Quercus_dumosa", "Quercus_ilicifolia",
              "Quercus_imbricaria", "Quercus_pagoda","Quercus_shumardii","Quercus_texana")
d <- d[!d$latbi %in% missing,]
subby <- unique(d$latbi)

namesphy <- phylo$tip.label
phylo <- phytools::force.ultrametric(phylo, method="extend")
phylo$node.label <- seq(1,length(phylo$node.label),1)
ape::is.ultrametric(phylo)
# plot(phylo, cex=0.7)

phylo <- ape::keep.tip(phylo, subby) # exclude gymnosperms
# plot(phylo, cex=0.7)
cphy <- ape::vcv.phylo(phylo,corr=TRUE)
rm(subby)

cphy <- vcv.phylo(phylo,corr=TRUE)

# numsp
# Prepare data for Stan - chilling hours between -20 and 10
d$numspp = as.integer(factor(d$latbi, levels = colnames(cphy)))
d$responseValueProp <- d$responseValue/100
d$chillDurationS <- scale(d$chillDuration)
d$tempDayS <- scale(d$tempDay)

mdl.dataUSDA <- list(N_degen = sum(d$responseValueProp %in% c(0,1)),
                 N_prop = sum(d$responseValueProp>0 & d$responseValueProp<1),
                 
                 Nsp =  length(unique(d$latbi)),
                 sp_degen = array(d$numspp[d$responseValueProp %in% c(0,1)],
                                  dim = sum(d$responseValueProp%in% c(0,1))),
                 sp_prop = array(d$numspp[d$responseValueProp>0 & d$responseValueProp<1],
                                 dim = sum(d$responseValueProp>0 & d$responseValueProp<1)),
                 
                 y_degen = array(d$responseValueProp[d$responseValueProp %in% c(0,1)],
                                 dim = sum(d$responseValueProp%in% c(0,1))),
                 y_prop = array(d$responseValueProp[d$responseValueProp>0 & d$responseValueProp<1],
                                dim = sum(d$responseValueProp>0 & d$responseValueProp<1)),
                 
                 t_degen = array(d$chillDurationS[d$responseValueProp %in% c(0,1)],
                                 dim = sum(d$responseValueProp%in% c(0,1))),
                 t_prop = array(d$chillDurationS[d$responseValueProp>0 & d$responseValueProp<1],
                                dim = sum(d$responseValueProp>0 & d$responseValueProp<1)),
                 
                 f_degen = array(d$tempDayS[d$responseValueProp %in% c(0,1)],
                                 dim = sum(d$responseValueProp%in% c(0,1))),
                 f_prop = array(d$tempDayS[d$responseValueProp>0 & d$responseValueProp<1],
                                dim = sum(d$responseValueProp>0 & d$responseValueProp<1)),
                 Vphy = cphy)

# Compile and run model
smordbeta <-stan_model("stan/orderedbetalikelihood_2slopes.stan")
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

saveRDS(fit, file = 'analyseBudSeed/output/fit_usda.rds')
saveRDS(summ, file = 'analyseBudSeed/output/summary_usda.rds')
saveRDS(diagnostics, file = 'analyseBudSeed/output/diagnostics_usda.rds')

