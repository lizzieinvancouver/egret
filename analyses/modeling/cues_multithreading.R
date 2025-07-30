# Started Feb 12, 2025
# by Victor

library(stringr)
library(ape)
library(phytools)
library(cmdstanr)
library(dplyr) # oops
# this preliminary version uses dplyr, will soon take the time to move to a full base R version

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
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# Load data, discard some experiments following various decision rules
source('studyDesign/decisionRules.R')

# Prepare phylogeny
phylo <- ape::read.tree("output/phylogeny.tre")
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
# (2) - transform NA chilling to 0
# (3) - removing species not present in the phylo tree)
# (4) - transform response to proportion and covariates to numeric
# (5) - transform values a bit above or below 0 (due to scrapping uncertainty)
# (6) - computing chilling hours, scale covariates
modeld <- newd %>%
  dplyr::filter(!is.na(germDuration) & !is.na(germTempGen) & germDuration != 'unknown' & germTempGen != "ambient") %>% # (1)
  dplyr::mutate(dormancyTemp = if_else(dormancyTemp %in% c(NA, "NA"), "0", dormancyTemp)) %>% # (2)
  dplyr::filter(genusspecies %in% phylo$tip.label) %>% # (3)
  dplyr::mutate(responseValue = as.numeric(responseValue)/100, # (4)
                germDuration = as.numeric(germDuration), # (4)
                germTempGen = as.numeric(germTempGen), # (4)
                dormancyDuration = as.numeric(dormancyDuration), # (4)
                dormancyTemp = as.numeric(dormancyTemp)) %>% # (4)
  dplyr::filter(responseValue < 1.05) %>% # temporary - need to check whether odd values (>>> scrapping uncertainty) have been corrected
  dplyr::mutate(responseValue = if_else(responseValue > 1, 1, responseValue), # (5)
                responseValue = if_else(responseValue < 0, 0, responseValue), # (5)
                germDuration = if_else(germDuration < 0, 0, germDuration), # (5)
                time = scale(germDuration)[,1], # (6)
                forcing = scale(germTempGen)[,1], # (6)
                chillh10 = scale(dormancyDuration * 24 * as.numeric(dormancyTemp < 10 & dormancyTemp > -20))[,1], # (6)
                chillh5 = scale(dormancyDuration * 24 * as.numeric(dormancyTemp < 5 & dormancyTemp > -20))[,1]) %>% # (6)
  dplyr::filter(!is.na(germTempGen)) # still needed because germTempGen is far from being in a nice format (e.g. "15 and 25 then 3"...)

# Removing potential duplicates
modeld_wodup <- modeld[!duplicated(modeld[c('datasetID', 'study', 'genusspecies', 'responseValue', 'time', 'forcing', 'chillh10')]),]
message(paste0("Removing ", nrow(modeld)-nrow(modeld_wodup), ' potential duplicates!'))# 168 rows 
modeld <- modeld_wodup 
rm(modeld_wodup)

# # Other test for duplicate removal: does not change anything
# modeld$responseValueRounded <- round(modeld$responseValue,2) # rounded to 2 digits, ie percentage with 0 digits (data scraping uncertainty...?)
# test <- modeld[!duplicated(modeld[c('datasetID', 'study', 'genusspecies', 'responseValueRounded', 'time', 'forcing', 'chillh')]),]
# nrow(modeld)-nrow(test) # still 17!

# Trim the phylo tree with species present in the dataset
spp <-  unique(modeld$genusspecies)
length(spp)
length(phylo$node.label)
phylo2 <- keep.tip(phylo, spp)
cphy <- vcv.phylo(phylo2,corr=TRUE)

# Prepare data for Stan - chilling hours between -20 and 10
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
mdl.data$grainsize <- ceiling(mdl.data$N_prop/4)

smordbeta <- cmdstan_model("stan/orderedbetalikelihood_3slopes_reducesum.stan", 
                           cpp_options = list(stan_threads = TRUE, "CXXFLAGS += -pthread"), force_recompile = TRUE)

mdl.data$grainsize <- ceiling(mdl.data$N_prop/4)
fit <- smordbeta$sample(mdl.data, chains = 4, parallel_chains = 4, threads_per_chain = 4,
                        iter_sampling = 1000, iter_warmup = 3000)
summ <- summarise_draws(fit, ~ quantile(., probs = c(0.025, 0.25, 0.5, 0.75, 0.975)), default_convergence_measures())
sampler_params  <- fit$sampler_diagnostics(inc_warmup = FALSE)
diagnostics <- list(
  max_treedepth= max(sapply(sampler_params, function(x) max(sampler_params[,,'treedepth__']))),
  max_divergence = max(sapply(sampler_params, function(x) sum(sampler_params[,,'divergent__']))),
  max_rhat = max(summ$rhat, na.rm = TRUE),
  min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE),
  min_ess_tail = min(summ$ess_tail, na.rm = TRUE)
)
saveRDS(fit, file = 'modeling/output/3slopes/fit_chillh10.rds')
saveRDS(summ, file = 'modeling/output/3slopes/summary_chillh10.rds')
saveRDS(diagnostics, file = 'modeling/output/3slopes/diagnostics_chillh10.rds')
