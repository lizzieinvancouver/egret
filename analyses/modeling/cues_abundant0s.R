# Started 29 July 2025
# by the team!

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
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# Load data, discard some experiments following various decision rules
source('studyDesign/decisionRules_abundant0s.R')

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

modeld <- modeld[, c('datasetID', 'study', 'genusspecies', 'responseValueNum', 'warmStratDur', 'coldStratDur', 'germTempGen', 'germDuration')]
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
phylo2 <- keep.tip(phylo, spp)
cphy <- vcv.phylo(phylo2,corr=TRUE)

# Prepare data for Stan
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
                 
                 ws_degen = array(modeld$warmStratDur[modeld$responseValueNum %in% c(0,1)],
                                  dim = sum(modeld$responseValueNum%in% c(0,1))),
                 ws_prop = array(modeld$warmStratDur[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                 dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)),
                 
                 Vphy = cphy)

smordbeta <- stan_model("stan/orderedbetalikelihood_4slopes.stan")
fit <- sampling(smordbeta, mdl.data, 
                iter = 4000, warmup = 3000,
                chains = 4)
