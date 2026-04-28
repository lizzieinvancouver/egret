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



# Load data, discard some experiments following various decision rules
source('analyseSeedCues/provenance/decisionRules.R')
# source('studyDesign/decisionRules_abundant0s_Deirdre.R')

# load Mike's diagnostic tools
util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

runmodels <- FALSE

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Model WITH forcing ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
##### Prep data #####
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

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### How many rows with gymnosperms #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
modeldnogymn <- subset(modeld, genusspecies %in% c("Picea_glauca", "Picea_orientalis"))
nrow(modeld) - nrow(modeldnogymn)
nrow(modeldnogymn)
# I hate doing this, but I want to go swimmmmmiiiiing
modeld$warmStratDur <- scale(modeld$warmStratDur)[,1]
modeld$coldStratDur <- scale(modeld$coldStratDur)[,1]
modeld$germDuration <- scale(modeld$germDuration)[,1]
modeld$germTempGen <- scale(modeld$germTempGen)[,1]

# Prepare data for Stan
modeld$numspp  <- as.integer(factor(modeld$genusspecies))
modeld$numprov <- as.integer(factor(modeld$provLatLonAlt))

# trim the \t weird thingy
modeld$provLatLonAlt <- trimws(modeld$provLatLonAlt)

mdl.data <- list(N_degen = sum(modeld$responseValueNum %in% c(0,1)),
                 N_prop = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1),
                 Ndataset = length(unique(modeld$idstudy)),
                 numdat = as.integer(factor(modeld$idstudy)),
                 
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
                                 dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)))

# Posterior quantification
# smordbeta <- stan_model("stan/provenance/orderedbetalikelihood_3slopes_provenance.stan")
# fit <- sampling(smordbeta, mdl.data,
#                 iter = 2024, warmup = 1000,
#                 chains = 4)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Run model with forcing, no phylogeny #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
if(runmodels){

smordbeta_nophy <- stan_model("stan/provenance/orderedbetalikelihood_3slopes_provenance_nophylo.stan")
fit_nophy <- sampling(smordbeta_nophy, mdl.data,
                iter = 2000, warmup = 1000, chains = 4)
}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Diagnostics #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# read model on christophe's computer
# saveRDS(fit_nophy, "/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy.rds")
fit_nophy <- readRDS("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy.rds")

diagnostics <- util$extract_hmc_diagnostics(fit_nophy)
util$check_all_hmc_diagnostics(diagnostics)
samples <- util$extract_expectand_vals(fit_nophy)

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

# Retrodictive check histogram
par(mfrow=c(1, 1), mar = c(4,4,2,2))
names <- c(sapply(1:mdl.data$N_prop, function(n) paste0('y_prop_gen[',n,']')),
           sapply(1:mdl.data$N_degen, function(n) paste0('y_degen_gen[',n,']')))
preds <- samples[names]
names(preds) <- sapply(1:length(preds), function(n) paste0('y_gen[',n,']'))
pdf("analyseSeedCues/provenance/figures/retrodictiveChecks/hist.pdf",
    width = 8, height = 6)
util$plot_hist_quantiles(preds, 'y_gen', 0, 1, 0.1,
                         baseline_values=c(mdl.data$y_prop, mdl.data$y_degen), 
                         xlab="Germination perc.")
dev.off()

# Look at the sigmas distributions
par(mfrow=c(3, 2), mar = c(4,4,1,1))
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
# util$plot_expectand_pushforward(samples[['sigma_bf']], 20,
#                                 flim = c(0,6),
#                                 display_name="sigma_bf")
# util$plot_expectand_pushforward(samples[['sigma_bf_prov']], 20,
#                                 flim = c(0,6),
#                                 display_name="sigma_bf_prov")
util$plot_expectand_pushforward(samples[['sigma_bcs']], 20,
                                flim = c(0,6),
                                display_name="sigma_bcs")
util$plot_expectand_pushforward(samples[['sigma_bcs_prov']], 20,
                                flim = c(0,6),
                                display_name="sigma_bcs_prov")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Retrodictive checks: Chilling #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
pdf("analyseSeedCues/provenance/figures/retrodictiveChecks/provpersppChill.pdf",
    width = 8, height = 6)

# get unique identifiers for chilling, so we can plot time
all_labels <- paste0(
  c(mdl.data$sp_prop,  mdl.data$sp_degen),
  c(mdl.data$prov_prop, mdl.data$prov_degen),
  c(mdl.data$t_prop,  mdl.data$t_degen))

# give it an integer, as we do in the lists
shared_factor <- as.integer(as.factor(all_labels))

# index for prop so we have the correct location 
timeid_prop  <- shared_factor[seq_len(mdl.data$N_prop)]
# then from n_prop until the end
timeid_degen <- shared_factor[seq(mdl.data$N_prop + 1, mdl.data$N_prop + mdl.data$N_degen)]

all_timeids <- sort(unique(c(timeid_prop, timeid_degen)))

par(mfrow=c(3,3), mar=c(4,4,4,1), cex.main=0.8)

for(i in all_chillids) { # i = 2
  idx_prop  <- which(timeid_prop  == i)
  idx_degen <- which(timeid_degen == i)
  
  # recover sp and prov for the title
  sp <- unique(c(mdl.data$sp_prop[idx_prop],   mdl.data$sp_degen[idx_degen]))
  p  <- unique(c(mdl.data$prov_prop[idx_prop],  mdl.data$prov_degen[idx_degen]))
  t_val <- unique(c(mdl.data$t_prop[idx_prop], mdl.data$t_degen[idx_degen]))
  
  names <- unlist(c(
    sapply(idx_prop,  function(n) paste0('y_prop_gen[',  n, ']')),
    sapply(idx_degen, function(n) paste0('y_degen_gen[', n, ']'))))
  
  cs_vals  <- c(mdl.data$cs_prop[idx_prop],  mdl.data$cs_degen[idx_degen])
  y_vals  <- c(mdl.data$y_prop[idx_prop],  mdl.data$y_degen[idx_degen])
  orderx  <- order(cs_vals)
  names   <- names[orderx]
  cs_vals  <- cs_vals[orderx]
  y_vals  <- y_vals[orderx]
  
  util$plot_conn_pushforward_quantiles(
    samples, names, plot_xs = cs_vals,
    main = paste0(levels(factor(modeld$genusspecies))[sp],
                  ", Prov: ", unique(modeld$provLatLonAlt[modeld$numprov == p]),
                  ", CS: ", round(t_val, 2)),
    xlab = "Chilling (scaled)",
    ylab = "Germ. perc.",
    display_ylim = c(0, 1))
  
  points(cs_vals, y_vals, pch=16, cex=1.2, col="white")
  points(cs_vals, y_vals, pch=16, cex=0.8, col="black")
}

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Retrodictive checks: Time #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
pdf("analyseSeedCues/provenance/figures/retrodictiveChecks/provpersppTime.pdf",
    width = 8, height = 6)

# get unique identifiers for chilling, so we can plot time
all_labels <- paste0(
  c(mdl.data$sp_prop,  mdl.data$sp_degen),
  c(mdl.data$prov_prop, mdl.data$prov_degen),
  c(mdl.data$cs_prop,  mdl.data$cs_degen))
# give it an integer, as we do in the lists
shared_factor <- as.integer(as.factor(all_labels))

# index for prop so we have the correct location 
chillid_prop  <- shared_factor[seq_len(mdl.data$N_prop)]
# then from n_prop until the end
chillid_degen <- shared_factor[seq(mdl.data$N_prop + 1, mdl.data$N_prop + mdl.data$N_degen)]

all_chillids <- sort(unique(c(chillid_prop, chillid_degen)))

par(mfrow=c(3,3), mar=c(4,4,4,1), cex.main=0.8)

for(i in all_chillids) { # i = 2
  idx_prop  <- which(chillid_prop  == i)
  idx_degen <- which(chillid_degen == i)
  
  # recover sp and prov for the title
  sp <- unique(c(mdl.data$sp_prop[idx_prop],   mdl.data$sp_degen[idx_degen]))
  p  <- unique(c(mdl.data$prov_prop[idx_prop],  mdl.data$prov_degen[idx_degen]))
  cs_val <- unique(c(mdl.data$cs_prop[idx_prop], mdl.data$cs_degen[idx_degen]))
  
  names <- unlist(c(
    sapply(idx_prop,  function(n) paste0('y_prop_gen[',  n, ']')),
    sapply(idx_degen, function(n) paste0('y_degen_gen[', n, ']'))))
  
  t_vals  <- c(mdl.data$t_prop[idx_prop],  mdl.data$t_degen[idx_degen])
  y_vals  <- c(mdl.data$y_prop[idx_prop],  mdl.data$y_degen[idx_degen])
  orderx  <- order(t_vals)
  names   <- names[orderx]
  t_vals  <- t_vals[orderx]
  y_vals  <- y_vals[orderx]
  
  util$plot_conn_pushforward_quantiles(
    samples, names, plot_xs = t_vals,
    main = paste0(levels(factor(modeld$genusspecies))[sp],
                  ", Prov: ", unique(modeld$provLatLonAlt[modeld$numprov == p]),
                  ", CS: ", round(cs_val, 2)),
    xlab = "Time (scaled)",
    ylab = "Germ. perc.",
    display_ylim = c(0, 1))
  
  points(t_vals, y_vals, pch=16, cex=1.2, col="white")
  points(t_vals, y_vals, pch=16, cex=0.8, col="black")
}

dev.off()


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Retrodictive checks: Forcing #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Model WITHOUT forcing ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# some checks
modeld_noforc <- newd
nrow(modeld_noforc)
nrow(modeld_noforc)- nrow(modeld)
setdiff(modeld_noforc$genusspecies, modeld$genusspecies)

modeld_noforc_nogymno <- subset(modeld_noforc, !genusspecies %in% c("Juniperus_communis", "Tsuga_heterophylla"))

nrow(modeld_noforc_nogymno) - nrow(modeld)

# forcing 
nrow(newd[is.na(newd$germTempGen),])
nrow(modeld[is.na(modeld$germDuration),])

# Select data
# (2) - separating warm and cold strat. durations
modeld_noforc$warmStratDur <- as.numeric(sapply(1:nrow(modeld_noforc), function(i){
  seq <-  unlist(stringr::str_split(modeld_noforc$stratSequence_condensed[i], ' then '))
  temp <-  unlist(stringr::str_split(modeld_noforc$stratDur_condensed[i], ' then '))
  id <- which(seq == 'warm')
  return(ifelse(is.null(id), NA, temp[id]))
}))
modeld_noforc$coldStratDur <- as.numeric(sapply(1:nrow(modeld_noforc), function(i){
  seq <-  unlist(stringr::str_split(modeld_noforc$stratSequence_condensed[i], ' then '))
  temp <-  unlist(stringr::str_split(modeld_noforc$stratDur_condensed[i], ' then '))
  id <- which(seq == 'cold')
  return(ifelse(is.null(id), NA, temp[id]))
}))
# (3) - assuming NA strat. mean 0
modeld_noforc$warmStratDur <- ifelse(is.na(modeld_noforc$warmStratDur), 0, modeld_noforc$warmStratDur)
modeld_noforc$coldStratDur <- ifelse(is.na(modeld_noforc$coldStratDur), 0, modeld_noforc$coldStratDur)
# (4) - removing species not present in the phylo tree
modeld_noforc$genusspecies <- sapply(modeld_noforc$genusspecies, function(i) stringr::str_split_i(i, ' ', 1))

# (4) - transform response to proportion and germ. covariates to numeric
modeld_noforc$responseValueNum <- as.numeric(modeld_noforc$responseValueNum)/100
# modeld_noforc$germTempGen <- as.numeric(modeld_noforc$germTempGen)

# (5) - transform values a bit above or below 0 (due to scrapping uncertainty)
modeld_noforc$responseValueNum <- ifelse(modeld_noforc$responseValueNum > 1, 1, modeld_noforc$responseValueNum)
modeld_noforc$responseValueNum <- ifelse(modeld_noforc$responseValueNum < 0, 0, modeld_noforc$responseValueNum)

modeld_noforc <- modeld_noforc[, c('uniqueID','datasetID', 'study', 'genusspecies', 'provLatLonAlt', 'responseValueNum', 'warmStratDur', 'coldStratDur', 'germDuration')]
modeld_noforc2 <- na.omit(modeld_noforc) 

# Removing potential duplicates
modeld_noforc_wodup <- modeld_noforc2[!duplicated(modeld_noforc2),]
message(paste0("Removing ", nrow(modeld_noforc2)-nrow(modeld_noforc_wodup), ' potential duplicates!')) 

# Other test for duplicate removal
modeld_noforc2$responseValueRounded <- round(modeld_noforc2$responseValueNum,3) # rounded to 3 digits, ie percentage with 1 digits (data scraping uncertainty...?)
modeld_noforc_wodup <- modeld_noforc2[!duplicated(modeld_noforc2[c('datasetID', 'study', 'genusspecies', 'responseValueRounded', 'warmStratDur', 'coldStratDur', 'germDuration')]),]
nrow(modeld_noforc2)-nrow(modeld_noforc_wodup) # 14() when responseValue rounded to 3 digits (XX.X%)
modeld_noforc2 <- modeld_noforc_wodup 
rm(modeld_noforc_wodup)

# I hate doing this, but I want to go swimmmmmiiiiing
modeld_noforc2$warmStratDur <- scale(modeld_noforc2$warmStratDur)[,1]
modeld_noforc2$coldStratDur <- scale(modeld_noforc2$coldStratDur)[,1]
modeld_noforc2$germDuration <- as.numeric(modeld_noforc2$germDuration)
modeld_noforc2$germDuration <- scale(modeld_noforc2$germDuration)[,1]


# check which species I'm getting back when I don't drop forcing
nrow(modeld_noforc2) - nrow(modeld)
setdiff(modeld_noforc2$genusspecies, modeld$genusspecies)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# some checks to figure out where species get dropped out
length(unique(newd$genusspecies)) # 4 species lost when phylogeny gets dropped out because of forcing
length(unique(modeld_noforc2$genusspecies))

# which species
setdiff(unique(newd$genusspecies), unique(modeld_noforc2$genusspecies))
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
###### Prepare data for Stan ######
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Prepare data for Stan
modeld$numspp  <- as.integer(factor(modeld$genusspecies))
modeld$numprov <- as.integer(factor(modeld$provLatLonAlt))

# trim the \t weird thingy
modeld$provLatLonAlt <- trimws(modeld$provLatLonAlt)
mdl.data <- list(N_degen = sum(modeld_noforc2$responseValueNum %in% c(0,1)),
                 N_prop = sum(modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1),
                 
                 Nsp =  length(unique(modeld_noforc2$numspp)),
                 sp_degen = array(modeld_noforc2$numspp[modeld_noforc2$responseValueNum %in% c(0,1)],
                                  dim = sum(modeld_noforc2$responseValueNum%in% c(0,1))),
                 sp_prop = array(modeld_noforc2$numspp[modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1],
                                 dim = sum(modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1)),
                 
                 Nprov =  length(unique(modeld_noforc2$numprov)),
                 prov_degen = array(modeld_noforc2$numprov[modeld_noforc2$responseValueNum %in% c(0,1)],
                                    dim = sum(modeld_noforc2$responseValueNum%in% c(0,1))),
                 prov_prop = array(modeld_noforc2$numprov[modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1],
                                   dim = sum(modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1)),
                 
                 y_degen = array(modeld_noforc2$responseValueNum[modeld_noforc2$responseValueNum %in% c(0,1)],
                                 dim = sum(modeld_noforc2$responseValueNum%in% c(0,1))),
                 y_prop = array(modeld_noforc2$responseValueNum[modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1],
                                dim = sum(modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1)),
                 
                 t_degen = array(modeld_noforc2$germDuration[modeld_noforc2$responseValueNum %in% c(0,1)],
                                 dim = sum(modeld_noforc2$responseValueNum%in% c(0,1))),
                 t_prop = array(modeld_noforc2$germDuration[modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1],
                                dim = sum(modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1)),
                 
                 cs_degen = array(modeld_noforc2$coldStratDur[modeld_noforc2$responseValueNum %in% c(0,1)],
                                  dim = sum(modeld_noforc2$responseValueNum%in% c(0,1))),
                 cs_prop = array(modeld_noforc2$coldStratDur[modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1],
                                 dim = sum(modeld_noforc2$responseValueNum>0 & modeld_noforc2$responseValueNum<1)))


smordbeta_nophy <- stan_model("stan/provenance/orderedbetalikelihood_3slopes_provenance_nophylo_noforcing.stan")
fit_nophy_noforcing <- sampling(smordbeta_nophy, mdl.data,
                        iter = 2024, warmup = 1000, chains = 4)
# saveRDS(fit_nophy_noforcing, "/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy_noforcing.rds")

}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Retrodictive checks: Chilling #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
pdf("analyseSeedCues/provenance/figures/retrodictiveChecks/provpersppChill.pdf",
    width = 8, height = 6)

# get unique identifiers for chilling, so we can plot time
all_labels <- paste0(
  c(mdl.data$sp_prop,  mdl.data$sp_degen),
  c(mdl.data$prov_prop, mdl.data$prov_degen),
  c(mdl.data$t_prop,  mdl.data$t_degen))

# give it an integer, as we do in the lists
shared_factor <- as.integer(as.factor(all_labels))

# index for prop so we have the correct location 
timeid_prop  <- shared_factor[seq_len(mdl.data$N_prop)]
# then from n_prop until the end
timeid_degen <- shared_factor[seq(mdl.data$N_prop + 1, mdl.data$N_prop + mdl.data$N_degen)]

all_timeids <- sort(unique(c(timeid_prop, timeid_degen)))

par(mfrow=c(3,3), mar=c(4,4,4,1), cex.main=0.8)

for(i in all_chillids) { # i = 2
  idx_prop  <- which(timeid_prop  == i)
  idx_degen <- which(timeid_degen == i)
  
  # recover sp and prov for the title
  sp <- unique(c(mdl.data$sp_prop[idx_prop],   mdl.data$sp_degen[idx_degen]))
  p  <- unique(c(mdl.data$prov_prop[idx_prop],  mdl.data$prov_degen[idx_degen]))
  t_val <- unique(c(mdl.data$t_prop[idx_prop], mdl.data$t_degen[idx_degen]))
  
  names <- unlist(c(
    sapply(idx_prop,  function(n) paste0('y_prop_gen[',  n, ']')),
    sapply(idx_degen, function(n) paste0('y_degen_gen[', n, ']'))))
  
  cs_vals  <- c(mdl.data$cs_prop[idx_prop],  mdl.data$cs_degen[idx_degen])
  y_vals  <- c(mdl.data$y_prop[idx_prop],  mdl.data$y_degen[idx_degen])
  orderx  <- order(cs_vals)
  names   <- names[orderx]
  cs_vals  <- cs_vals[orderx]
  y_vals  <- y_vals[orderx]
  
  util$plot_conn_pushforward_quantiles(
    samples, names, plot_xs = cs_vals,
    main = paste0(levels(factor(modeld$genusspecies))[sp],
                  ", Prov: ", unique(modeld$provLatLonAlt[modeld$numprov == p]),
                  ", CS: ", round(t_val, 2)),
    xlab = "Chilling (scaled)",
    ylab = "Germ. perc.",
    display_ylim = c(0, 1))
  
  points(cs_vals, y_vals, pch=16, cex=1.2, col="white")
  points(cs_vals, y_vals, pch=16, cex=0.8, col="black")
}

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Retrodictive checks: Time #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
pdf("analyseSeedCues/provenance/figures/retrodictiveChecks/provpersppTime.pdf",
    width = 8, height = 6)

# get unique identifiers for chilling, so we can plot time
all_labels <- paste0(
  c(mdl.data$sp_prop,  mdl.data$sp_degen),
  c(mdl.data$prov_prop, mdl.data$prov_degen),
  c(mdl.data$cs_prop,  mdl.data$cs_degen))
# give it an integer, as we do in the lists
shared_factor <- as.integer(as.factor(all_labels))

# index for prop so we have the correct location 
chillid_prop  <- shared_factor[seq_len(mdl.data$N_prop)]
# then from n_prop until the end
chillid_degen <- shared_factor[seq(mdl.data$N_prop + 1, mdl.data$N_prop + mdl.data$N_degen)]

all_chillids <- sort(unique(c(chillid_prop, chillid_degen)))

par(mfrow=c(3,3), mar=c(4,4,4,1), cex.main=0.8)

for(i in all_chillids) { # i = 2
  idx_prop  <- which(chillid_prop  == i)
  idx_degen <- which(chillid_degen == i)
  
  # recover sp and prov for the title
  sp <- unique(c(mdl.data$sp_prop[idx_prop],   mdl.data$sp_degen[idx_degen]))
  p  <- unique(c(mdl.data$prov_prop[idx_prop],  mdl.data$prov_degen[idx_degen]))
  cs_val <- unique(c(mdl.data$cs_prop[idx_prop], mdl.data$cs_degen[idx_degen]))
  
  names <- unlist(c(
    sapply(idx_prop,  function(n) paste0('y_prop_gen[',  n, ']')),
    sapply(idx_degen, function(n) paste0('y_degen_gen[', n, ']'))))
  
  t_vals  <- c(mdl.data$t_prop[idx_prop],  mdl.data$t_degen[idx_degen])
  y_vals  <- c(mdl.data$y_prop[idx_prop],  mdl.data$y_degen[idx_degen])
  orderx  <- order(t_vals)
  names   <- names[orderx]
  t_vals  <- t_vals[orderx]
  y_vals  <- y_vals[orderx]
  
  util$plot_conn_pushforward_quantiles(
    samples, names, plot_xs = t_vals,
    main = paste0(levels(factor(modeld$genusspecies))[sp],
                  ", Prov: ", unique(modeld$provLatLonAlt[modeld$numprov == p]),
                  ", CS: ", round(cs_val, 2)),
    xlab = "Time (scaled)",
    ylab = "Germ. perc.",
    display_ylim = c(0, 1))
  
  points(t_vals, y_vals, pch=16, cex=1.2, col="white")
  points(t_vals, y_vals, pch=16, cex=0.8, col="black")
}

dev.off()



