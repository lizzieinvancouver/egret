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

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Run model with forcing, no phylogeny #### 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if(runmodels){

smordbeta_nophy <- stan_model("stan/provenance/orderedbetalikelihood_3slopes_provenance_nophylo.stan")
fit_nophy <- sampling(smordbeta_nophy, mdl.data,
                iter = 600, warmup = 200, chains = 4)

# smordbeta_nophy_noprov <- stan_model("stan/provenance/orderedbetalikelihood_3slopes_noprovenance_nophylo.stan")
# fit_nophy_noprov <- sampling(smordbeta_nophy_noprov, mdl.data,
#                       iter = 2024, warmup = 1000,
#                       chains = 4)

# Diagnostics #### 
# read model on christophe's computer
# saveRDS(fit_nophy, "/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy.rds")
smordbeta_nophy <- readRDS("/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy.rds")

diagnostics <- util$extract_hmc_diagnostics(smordbeta_nophy)
util$check_all_hmc_diagnostics(diagnostics)
samples <- util$extract_expectand_vals(smordbeta_nophy)

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

#  
pdf("analyseSeedCues/provenance/figures/retrodictiveChecks/provperspp.pdf",
    width = 8, height = 6)

# par(mfrow=c(2, 2), mar = c(4,4,4,1), cex.main = 0.8)
# mdl.data$prov_prop[mdl.data$sp_prop == sp]
# modeld[modeld$numspp == 2,]

# levels(factor(modeld$provLatLonAlt))[c(37, 43)]
str(mdl.data)

# get a vector of ALL species, both in prop and degen
all_sp <- unique(c(mdl.data$sp_prop, mdl.data$sp_degen))

for(sp in all_sp) { # sp = 4
  provs <- unique(c(mdl.data$prov_prop[mdl.data$sp_prop == sp],
                    mdl.data$prov_degen[mdl.data$sp_degen == sp]))
  # calculate the number of pannels for each species per page
  nprov <- length(provs)
  ncols <- 3
  nrows <- ceiling(nprov / ncols)
  
  # so we have 1 species per page
  par(mfrow=c(nrows, ncols), mar=c(4,4,4,1), cex.main=0.8)
  
  for(p in provs) { # p = provs
    idx_prop  <- which(mdl.data$sp_prop  == sp & mdl.data$prov_prop  == p)
    idx_degen <- which(mdl.data$sp_degen == sp & mdl.data$prov_degen == p)
    
    names <- unlist(c(
      sapply(idx_prop,  function(n) paste0('y_prop_gen[',  n, ']')),
      sapply(idx_degen, function(n) paste0('y_degen_gen[', n, ']'))))
    
    cs <- c(mdl.data$cs_prop[idx_prop], mdl.data$cs_degen[idx_degen])
    orderx <- order(cs)
    names  <- names[orderx]
    cs     <- cs[orderx]
    
    util$plot_conn_pushforward_quantiles(
      samples, names, plot_xs = cs,
      main = paste0(levels(factor(modeld$genusspecies))[sp], ", Prov: ", 
                    unique(modeld$provLatLonAlt[modeld$numprov %in% p])),
      xlab = 'Chilling (scaled)',
      ylab = 'Germ. perc. (marginal quantiles)',
      display_xlim = c(-0.5, 1.1), display_ylim = c(0, 1))
    
    y <- c(mdl.data$y_prop[idx_prop], mdl.data$y_degen[idx_degen])
    y <- y[orderx]
    points(cs, y, pch=16, cex=1.2, col="white")
    points(cs, y, pch=16, cex=0.8, col="black")
  }
}
dev.off()

unique(modeld$genusspecies[modeld$numspp %in% mdl.data$sp_degen])

sum(modeld$responseValueNum[modeld$numspp %in% mdl.data$sp_degen] == 0 )
sum(modeld$responseValueNum[modeld$numspp %in% mdl.data$sp_degen] == 1 )

# diagnose what's going on
modeld[modeld$genusspecies %in% "Eucalyptus_delegatensis", ]
unique(modeld$provLatLonAlt[modeld$genusspecies %in% "Eucalyptus_delegatensis"])
modeld$provLatLonAlt[modeld$genusspecies %in% "Eucalyptus_delegatensis"]
modeld$provLatLonAlt[modeld$genusspecies %in% "Eucalyptus_delegatensis"]
unique(modeld$germTempGen[modeld$genusspecies %in% "Eucalyptus_delegatensis"])

unique(modeld$germDuration[modeld$genusspecies %in% "Eucalyptus_delegatensis"])

modeld[modeld$numprov %in% provs, ]
modeld[modeld$numspp %in% sp, ]

# checks
modeld[modeld$genusspecies == "Penstemon_fremontii",
       c("provLatLonAlt", "numprov", "responseValueNum")]

# 2. what position are those strings at in the global factor?
which(levels(factor(modeld$provLatLonAlt)) %in% 
        unique(modeld$provLatLonAlt[modeld$genusspecies == "Penstemon_fremontii"]))


# 3. what does mdl.data think fremontii's provenance indices are?
sp <- which(levels(factor(modeld$genusspecies)) == "Penstemon_fremontii")
mdl.data$prov_prop[mdl.data$sp_prop == sp]
mdl.data$prov_degen[mdl.data$sp_degen == sp]

# 4. what do those indices resolve to in the global factor?
levels(factor(modeld$provLatLonAlt))[mdl.data$prov_prop[mdl.data$sp_prop == sp]]
levels(factor(modeld$provLatLonAlt))[mdl.data$prov_degen[mdl.data$sp_degen == sp]]


# what species indices exist in each subset?
unique(mdl.data$sp_prop)
unique(mdl.data$sp_degen)

# and what species names do those indices correspond to?
levels(factor(modeld$genusspecies))[mdl.data$sp_prop %in% sp]
levels(factor(modeld$genusspecies))[unique(mdl.data$sp_degen)]

sp <- 7
provs <- c(mdl.data$prov_prop[which(mdl.data$sp_prop == sp)], mdl.data$prov_degen[which(mdl.data$sp_degen == sp)])
par(mfrow=c(2, 2), mar = c(4,4,4,1), cex.main = 1.1)
for(p in unique(provs)) {
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
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Drop forcing ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
modeld_noforc <- newd

nrow(modeld_noforc)
nrow(modeld_noforc)- nrow(modeld)

setdiff(modeld_noforc$genusspecies, modeld$genusspecies)

modeld_noforc_nogymno <- subset(modeld_noforc, !genusspecies %in% c("Juniperus_communis", "Tsuga_heterophylla"))

nrow(modeld_noforc_nogymno) - nrow(modeld)

# forcing 
nrow(newd[is.na(newd$germTempGen),])
nrow(modeld[is.na(modeld$germDuration),])

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

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit models ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if (runmodels) {

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---  
##### without forcing full data #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Prepare data for Stan
modeld_noforc2$numspp <-  match(modeld_noforc2$genusspecies, unique(modeld_noforc2$genusspecies))
modeld_noforc2$numprov = as.integer(factor(modeld_noforc2$provLatLonAlt))

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
                        iter = 2024, warmup = 1000,
                        chains = 4)
# saveRDS(fit_nophy_noforcing, "/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy_noforcing.rds")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---  
##### without forcing restricted data #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
mdl.data2 <- list(N_degen = sum(modeld$responseValueNum %in% c(0,1)),
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
                 
                 cs_degen = array(modeld$coldStratDur[modeld$responseValueNum %in% c(0,1)],
                                  dim = sum(modeld$responseValueNum%in% c(0,1))),
                 cs_prop = array(modeld$coldStratDur[modeld$responseValueNum>0 & modeld$responseValueNum<1],
                                 dim = sum(modeld$responseValueNum>0 & modeld$responseValueNum<1)))

fit_nophy_noforcingRestric <- sampling(smordbeta_nophy, mdl.data2,
                                iter = 2024, warmup = 1000,
                                chains = 4)
# saveRDS(fit_nophy_noforcingRestric, "/Users/christophe_rouleau-desrochers/Desktop/UBC/egretLOCAL/fit_nophy_noforcingRestr.rds")
}


