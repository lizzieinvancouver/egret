# Started Feb 2026
# by the team!

# Messy script, used during model development

library(stringr)
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
  setwd('~/projects/egret/analyses/modeling')
  util <- new.env()
  source('mcmc_analysis_tools_rstan.R', local=util)
  source('mcmc_visualization_tools.R', local=util)
  setwd('~/projects/egret/analyses')
} 

# Load data, discard some experiments following various decision rules
source('studyDesign/decisionRules_numberofseeds.R')
# source('studyDesign/decisionRules_abundant0s_Deirdre.R')


# # Process data 
# # (1) - removing rows where we do not have any info on forcing) 
modeld <- newd[!is.na(newd$germDuration) & !is.na(newd$germTempGen) & newd$germDuration != 'unknown' & newd$germTempGen != "ambient",] 
# (2) - separating warm and cold strat. durations
modeld$warmStratDur <- as.numeric(sapply(1:nrow(modeld), function(i){
  seq <-  unlist(stringr::str_split(modeld$stratSequence_condensed[i], ' then '))
  temp <-  unlist(stringr::str_split(modeld$stratDur_condensed[i], ' then '))
  id <- which(seq == 'warm')
  return(ifelse(is.null(id), NA, temp[id]))
}))
modeld$warmStratTemp <- as.numeric(sapply(1:nrow(modeld), function(i){
  seq <-  unlist(stringr::str_split(modeld$stratSequence_condensed[i], ' then '))
  dur <-  unlist(stringr::str_split(modeld$stratDur_condensed[i], ' then '))
  id <- which(seq == 'warm')
  temp <-  unlist(stringr::str_split(modeld$stratTemp_condensed[i], ' then '))
  return(ifelse(is.null(id), NA, temp[id]))
}))
modeld$coldStratDur <- as.numeric(sapply(1:nrow(modeld), function(i){
  seq <-  unlist(stringr::str_split(modeld$stratSequence_condensed[i], ' then '))
  temp <-  unlist(stringr::str_split(modeld$stratDur_condensed[i], ' then '))
  id <- which(seq == 'cold')
  return(ifelse(is.null(id), NA, temp[id]))
}))
modeld$coldStratTemp <- as.numeric(sapply(1:nrow(modeld), function(i){
  seq <-  unlist(stringr::str_split(modeld$stratSequence_condensed[i], ' then '))
  dur <-  unlist(stringr::str_split(modeld$stratDur_condensed[i], ' then '))
  id <- which(seq == 'cold')
  temp <-  unlist(stringr::str_split(modeld$stratTemp_condensed[i], ' then '))
  return(ifelse(is.null(id), NA, temp[id]))
}))


# (3) - assuming NA strat. mean 0
modeld$warmStratDur <- ifelse(is.na(modeld$warmStratDur) & !is.na(modeld$warmStratTemp), NA,
                              ifelse(is.na(modeld$warmStratDur) & is.na(modeld$warmStratTemp), 0, modeld$warmStratDur))
modeld$coldStratDur <- ifelse(is.na(modeld$coldStratDur) & !is.na(modeld$coldStratTemp), NA,
                              ifelse(is.na(modeld$coldStratDur) & is.na(modeld$coldStratTemp), 0, modeld$coldStratDur))
modeld$warmStratTemp <- ifelse(modeld$warmStratDur == 0, 0, modeld$warmStratTemp)
modeld$coldStratTemp <- ifelse(modeld$coldStratDur == 0, 0, modeld$coldStratTemp)



# (4) - removing species not present in the phylo tree
# modeld$genusspecies <- sapply(modeld$genusspecies, function(i) stringr::str_split_i(i, ' ', 1))
# # test <- modeld[!(modeld$genusspecies %in% phylo$tip.label), ]
# # unique(test$genusspecies) # check only Gymno!
# modeld <- modeld[(modeld$genusspecies %in% phylo$tip.label), ]
# (4) - transform response to proportion and germ. covariates to numeric
modeld$responseValueNum <- as.numeric(modeld$responseValueNum)/100
modeld$germDuration <- as.numeric(modeld$germDuration)
modeld$germTempGen <- as.numeric(modeld$germTempGen)
# temporary - need to check whether odd values (>>> scrapping uncertainty) have been corrected
# modeld[modeld$responseValueNum < 1.05,] # not needed anymore!
# (5) - transform values a bit above or below 0 (due to scrapping uncertainty)
# modeld$responseValueNum <- ifelse(modeld$responseValueNum > 1, 1, modeld$responseValueNum)
# modeld$responseValueNum <- ifelse(modeld$responseValueNum < 0, 0, modeld$responseValueNum)
modeld$germDuration <- ifelse(modeld$germDuration < 0, 0, modeld$germDuration)

modeld <- modeld[, c('datasetID', 'study', 'genusspecies', 'variety', 'provLatLonAlt', 'treatment', 'other.treatment', 'other.treatment.details', 'responseValueNum', 'nseeds', 'nseeds_total', 
                     'warmStratTemp', 'warmStratDur', 'chillTemp', 'coldStratTemp', 'coldStratDur', 'germTemp', 'germTempGen', 'germDuration')]
modeld <- modeld[complete.cases(modeld[ , setdiff(names(modeld), c('variety',"provLatLonAlt", 'chillTemp', 'treatment', 'other.treatment', 'other.treatment.details'))]), ]


# # Removing potential duplicates
modeld_wodup <- modeld[!duplicated(modeld),]
message(paste0("Removing ", nrow(modeld)-nrow(modeld_wodup), ' potential duplicates!'))# 137 rows
# Other test for duplicate removal
# modeld$responseValueRounded <- round(modeld$responseValueNum,3) # rounded to 3 digits, ie percentage with 1 digits (data scraping uncertainty...?)
# modeld_wodup <- modeld[!duplicated(modeld[c('datasetID', 'study', 'genusspecies', 'responseValueRounded', 'warmStratDur', 'coldStratDur', 'germTempGen', 'germDuration')]),]
# nrow(modeld)-nrow(modeld_wodup) # 14() when responseValue rounded to 3 digits (XX.X%)
modeld <- modeld_wodup
rm(modeld_wodup)

modeld$uniqueID <- paste(modeld$datasetID, modeld$study, modeld$genusspecies, modeld$provLatLonAlt,
                         modeld$other.treatment.details, 
                         modeld$warmStratTemp, modeld$warmStratDur, 
                         modeld$coldStratTemp, modeld$coldStratDur, modeld$germTemp,
                         sep = '_')

misctreatments_tokeep <- c("brown/black seeds", "light/dark seeds")
modeld$uniqueID <- ifelse(modeld$other.treatment %in% misctreatments_tokeep, 
                          paste(modeld$uniqueID, modeld$other.treatment.details, sep = '_'),
                          paste(modeld$uniqueID, NA, sep = '_'))

# al-absi10 has two different controls
modeld[modeld$datasetID == 'al-absi10', 'uniqueID'] <- paste(modeld[modeld$datasetID == 'al-absi10', 'uniqueID'],
                                                             modeld[modeld$datasetID == 'al-absi10', 'treatment'], sep = '_')

# ren15 has a subspecies
modeld[modeld$datasetID == 'ren15' & modeld$variety %in% "subsp. albosinensis", 'uniqueID'] <- paste(modeld[modeld$datasetID == 'ren15' & modeld$variety %in% "subsp. albosinensis", 'uniqueID'], 
                                                                                                     'subsp', sep = '_')

# wang09 alternate chilling temp, but the average is the same
modeld[modeld$datasetID %in% 'wang09' & modeld$study %in% 'exp1' & modeld$chillTemp %in% '15 and 5', 'uniqueID'] <-
  paste(modeld[modeld$datasetID %in% 'wang09' & modeld$study %in% 'exp1' & modeld$chillTemp %in% '15 and 5', 'uniqueID'], 
        "alternate", sep = '_')

# liu13 has a 0% that should not be 0%
modeld <- modeld[!(modeld$datasetID %in% 'liu13' & modeld$study %in% 'exp2' & modeld$germDuration %in% 21 & modeld$nseeds == 0), ]


modeld <- modeld[!(modeld$genusspecies == "Cytisus_scoparius"), ]



# Prepare data
uniq_exp_ids <- unique(modeld$uniqueID)
Nexps <- length(uniq_exp_ids)

source('analyseSeedCues/checkExperiments.R')


uniq_species_ids <- unique(modeld$genusspecies)
Nspecies <- length(uniq_species_ids)


modeld <- modeld[!(modeld$uniqueID %in% toremove), ]

uniq_exp_ids <- unique(modeld$uniqueID)
Nexps <- length(uniq_exp_ids)

uniq_species_ids <- unique(modeld$genusspecies)
Nspecies <- length(uniq_species_ids)

species_idxs <- c()
exp_start_idxs <- c()
exp_end_idxs <- c()

chill <- c()
forcing <- c()

seeds <- c()
d <- c()


idx <- 1
for(e in uniq_exp_ids){
  
  d_exp <- modeld[modeld$uniqueID %in% e,]
  d_exp <- unique(d_exp[,names(d_exp)[names(d_exp) != 'treatment']])
  
  Nobs_exp <- nrow(d_exp)+1 # +1 for non germinated seeds
  
  species_exp <- which(uniq_species_ids == d_exp$genusspecies[1])
  species_idxs <- c(species_idxs, species_exp)
  
  exp_start_idxs <- c(exp_start_idxs, idx)
  idx <- idx + Nobs_exp
  exp_end_idxs <- c(exp_end_idxs, idx - 1)
  
  ord <- order(d_exp$germDuration)
  
  d_exp$nseeds <- round(d_exp$nseeds,0)
  resp <- d_exp$nseeds[ord]
  resp <- apply_tol(resp, tolerance = ceiling(max(resp)*0.01)) # tolerance of +/- 1% of max.
  
  d <- c(d, c(0, round(d_exp$germDuration[ord],2)))
  
  nongermseeds <- d_exp$nseeds_total[1]-max(resp)
  
  # Max can be sometimes be a bit higher... likely rounding issues
  if(any(nongermseeds < 0)){
    
    if(abs(nongermseeds) > 0.01 * d_exp$nseeds_total[1]){ # more than 1%?
      stop()
    }
    
    resp[resp > d_exp$nseeds_total[1]] <- d_exp$nseeds_total[1]
    nongermseeds <- d_exp$nseeds_total[1]-max(resp)
  }
  
  germseeds  <- resp # cumulative
  if(Nobs_exp > 2){
    germseeds <- germseeds - c(0, germseeds[1:(length(germseeds)-1)])
  }
  
  if(any(germseeds < 0)){stop('Negative counts!')}
  
  
  seeds <- c(seeds, c(nongermseeds, germseeds))
  
  if(length(c(nongermseeds, germseeds)) != length(c(0, d_exp$germDuration[ord]))){stop()}
  
  if(length(unique(d_exp$coldStratDur)) > 1){stop()}
  chill <- c(chill, unique(d_exp$coldStratDur))
  
  if(length(unique(d_exp$germTempGen)) > 1){stop()}
  forcing <- c(forcing, unique(d_exp$germTempGen))
  
}

N <- length(seeds)

# Prepare data for Stan
mdl.data <- list(
  N = N,
  Nexps = Nexps, 
  Nspecies = Nspecies, 
  species_idxs = species_idxs, 
  exp_start_idxs = exp_start_idxs,
  exp_end_idxs = exp_end_idxs,
  seeds = round(seeds, 0),
  chill = chill/7, #in weeks
  forcing = forcing/10, # in x10 degC
  d = d,
  uniq_species_ids = uniq_species_ids
)
