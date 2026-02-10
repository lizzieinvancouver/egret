# Started Feb 9 by Deirdre 

# Aim of this code is to prep the egret data for running in the USDA model

rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdre", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# packages
require(dplyr)

# cleaning egret data---will use the same decision rules as used to the abundant 0's model:
source('studyDesign/decisionRules_abundant0s.R')

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
# modeld <- modeld[(modeld$genusspecies %in% phylo$tip.label), ]
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
modeld <- subset(modeld, !is.na(chillDuration))
modeld <- subset(modeld, !is.na(germTempGen))

# mdlEgret <- modeld[, c('datasetID', 'study', 'genusspecies', 'responseValueNum', 'coldStratDur', 'germTempGen',"germDuration")]

modeld$keep <- paste(modeld$datasetID, modeld$study, modeld$genusspecies, 
                     modeld$coldStatDur, modeld$chillTemp, modeld$chillDuration, modeld$germTemp,
                     modeld$germPhotoperiod, modeld$scarifTypeSpe, modeld$provLatLonAlt,
                     sep = "_")

noPts <- aggregate(modeld[c("germDuration")], modeld[c("keep")], FUN = function(x)length(unique(x)))
noPts$germDuration <- as.numeric(noPts$germDuration)
timeSeries <- subset(noPts, germDuration > 1)

trtVar <- sort(unique(timeSeries$keep))
singlePerG <- vector()
for(i in 1:length(trtVar)){
  temp <- subset(modeld, keep == trtVar[i])
  singlePerG <- rbind(singlePerG,temp[which(temp$germDuration == max(temp$germDuration)),])
}
singlePerG


onePt <- modeld[!modeld$keep %in% trtVar, ]

egretData <- rbind(onePt, singlePerG)

#####################################################
# Prep the usda data using the same cleaning rules 

