#=== === === === === === === === === === === === === === === === === === === ===
# Created by Dan July 2024 to add chilling and warm strat information
# and get data all ready for model fitting


# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

# libraries
library(dplyr)
library(chillR)
library(stringr)
library(ggplot2)

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
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 


# read file
d <- read.csv("..//output/egretclean.csv")

#Create warm strat column
d$warmstrat <- NA
# vector for all treatments that have warm stratification
warmstrat.names <- unique(d$treatment[grep("warm", d$treatment)])
# remove entry that shouldn't be there
warmstrat.names[!warmstrat.names %in% c("cold strat + soak in warm water")]
# add a 1 in warmstrat column whenever "warm" appepeared in the treatment column
d$warmstrat[which(d$treatment %in% warmstrat.names)] <- 1


#####add chilling units
source("analyseSeedCues/source/addChill.R") ### add chill

####deal with exta treatments
#source("analyseSeedCues/source/dropExtraTreats.R")# as of 31/07/24 not functional
