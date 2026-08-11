# Started 11 August 2026 
# by Ken and Lizzie

# aim is look at some new storage/strat data .. .and try to run a model  maybe 

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
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# get the data!
d  <- read.csv("output/egretcleanmostoragedetrals.csv")

library(ggplot2)

sort(table(d$responseVar))
dperc <- d[which(d$responseVar=="percent.germ"),]

# storTempEst, storDurEst, stratTempEst, stratDurEst

# Hmm, okay, I would need to get this to plot across multiple pages to be useful...
ggplot(dperc[1:1000,], aes(y=responseValueNum, x=stratTempEst, color=storDurEst, group=datasetID)) +
  geom_point() +
  facet_wrap(.~datasetID)

ggplot(dperc, aes(y=storDurEst, x=stratTempEst, color=stratTempEst)) +
  geom_point() 

# checking ...
table(d$stratTempEst)
table(d$stratDurEst)
table(d$storTempEst)
table(d$storDurEst)

stordur <- d[which(is.na(d$storDurEst)==FALSE),] # 8261
stortemp <- d[which(is.na(d$storTempEst)==FALSE),] # 7879
stratdur <- d[which(is.na(d$stratDurEst)==FALSE),] # 16877
strattemp <- d[which(is.na(d$stratTempEst)==FALSE),] # 15647

everybody <- d[which(is.na(d$storDurEst)==FALSE & 
  is.na(d$storTempEst)==FALSE &
  is.na(d$stratDurEst)==FALSE &
  is.na(d$stratTempEst)==FALSE),] # 3868

table(everybody$responseVar)

ggplot(everybody, aes(y=responseValueNum, x=stratTempEst, color=storDurEst, group=datasetID)) +
  geom_point() +
  facet_wrap(.~datasetID)

ggplot(everybody, aes(y=storDurEst, x=storTempEst, color=stratDurEst)) +
  geom_point() 
ggplot(everybody, aes(y=stratDurEst, x=stratTempEst, color=storDurEst)) +
  geom_point() 
ggplot(everybody, aes(y=stratTempEst, x=storTempEst, color=datasetID)) +
  geom_point() 
