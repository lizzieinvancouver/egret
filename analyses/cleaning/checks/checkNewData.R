# started August 27, 2024
# aim of this code is to plot newly scraped data and check that it now includes all data and treatments needed to fit curves.
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("~/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("frederik", getwd())) > 0){
  setwd("/Users/frederik/github/egret/analyses")
}

d <- read.csv("..//data/scrapedEgret/egret_SS.csv")

# were all the right papers/fig scraped?

d$studyFig <- paste(d$datasetID, d$figure, sep = "_")
sort(unique(d$studyFig))

# create the mega label that we want to keep data by: taken from germResponseVar.R lines 25-28
d$germDuration <- as.numeric(d$germDuration)
d$latbi <- paste(d$genus, d$species, sep = "_")
d$trt <- paste(d$chillDuration, d$chillTemp, d$germTemp, d$chemicalCor, d$scarifType, sep = "_")
d$keep <- paste(d$datasetIDstudy, d$latbi, d$trt, d$provenance.lat, d$provenance.long, d$other.treatment, d$photoperiod, d$figure)

# write a loop that saves a figure for each unique study to see that curves are correct
