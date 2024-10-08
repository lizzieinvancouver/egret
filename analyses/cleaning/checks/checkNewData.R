# started August 27, 2024
# aim of this code is to plot newly scraped data and check that it now includes all data and treatments needed to fit curves.
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdre", getwd()) > 0)) {
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

s <- read.csv("..//data/scrapedEgret/egret_SS.csv")

# were all the right papers/fig scraped?

s$studyFig <- paste(s$datasetID, s$figure, sep = "_")
sort(unique(s$studyFig))

# create the mega label that we want to keep data by: taken from germResponseVar.R lines 25-28
s$response <- as.numeric(s$response) # two rows of NA Chen15

s$germDuration <- as.numeric(s$germ.duration)
s$latbi <- paste(s$genus, s$species, sep = "_")
s$trt <- paste(s$chill.duration, s$chill.temp, s$germ.temp, s$chemical.concent, s$scarif.type, sep = "_")
s$keep <- paste(s$datasetID, s$study, s$latbi, s$trt, s$provenance.lat, s$provenance.long, s$other.treatment, s$photoperiod, s$figure, sep = "_")
s$datasetID <- as.factor(s$datasetID)
s$keep <- as.factor(s$keep)
# write a loop that saves a figure for each unique study to see that curves are correct

#subset to datasets with percent germ
curve <- subset(s, respvar == "per.germ")

studyFigs <- unique(curve$studyFig)
study <- unique(curve$datasetID)

studyFigs <- unique(curve$studyFig)
j <- 9
#for (j in 1:length(unique(studyFigs))){
temp2 <- curve[which(curve$studyFig == studyFigs[j]),]

keeping <- unique(temp2$keep)

plot(temp2$response ~ temp2$germ.duration, type = "n", xlim = c(0,35), ylim = c(0,100))
for (i in 1:length(keeping)){

  temp3 <- temp2[which(temp2$keep == keeping[i]),]
  
# plot(temp3$response ~ temp3$germ.duration, type = "n", xlim = c(0,35), ylim = c(0,100))
points(temp3$response ~ temp3$germ.temp)
#points(temp3$response ~ temp3$germ.duration, type = "l")
}

s$datasetID <- tolower(s$datasetID)
ssData <- unique(s$datasetID)
s$keep <- paste(s$latbi, s$trt, s$provenance.lat, s$provenance.long, s$other.treatment, s$photoperiod, s$figure)

d$keep <- paste( d$latbi, d$trt, d$provenance.lat, d$provenance.long, d$other.treatment, d$photoperiod, d$figure)

tempS <- s[which(s$datasetID == ssData[4]),]; unique(tempS$keep)

tempD <- d[which(d$datasetID == ssData[9]),]; unique(tempD$keep)
