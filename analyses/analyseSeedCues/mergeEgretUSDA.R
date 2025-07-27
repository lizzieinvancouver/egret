# started Feb 13, 2025 by Deirdre
# aim of this code is to merge the egret and the usda data together:
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

require(reshape2)

u <- read.csv("output/usdaGerminationCleaned.csv")
d <- read.csv("output/egretclean.csv")

u$avgGerminationPercen

rmCol <- c("rowID", "pdfPageNumber", "pdfTableNumber", 
           "seedType", "medium", "samplesMin", "samplesMax", # only one instance where there was an actual range in sample so removing these
            "scrapedTableNumber","pdfTableNumber", "medium", "pretreatment", 
           "pretreatmentDuration", "pretreatmentHotWaterTemp")

uTemp <- u[!u %in% rmCol, ]
uTemp <- u[,c(4:8,10, 12:26, 28)]

uTemp <- u[,c("latbi", "coldStratDurMin", "coldStratDurMax", "germDurationMin", "germDurationMax","tempDayMin","tempDayMax", "tempNightMin","tempNightMax", "photoperiodMin", "photoperiodMax","responseValueMin","responseValueMin" )]
uTemp$germTemp <- uTemp$tempDayMin
testy <- melt(uTemp, id=c("latbi","responseVar","responseValue"))

# germDuration, chillDuration, and day/night temp


