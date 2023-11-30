## Started 29 November 2023 ##
## By Lizzie & edited by Deirdre

## The first EGRET retreat rocks on! Now we tackle respvar ##
## Goal here is a sense of which variables we have a lot of useful data on ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(stringr)
library(plyr)
library(ggplot2)
library(viridis)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret")
} else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/others/deirdre/Synchrony")
} else{
  setwd("/Users/Lizzie/Documents/git/projects/egret/analyses") # for Lizzie
}

# Grab de data!
d <- read.csv("analyses/output/egretData.csv")
d$latbi <- paste(d$genus, d$species)
d$datasetIDstudy <- paste(d$datasetID, d$study)

## Let's get a df of things we especially care about 
dsubset <- subset(d, select=c("datasetID", "study","datasetIDstudy", "genus" , "species", "latbi", 
	"chill.temp", "chill.duration" , "germ.temp", "respvar", "response"))
dsm <- dsubset[!duplicated(dsubset), ]

## summarizing data
summaryhere <- data.frame(respvar=character(), ndatasets=numeric(), nspecies=numeric(), nchilldurations=numeric())
for(i in unique(dsm$respvar)){
	subby <- subset(dsm, respvar==i)
	summaryhereadd <- data.frame(respvar=i, 
		ndatasets=length(unique(subby$datasetIDstudy)), 
		nspecies=length(unique(subby$latbi)), 
		nchilldurations=length(unique(subby$chill.duration)))
	summaryhere <- rbind(summaryhere, summaryhereadd)
}

summaryhere <- summaryhere[with(summaryhere, order(-ndatasets, -nspecies, -nchilldurations)), ]

##### Cleaning resp var ##########

dsubset$respclean <- dsubset$respvar


dsubset$respclean[which(dsubset$respclean == "germ.speed")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "germ speed")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "germ.rt")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "rates.germ")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "germ.rate (days)")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "germ rate (days)")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "rate of seed germ")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "germ rate")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "germination rate")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "germ.rate.total")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "germ.rate.germinating.only")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "germ.speed(%/day)")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "germ time (days)")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "average germination speed")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "rates.germ")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "rate of seed germ")] <- "germ.rate"
dsubset$respclean[which(dsubset$respclean == "mean.germ.rate")] <- "germ.rate"

dsubset$respclean[which(dsubset$respclean == "germ spread (10% to 90% germ)")] <- "T90-T10"

dsubset$respclean[which(dsubset$respclean == "germ.proportion")] <- "prop.germ"
dsubset$respclean[which(dsubset$respclean == "germ.proportion")] <- "prop.germ"


dsubset$respclean[which(dsubset$respclean == "mtg")] <- "mgt"
dsubset$respclean[which(dsubset$respclean == "mean.germ.time")] <- "mgt"
dsubset$respclean[which(dsubset$respclean == "MGT")] <- "mgt"
dsubset$respclean[which(dsubset$respclean == "MTG")] <- "mgt"
dsubset$respclean[which(dsubset$respclean == "M.T.G")] <- "mgt"


dsubset$respclean[which(dsubset$respclean == "half time (days)")] <- "T50"
dsubset$respclean[which(dsubset$respclean == "time taken to 50% germ")] <- "T50"
dsubset$respclean[which(dsubset$respclean == "50% germ")] <- "T50"
dsubset$respclean[which(dsubset$respclean == "days to 50% emergence")] <- "T50"
dsubset$respclean[which(dsubset$respclean == "half time (days)")] <- "T50"


dsubset$respclean[which(dsubset$respclean == "germ index")] <- "germ.index"
dsubset$respclean[which(dsubset$respclean == "mean germ index")] <- "germ.index"


dsubset$respclean[which(dsubset$respclean == "per.germ.cumulative")] <- "per.germ"
dsubset$respclean[which(dsubset$respclean == "per.germ.cumulative.cumulative")] <- "per.germ"
dsubset$respclean[which(dsubset$respclean == "mean per germ")] <- "per.germ"


dsubset$respclean[which(dsubset$respclean == "days to 1st germination")] <- "days to first germination"
dsubset$respclean[which(dsubset$respclean == "days of first observation of seedling emergence")] <- "days to first germination"
dsubset$respclean[which(dsubset$respclean == "days to 1st seed germinated")] <- "days to first germination"
dsubset$respclean[which(dsubset$respclean == "days taken until first seed germination")] <- "days to first germination"


lookUp <- c("days to final germination", 
            "AUGPC", "germination (arcsin)", 
            "germ.rate ((l/t50)", 
            "meandaystogerm", 
            "peak.value", 
            "LAG","normal seedling percentage", 
            "mean time (day)", 
            "number per.seedlings", 
            "IVG", "med.time.germ", 
            "GSI",
            "imbi.period",
            "germ.period", 
            "speed.emerge",
            "per.germ.runningwater", 
            "per.germ.petri")

# resp var we are not interested in 
exclude <- c("adventitious.root.diameter", 
             "adventitious.root.length", 
             "amylase.concentration",
             "amylase.unit.activity",
             "amylase.specific.activity",
             "RNA.content",
             "cotyledon.area",
             "cotyledon.length", 
             "cotyledon.width",
             "hypocotyl.ave.diameter", 
             "hypocotyl.length",
             "primary.root.diameter",
             "primary.root.length",
             "respiratory.rate",
             "moisture content",
             "oxygen.absorbance",
             "growth.height.cm",
             "growth.collar.diameter.cm",
             "seed.moisture.content.fresh.weight",
             "embryo/seed ratio",
             "GA3_embryo (μg/g)",
             "ABA_embryo (μg/g)",
             "GA/ABA_embryo",
             "GA3_endosperm (μg/g)",
             "ABA_endosperm (μg/g)",
             "GA/ABA_endosperm",
             "base temperature",
             "thermal time",
             "moisture",
             "survival",
             "shoot.height(cm)",
             "shoot.diam(mm)",
             "shoot.dryweight(g)",
             "root.dryweight(g)",
             "num.shoot",
             "shootroot.ratio",
             "water.absorption",
             "O2.uptake",
             "CO2.evolution",
             "ethanol.formation",
             "L.sativa.per.germ",
             "weight.gram",
             "per.weight.gain",
             "per.increase.seedmass",
             "growth.rate (cm days^-1)")

length(unique(d$datasetID))
# data with cureves have 61 respvars:
# [1] "50% germ"                       "a"                              "a.max"                          "a.min"                         
# [5] "adventitious.root.diameter"     "adventitious.root.length"       "amylase.concentration"          "b"                             
# [9] "b.max"                          "b.min"                          "c"                              "c.max"                         
# [13] "c.min"                          "cotyledon.area"                 "cotyledon.length"               "cotyledon.width"               
# [17] "D_lag-50"                       "D_lag-50.max"                   "D_lag-50.min"                   "days to 1st germination"       
# [21] "days to final germination"      "early per.germ"                 "embryo/seed ratio"              "final per.germ"                
# [25] "G.E.I"                          "germ.cap"                       "germ.rate"                      "germ.rt"                       
# [29] "germ.speed"                     "germ.time"                      "germ.val"                       "half time (days)"              
# [33] "hypocotyl.ave.diameter"         "hypocotyl.length"               "lag"                            "lag.max"                       
# [37] "lag.min"                        "mean per.germ"                  "mean.daily.germ"                "mean.germ.rate"                
# [41] "mgt"                            "mtg"                            "MTG"                            "NA"                            
# [45] "number per.seedlings"           "per.germ"                       "per.germ.cumulative"            "per.germ.cumulative.cumulative"
# [49] "per.germ.energy"                "per.ungerminated.fresh"         "per.weight.gain"                "primary.root.diameter"         
# [53] "primary.root.length"            "rate of seed germ"              "T50"                            "time taken to 50% germ"        
# [57] "TMGR"                           "TMGR.max"                       "TMGR.min"                       "vigour index (days)"           
# [61] "weight.gram"           