# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# USDA MANUAL GERMINATION CLEANING FINAL ROUND
# Started by Justin
# 7 JULY 2024
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#library(tidyverse)
#d <- read_csv("output/usdaGerminationJINJJA.csv")

if(FALSE){
# Changing column names to better fit EGRET
colnames(d) <- c("speciesID","filePath","pdfPageNumber","scrapedTableNumber",
"pdfTableNumber","genus","species","seed.type","source.population","medium",
"pretreatment.duration","pretreatment.hotwater","pretreatment","stratification.temp",
"warm.stratification.duration","cold.stratification.duration","photoperiod",
"tempDay","tempNight","germ.duration","samples","genusID","pretreatmentFeces",
"chilling","chill.duraton","scarifType","scarifTypeGen","responseVar","responseValue",
"pretreatmentMin","pretreatmentMax","cold.strat.dur.Min","cold.strat.dur.Max",
"photoperiodMin","photoperiodMax","tempDayMin","tempDayMax","tempNightMin",
"tempNightMax","germ.dur.Min","germ.dur.Max","samplesMin","samplesMax","chill.dur.Min",
"chill.dur.Max","responseValueMin","responseValueMax","responseValueAvg","pretreatmentAvg",
"cold.strat.dur.Avg","photoperiodAvg","tempDayAvg","tempNightAvg","germ.dur.Avg",
"samplesAvg","chill.dur.Avg")}

# Changing the responseVar to better fit EGRET
colnames(d)[23]<-"responseVar"
unique(d$responseVar)
d$responseVar[which(d$responseVar == "germTime")] <- "mgt"
d$responseVar[which(d$responseVar == "germPercentTotal")] <- "percent.germ.total"
d$responseVar[which(d$responseVar == "germPercentAvg")] <- "percent.germ"
d$responseVar[which(d$responseVar == "germRate")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germEnergyPercentAvg")] <- "mean.percent.germ.energy"
d$responseVar[which(d$responseVar == "germEnergyPercent")] <- "percent.germ.energy"
d$responseVar[which(d$responseVar == "germCapacityAvg")] <- "mean.germ.capacity"
d$responseVar[which(d$responseVar == "germCapacity")] <- "germ.capacity"
d$responseVar[which(d$responseVar == "germPercent15degIncubated")] <- "percent.germ.15degC.incubated"
d$responseVar[which(d$responseVar == "germPercent20degIncubated")] <- "percent.germ.20degC.incubated"

#write.csv(d,"output/usdaGerminationData.csv")

