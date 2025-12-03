# Oct 29, 2025 started by D. Loughnan
# aim of this code is to just visually explore the USDA data

rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
graphics.off()

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses/")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
}

library(ggplot2)
library(viridisLite)
library(chillR)

d <- read.csv("output/usdaGerminationCleaned.csv")

osp<-read.csv("input/ospreeforegret.csv")
ospSp <- unique(osp$latbi)

d$datasetID <- paste(d$pdf_page_number, d$pdf_table_number, sep = "_")
# we want data on: chilling, germ temps, light

keep <- c("datasetID", "latbi", "cold_stratification_temp_C", "cold_stratification_days", "dailyl_light_hours", "photoperiodCor",
          "day_temp_celsius", "night_temp_celsius", "temp_unspecified_time_of_day_celsius", "test_duration_in_days",
          "pretreatmentChill", "pretreatmentChillDuration", "responseType", "responseVarClean", "responseValue")

min <- c("datasetID", "latbi", "photoperiodCor","chillDurationMin", "chillTempMin" , "responseType", "responseVarClean", "responseValueMin", "tempDayMin", "tempNightMin","unspecTempMin")

dMin <- d[,min]
dMin <- dMin[complete.cases(dMin$responseValueMin),]
dMin$minMax <- "min"
names(dMin) <- c("datasetID" ,"latbi","photoperiodCor","chillDuration", "chillTemp", "responseType", "responseVarClean", "responseValue", "tempDay", "tempNight", "unspecTemp","minMax")   

max <- c("datasetID", "latbi", "photoperiodCor","chillDurationMax", "chillTempMax" , "responseType", "responseVarClean", "responseValueMax", "tempDayMax", "tempNightMax","unspecTempMax")

dMax <- d[,max]
dMax <- dMax[complete.cases(dMax$responseValueMax),]
dMax$minMax <- "max"
names(dMax) <- c("datasetID" ,"latbi","photoperiodCor","chillDuration", "chillTemp", "responseType", "responseVarClean", "responseValue", "tempDay", "tempNight", "unspecTemp","minMax")   

dMaxMin <- rbind(dMax, dMin)

dMaxMin$tempDay[which(is.na(dMaxMin$tempDay))] <- dMaxMin$unspecTemp[is.na(dMaxMin$tempDay)]
# with max and min values: 1532 rows of data 426 species

dMaxMin <- dMaxMin[complete.cases(c(dMaxMin$chillDuration)),] 
# with only chill duration n = 733, nspp = 220
# with chilling data, if chillTemp included also always includes duration, n = 567, nspp = 179

# dMaxMin$tempDay[which(dMaxMin$tempDay == "20 and 10 then 20")] <- 16.67
# dMaxMin$tempDay <- as.numeric(dMaxMin$tempDay)
# 
# dMaxMin <- dMaxMin[complete.cases(c(dMaxMin$tempDay)),] 
# complete data for chilling and day temp: n = 446, 

# percent germination:
dPer <- subset(dMaxMin, responseVarClean == "percent.germ")
length(unique(dPer$latbi))
# n = 465; nspp = 218

## How much overlap do we have between USDA and OSPREE if just chill duration data?
dOspree <- dPer[dPer$latbi %in% ospSp,] 
length(unique(dOspree$latbi))
# n = 95; nspp = 36

plot(dPer$responseValue ~ dPer$chillTemp)
plot(dPer$responseValue ~ dPer$chillDuration)
plot(dPer$responseValue ~ dPer$tempDay)
plot(dPer$responseValue ~ dPer$chillTemp, bg = dPer$tempDay, pch = 21)
plot(dPer$chillTemp, dPer$tempDay, bg = dPer$chillDuration, pch = 21)


viridis_colors <- viridis(length(unique(dOspree$datasetID)))
dOspree$vCol <- viridis_colors[as.numeric(factor(dOspree$datasetID))]

pdf("analyseBudSeed/figures/ospreeSppChillDuration.pdf", width = 8, height = 5)
plot(dOspree$responseValue ~ dOspree$chillDuration, bg = dOspree$vCol, pch = 21, cex =1.5,
     main = "% germ with chill temp",
     xlab = "Chill Duration",
     ylab = "Percent germination")
dev.off()

## How much overlap do we have between USDA and OSPREE if just chill duration data AND germ temp?
dMaxMin$tempDay[which(dMaxMin$tempDay == "20 and 10 then 20")] <- 16.67
dMaxMin$tempDay <- as.numeric(dMaxMin$tempDay)

dMaxMinT <- dMaxMin[complete.cases(c(dMaxMin$tempDay)),]
# complete data for chilling and day temp: n = 446,

dPerCT <- subset(dMaxMinT, responseVarClean == "percent.germ")
length(unique(dPerCT$latbi))
# n = 325; nspp = 163

## How much overlap do we have between USDA and OSPREE if just chill duration data?
dOspreeCT <- dPerCT[dPerCT$latbi %in% ospSp,] 
length(unique(dOspreeCT$latbi))
# n = 77; nspp = 33

viridis_colors <- viridis(length(unique(dOspreeCT$datasetID)))
dOspreeCT$vCol <- viridis_colors[as.numeric(factor(dOspreeCT$datasetID))]

pdf("analyseBudSeed/figures/ospreeSppTemp.pdf", width = 8, height = 5)
plot(dOspreeCT$responseValue ~ dOspreeCT$tempDay, bg = dOspreeCT$vCol, pch = 21, cex =1.5,
     main = "% germ with germination temp",
     xlab = "Germ temp",
     ylab = "Percent germination")
dev.off()

viridis_colors <- viridis(length(unique(dPer$datasetID)))
dPer$vCol <- viridis_colors[as.numeric(factor(dPer$datasetID))]

pdf("analyseBudSeed/figures/allSppChillDuration.pdf", width = 8, height = 5)
plot(dPer$responseValue ~ dPer$chillDuration, bg = dPer$vCol, pch = 21, cex =1.5,
     main = "% germ with chill temp",
     xlab = "Chill Duration",
     ylab = "Percent germination")
dev.off()


## How many species have more than one chilling duration?
multiChill <- aggregate(dOspree["chillDuration"], dOspree[c("latbi")], FUN = function(x)length(unique(x)))
nrow(subset(multiChill, chillDuration == 1))/dim(multiChill)[1] 
# 20 or 55% of the spp only has one chill Duration

pdf("analyseBudSeed/figures/hisSppMultiChill.pdf", width = 8, height = 5)
hist(multiChill$chillDuration, main = "")
dev.off()

## How many ospree spp are there in the egret data:
egret <- read.csv("output/egretclean.csv")

dEgret <- egret[egret$latbi %in% ospSp,]
dEgret <- subset(dEgret, responseVar == "percent.germ")

dEgret <- dEgret[complete.cases(c(dEgret$chillDuration)),] 
