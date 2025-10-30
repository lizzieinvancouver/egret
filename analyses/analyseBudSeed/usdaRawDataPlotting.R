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

names(d)

d$datasetID <- paste(d$pdf_page_number, d$pdf_table_number, sep = "_")
# we want data on: chilling, germ temps, light

head(d$X) # needs to be cleaned out

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

dMaxMin <- dMaxMin[complete.cases(c(dMaxMin$chillTemp)),] 
# with chilling data, if chillTemp included also always includes duration, n = 567, nspp = 179

dMaxMin$tempDay[which(dMaxMin$tempDay == "20 and 10 then 20")] <- 16.67
dMaxMin$tempDay <- as.numeric(dMaxMin$tempDay)

dMaxMin <- dMaxMin[complete.cases(c(dMaxMin$tempDay)),] 
# complete data for chilling and day temp: n = 446, 

# percent germination:
dPer <- subset(dMaxMin, responseVarClean == "percent.germ")
# n = 267; nspp = 132

plot(dPer$responseValue ~ dPer$chillTemp)
plot(dPer$responseValue ~ dPer$chillDuration)
plot(dPer$responseValue ~ dPer$tempDay)
plot(dPer$responseValue ~ dPer$chillTemp, bg = dPer$tempDay, pch = 21)
plot(dPer$chillTemp, dPer$tempDay, bg = dPer$chillDuration, pch = 21)

## How much overlap do we have between USDA and OSPREE?
osp<-read.csv("input/ospreeforegret.csv")
ospSp <- unique(osp$latbi)

dOspree <- dPer[dPer$latbi %in% ospSp,] # n = 53, nspp = 22
viridis_colors <- viridis(length(unique(dOspree$chillDuration)))
dOspree$vCol <- viridis_colors[as.numeric(factor(dOspree$chillDuration))]

pdf("analyseBudSeed/figures/ospreeSppChillTemp.pdf", width = 8, height = 5)
plot(dOspree$responseValue ~ dOspree$chillTemp, bg = dOspree$vCol, pch = 21, cex =1.5,
     main = "% germ with chill temp, with colors showing chill duration for 22 OSPEE spp.",
     xlab = "Chill Temp",
     ylab = "Percent germination")
dev.off()

# day temp: 
viridis_colors <- viridis(length(unique(dOspree$tempDay)))
dOspree$vCol <- viridis_colors[as.numeric(factor(dOspree$tempDay))]

plot(dOspree$responseValue ~ dOspree$chillTemp, bg = dOspree$vCol, pch = 21, cex =1.5, cex.lab = 1.5,
     main = "Percent germ with chill temp, with colors showing chill duration for 22 OSPEE spp.",
     xlab = "Chill Temp",
     ylab = "Percent germination")


######################################### More generally, how many ospree spp are there in the usda data:

dOspree <- dMaxMin[dMaxMin$latbi %in% ospSp,]
# n= 107; nspp 24