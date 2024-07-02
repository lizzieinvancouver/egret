# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# USDA MANUAL GERMINATION CLEANING FINAL FINAL FINAL SCRIPT
# Started by Justin
# 27 June 2024
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(tidyverse)
library(reshape2)

d <- read.csv("output/germinationCleaned_official.csv")

# Changing column names
colnames(d) <- c("X","rowID","filePath","pdfPageNumber",
                 "scrapedTableNumber","pdfTableNumber","genus","species",
                 "seedType","seedSource","medium","pregermTreatment",
                 "pregermTreatmentHotWater","pretreatment","stratification",
                 "warmStratificationDuration","coldStratificationDuration",
                 "photoperiod","tempDay","tempNight","darkRange",
                 "testDuration","germTime","germPercentTotal","germPercentAvg",
                 "samples","germRate","germEnergyPercentAvg","germEnergyPercent",
                 "germCapacityAvg","germCapacity","germPercent15degIncubated",
                 "germPercent20degIncubated","genusID","pretreatmentFeces",
                 "pretreatmentChill","pretreatmentChillDuration","pretreatmentScarifType",
                 "pretreatmentScarifTypeGen")


# Checking class to see if any conflicts in variable type
sapply(d, class)

# Converting integer to character for pivot_longer()
d$germCapacity <- as.character(d$germCapacity)
d$germPercent15degIncubated <- as.character(d$germPercent15degIncubated)
d$germPercent20degIncubated <- as.character(d$germPercent20degIncubated)

# Converting the data into long format
dcopy <- d %>%
  group_by(species) %>%
  pivot_longer(cols = c("germTime","germPercentTotal","germPercentAvg",
                        "germRate","germEnergyPercentAvg","germEnergyPercent",
                        "germCapacityAvg","germCapacity","germPercent15degIncubated",
                        "germPercent20degIncubated"),
               names_to = "responseType",
               values_to = "responseValue")

# Replacing - in cold strat duration with "to" since I missed it first time
dcopy$coldStratificationDuration <- gsub("-"," to ",dcopy$coldStratificationDuration)

# Finding which columns have ranges of numeric values
colnames(dcopy)[apply(dcopy, 2, function(col) any(str_detect(col, " to ")))]

# Cleaning up some of the response value data
dcopy$responseValue[which(dcopy$responseValue == "16:")] <- "16"
dcopy$responseValue[which(dcopy$responseValue == "<")] <- NA

# Making min and max columns for each column that has a range
# First using strsplit() to separate values into new columns
breakbyto <- strsplit(dcopy$pregermTreatment, " to ", fixed=TRUE)
dcopy$pregermTrtMin <- unlist(lapply(breakbyto, function(x) x[1]))
dcopy$pregermTrtMax <- unlist(lapply(breakbyto, function(x) x[2]))

breakbyto2 <- strsplit(dcopy$coldStratificationDuration, " to ", fixed=TRUE)
dcopy$coldStratDurMin <- unlist(lapply(breakbyto2, function(x) x[1]))
dcopy$coldStratDurMax <- unlist(lapply(breakbyto2, function(x) x[2]))

breakbyto3 <- strsplit(dcopy$photoperiod, " to ", fixed=TRUE)
dcopy$photoperiodMin <- unlist(lapply(breakbyto3, function(x) x[1]))
dcopy$photoperiodMax <- unlist(lapply(breakbyto3, function(x) x[2]))

breakbyto4 <- strsplit(dcopy$tempDay, " to ", fixed=TRUE)
dcopy$tempDayMin <- unlist(lapply(breakbyto4, function(x) x[1]))
dcopy$tempDayMax <- unlist(lapply(breakbyto4, function(x) x[2]))

breakbyto5 <- strsplit(dcopy$tempNight, " to ", fixed=TRUE)
dcopy$tempNightMin <- unlist(lapply(breakbyto5, function(x) x[1]))
dcopy$tempNightMax <- unlist(lapply(breakbyto5, function(x) x[2]))

breakbyto6 <- strsplit(dcopy$testDuration, " to ", fixed=TRUE)
dcopy$testDurMin <- unlist(lapply(breakbyto6, function(x) x[1]))
dcopy$testDurMax <- unlist(lapply(breakbyto6, function(x) x[2]))

breakbyto7 <- strsplit(dcopy$samples, " to ", fixed=TRUE)
dcopy$samplesMin <- unlist(lapply(breakbyto7, function(x) x[1]))
dcopy$samplesMax <- unlist(lapply(breakbyto7, function(x) x[2]))

breakbyto8 <- strsplit(dcopy$pretreatmentChillDuration, " to ", fixed=TRUE)
dcopy$pretrtChillDurMin <- unlist(lapply(breakbyto8, function(x) x[1]))
dcopy$pretrtChillDurMax <- unlist(lapply(breakbyto8, function(x) x[2]))

breakbyto9 <- strsplit(dcopy$responseValue, " to ", fixed=TRUE)
dcopy$responseValueMin <- unlist(lapply(breakbyto9, function(x) x[1]))
dcopy$responseValueMax <- unlist(lapply(breakbyto9, function(x) x[2]))

# Making mean average columns for the above columns
# unique(dcopy$responseValueMin)
dcopy$responseValueMin[which(dcopy$responseValueMin == "69 (18")] <- "69" #GIT ISSUE 20 IN EGRET; Keeping the range of values in the original data but leaving just the given mean for the responseValueAvg column
dcopy$responseValueMin[which(dcopy$responseValueMin == "93 (84")] <- "93"
dcopy$responseValueMin[which(dcopy$responseValueMin == "60 (40")] <- "60"
# unique(dcopy$responseValueMax)
dcopy$responseValueMax[which(dcopy$responseValueMax == "94)")] <- NA
dcopy$responseValueMax[which(dcopy$responseValueMax == "96)")] <- NA
dcopy$responseValueMax[which(dcopy$responseValueMax == "88)")] <- NA

# Converting to integer
dcopy$responseValueMin <- as.integer(dcopy$responseValueMin)
dcopy$responseValueMax <- as.integer(dcopy$responseValueMax)

# using rowMeans() conserves the rows where just one value is present so that it doesn't try to make a mean out of a valid value in the Min column and NA in the Max column
dcopy$responseValueAvg <- rowMeans(dcopy[, c("responseValueMin", "responseValueMax")], na.rm = TRUE)
dcopy$responseValueAvg[which(is.nan(dcopy$responseValueAvg))] <- NA

# First looking at which values are weird, converting them to numeric placeholders in the Min column, then turning them back to their original value in the Avg column
# Pregermination treatment
# unique(dcopy$pregermTrtMin)
dcopy$pregermTrtMin[which(dcopy$pregermTrtMin == " ttc (time")] <- 99991
dcopy$pregermTrtMin[which(dcopy$pregermTrtMin == "Overnight")] <- 99992
# unique(dcopy$pregermTrtMax)
dcopy$pregermTrtMax[which(dcopy$pregermTrtMax == "cool")] <- NA

dcopy$pregermTrtMin <- as.integer(dcopy$pregermTrtMin)
dcopy$pregermTrtMax <- as.integer(dcopy$pregermTrtMax)
dcopy$pregermTrtAvg <- rowMeans(dcopy[, c("pregermTrtMin", "pregermTrtMax")], na.rm = TRUE)
dcopy$pregermTrtAvg[which(is.nan(dcopy$pregermTrtAvg))] <- NA

dcopy$pregermTrtAvg[which(dcopy$pregermTrtMin == 99991)] <- "ttc (time to cool to room temperature) varied from  several hours to overnight"
dcopy$pregermTrtAvg[which(dcopy$pregermTrtMin == 99992)] <- "Overnight"
dcopy$pregermTrtMin[which(dcopy$pregermTrtMin == 99991)] <- "ttc (time to cool to room temperature) varied from  several hours to overnight"
dcopy$pregermTrtMin[which(dcopy$pregermTrtMin == 99992)] <- "Overnight"

# cold stratification duration
# unique(dcopy$coldStratDurMin)
dcopy$coldStratDurMin[which(dcopy$coldStratDurMin == "Unspecified period")] <- 99991
dcopy$coldStratDurMin[which(dcopy$coldStratDurMin == "Stratification and germination as a continuum under the same conditions")] <- 99992
dcopy$coldStratDurMin[which(dcopy$coldStratDurMin == "Var.")] <- 99993
# unique(dcopy$coldStratDurMax)

dcopy$coldStratDurMin <- as.integer(dcopy$coldStratDurMin)
dcopy$coldStratDurMax <- as.integer(dcopy$coldStratDurMax)
dcopy$coldStratDurAvg <- rowMeans(dcopy[, c("coldStratDurMin", "coldStratDurMax")], na.rm = TRUE)
dcopy$coldStratDurAvg[which(is.nan(dcopy$coldStratDurAvg))] <- NA

dcopy$coldStratDurAvg[which(dcopy$coldStratDurMin == 99991)] <- "Unspecified period"
dcopy$coldStratDurAvg[which(dcopy$coldStratDurMin == 99992)] <- "Stratification and germination as a continuum under the same conditions"
dcopy$coldStratDurAvg[which(dcopy$coldStratDurMin == 99993)] <- "Var."
dcopy$coldStratDurMin[which(dcopy$coldStratDurMin == 99991)] <- "Unspecified period"
dcopy$coldStratDurMin[which(dcopy$coldStratDurMin == 99992)] <- "Stratification and germination as a continuum under the same conditions"
dcopy$coldStratDurMin[which(dcopy$coldStratDurMin == 99993)] <- "Var."

# Photoperiod
# unique(dcopy$photoperiodMin)
dcopy$photoperiodMin[which(dcopy$photoperiodMin == "Natural daylength in a greenhouse")] <- 99991
dcopy$photoperiodMin[which(dcopy$photoperiodMin == "<16")] <- 99992
dcopy$photoperiodMin[which(dcopy$photoperiodMin == ">8")] <- 99993
dcopy$photoperiodMin[which(dcopy$photoperiodMin == "Dark")] <- 0
# unique(dcopy$photoperiodMax)

dcopy$photoperiodMin <- as.integer(dcopy$photoperiodMin)
dcopy$photoperiodMax <- as.integer(dcopy$photoperiodMax)
dcopy$photoperiodAvg<- rowMeans(dcopy[, c("photoperiodMin", "photoperiodMax")], na.rm = TRUE)
dcopy$photoperiodAvg[which(is.nan(dcopy$photoperiodAvg))] <- NA

dcopy$photoperiodAvg[which(dcopy$photoperiodMin == 99991)] <- "Natural daylength in a greenhouse"
dcopy$photoperiodAvg[which(dcopy$photoperiodMin == 99992)] <- "<16"
dcopy$photoperiodAvg[which(dcopy$photoperiodMin == 99993)] <- ">8"
dcopy$photoperiodMin[which(dcopy$photoperiodMin == 99991)] <- "Natural daylength in a greenhouse"
dcopy$photoperiodMin[which(dcopy$photoperiodMin == 99992)] <- "<16"
dcopy$photoperiodMin[which(dcopy$photoperiodMin == 99993)] <- ">8"

# Temperature Day
# unique(dcopy$tempDayMin)
# unique(dcopy$tempDayMax)

dcopy$tempDayMin <- as.integer(dcopy$tempDayMin)
dcopy$tempDayMax <- as.integer(dcopy$tempDayMax)
dcopy$tempDayAvg <- rowMeans(dcopy[, c("tempDayMin", "tempDayMax")], na.rm = TRUE)
dcopy$tempDayAvg[which(is.nan(dcopy$tempDayAvg))] <- NA

# Temperature Night
# unique(dcopy$tempNightMin)
# unique(dcopy$tempNightMax)

dcopy$tempNightMin <- as.integer(dcopy$tempNightMin)
dcopy$tempNightMax <- as.integer(dcopy$tempNightMax)
dcopy$tempNightAvg <- rowMeans(dcopy[, c("tempNightMin", "tempNightMax")], na.rm = TRUE)
dcopy$tempNightAvg[which(is.nan(dcopy$tempNightAvg))] <- NA

# Test Duration
# unique(dcopy$testDurMin)
dcopy$testDurMin[which(dcopy$testDurMin == "NP")] <- 99991
dcopy$testDurMin[which(dcopy$testDurMin == ">60")] <- 99992
# unique(dcopy$testDurMax)

dcopy$testDurMin <- as.integer(dcopy$testDurMin)
dcopy$testDurMax <- as.integer(dcopy$testDurMax)
dcopy$testDurAvg <- rowMeans(dcopy[, c("testDurMin", "testDurMax")], na.rm = TRUE)
dcopy$testDurAvg[which(is.nan(dcopy$testDurAvg))] <- NA

dcopy$testDurAvg[which(dcopy$testDurMin == 99991)] <- "NP"
dcopy$testDurAvg[which(dcopy$testDurMin == 99992)] <- ">60"
dcopy$testDurMin[which(dcopy$testDurMin == 99991)] <- "NP"
dcopy$testDurMin[which(dcopy$testDurMin == 99992)] <- ">60"

# Samples
unique(dcopy$samplesMin)

dcopy$samplesMin[which(dcopy$samplesMin == ">7")] <- 99991
dcopy$samplesMin[which(dcopy$samplesMin == "7t")] <- "7"
# unique(dcopy$samplesMax)

dcopy$samplesMin <- as.integer(dcopy$samplesMin)
dcopy$samplesMax <- as.integer(dcopy$samplesMax)
dcopy$samplesAvg <- rowMeans(dcopy[, c("samplesMin", "samplesMax")], na.rm = TRUE)
dcopy$samplesAvg[which(is.nan(dcopy$samplesAvg))] <- NA

dcopy$testDurAvg[which(dcopy$testDurMin == 99991)] <- ">7"
dcopy$testDurMin[which(dcopy$testDurMin == 99991)] <- ">7"

# Pretreatment Chill Duration
# unique(dcopy$pretrtChillDurMin)
# unique(dcopy$pretrtChillDurMax)

dcopy$pretrtChillDurMin <- as.integer(dcopy$pretrtChillDurMin)
dcopy$pretrtChillDurMax <- as.integer(dcopy$pretrtChillDurMax)
dcopy$pretrtChillDurAvg <- rowMeans(dcopy[, c("pretrtChillDurMin", "pretrtChillDurMax")], na.rm = TRUE)
dcopy$pretrtChillDurAvg[which(is.nan(dcopy$pretrtChillDurAvg))] <- NA

write.csv(dcopy,"output/USDAgerminationCleanedFinal.csv")
