# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# USDA MANUAL GERMINATION CLEANING FINAL FINAL FINAL SCRIPT
# Started by Justin
# 27 June 2024
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#library(tidyverse)
#library(reshape2)

#d <- read.csv("..//output/earlyIterationDataSheets/germinationCleaned_official.csv")
colnames(d)

# Changing column names
colnames(d) <- c("filePath","pdfPageNumber",
                 "scrapedTableNumber","pdfTableNumber",
                 "genus","species",
                 "seedType","seedSource",
                 "medium","pregermTreatment",
                 "pregermTreatmentHotWater","pretreatment",
                 "stratification","warmStratificationDuration",
                 "coldStratificationDuration","photoperiod",
                 "tempDay","tempNight",
                 "darkRange","testDuration",
                 "germTime","germPercentTotal",
                 "germPercentAvg","samples",
                 "germRate","germEnergyPercentAvg",
                 "germEnergyPercent","germCapacityAvg",
                 "germCapacity","germPercent15degIncubated",
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


#write.csv(dcopy,"output/USDAgerminationCleanedFinal.csv")
