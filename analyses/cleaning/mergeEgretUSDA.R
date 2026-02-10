# started Feb 13, 2025 by Deirdre
# aim of this code is to merge the egret and the usda data together:
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)

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

source("cleaning/cleanall.R")
source("analyseSeedCues/summarizeStrat.R")

e <- d # change name of egret data
e$dataType <- "egret"

d <- read.csv("output/usdaGerminationCleaned.csv")
source("scrapeUSDAseedmanual/cleaning/source/cleanEgretColUsda.R")

temp <- d[,c("cold_stratification_temp_C","chillDuration","chillDurationMin","chillDurationMax")]


maxV <- d[,c("pdf_page_number","pdf_table_number","genus", "species" ,"latbi", "source.population" , "scarifTypeGen", "scarifTypeSpe","reps", 
              "treatment","responseVarClean", "responseValue",
              "cold_stratification_temp_C","chillDuration", "warm_stratification_temp_C", "warm_stratification_days",
             "chillTempMax","chillDurationMax", 
             "warmTempMax", "warmDurationMax", "pregermTrtMax","photoperiodCor", 
             "tempDayMax","tempNightMax", "testDurMax","responseValueMax")]; maxV$dataType <- "usdaMax"
names(maxV) <- c("pdf_page_number","pdf_table_number","genus", "species" ,"latbi", "source.population" , "scarifTypeGen", "scarifTypeSpe","reps", 
                 "treatment","responseVar", "responseValue", "coldStratDays", "coldStratTemp", "warmStratTemp", "warmStratDays",
                 "chillTemp","chillDuration", 
                 "warmStratTemp", "warmDuration", "pregermTrt","photoperiodCor", 
                 "tempDay","tempNight","germDuration",   
                 "responseValueNum", "dataType")
minV <- d[,c("pdf_page_number","pdf_table_number","genus", "species" ,"latbi", "source.population" , "scarifTypeGen", "scarifTypeSpe","reps", 
             "treatment","responseVarClean", "responseValue",
             "cold_stratification_temp_C","chillDuration", "warm_stratification_temp_C", "warm_stratification_days",
             "chillTempMin","chillDurationMin", 
             "warmTempMin", "warmDurationMin", "pregermTrtMin","photoperiodCor", 
             "tempDayMin","tempNightMin", "testDurMin",         
             "responseValueMin")]; minV$dataType <- "usdaMin"
names(minV) <- c("pdf_page_number","pdf_table_number","genus", "species" ,"latbi", "source.population" , "scarifTypeGen", "scarifTypeSpe","reps", 
                 "treatment","responseVar", "responseValue","coldStratDays", "coldStratTemp", "warmStratTemp", "warmStratDays",
                 "chillTemp","chillDuration", 
                 "warmStratTemp", "warmDuration", "pregermTrt","photoperiodCor", 
                 "tempDay","tempNight","germDuration",        
                 "responseValueNum", "dataType")

uMerge <- rbind(maxV, minV)
lookInto <- subset(uMerge, is.na(responseValue)) # 1300

uLong <- subset(uMerge, !is.na(responseValueNum)) # 1530

uLong$datasetID <- paste(uLong$pdf_page_number, uLong$pdf_table_number, sep = "_") 
uLong$woody <- "Y"
uLong$storageType <- NA
uLong$storageDuration <- NA

uLong$storageType[which(uLong$treatment == "dry storage")] <- "dry storage"
uLong$storageType[which(uLong$treatment == "moist storage")] <- "moist storage"
uLong$storageType[which(uLong$treatment == "storage")] <- "storage"

uLong$storageDuration[which(uLong$storageType == "dry storage")] <- uLong$pregermTrt[which(uLong$storageType == "dry storage")]
uLong$storageDuration[which(uLong$storageType == "moist storage")] <- uLong$pregermTrt[which(uLong$storageType == "moist storage")]
uLong$storageDuration[which(uLong$storageType == "storage")] <- uLong$pregermTrt[which(uLong$storageType == "storage")]

uLong$dormancyTemp <- uLong$chillTemp

colnames(uLong)[colnames(uLong) == "tempDay"] <- "germTempDay"
colnames(uLong)[colnames(uLong) == "tempNight"] <- "germTempNight"

# the USDA data appears to be missing any data for:
#cold strat temp; germ photoperiod; soaking
usdaEgret <- rbind.fill(d, uLong)

write.csv(usdaEgret, "analyseBudSeed/output/egretUSDA.csv", row.names = FALSE)

length(unique(usdaEgret$latbi)) # 425

unique(sort(uLong$warmDuration)) 
