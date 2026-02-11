# Started Feb 9 by Deirdre 

# Aim of this code is to prep the egret data for running in the USDA model

# rm(list=ls()) 
# options(stringsAsFactors = FALSE)
# 
# if(length(grep("deirdre", getwd()) > 0)) {
#   setwd("~/Documents/github/egret/analyses")
# } else if(length(grep("lizzie", getwd()) > 0)) {
#   setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
# } else if(length(grep("Xiaomao", getwd()) > 0)) {
#   setwd("C:/PhD/Project/egret/analyses")
# } else if(length(grep("Ken", getwd())) > 0){
#   setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
# } else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
#   setwd("/Users/christophe_rouleau-desrochers/github/egret/analyses")
# } else if(length(grep("victor", getwd())) > 0){
#   setwd('~/projects/egret/analyses')
# } 

# packages
require(dplyr)

egret <- FALSE
if(egret){
# cleaning egret data---will use the same decision rules as used to the abundant 0's model:
source('studyDesign/decisionRules_abundant0s.R')

# Process data 
# (1) - removing rows where we do not have any info on forcing) 
modeld <- newd[!is.na(newd$germDuration) & !is.na(newd$germTempGen) & newd$germDuration != 'unknown' & newd$germTempGen != "ambient",] 
# (2) - separating warm and cold strat. durations
modeld$warmStratDur <- as.numeric(sapply(1:nrow(modeld), function(i){
  seq <-  unlist(stringr::str_split(modeld$stratSequence_condensed[i], ' then '))
  temp <-  unlist(stringr::str_split(modeld$stratDur_condensed[i], ' then '))
  id <- which(seq == 'warm')
  return(ifelse(is.null(id), NA, temp[id]))
}))
modeld$coldStratDur <- as.numeric(sapply(1:nrow(modeld), function(i){
  seq <-  unlist(stringr::str_split(modeld$stratSequence_condensed[i], ' then '))
  temp <-  unlist(stringr::str_split(modeld$stratDur_condensed[i], ' then '))
  id <- which(seq == 'cold')
  return(ifelse(is.null(id), NA, temp[id]))
}))
# (3) - assuming NA strat. mean 0
modeld$warmStratDur <- ifelse(is.na(modeld$warmStratDur), 0, modeld$warmStratDur)
modeld$coldStratDur <- ifelse(is.na(modeld$coldStratDur), 0, modeld$coldStratDur)
# (4) - removing species not present in the phylo tree
modeld$genusspecies <- sapply(modeld$genusspecies, function(i) stringr::str_split_i(i, ' ', 1))
# test <- modeld[!(modeld$genusspecies %in% phylo$tip.label), ]
# unique(test$genusspecies) # check only Gymno!
# modeld <- modeld[(modeld$genusspecies %in% phylo$tip.label), ]
# (4) - transform response to proportion and germ. covariates to numeric
modeld$responseValueNum <- as.numeric(modeld$responseValueNum)/100
modeld$germDuration <- as.numeric(modeld$germDuration)
modeld$germTempGen <- as.numeric(modeld$germTempGen)
# temporary - need to check whether odd values (>>> scrapping uncertainty) have been corrected
# modeld[modeld$responseValueNum < 1.05,] # not needed anymore!
# (5) - transform values a bit above or below 0 (due to scrapping uncertainty)
modeld$responseValueNum <- ifelse(modeld$responseValueNum > 1, 1, modeld$responseValueNum)
modeld$responseValueNum <- ifelse(modeld$responseValueNum < 0, 0, modeld$responseValueNum)
modeld$germDuration <- ifelse(modeld$germDuration < 0, 0, modeld$germDuration)
modeld <- subset(modeld, !is.na(chillDuration))
modeld <- subset(modeld, !is.na(germTempGen))

# mdlEgret <- modeld[, c('datasetID', 'study', 'genusspecies', 'responseValueNum', 'coldStratDur', 'germTempGen',"germDuration")]

modeld$keep <- paste(modeld$datasetID, modeld$study, modeld$genusspecies, 
                     modeld$coldStatDur, modeld$chillTemp, modeld$chillDuration, modeld$germTemp,
                     modeld$germPhotoperiod, modeld$scarifTypeSpe, modeld$provLatLonAlt,
                     sep = "_")

noPts <- aggregate(modeld[c("germDuration")], modeld[c("keep")], FUN = function(x)length(unique(x)))
noPts$germDuration <- as.numeric(noPts$germDuration)
timeSeries <- subset(noPts, germDuration > 1)

trtVar <- sort(unique(timeSeries$keep))
singlePerG <- vector()
for(i in 1:length(trtVar)){
  temp <- subset(modeld, keep == trtVar[i])
  singlePerG <- rbind(singlePerG,temp[which(temp$germDuration == max(temp$germDuration)),])
}
singlePerG

onePt <- modeld[!modeld$keep %in% trtVar, ]

egretData <- rbind(onePt, singlePerG)

egretData <- egretData[, c("datasetID", "genusspecies", "coldStratDur", "responseVar",  "responseValueNum","germTempGen")]

colnames(egretData)[colnames(egretData) == "genusspecies"] <- "latbi"
colnames(egretData)[colnames(egretData) == "responseVar"] <- "responseVarClean"
colnames(egretData)[colnames(egretData) == "responseValueNum"] <- "responseValue"
colnames(egretData)[colnames(egretData) == "coldStratDur"] <- "chillDuration"

}
#####################################################
# Prep the usda data using the same cleaning rules 
d <- read.csv("output/usdaGerminationCleaned.csv")

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
dMaxMin$tempDay[which(dMaxMin$tempDay == "20 and 10 then 20")] <- 16.67
dMaxMin$tempDay[which(dMaxMin$tempDay == "10 then 25")] <- 17.5
dMaxMin$tempDay[which(dMaxMin$tempDay == "10 then 20")] <- 15
dMaxMin$tempDay[which(dMaxMin$tempDay == "20 then 30")] <- 25
dMaxMin$tempDay[which(dMaxMin$tempDay == "5 then 15")] <- 10

dMaxMin$tempDay <- as.numeric(dMaxMin$tempDay)
# The weighted average is the sum of tempDay and tempNight multiplied by their corresponding ratio of day or night hours out of 24
dMaxMin$germTempGen <- NA
for(i in 1:nrow(d)){
  if(!is.na(dMaxMin$tempDay[i]) && !is.na(dMaxMin$tempNight[i])){
    dMaxMin$germTempGen[i] <- (dMaxMin$tempDay[i]*12)/24+(dMaxMin$tempNight[i]*12/24)
  }
  }
# Now what about the columns in which there's constant temperature and therefore only values in tempDay and not in tempNight?
# We can do regular mean average in these ones then
for(i in 1:nrow(dMaxMin)){
  
  if(is.na(dMaxMin$tempNight[i] && !is.na(dMaxMin$tempDay[i]))){
    dMaxMin$germTempGen[i] <- (dMaxMin$tempDay[i])
  }
}

# These for loops might assume that darkness treatments where photoperiod is 0 will always be cool, but this is not true, because sometimes seeds incubated in darkness 
# were still exposed to alternating temperatures

# dMaxMin <- dMaxMin[complete.cases(c(dMaxMin$chillDuration)),] 

# percent germination:
dPer <- subset(dMaxMin, responseVarClean == "percent.germ")

usdaData <- dPer[, c("datasetID", "latbi", "chillDuration", "responseVarClean",  "responseValue","germTempGen")]

length(unique(usdaData$latbi)) # if keep data with NA for chill duration and germTempGen---362 spp in usda

#Take only the max value for rows with same chilling and forcing, keep the ones with only one value
usdaData <- usdaData %>%
  dplyr::group_by(datasetID, latbi,responseVarClean, chillDuration, germTempGen) %>%
  dplyr::filter(
    n_distinct(responseValue) == 1 | 
      responseValue == max(responseValue, na.rm = TRUE)
  ) %>%
  ungroup()

usdaData$latbi[which(usdaData$latbi == "Aronia_x prunifolia")] <-"Aronia_x_prunifolia"


## merge
#d <- rbind(usdaData, egretData)
#length(unique(d$latbi)) #568 spp if left incomplete
