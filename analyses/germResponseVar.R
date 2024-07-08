# July 8, 2024
# Code was started to do a quick review of the number of columns that we have data for 1. chilling, 2. germination, photoperiod, and no chemicals, with just a final germination % response variable

#things discussed:
# photoperiod--- light vs dark, ordinal data---if don't say anything we assume that they are without light

rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
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
}

d <- read.csv("output/egretclean.csv")

chilled <- subset(d, !is.na(chill.tempCor)) # chilling temps for 17141 rows or 213 studies

chillGerm <- subset(chilled, !is.na(chilled$germ.tempCor)) # chilled with germ temps 16500

chillGermPhoto <- subset(chillGerm, !is.na(chillGerm$photoperiod)) # maybe we skipp photoperiod; 12351 rows data

cgpchem <- subset(chillGermPhoto, is.na(chillGermPhoto$chemical.concent))

perDat <- subset(cgpchem, responseVar == "percent.germ")

temp <- unique(perDat[, c("datasetID", "genus", "species", "germDuration")])
temp$count <- 1
tempy <-  aggregate(temp["count"], temp[c("datasetID", "genus", "species")], FUN = sum)
tempyyy <- subset(tempy, count < 3)

length(unique(tempyyy$datasetID))

## Summarize response variable 
# rows of data, number study (experiments per paper), papers, species
# how many of these are time series, how many of the mgt species are also in the percent germ

d$sp.name <- paste(d$genus, d$species, sep = "_")
d$studyExp <- as.factor(paste(d$datasetID, d$study, sep = " "))


responses <- c("percent.germ", "prop.germ", "germ.rate", "mgt", "50%.germ", "germ.time")

germResp <- data.frame(row.names = c("percentRow", "noStudy", "noExp", "noSp"))

for(i in 1:length(responses)){
temp <- subset(d, responseVar == responses[i])
germResp[1,i] <- round((nrow(temp)/nrow(d))*100,0)
germResp[2,i] <- length(unique(temp$datasetID)) # 19
germResp[3,i]<- length(unique(temp$studyExp)) # 25
germResp[4,i]<- length(unique(temp$sp.name)) # 23
}
names(germResp) <-  c("percent.germ", "prop.germ", "germ.rate", "mgt", "50%.germ", "germ.time")
germResp

# script plot time series; how many 
#how many of the mgt species are also in the percent germ
mgt <- subset(d, responseVar == "mgt")
mgtSp <- unique(mgt$sp.name)

perGerm <- subset(d, responseVar == "percent.germ")
temp <- perGerm[perGerm$sp.name %in% mgtSp, ]; length(unique(temp$sp.name))
