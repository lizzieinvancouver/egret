# July 8, 2024
# Code was started to do a quick review of the number of columns that we have data for 1. chilling, 2. germination, photoperiod, and no chemicals, with just a final germination % response variable

#things discussed:
# photoperiod--- light vs dark, ordinal data---if don't say anything we assume that they are without light

rm(list=ls()) 
options(stringsAsFactors = FALSE)

require(ggplot2)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} 

d <- read.csv("output/egretclean.csv")

# forcing a few things to be numberic:
d$germDuration <- as.numeric(d$germDuration)
d$latbi <- paste(d$genus, d$species, sep = "_")
d$trt <- paste(d$chillDuration, d$chillTemp, d$germTemp, d$chemicalCor, d$scarifType, sep = "_")
d$keep <- paste(d$datasetIDstudy, d$latbi, d$trt, d$provenance.lat, d$provenance.long)

chilled <- subset(d, !is.na(chillTemp)) # chilling temps for 17141 rows or 213 studies

chillGerm <- subset(chilled, !is.na(chilled$germTemp)) # chilled with germ temps 16500

chillGermPhoto <- subset(chillGerm, !is.na(chillGerm$photoperiod)) # maybe we skipp photoperiod; 12351 rows data

cgpchem <- subset(chillGermPhoto, is.na(chillGermPhoto$chemical.concent))

perDat <- subset(cgpchem, responseVar == "percent.germ")

temp <- unique(perDat[, c("datasetID", "genus", "species", "germDuration")])

tempy <-  aggregate(temp["count"], temp[c("datasetID", "genus", "species")], FUN = length)
tempyyy <- subset(tempy, count < 3)

length(unique(tempyyy$datasetID))

## Summarize response variable 
# rows of data, number study (experiments per paper), papers, species
# how many of these are time series, how many of the mgt species are also in the percent germ
#d$datasetIDstudy <- as.factor(paste(d$datasetID, d$study, sep = " "))

responses <- c("percent.germ", "prop.germ", "germ.rate", "mgt", "50%.germ", "germ.time")

germResp <- data.frame(row.names = c("percentRow", "noStudy", "noExp", "noSp"))

for(i in 1:length(responses)){
temp <- subset(d, responseVar == responses[i])
germResp[1,i] <- round((nrow(temp)/nrow(d))*100,0)
germResp[2,i] <- length(unique(temp$datasetID)) # 19
germResp[3,i]<- length(unique(temp$datasetIDstudy)) # 25
germResp[4,i]<- length(unique(temp$latbi)) # 23
}
names(germResp) <-  c("percent.germ", "prop.germ", "germ.rate", "mgt", "50%.germ", "germ.time")
germResp

# script plot time series; how many 
#how many of the mgt species are also in the percent germ
mgt <- subset(d, responseVar == "mgt")
mgtSp <- unique(mgt$latbi)

perGerm <- subset(d, responseVar == "percent.germ")
temp <- perGerm[perGerm$latbi %in% mgtSp, ]; length(unique(temp$latbi))

# How much of the data in the percent germination is time series data?
#clean_bbperctodays.R---> do multiple loops 
perGerm <- subset(perGerm, !is.na(germDuration))
curved <- unique(perGerm[, c("datasetID", "datasetIDstudy", "trt","latbi", "germDuration","provenance.lat", "provenance.long", "keep")])
noDurations <- aggregate(curved[c("germDuration")], curved[c("datasetID","datasetIDstudy", "latbi","trt","provenance.lat", "provenance.long","keep" )], FUN = length)
curvey <- subset(noDurations, germDuration > 1) 
curvey$keep <- paste(curvey$datasetIDstudy, curvey$latbi, curvey$trt, curvey$provenance.lat, curvey$provenance.long)
length(unique(curvey$datasetID)) #46

curveStudy <- sort(unique(curvey$keep))
perGerm$keep <- paste(perGerm$datasetIDstudy, perGerm$latbi, perGerm$trt, perGerm$provenance.lat, perGerm$provenance.long)
dCurve <- perGerm[perGerm$keep %in% curveStudy, ]

length(unique(dCurve$latbi)) #50

#Simple plots of study by species---not grouping by treatments
# for(i in 1:length(curveStudy)){
#    pdf(paste("figures/timeSeriesCurves/", curveStudy[i], ".pdf"), width = 5, height = 5)
#   #i <- 1
#    temp <- subset(dCurve, datasetID == curveStudy[i])
#    
#    plot(temp$responseValueNum ~ temp$germDuration, type = 'n', main = curveStudy[i],
#      xlim = c(0,200))
#    
#    for(j in 1:length(unique(temp$latbi))){
#     # j <- 2
#      sp <- unique(temp$latbi)
#      tempy <- subset(temp, latbi == sp[j])
#    points(tempy$responseValueNum ~ tempy$germDuration, type = "l")
#    
#    }
#    dev.off()
# }

# Want to extract the last % germ for every species and the maximum length of time, but account for treatment differences

pgSub <- perGerm[,c("datasetID","datasetIDstudy", "latbi","chillTemp", "chillDuration", "germDuration", "germTemp", "scarifType", "chemicalCor", "responseValueNum", "trt", "provenance.lat", "provenance.long", "keep")]

#temp <- aggregate(pgSub[c("responseValueNum")], pgSub[c("datasetID", "study","latbi","chillTrt", "germTrt", "scarifType", "chemicalCor")], FUN = max, na.rm = T)

studyID <- sort(unique(pgSub$datasetID))
dataExp <- sort(unique(pgSub$datasetIDstudy))
trtVar <- sort(unique(pgSub$keep))

length(unique(pgSub$datasetID)) #234 study

singlePerG <- vector()
for(i in 1:length(studyID)){
  temp <- subset(pgSub, datasetID == studyID[i])
  trtVar <- unique(temp$keep)
  for(j in 1:length(trtVar)){
   # j <- 1
    tempV <- subset(temp, keep == trtVar[j])
    
    if(nrow(tempV) == 1){
      singlePerG <- rbind(singlePerG, tempV)
    } else {
      singlePerG <- rbind(singlePerG,tempV[which(tempV$responseValueNum == max(tempV$responseValueNum)),])
    }
  }
}

head(singlePerG)

singlePerG$massiveID <- paste(singlePerG$keep, singlePerG$responseValueNum, singlePerG$germDuration)
pgSub$massiveID <- paste(pgSub$keep, pgSub$responseValueNum, pgSub$germDuration)

pgSub$rowIsMax <- NA
pgSub$rowIsMax <- ifelse(pgSub$massiveID %in% unique(singlePerG$massiveID),1,0)


if(pgSub )
if(pgSub)
pgSub$rowIsMax[which(pgSub$massiveID %in% unique(singlePerG$massiveID))] <- 1

head(pgSub$massiveID)
head(singlePerG$massiveID)

curveStudy <- sort(unique(dCurve$datasetID))
pdf("figures/timeSeriesCurves_trt.pdf", width = 12, height = 12)
par(mfrow = c(4,2))
for(i in 1:length(curveStudy)){
 # i <- 5
  temp <- subset(dCurve, datasetID == curveStudy[i])
  
  for(j in 1:length(unique(temp$latbi))){
   # j<- 1
    sp <- unique(temp$latbi)
    tempSp <- subset(temp, latbi == sp[j])
    plot(tempSp$responseValueNum ~ tempSp$germDuration, type = 'n', main = paste(curveStudy[i], sp[j], sep = "_"), xlim = c(0,400), ylim = c(0,120), xlab = "time", ylab = "percent germ")
    
    for(t in 1:length(unique(tempSp$trt))){
     # t<-1
    trtVar <- unique(tempSp$trt)
    tempTrt <- subset(tempSp, trt == trtVar[t])
    points(tempTrt$responseValueNum ~ tempTrt$germDuration, type = "l")
    }
  }
}
dev.off()


#switch to ggplot
# update the git issue to fix the amount of data that is in curves
pdf("figures/timeSeriesCurves_trt_ggplot.pdf", width = 12, height = 12)
ggplot(dCurve, aes(x= germDuration, y = responseValueNum)) +
  geom_point(aes(group = keep, col = keep)) +
  geom_smooth(aes(group = keep, col = keep)) +
  theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    legend.position="none") +
  labs(x = "Time (days)",
    y = "Percent germination",
    color = "Legend") +
  facet_wrap(vars(datasetID, latbi), scales = "free")
dev.off()
  
temp <- subset(dCurve, datasetID == "dalling99")
tempy <- subset(temp, keep == "dalling99exp2 Combretum_bracteosum NA_NA_20_NA_partly scarified -39.983333 18.433333")

ggplot(tempy, aes(x = germDuration, y = responseValueNum)) +
   geom_line(aes(group = study, col = study))

tempy <- subset(temp, keep == "dalling99exp1 Combretum_bracteosum NA_NA_30_NA_NA -39.983333 18.433333")

ggplot(tempy, aes(x = germDuration, y = responseValueNum)) +
  geom_line(aes(group = study, col = study))

# other checks
temp <- subset(dCurve, datasetID == "yang18_2")
unique(temp$keep)
tempy <- subset(temp, keep == "yang18_2exp1 Pasania_glabra NA_NA_30/20_NA_NA 24.68 121.33")

ggplot(tempy, aes(x = germDuration, y = responseValueNum)) +
  geom_line(aes(group = study, col = study))

tempy <- subset(temp, keep == "dalling99exp1 Combretum_bracteosum NA_NA_30_NA_NA -39.983333 18.433333")

ggplot(tempy, aes(x = germDuration, y = responseValueNum)) +
  geom_line(aes(group = study, col = study))
####################################
# MGT 
length(unique(paste(d$datasetIDstudy, d$latbi, sep = "")))

  