# Started 11 August 2026 
# by Ken and Lizzie

# aim is look at some new storage/strat data .. .and try to run a model  maybe 

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

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

# get the data!
d  <- read.csv("output/egretcleanmostoragedetrals.csv")

library(chillR)
library(ggplot2)

sort(table(d$responseVar))
dperc <- d[which(d$responseVar=="percent.germ"),]

# storTempEst, storDurEst, stratTempEst, stratDurEst

# Hmm, okay, I would need to get this to plot across multiple pages to be useful...
ggplot(dperc[1:1000,], aes(y=responseValueNum, x=stratTempEst, color=storDurEst, group=datasetID)) +
  geom_point() +
  facet_wrap(.~datasetID)

ggplot(dperc, aes(y=storDurEst, x=stratTempEst, color=stratTempEst)) +
  geom_point() 

# checking ...
table(d$stratTempEst)
table(d$stratDurEst)
table(d$storTempEst)
table(d$storDurEst)

stordur <- d[which(is.na(d$storDurEst)==FALSE),] # 8261
stortemp <- d[which(is.na(d$storTempEst)==FALSE),] # 7879
stratdur <- d[which(is.na(d$stratDurEst)==FALSE),] # 16877
strattemp <- d[which(is.na(d$stratTempEst)==FALSE),] # 15647

everybody <- d[which(is.na(d$storDurEst)==FALSE & 
  is.na(d$storTempEst)==FALSE &
  is.na(d$stratDurEst)==FALSE &
  is.na(d$stratTempEst)==FALSE),] # 3868

table(everybody$responseVar)

ggplot(everybody, aes(y=responseValueNum, x=stratTempEst, color=storDurEst, group=datasetID)) +
  geom_point() +
  facet_wrap(.~datasetID)

ggplot(everybody, aes(y=storDurEst, x=storTempEst, color=stratDurEst)) +
  geom_point() 
ggplot(everybody, aes(y=stratDurEst, x=stratTempEst, color=storDurEst)) +
  geom_point() 
ggplot(everybody, aes(y=stratTempEst, x=storTempEst, color=datasetID)) +
  geom_point() 

# some checks
dss <- subset(d, responseVar == "percent.germ" &
                !is.na(storTempEst) &
                !is.na(storDurEst))

dss$storTypeGen <- 'dry'
dss$storTypeGen[which(dss$storageType %in% c('moist', 'moist/cold', 'moisture-controlled'))] <- 'moist'
dss$chillPortionsEst <- NA

# for(i in 1:nrow(dss)){
#   dss$chillPortionsEst[i] <- Dynamic_Model(rep(dss$stratTempEst[i], dss$stratDurEst[i] * 24))[length(rep(dss$stratTempEst[i], dss$stratDurEst[i] * 24))]
# }

check <- dss[c('storageTemp', 'storageDuration', 'chillTemp', 'chillDuration',
               'storTempEst', 'storDurEst', 'stratTempEst', 'stratDurEst',
               'chillPortionsEst')]

plot(dss$storTempEst, dss$responseValueNum, xlim = c(-20, 30))
plot(dss$storDurEst, dss$responseValueNum)
plot(dss$stratTempEst, dss$responseValueNum)
plot(dss$stratDurEst, dss$responseValueNum)
# plot(dss$chillPortionsEst, dss$responseValueNum)

ggplot(dss, aes(y = responseValueNum, x = germDuration, color = storDurEst)) +
  geom_point() +
  xlim(0, 300) +
  scale_color_gradient(limits = c(0, 400))

ggplot(dss, aes(y = responseValueNum, x = germDuration, color = storTempEst)) +
  geom_point() +
  xlim(0, 300) +
  scale_color_gradient(limits = c(-50, 50))

ggplot(dss, aes(y = responseValueNum, x = germDuration, color = storDurEst, group = germTemp)) +
 geom_point() + 
 facet_wrap(.~germTemp)

# check effect of chill portions grouped by storage temperature
# on percent germ vs germ duration
dss <- subset(d, responseVar == "percent.germ" &
                !is.na(responseValueNum) &
                !is.na(germDuration) &
                !is.na(storTempEst) &
                !is.na(storDurEst) &
                !is.na(stratTempEst) &
                !is.na(stratDurEst))

dss$chillPortionsEst <- NA
dss$chillPortionsGroup <- NA
for(i in 1:nrow(dss)){
  dss$chillPortionsEst[i] <- Dynamic_Model(rep(dss$stratTempEst[i], dss$stratDurEst[i] * 24))[length(rep(dss$stratTempEst[i], dss$stratDurEst[i] * 24))]
}
dss$chillPortionsGroup <- ceiling(dss$chillPortionsEst / 10)

pdf('modeling/figures/storEff/20to30.pdf', width = 8, height = 6)
ggplot(dss, aes(y = responseValueNum, x = germDuration, color = storTempEst,
                group = chillPortionsGroup)) +
  geom_point() +
  scale_color_gradient(limits = c(-200, 0)) +
  facet_wrap(.~chillPortionsGroup)
dev.off()

# find studies which change storage
storTemps <- rep(NA, length(unique(dss$datasetID)))

for(i in 1:length(unique(dss$datasetID))){
  dssSub <- subset(dss, datasetID == unique(dss$datasetID)[i])
  storTemps[i] <- length(unique(dssSub$storTempEst))
}
