## Started July 2024 ##
## Started by Dan, edits by Mao ##

## Quick look at what dormancy classes we have data on ##

rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
graphics.off()

library(dplyr)

if(length(grep("dan?#you can change this later", getwd()) > 0)) {
  setwd(dir = "~/Documents/git/egret/analyses/output/")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
}

d<-read.csv("output/egretclean.csv")
bask<-read.csv("input/Baskin_Dormancy_Database.csv")

colnames(bask)<-bask[2,]
bask<-bask[3:14256,]

d$Genus_species<-paste(d$genus,d$species,sep="_")


d<-left_join(d,bask)
colnames(d)
d<-filter(d,!is.na(`Dormancy Class`))

table(d$respvar)
d.perc<-filter(d, respvar %in% c("per.germ"))

makefin<-d.perc %>% group_by(datasetID,study,Genus_species) %>% slice(which.max(response))

counter<-d %>% group_by(`Dormancy Class`, Genus_species) %>% count()

#Counting how many species are there in every dormancy class
d.perc$DormancyClass <- as.factor(d.perc$`Dormancy Class`)
d.perc$scarification <- as.factor(d.perc$scarification)
dormclass <- aggregate(Genus_species ~ DormancyClass, data = d.perc, FUN = function(x) length(unique(x)))

print(dormclass)
#MPD-22
#ND-22
#PD-142
#PY-8
#PYPD-2

#Subset every dormancy class
MPD <- d.perc[d.perc$DormancyClass == "MPD", ]
summary(MPD$scarification)
ND <- d.perc[d.perc$DormancyClass == "ND", ]
summary(ND$scarification)
PD <- d.perc[d.perc$DormancyClass == "PD", ]
summary(PD$scarification)
PY <- d.perc[d.perc$DormancyClass == "PY", ]
summary(PY$scarification)
PYPD <- d.perc[d.perc$DormancyClass == "PYPD", ]
summary(PYPD$scarification)

library(ggplot2)
ggplot(d.perc,aes(scarification,response))+geom_boxplot(aes(color=`Dormancy Class`,group=`Dormancy Class`))

