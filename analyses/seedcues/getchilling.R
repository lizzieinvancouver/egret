#=== === === === === === === === === === === === === === === === === === === ===
# Created by Dan to add chilling and warm strat infomraion

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
library(dplyr)
library(chillR)
library(stringr)
library(ggplot2)

if(length(grep("christophe_rouleau-desrochers", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses/")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
}

# read file
d <- read.csv2("output/egretclean.csv", sep=",")

#Create warm strat column
d$warmstrat <- NA
# vector for all treatments that have warm stratification
warmstrat.names <- unique(d$treatment[grep("warm", d$treatment)])
# remove entry that shouldn't be there
warmstrat.names[!warmstrat.names %in% c("cold strat + soak in warm water")]
# add a 1 in warmstrat column whenever "warm" appepeared in the treatment column
d$warmstrat[which(d$treatment %in% warmstrat.names)] <- 1

source("cleaning/source/addChill.R") ### add chill

unique(d$germTemp)
highones<-dplyr::filter(d,germTemp %in% c("50","60","70","80","90","100"))

mgtdat<-filter(d,responseVar=="mgt")

mgtdat$germTemp<-as.numeric(mgtdat$germTemp)
mgtdat$responseValue<-as.numeric(mgtdat$responseValue)

ggplot(mgtdat,aes(germTemp,responseValue))+geom_point(aes(color=latbi))+geom_smooth(method="lm",aes(color=latbi))
ggplot(mgtdat,aes(chillPortions,responseValue))+geom_point(aes(color=latbi))+
  geom_smooth(method="lm",aes(color=latbi))+coord_cartesian(ylim=c(0,150))

range(mgtdat$responseValue,na.rm=TRUE)

d$c<-NA
d$c<-ifelse( column of interest %in% vector of interr, 1, 0)



