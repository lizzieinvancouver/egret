# started Nov 29 by Deirdre L.

# Aim of this study is to check the ahola99 and javanmard14
# 1. Do they appear accurrately entered?
# 2. What does the data look like?

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)
library(plyr)
library(stringr)
library(lattice)
require(cowplot)


# aim of this code is to combine the cleaning work of on Dinara and Hoai Huong 

# Also to determine what datasets are still missing
if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret")
} else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/others/deirdre/Synchrony")
} else{
  setwd("/home/deirdre/Synchrony") # for midge
}

eight <- read.csv("analyses/output/ospree8studies.csv")

ahola <- subset(eight, datasetID == "ahola99")
java <- subset(eight, datasetID == "javanmard14")

####################################################
# ahola99

unique(ahola$study) # 1 exp
unique(ahola$treatment) #"control"          "dark/moist chill"
unique(ahola$chill.temp) #NA or 3, NA = no chill control
unique(ahola$photoperiod)
unique(ahola$storage.temp) # only one
unique(ahola$germ.temp) #"10" "13" "16" "20"

# Things to clean: 
# no lat long: Palvaanjärvi; 60°51′ N, 27°29′ E; 70 m altitude; 98 clones) and No. 249 (Metsäväärä; 61°34′ N, 26°18′ E; 130 m altitude; 429 clone
ahola$source.population[which(ahola$sp.name == "Betula pendula")] <- "Patama, Finland"
ahola$source.population[which(ahola$sp.name == "Picea abies")] <- "Palvaanjär, Finland"

ahola$provenance.lat[which(ahola$sp.name == "Betula pendula")] <- "60.85"
ahola$provenance.long[which(ahola$sp.name == "Picea abies")] <- "27.48v"
ahola$provenance.altitude[which(ahola$sp.name == "Picea abies")] <- "70"

# resovar in log10 
# photoperiod 24/0 for all --- is zero the control or is 24?
ahola$photoperiod[which((ahola$treatment) == "control")] <- "NA"
ahola$photoperiod[which((ahola$treatment) == "dark/moist chill")] <- "NA"
#what does chill.temp=na mean?
ahola$chill.duration[which((ahola$treatment) == "control")] <- "0"

# year of study -- recieved seeds Feb 1996
ahola$year.germination[which((ahola$year.collected) == "1993")] <- "1996"

# continent 
ahola$continent[which((ahola$year.collected) == "1993")] <- "Europe"

# log10(mgt)
aholaMgt <- subset(ahola, respvar == "log10(mgt)")

ggplot(aholaMgt, aes(x= chemical.concent, y = response)) +
  geom_point(aes(colour = sp.name, shape = germ.temp))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Infrared concentration",
       y = "log10(mgt)") 

pdf("analyses/figures/ahola99/mgtInfraredSp.pdf", height = 3, width = 12)
ggplot(aholaMgt, aes(x= chemical.concent, y = response)) +
  geom_point(aes(colour = sp.name, shape = chill.duration))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Infrared concentration",
       y = "log10(mgt)") + facet_grid(cols = vars(germ.temp))
dev.off()

ggplot(aholaMgt, aes(x= chemical.concent, y = response)) +
  geom_point(aes(colour = sp.name))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Infrared concentration",
       y = "log10(mgt)") + facet_grid(cols = vars(germ.temp), rows = vars(chill.duration))
  
# germ.prob
aholaProb <- subset(ahola, respvar == "germ.prob")

ggplot(aholaProb, aes(x= chemical.concent, y = response)) +
  geom_point(aes(colour = sp.name, shape = germ.temp))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "Infrared concentration",
       y = "Germination probability") 

pdf("analyses/figures/ahola99/probGermInfraredSp.pdf", height = 3, width = 12)
ggplot(aholaProb, aes(x= chemical.concent, y = response)) +
  geom_point(aes(colour = sp.name, shape = chill.duration))+ 
  scale_x_log10() +
  # theme(panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), 
  #       axis.line = element_line(colour = "black")) +
  labs(x = "Infrared concentration",
       y = "Germination probability") + facet_grid(cols = vars(germ.temp))
dev.off()

pdf("analyses/figures/ahola99/probGermInfraGermTemp.pdf", height = 3, width = 6)
ggplot(aholaProb, aes(x= chemical.concent, y = response)) +
  geom_point(aes(colour = sp.name, shape = germ.temp))+ 
  scale_x_log10() +
  # theme(panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), 
  #       axis.line = element_line(colour = "black")) +
  labs(x = "Infrared concentration",
       y = "Germination probability") + facet_grid(cols = vars(chill.duration), rows = vars(sp.name))
dev.off()
####################################################
# javanmard14

unique(java$study) # 1 exp
unique(java$treatment) #"NA"
unique(java$chill.temp) #"4+/-0.5"
unique(java$photoperiod) #NA
unique(java$storage.temp) # only one
unique(java$germ.temp) #"13+/-0.5"
unique(java$chill.duration) #"28" "42" "56"
unique(java$chemical) #"GA3"
unique(java$chemical.concent) # 0  250  500 1000
unique(java$respvar) #"per.germ" "germ.rt" 

# to clean:
# really doesn't say where the seeds are from!
# cold strat was dark, but no mention of whether germination growth chamber was dark
# no mention of washing trt - unwashed vs 24h under a running tap --- missing Fig 2 data

ahola$other.treatment[which(ahola$sp.name == "Betula pendula")] <- "Patama, Finland"
# per.germ
javaPer <- subset(java, respvar == "per.germ")

pdf("analyses/figures/javanmard14/perGermGA3Chill.pdf", width = 4, height =3)
ggplot(javaPer, aes(x= chemical.concent, y = response)) +
  geom_point(aes(colour = chill.duration))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "GA3 concentration",
       y = "Percent germination") 
dev.off()

# germ.rt
javaRt <- subset(java, respvar == "germ.rt")

pdf("analyses/figures/javanmard14/rateGA3Chill.pdf", width = 4, height =3)
ggplot(javaRt, aes(x= chemical.concent, y = response)) +
  geom_point(aes(colour = chill.duration))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(x = "GA3 concentration",
       y = "Germination Rate") 
dev.off()