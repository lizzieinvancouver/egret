# Started May 3, 2022 by deirde
# Updates in November 2023 by Lizzie

# Compare the EGRET data to other datasets (currently Baskin^2 and OSPREE)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(plyr)
library(stringr)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else{
  setwd("boomdittyboom") # for midge
}

# Get the datasets
egret <- read.csv("input/egretData.csv")
egret$latbi <- paste(egret$genus, egret$species, sep = "_")

ospree <- read.csv("input/ospree_clean_withchill.csv")
ospree$latbi <- paste(ospree$genus, ospree$species, sep = "_")

# Merge with OSPREE 
egret$latbi <- str_trim(egret$latbi)
ospree$latbi  <- str_trim(ospree$latbi)
ospreeSp <- unique(ospree$latbi)
egretSp <- unique(egret$latbi)
egretSub <- egret[egret$latbi %in% ospreeSp,]

sort(unique(egretSub$latbi))
spInterest <- c("Fagus_sylvatica", "Betula_pendula", "Carpinus_betulus", "Picea_abies", "Picea_glauca",
  "Liquidambar_styracilua", "Prunus_avium","Pinus_sylvestris")
egretStudy <- egretSub[egretSub$latbi %in% spInterest,] 
unique(egretStudy$datasetID) 

write.csv(egretStudy, "output/ospree8studies.csv", row.names = FALSE)

# Merge with BaskinSquared
bb <- read.csv("input/Baskin_Dormancy_Database.csv", skip=2) 
bb$X1 <- NULL
baskinsp <- unique(bb$Genus_species)

# FYI, there is more than 1 row per sp
dim(bb)
length(baskinsp)

ospbask <- bb[which(bb$Genus_species %in% ospreeSp),]
egrbask <- bb[which(bb$Genus_species %in% egretSp),]

length(egretSp)
dim(egrbask)

table(ospbask$Dormancy.Class)
table(egrbask$Dormancy.Class)

subset(ospbask, Dormancy.Class=="ND")