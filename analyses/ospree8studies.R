# Started May 3, 2022 by deirde

# the aim of this code is to generate the model output for my phenology ms
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rstan)
library(shinystan)
#library(reshape2)
#library(bayesplot)
library(ggplot2)
library(dplyr)
library(plyr)
library(stringr)
library(phytools)
#library(ggpubr)
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

egret <- read.csv("input/egretData.csv")
egret$sp.name <- paste(egret$genus, egret$species, by = " ")

ospree <- read.csv("..//ospree/analyses/output/ospree_clean.csv")
ospree$sp.name <- paste(ospree$genus, ospree$species, by = " ")

egret$sp.name <- str_trim(egret$sp.name)
ospree$sp.name <- str_trim(ospree$sp.name)
ospreeSp <- unique(ospree$sp.name)

egretSub <- egret[egret$sp.name %in% ospreeSp,]

sort(unique(egretSub$sp.name))

 spInterest <- c("Fagus sylvatica", "Betula pendula", "Carpinus betulus", "Picea abies", "Picea glauca","Liquidambar styracilua", "Prunus avium","Pinus sylvestris")

 egretStudy <- egretSub[egretSub$sp.name %in% spInterest,] 
unique(egretStudy$datasetID) 

write.csv(egretStudy, "analyses/output/ospree8studies.csv", row.names = FALSE)
