# Started May 3, 2022 by deirde
# Updates in November 2023 by Lizzie
# Updates in July 2024 by Mao
# Updates in Feb 2025 by Mao
# Compare the EGRET data to other datasets (currently Baskin^2 and OSPREE, and USDA)
# Updates in July 2025 by Mao

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
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else{
  setwd("boomdittyboom") # for midge
}

# Get the datasets
egret <- read.csv("output/egretclean.csv", header = TRUE)
egret$latbi <- paste(egret$genus, egret$species, sep = " ")


ospree <- read.csv("output/ospreeEgretCleaned.csv", header = TRUE)
ospree$latbi <- paste(ospree$genus, ospree$species, sep = " ")

USDA <- read.csv("output/usdaGerminationCleaned.csv", header = TRUE)
USDA$latbi <- paste(USDA$genus, USDA$species, sep = " ")


bbegret <- read.csv("output/baskinegretclean.csv", header = TRUE) 
bbusda <- read.csv("output/baskinusdaclean.csv", header = TRUE)


ospreeSp <- unique(ospree$latbi)
egretSp <- unique(egret$latbi)
usdaSp <- unique(USDA$latbi)
bbegretSp <- unique(bbegret$Genus_species)
bbusdaSp <- unique(bbusda$Genus_species)

# Compare OSPREE x EGRET
intersect_eo <- intersect(egretSp, ospreeSp)
length(intersect_eo)# 9 sp

# Compare OSPREE x USDA
intersect_ou <- intersect(ospreeSp, usdaSp)
length(intersect_ou)# 66 sp

# Compare EGRET x USDA (for fun!)
intersect_eu <- intersect(egretSp, usdaSp)
length(intersect_eu)# 26 sp

# Compare Baskin x EGRET
intersect_be <- intersect(bbegretSp, egretSp)
length(intersect_be)# 207 sp

# Compare Baskin x USDA
intersect_bu <- intersect(bbusdaSp, usdaSp)
length(intersect_bu)# 173 sp

# Combine egret and USDA
egretusda <- data.frame(latbi = c(egretSp, usdaSp))

# Compare OSPREE x (EGRET + USDA)
intersect_o_eu <- intersect(ospreeSp, egretusda$latbi)
length(intersect_o_eu)# 69 sp
