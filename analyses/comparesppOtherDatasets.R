# Started May 3, 2022 by deirde
# Updates in November 2023 by Lizzie
# Updates in July 2024 by Mao
# Compare the EGRET data to other datasets (currently Baskin^2 and OSPREE, and USDA)

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
egret <- read.csv("output/egretclean.csv")
egret$latbi <- paste(egret$genus, egret$species, sep = " ")

ospree <- read.csv("input/ospree_clean_withchill.csv")
ospree$latbi <- paste(ospree$genus, ospree$species, sep = " ")

USDA <- read.csv("scrapeUSDAseedmanual/output/usdaGerminationData.csv")
USDA$latbi <- paste(USDA$genus, USDA$species, sep = " ")

bb <- read.csv("output/baskinclean.csv", header =T) 
bb$latbi <- bb$matched_name2

egret$latbi <- str_trim(egret$latbi)
ospree$latbi  <- str_trim(ospree$latbi)
USDA$latbi <- str_trim(USDA$latbi)
bb$latbi <- str_trim(bb$latbi)

ospreeSp <- unique(ospree$latbi)
egretSp <- unique(egret$latbi)
usdaSp <- unique(USDA$latbi)
bbSp <- unique(bb$latbi)


# Compare OSPREE x EGRET
intersect_eo <- intersect(egretSp, ospreeSp)
length(intersect_eo)# 19 sp

# Compare OSPREE x USDA
intersect_ou <- intersect(ospreeSp, usdaSp)
length(intersect_ou)# 60 sp

# Compare EGRET x USDA (for fun!)
intersect_eu <- intersect(egretSp, usdaSp)
length(intersect_eu)# 17 sp

# Compare Baskin x EGRET
intersect_be <- intersect(bbSp, egretSp)
length(intersect_be)# 201 sp

# Compare Baskin x USDA
intersect_bu <- intersect(bbSp, usdaSp)
length(intersect_bu)# 202 sp

# Combine egret and USDA
egretusda <- data.frame(latbi = c(egretSp, usdaSp))

# Compare OSPREE x (EGRET + USDA)
intersect_o_eu <- intersect(ospreeSp, egretusda$latbi)
length(intersect_o_eu)# 71 sp

# Compare Baskin x (EGRET + USDA)
intersect_b_eu <- intersect(bbSp, egretusda$latbi)
length(intersect_b_eu)# 387 sp

# Egret x Ospree x USDA x Baskin
ospreeSp <- unique(ospree$latbi)
egretSp <- unique(egret$latbi)
usdaSp <- unique(USDA$latbi)
bbSp <- unique(bb$latbi)
egretSub <- egret[egret$latbi %in% ospreeSp,]
egretSub <- egretSub[egretSub$latbi %in% usdaSp,]
egretSub <- egretSub[egretSub$latbi %in% bbSp,]

sort(unique(egretSub$latbi))# 8 sp
