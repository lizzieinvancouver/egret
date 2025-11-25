# Started Nov 3, 2025 by Mao

library(ape)
library(phytools)
setwd("C:/PhD/Project/egret")

ospree <- read.csv("analyses/input/ospree_clean_withchill.csv", header = TRUE)
ospree$latbi <- paste(ospree$genus, ospree$species, sep = "_")

USDA <- read.csv("analyses/output/usdaGerminationCleaned.csv", header = TRUE)
USDA$latbi <- paste(USDA$genus, USDA$species, sep = "_")

intersect_ou <- intersect(ospree$latbi, USDA$latbi)


## load OSPREE tree
phy.ospree <- read.tree("analyses/input/ospreeforegret.tre")
phy.ospree$tip.label[1:10]
species_to_drop <- setdiff(phy.ospree$tip.label, intersect_ou)
ospreeusda <- drop.tip(phy.ospree, species_to_drop)

plot(ospreeusda, cex = 0.8)
write.tree(ospreeusda,"analyses/output/ospreeUsdaTree.tre")

