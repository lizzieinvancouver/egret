## Started 8 July 2024 by Mao##


library("taxize")
library("stringr")

setwd("C:/PhD/Project/egret/analyses")
# General chekcs:
#1. fix typos and minor issues:
### Clean Species ##############################
usda <- read.csv("scrapeUSDASeedmanual/output/usdaGerminationData.csv", header = T)
usda$species <- tolower(usda$species)
usda_species <- unique(paste(usda$genus, usda$species))

# Use taxize package to inspect whether names are correct
ref <- gnr_datasources() # Full list of dabases available
fix_names <- gnr_resolve(sci = usda_species, with_canonical_ranks = T)
usda_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(usda_species, usda_species_fix)
names_changed

# Seems like nothing need to be change, there are just three species names, two hybrids and one cultivated sp, which I think it's not sp we might be interested in.

sort(usda_species)


