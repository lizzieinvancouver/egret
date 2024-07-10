## Started 8 July 2024 by Mao##


library("taxize")
library("stringr")


setwd("C:/PhD/Project/egret/analyses")
# General chekcs:
#1. fix typos and minor issues:
### Clean Species ##############################
baskin <- read.csv("input/Baskin_Dormancy_Database.csv", skip=2) 
baskin$X1 <- NULL
# Substitude the underscore with space
baskin$Genus_species <- sub("_", " ", baskin$Genus_species)
# Didn't work, try separate the Genus_species, and then using unique(paste())
library("tidyr")
baskin_sp <- separate(baskin, Genus_species, into = c("genus", "species"), sep = "_", remove = FALSE)
baskinsp <- unique(paste(baskin_sp$genus,baskin_sp$species))
# Still didn't work...
# Use taxize package to inspect whether names are correct
ref <- gnr_datasources() # Full list of dabases available
fix_names <- gnr_resolve(sci = baskinsp, with_canonical_ranks = T)
baskin_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(baskin$Genus_species, baskin_species_fix)
names_changed


sort(baskinsp)


