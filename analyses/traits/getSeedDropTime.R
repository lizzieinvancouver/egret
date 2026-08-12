## Started 22 July 2026 ##
## Started by Lizzie then by CRD ##
## Try to get a file of when seeds disperse by merging several sources ##


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd()) > 0)) {
  setwd("/Users/christophe_rouleau-desrochers/github/egret/analyses")
} else{
  setwd("boomdittyboom") # for midge
}

# grab data
ds <- read.csv("output/lifecycleWeedsScraped.csv") # scraped in scrapeUSDAweeds.R
dm <- read.csv("input/traits/mtsv_data_publish.csv") # data Dan scraped from Michigan Trees 

# check shrubs and vines species from the photos I took and that claude transformed into csvs
shrubscheck <- read.csv("/Users/christophe_rouleau-desrochers/Downloads/shrubs_vines_index.csv")[,c(1,3,4)]
treescheck <- read.csv("/Users/christophe_rouleau-desrochers/Downloads/trees.csv")

# bind them!
check <- rbind(shrubscheck, treescheck)
check$latbi <- paste0(check$genus, "_", check$species)

# Read newly scraped trees and shrubs and vines
dm2 <- read.csv("input/traits/MTSV_v2.csv")

dm2$latbi <- paste0(dm2$Genus, "_", dm2$Species)

# check if I entered species that dan already input
vec <- intersect(dm2$latbi, dm$name)

# select Dan's entries for instances where I input data that was already in input
dm2 <- subset(dm2, !(latbi %in% vec))

# Prep structure for merge
dm2_2 <- dm2[, c(2:8, 11)]

colnames(dm2_2)[colnames(dm2_2) == "flo.time"] <- "flo_time"

dm_2 <- dm[,1:7]
dm_2$latbi <- paste0(dm_2$Genus, "_", dm_2$Species)

# rbind!
df <- rbind(dm2_2, dm_2)

# === === === === === === === === === === === === === === ===
# NEED TO CLEAN THE FRUITING TIME:
# Autumn --> numerical
# === === === === === === === === === === === === === === ===

# Clean species names:
# I am cleaning the species names from Dan's MTSV data and the extra species I found in these book, using the Worldflora package. The Worldflora backbone data set can be found on the lab One Drive: TemporalEcologyLab/Documents/egret/classification.csv
library("WorldFlora")
# Read in the backbone dataset
backbone <- read.csv("~/Desktop/UBC/egretLOCAL/classification.csv",head = TRUE, sep="\t")
df$Species <- tolower(df$Species)
# Remove trailing spaces:
df$Species <- str_trim(df$Species)
df$Genus <- str_trim(df$Genus)
df_species <- unique(paste(df$Genus, df$Species))
checks<-WFO.match(spec.data=df_species, WFO.data=backbone, counter=1, verbose=TRUE)
d_species_fix <- unique(checks$scientificName)
names_changed <- setdiff(df_species, d_species_fix)

df$latbi_cleaned <- checks$scientificName[match(paste(df$Genus, df$Species), 
                                                checks$spec.name )]
setdiff(df$latbi_cleaned, paste(df$Genus, df$Species))

# Still need to make additional checks to make sure these species names still correpond to the egret database

# Some minor cleaning
ds$type[ds$type == "Annual"] <- "annual"
ds$type[ds$type == "Perennial"] <- "perrenial"
ds$type[ds$type == "Biennial"] <- "biennial"

#

# I think we should just adjust dm to match ds and merge it in (do any of the species overlap?)
# From dm, we need the genus, species, fruiting and we can label them all as perennials 

# Species overlap
intersect(ds$genus, dm$Genus) # no species overlap; only 1 genus!

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Clean dm to prepare to merge with ds ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
subset(ds, type != "annual" & != "Annual")

# here I add a column for "type", but they're all annual
dm$type <- "Annual"
dm2 <- dm[, c("name", "type")]

# Some species cleaning:
dm$Species[dm$Species == "strigosis"] <- "strigosus"
