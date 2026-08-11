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

nrow(subset(treescheck, genus == "Acer"))
nrow(subset(dm, Genus == "Acer"))
# bind them!
check <- rbind(shrubscheck, treescheck)

check$latbi <- paste0(check$genus, "_", check$species)

length(unique(check$latbi))
length(unique(dm$name))
intersect(check$latbi, dm$name)

setdiff(check$latbi, dm$name)
setdiff(dm$name, check$latbi)
ds <- ds[order(ds$genus),]
dm <- dm[order(dm$Genus),]



# Some minor cleaning
ds$type[ds$type == "Annual"] <- "annual"
ds$type[ds$type == "Perennial"] <- "perrenial"
ds$type[ds$type == "Biennial"] <- "biennial"
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
