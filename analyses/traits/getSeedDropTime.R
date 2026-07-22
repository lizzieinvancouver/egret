## Started 22 July 2026 ##
## Started by Lizzie ##
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
} else{
  setwd("boomdittyboom") # for midge
}

# grab data
ds <- read.csv("output/lifecycleWeedsScraped.csv") # scraped in scrapeUSDAweeds.R
dm <- read.csv("input/traits/mtsv_data_publish.csv") # data Dan scraped from Michigan Trees 

# I think we should just adjust dm to match ds and merge it in (do any of the species overlap?)
# From dm, we need the genus, species, fruiting and we can label them all as perennials 
