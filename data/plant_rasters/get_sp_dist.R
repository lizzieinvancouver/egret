## Started 27 April 2026
## By Ken

## Subset species distribution files

rm(list = ls())
options(stringsAsFactors = FALSE)

if(length(grep("deirdre", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/github/egret/analyses")
} else if(length(grep("crouleau", getwd())) > 0){
  setwd("/home/crouleau/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

library(sf)
library(terra)

data <- read.csv("output/ospreeEgretCleaned.csv")
sp_prop <- unique(data$spps)
sp <- gsub("_", " ", sp_prop)
sp <- tolower(sp)
sp_dists <- list()

#iNaturalist

avail <- rep(FALSE, length(sp))
for(i in 1:9){
  dist <- st_read(dsn = paste0('../data/plant_rasters/iNaturalist_geomodel_Plantae_', i, '.gpkg'),
                  layer = paste0('iNaturalist_geomodel_Plantae_', i))
  names <- tolower(dist[['name']])
  
  check <- match(sp, names)
  avail[which(check > 0)] <- TRUE
  
  sp_dists[[i]] <- na.omit(dist[check, ])
}

dist <- data.frame("species" = sp_prop,
                   "dist_avail" = avail)
dist <- dist[order(dist$species),]

write.csv(dist, "../data/plant_rasters/dist_inat.csv", row.names = FALSE)

sp_dist <- do.call(rbind, sp_dists)
st_write(sp_dist, '../data/plant_rasters/dist_inat.gpkg')

# GreenMaps

files <- list.files("../data/plant_rasters/dist_gm_all")
files <- gsub(".tif", "", files)
files <- tolower(files)

check <- match(sp, files)
avail <- check > 0
avail[which(is.na(avail))] <- FALSE

dist <- data.frame("species" = sp_prop,
                   "dist_avail" = avail)
dist <- dist[order(dist$species),]

write.csv(dist, "../data/plant_rasters/dist_gm.csv", row.names = FALSE)

files <- list.files("../data/plant_rasters/dist_gm_all", full.names = TRUE)
sp_files <- files[check[which(!is.na(check))]]
file.copy(sp_files, "../data/plant_rasters/dist_gm")
