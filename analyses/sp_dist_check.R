## Started 28 April 2026
## By Ken

## Check species distributions

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

library(geodata)
library(sf)
library(terra)

world <- world(resolution = 5, path = 'input/worldclim')
sf_use_s2(TRUE)

#GreenMaps

avail <- read.csv('../data/plant_rasters/dist_gm.csv')
dist_files <- list.files('../data/plant_rasters/dist_gm', full.names = TRUE)
sp_gm <- gsub('../data/plant_rasters/dist_gm/', '', dist_files)
sp_gm <- gsub('.tif', '', sp_gm)

pdf('figures/sp_dist/sp_dist_gm.pdf', height = 6, width = 8)
plot.new()
legend('center',
       legend = c('presence', 'absence', 'presence centroid'),
       fill = c('green', 'red', 'magenta'))

for(i in 1:length(dist_files)){
  dist <- rast(dist_files[i])
  dist <- st_as_sf(as.polygons(dist))
  
  abs <- dist[which(dist[['median']] == 0),]
  abs <- st_union(abs)
  pres <- dist[which(dist[['median']] == 1),]
  pres <- st_union(pres)
  centroid <- st_centroid(pres)
  
  plot(world, main = sp_gm[i])
  plot(abs, col = 'red', border = NA, add = TRUE)
  plot(pres, col = 'green', border = NA, add = TRUE)
  plot(centroid, col = 'magenta', pch = 19, add = TRUE)
}
dev.off()

#iNaturalist

dist <- st_read(dsn = '../data/plant_rasters/dist_inat.gpkg',
                layer = 'dist_inat')
dist <- dist[order(dist[['name']]),]
sp_inat <- dist[['name']]

pdf('figures/sp_dist/sp_dist_inat.pdf', height = 6, width = 8)
plot.new()
legend('center',
       legend = c('presence', 'presence centroid'),
       fill = c('green', 'magenta'))

for(i in 1:nrow(dist)){
  sp_dist <- st_union(st_make_valid(dist[['geom']][i,]))
  centroid <- st_centroid(sp_dist)
  
  plot(world, main = sp_inat[i])
  plot(sp_dist, col = 'green', border = NA, add = TRUE)
  plot(centroid, col = 'magenta', pch = 19, add = TRUE)
}
dev.off()

#Comparison for species appearing in both sources

sp_both <- intersect(sp_gm, sp_inat)
pdf('figures/sp_dist/sp_dist_comp.pdf', height = 6, width = 8)
plot.new()
legend('center',
       legend = c('presence (iNaturalist only)',
                  'presence (GreenMaps only)',
                  'presence (both)',
                  'presence centroid (iNaturalist)',
                  'presence centroid (GreenMaps)'),
       fill = c('cyan', 'yellow', 'green', 'blue', 'gold'))

for(i in 1:length(sp_both)){
  gm_idx <- which(sp_gm == sp_both[i])
  inat_idx <- which(sp_inat == sp_both[i])
  
  dist_gm <- rast(dist_files[gm_idx])
  dist_gm <- st_as_sf(as.polygons(dist_gm))
  dist_gm <- dist_gm[which(dist_gm[['median']] == 1),]
  dist_gm <- st_union(dist_gm)
  centroid_gm <- st_centroid(dist_gm)
  
  dist_inat <- st_union(st_make_valid(dist[['geom']][i,]))
  centroid_inat <- st_centroid(dist_inat)
  
  dist_both <- st_intersection(dist_gm, dist_inat)
  
  plot(world, main = sp_both[i])
  plot(dist_inat, col = 'cyan', border = NA, add = TRUE)
  plot(dist_gm, col = 'yellow', border = NA, add = TRUE)
  plot(dist_both, col = 'green', border = NA, add = TRUE)
  plot(centroid_inat, col = 'blue', pch = 19, add = TRUE)
  plot(centroid_gm, col = 'gold', pch = 19, add = TRUE)
}
dev.off()
