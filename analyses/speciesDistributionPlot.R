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
library(leaflet)
library(sf)
library(terra)
library(viridisLite)

world <- world(resolution = 5, path = 'input/worldclim')
sf_use_s2(TRUE)


#inaturalist weird distributions

dist <- st_read(dsn = '../data/plant_rasters/dist_inat.gpkg',
                layer = 'dist_inat')
dist <- dist[order(dist[['name']]),]
sp_inat <- dist[['name']]

pdf('figures/spDist/iNatProb.pdf', height = 6, width = 8)
for(i in c(87, 139, 140)){
  sp_dist <- dist[i,]
  plot(world, main = sp_inat[i])
  plot(sp_dist, col = 'green', border = NA, add = TRUE)
}
dev.off()

#GreenMaps

avail <- read.csv('../data/plant_rasters/dist_gm.csv')
dist_files <- list.files('../data/plant_rasters/dist_gm', full.names = TRUE)
sp_gm <- gsub('../data/plant_rasters/dist_gm/', '', dist_files)
sp_gm <- gsub('.tif', '', sp_gm)

abs <- list()
pres <- list()
for(i in 1:length(dist_files)){
  dist <- rast(dist_files[i])
  dist <- as.polygons(dist)
  
  sp_abs <- rast(ext(-180, 180, -90, 90), resolution = 1/6, vals = 1)
  sp_pres <- rast(ext(-180, 180, -90, 90), resolution = 1/6, vals = 1)
  
  sp_abs <- rasterize(dist, sp_abs, field = 'median')
  sp_pres <- rasterize(dist, sp_pres, field = 'median')
  
  sp_abs[sp_abs == 1] <- NA
  sp_abs[sp_abs == 0] <- 1
  sp_pres[sp_pres == 0] <- NA
  
  abs[[i]] <- sp_abs
  pres[[i]] <- sp_pres
}

sp_abs <- rast(abs)
sp_pres <- rast(pres)

sum_abs <- app(sp_abs, fun = 'sum', na.rm = TRUE)
sum_pres <- app(sp_pres, fun = 'sum', na.rm = TRUE)

png('figures/spDist/egretGM.png', height = 480, width = 640)
plot(sum_pres, main = paste0('Number of Present Species (GreenMaps, ', length(sp_gm), ' species)'))
plot(world, add = TRUE)
plot(sum_pres, legend = NA, add = TRUE)
dev.off()

# abs <- list()
# pres <- list()
# for(i in 1:length(dist_files)){
#   dist <- rast(dist_files[i])
#   dist <- st_as_sf(as.polygons(dist))
#   
#   abs[[i]] <- st_combine(dist[which(dist[['median']] == 0),])
#   pres[[i]] <- st_combine(dist[which(dist[['median']] == 1),])
# }
# 
# abs <- as.data.frame(do.call(rbind, abs))
# abs$species <- sp_gm
# colnames(abs)[1] <- 'geometry'
# abs <- abs[c(2,1)]
# abs <- st_sf(abs, crs = 4326)
# 
# pres <- as.data.frame(do.call(rbind, pres))
# pres$species <- sp_gm
# colnames(pres)[1] <- 'geometry'
# pres <- pres[c(2,1)]
# pres <- st_sf(pres, crs = 4326)
# 
# sp_pres <- st_intersection(pres)
# sp_pres <- backup
# remidx <- c()
# for(i in 1:nrow(sp_pres)){
#   if(class(sp_pres$geometry[i])[1] == 'sfc_GEOMETRYCOLLECTION'){
#     if(length(st_collection_extract(sp_pres$geometry[i], 'POLYGON')) == 0){
#       sp_pres$geometry[i] <- NULL
#       remidx <- append(remidx, i)
#     } else{
#       sp_pres$geometry[i] <- st_combine(st_collection_extract(sp_pres$geometry[i], 'POLYGON'))
#     }
#   }
#   if(class(sp_pres$geometry[i])[1] != 'sfc_MULTIPOLYGON' && class(sp_pres$geometry[i])[1] != 'sfc_POLYGON'){
#     remidx <- append(remidx, i)
#   }
# }
# sp_pres <- sp_pres[-remidx, 'n.overlaps']
# sp_pres <- vect(sp_pres)
# 
# div <- rast(ext(-180, 180, -90, 90), resolution = 1/6)
# div <- rasterize(sp_pres, div, field = 'n.overlaps')
# 
# pal <- colorNumeric('viridis', domain = c(1 - max(sp_pres$n.overlaps), max(sp_pres$n.overlaps)))
# breaks <- seq(1, max(sp_pres$n.overlaps), length.out = 100)
# 
# plot(check, col = pal(breaks), range = c(1, max(sp_pres$n.overlaps)),
#      type = 'continuous')
# plot(world, add = TRUE)
# plot(check, col = pal(breaks), border = NA, legend = NA, add = TRUE)

pdf('figures/spDist/GM.pdf', height = 6, width = 8)
plot.new()
legend('center',
       legend = c('presence', 'absence', 'presence centroid'),
       fill = c('green', 'red', 'magenta'))

for(i in 1:length(dist_files)){
  dist <- rast(dist_files[i])
  dist <- st_as_sf(as.polygons(dist))
  
  abs <- dist[which(dist[['median']] == 0),]
  abs <- st_combine(abs)
  pres <- dist[which(dist[['median']] == 1),]
  pres <- st_combine(pres)
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
dist <- dist[-c(87, 139, 140),]
sp_inat <- dist[['name']]

dist$pres <- 1
check <- vect(dist['pres'])

pres <- list()
for(i in 1:length(sp_inat)){
  div <- rast(ext(-180, 180, -90, 90), resolution = 1/6)
  div <- rasterize(check[i], div, field = 'pres')
  pres[[i]] <- div
}
pres <- rast(pres)
sum_pres <- app(pres, fun = 'sum', na.rm = TRUE)

png('figures/spDist/egretINat.png', height = 480, width = 640)
plot(sum_pres, main = paste0('Number of Present Species (iNaturalist, ', length(sp_inat), ' species)'))
plot(world, add = TRUE)
plot(sum_pres, legend = NA, add = TRUE)
dev.off()

pdf('figures/spDist/iNat.pdf', height = 6, width = 8)
plot.new()
legend('center',
       legend = c('presence', 'presence centroid'),
       fill = c('green', 'magenta'))

for(i in 1:nrow(dist)){
  sp_dist <- st_combine(dist[['geom']][i,])
  centroid <- st_centroid(sp_dist)
  
  plot(world, main = sp_inat[i])
  plot(sp_dist, col = 'green', border = NA, add = TRUE)
  plot(centroid, col = 'magenta', pch = 19, add = TRUE)
}
dev.off()

#Comparison for species appearing in both sources

sp_both <- intersect(sp_gm, sp_inat)
pdf('figures/spDist/comp.pdf', height = 6, width = 8)
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
  dist_gm <- st_combine(dist_gm)
  centroid_gm <- st_centroid(dist_gm)
  
  dist_inat <- st_combine(dist[['geom']][inat_idx,])
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
