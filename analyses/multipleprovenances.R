# Started on 30 August 2024
# By Christophe

# goal of this scrip is to check how many studies have more than one provenance

# housekeeping
# rm(list=ls())  
# options(stringsAsFactors=FALSE)

# Package
library(ggplot2)
library(dplyr)

# how many provenances per study
provenance_count <- aggregate(provLatLon  ~ datasetID, data = d, function(x) length(unique(x)))
head(provenance_count)
# how many have more than one provenances
suby <- subset(provenance_count, provLatLon > 1)
head(suby)
length(rownames(suby))
# vector of datasetID with multiple provenances
vec <- suby$datasetID
# subset datasetID with multiple provenances
dformap <- subset(d, datasetID == vec)

# plotting the number of studies with more than 1 provenance
count <- ggplot(suby, aes(x = provLatLon)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  theme_minimal()

# create map where dots are studies with multiple id
world_coordinates <- map_data("world")
multipleprovenancesmap <- ggplot() + 
  geom_map(data = world_coordinates, map = world_coordinates,
           aes(map_id = region), 
           color = "white", fill = "lightgray", size = 0.1) +
  expand_limits(x = world_coordinates$long, y = world_coordinates$lat) +
  labs(title = "Position of papers with multiple provenances", x = "Longitude", y = "Latitude")+ 
  geom_point(
    data = dformap,
    aes(provenance.Long, provenance.Lat), color="red", # darkblue
    alpha = 0.7
  ) +
  theme_bw() + 
  theme(legend.position="none")
multipleprovenancesmap
