# Started on 30 August 2024
# By Christophe

# goal of this scrip is to check how many studies have more than one provenance, and other visualization on where the studies were conducted

# Package
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(viridisLite)

# run cleanall
source("/Users/christophe_rouleau-desrochers/github/egret/analyses/cleaning/cleanall.R")

#start by subsetting down to the studies that have provenances
d$idspp <- paste(d$datasetIDstudy, d$latbi, sep = "_")

provnona <- subset(d, provLatLon != "NA NA")

# how many provenances per study
provcount <- aggregate(provnona["provLatLon"], provnona[c("idspp")], function(x) length(unique(x)))

# remove duplicates
nrow(provcount[!duplicated(provcount$datasetIDstudy),])


# # # make some checks
test <- subset(provcount, provLatLon == 1)
nrow(test)

# check how many datasetIDs don't have provenance data
subby <- unique(d[, c("idspp", "provLatLon")])
# subset down and get the ones with no provenance data
subNA <- subset(subby, provLatLon == "NA NA")
# replace NA NA with NA
subNA$provLatLon[which(subNA$provLatLon == "NA NA")] <- 0

# rbind both dfs
provcount2 <- rbind(provcount, subNA) # recovered appropriate n ofdatasetIDs
nrow(provcount2)
# vec <- unique(provcount2$provLatLon)[2:length(unique(provcount2$provLatLon))]

# keep only 1 entry per datasetIDstudy since we want to know how many provenances/datasetIDstudy
# test <- provcountnodup[!duplicated(provcountnodup$datasetIDstudy),]
unique(test$provLatLon)
# morethan1 <- subset(provcount2, provLatLon > 1)
provcount2$provLatLon <- as.numeric(provcount2$provLatLon)

# add column to fit colors in the plot
provcount2$color <- NA
provcount2$color[which(provcount2$provLatLon < 1)] <- "NA provenance"
provcount2$color[which(provcount2$provLatLon == 1)] <- "1 provenance"
provcount2$color[which(provcount2$provLatLon > 1)] <- "More than 1 provenance"

# plotting the number of studies with more than 1 provenance, 1 prov AND NAs
count <- ggplot(provcount2, aes(x = provLatLon, fill = color)) +
  geom_histogram(binwidth = 1) +
  labs(title = "", x = "Number of provenances", y= "count datasetID X study X spp")+
  scale_color_manual() +
  theme_minimal() 
count
ggsave("figures/provenanceCount.jpeg", count)

# plotting only the ones with NAs 
count <- ggplot(provcount, aes(x = provLatLon, fill = color)) +
geom_histogram(binwidth = 1) +
  labs(title = "", x = "Number of provenances", y= "Studies count")+
  scale_color_manual()+
  theme_minimal()
count
ggsave("figures/provenanceCount.jpeg", count)
#### make map with color subsetting by most common treatments ####

# === === === === === === === === === #
#### Map for treatment x respVar ####
# === === === === === === === === === #

# quick check of which treatment happens the most often (for now need to run clean treatments!)
treatment_counts <- table(d$treatment)
treatment_df <- data.frame(treatment = names(treatment_counts), Frequency = as.numeric(treatment_counts))
treatment_df <- treatment_df[order(-treatment_df$Frequency), ]

# alright for now the most common treatments are :
vect <- treatment_df$treatment[1:6]

# Subset down by these treatments
subbytreat <- d[d$treatment %in% vect, ]

# check which are the most common resp var
respvar_counts <- table(d$responseVar)
respvar_df <- data.frame(respvar = names(respvar_counts), Frequency = as.numeric(respvar_counts))
respvar_df <- respvar_df[order(-respvar_df$Frequency), ]

vecr <- respvar_df$respvar[1:8]
# subset for the 20 most common respvar and by the 6 most common treatments
subbyrespvar <- subbytreat[subbytreat$responseVar %in% vecr, ]

# === === === === === === === === === #
### Map! ###
# === === === === === === === === === #
# transform lat long to numeric (will be deleted once clean coordinate is finished)
subbyrespvar$provenance.lat <- as.numeric(subbyrespvar$provenance.lat)
subbyrespvar$provenance.long <- as.numeric(subbyrespvar$provenance.long)
# get rid of nas
no.na.values <- subbyrespvar[!is.na(subbyrespvar$provenance.lat), ]
# Select only 1 entry per provenance
df.4.map <- no.na.values[!duplicated(no.na.values$provenance.lat), ]
# clean columns not necessary
df.4.map <- df.4.map[, c("datasetID", "provenance.lat", "provenance.long", "continent", "responseVar", "treatment")]


# set color
colors <- c("#66C2A5", "#E5C494", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#FC8D62", "#B3B3B3")
fig <- plot_ly(
  data = df.4.map,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  marker = list(size = 5, opacity = 0.8),
  color = ~responseVar,
  colors = colors,
  text = ~paste("Dataset ID:", datasetID, "<br>ResponseVar:", responseVar),
  hoverinfo = "text"
)

# Set map layout
fig <- fig %>% layout(
  title = "Locations of study color coded by responsVar",
  geo = list(
    projection = list(type = "natural earth"),
    showland = TRUE,
    landcolor = "rgb(243, 243, 243)"
  )
)
fig

# === === === === === === === === === === === #
#### Make a map and color code by perc germ ####
# === === === === === === === === === === === #
respvarmap <- FALSE
if(respvarmap){
  
# subset down to studies with perc germ 
PG_lang <- subset(d, responseVar == "percent.germ")

# remove rows with provenance lat NA
PG_lang2 <- PG_lang[!is.na(PG_lang$provenance.lat), ]

# aggregate by paperstudyID + Species
PG_lang$responseValue <- as.numeric(PG_lang$responseValue)
PG_langag <- aggregate(x=PG_lang$responseValue, 
                 by=list(PG_lang$datasetIDstudy, PG_lang$species, 
                         PG_lang$provenance.lat, PG_lang$provenance.long), 
                 FUN=mean, na.action=na.omit)
# change colnames
colnames(PG_langag) <- c("datasetIDstudy", "species", "provenance.lat", "provenance.long","responseValue")
# remove NAs 
PG_langag2map <- PG_langag[!is.na(c(PG_langag$responseValue, PG_langag$provenance.lat)), ] # to check not sure if that works

# problem with acosta13, so ill just remove all values >100
subset(PG_langag2map, responseValue>100)
PG_langag2map<- PG_langag2map[!PG_langag2map$responseValue>101,]

# add only datasetID
PG_langag2map$datasetID <- sub("exp\\d+", "", PG_langag2map$datasetIDstudy)

str(PG_langag2map)
unique(PG_langag2map$datasetID)
fig <- plot_ly(
  data = PG_langag2map,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  marker = list(size = 5, opacity = 0.8),
  color = ~responseValue,
  colors = "Oranges",
  text = ~paste("DatasetIDStudy:", datasetIDstudy, "<br>ResponseValue:", responseValue),
  hoverinfo = "text"
)

# Set map layout
fig <- fig %>% layout(
  title = "Locations of study color coded by responsVar",
  geo = list(
    projection = list(type = "natural earth"),
    showland = TRUE,
    landcolor = "rgb(243, 243, 243)"
  )
)
fig

}
# === === === === === === === === === === === #
#### Make a map for warm stratification ####
# === === === === === === === === === === === #
vec <- unique(d$treatmentCor[grepl("warm", d$treatmentCor)])
warmstrat <- subset(d, treatmentCor %in% vec)
# source victor's file I am not allowed to use
# the not condensed one details wether 
source("analyseSeedCues/summarizeStrat.R")

unique(d$stratSequence_condensed)

# get a smaller df to play around
strat <- d[, c("datasetID", "provenance.lat", "provenance.long", "provLatLon", "stratSequence_condensed")]
# add new cols
strat$coldstrat <- ifelse(grepl("cold", strat$stratSequence_condensed, ignore.case = TRUE), "Y", NA)
strat$warmstrat <- ifelse(grepl("warm", strat$stratSequence_condensed, ignore.case = TRUE), "Y", NA)

# then add a single colum with 3 classes: warmstrat, coldstrat, no strat. wait not now
strat$stratclasses <- NA
strat$stratclasses[which(strat$warmstrat == "Y" & strat$coldstrat == "Y")] <- "both"
strat$stratclasses[which(strat$warmstrat == "Y" & is.na(strat$coldstrat))] <- "warm"
strat$stratclasses[which(is.na(strat$warmstrat) & strat$coldstrat == "Y")] <- "cold"
strat$stratclasses[which(is.na(strat$warmstrat) & is.na(strat$coldstrat))] <- "nostrat"

# assign them colors 
strat$colmap <- NA
strat$colmap[which(strat$stratclasses == "both")] <- "purple"
strat$colmap[which(strat$stratclasses == "warm")] <- "#ff3300"
strat$colmap[which(strat$stratclasses == "cold")] <- "#0066ff"
strat$colmap[which(strat$stratclasses == "nostrat")] <- "#aaaaaa"

# convert prov to numeric
strat$provenance.lat <- as.numeric(strat$provenance.lat)
strat$provenance.long <- as.numeric(strat$provenance.long)

# get rid of nas
stratnona <- subset(strat, provLatLon != "NA NA")

# Select only 1 entry per provenance
stratFormap <- stratnona[!duplicated(stratnona$provLatLon), ]


stratmap <- plot_ly(
  data = stratnona,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  text = ~paste("datasetID:", datasetID, "<br>Strat:", stratclasses),
  hoverinfo = "text",
  marker = list(
    size = 5,  
    sizemode = "area",
    opacity = 0.8,
    color = ~I(colmap)
    # colorscale = unique(stratnona$colmap)
  )
)
# Set map layout
stratmap <- stratmap %>% layout(
  title = "Loc of studies of warm, cold, both and no strat treatments",
  geo = list(
    projection = list(type = "natural earth"),
    showland = TRUE,
    landcolor = "rgb(243, 243, 243)"
  )
)
stratmap

# make a small one with just warm strat
warmstrat <- subset(stratnona, stratclasses == "warm")

warmstratmap <- plot_ly(
  data = warmstrat,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  text = ~paste("datasetID:", datasetID, "<br>Strat:", stratclasses),
  hoverinfo = "text",
  marker = list(
    size = 5,  
    sizemode = "area",
    opacity = 0.8,
    color = ~I(colmap)
    # colorscale = unique(stratnona$colmap)
  )
)
# Set map layout
warmstratmap <- warmstratmap %>% layout(
  title = "Loc of studies of warm strat treatments",
  geo = list(
    projection = list(type = "natural earth"),
    showland = TRUE,
    landcolor = "rgb(243, 243, 243)"
  )
)
warmstratmap



# === === === === === === === === === === #
#### Make a map for provenance trials ####
# === === === === === === === === === === #
# get all the datasetIDs that have multiple provenances in at least 1 of their study
morethan1ids <- unique(morethan1$datasetID)
dfmorethan1 <- subset(d, datasetID %in% morethan1ids)

# remove duplicated locations
morethan1nona <- dfmorethan1[!duplicated(dfmorethan1$provLatLon),]
morethan1nona$provenance.lat <- as.numeric(morethan1nona$provenance.lat)
morethan1nona$provenance.long <- as.numeric(morethan1nona$provenance.long)

# grab the cols I want
provbycolor <- morethan1nona[,c("datasetIDstudy", "provenance.lat", "provenance.long", "provLatLon")]

# first average provenance per dataset ID and size by number of different source.population
provcountlat <- aggregate(provbycolor["provenance.lat"], provbycolor["datasetIDstudy"], function(x) mean(x))
provcountlong <- aggregate(provbycolor["provenance.long"], provbycolor["datasetIDstudy"], function(x) mean(x))

# count number of provenance per datasetIDstudy
provbycolor$provcount <- 1
count <- aggregate(provbycolor["count"], provbycolor["datasetIDstudy"], function(x) sum(x))

### need to merge them together, but not now
merged1 <- merge(provcountlat, provcountlong, by = "datasetIDstudy")
merged2 <- merge(merged1, count, by = "datasetIDstudy")

# get rid of nas
provbysize <- merged2[!is.na(merged2$provenance.lat), ]

# set colors
nbysize <- nrow(provbysize)
colors <- colorRampPalette(brewer.pal(12, "Paired"))(nbysize)
provbysize$color <- colors[as.numeric(as.factor(provbysize$datasetIDstudy))]

# scale count larger
provbysize$countscaled <- provbysize$count*5

fig <-plot_ly(
  data = provbysize,
  type = 'scattergeo',
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  marker = list(
    size = ~countscaled,  # or ~countscaled if you've pre-scaled
    sizemode = "area",          # ensures perceptual scaling
    # sizeref = 2.0 * max(provbysize$count, na.rm = TRUE) / (40^2),  # adjust 40 as max size
    sizemin = 2,
    opacity = 0.8,
    color = ~datasetIDstudy,
    colorscale = 'Viridis'  # optional or use your `colors`
  ),
  text = ~paste("datasetIDstudy:", datasetIDstudy, "<br>Provenance count:", count),
  hoverinfo = "text"
)

# Set map layout
fig <- fig %>% layout(
  title = "Locations of study using warm strat treatments",
  geo = list(
    projection = list(type = "natural earth"),
    showland = TRUE,
    landcolor = "rgb(243, 243, 243)"
  )
)
fig

# === === === === === === === === === === === === === #
##### All provenances and color code by dataset ID #####
# === === === === === === === === === === === === === #
# assign colors to each of the datasetID
# Create 48 unique colors from a qualitative palette

n <- length(unique(provbycolor))
colors <- colorRampPalette(brewer.pal(12, "Paired"))(n)
provbycolor$color <- colors[as.numeric(as.factor(provbycolor$datasetID))]

# make the map!
provbycolormap <- plot_ly(
  data = provbycolor,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  marker = list(size = 5, opacity = 0.8),
  color = ~datasetID,
  colors = colors,
  text = ~paste("Dataset ID:", datasetID, "<br>datasetID:", datasetID),
  hoverinfo = "text"
)

# Set map layout
provbycolormap <- provbycolormap %>% layout(
  title = "All provenance color-coded by datasetID",
  geo = list(
    projection = list(type = "natural earth"),
    showland = TRUE,
    landcolor = "rgb(243, 243, 243)"
  )
)
provbycolormap

# === === === === === === === === === === === #
#### Phylogenic tree X multiple provenances ####
# === === === === === === === === === === === #
provcount4phy <- aggregate(provnona["provLatLon"], provnona[c("datasetID", "datasetIDstudy", "latbi")], function(x) length(unique(x)))

# select only rows with more than 1 provenance
morethan1 <- subset(provcount4phy, provLatLon != "1")

# load tree
library(phytools)
library(taxize)
library(ape)

egretTree <- read.tree("output/egretPhylogenyFull.tre")

# get a vector of egretnames
allspp <- egretTree$tip.label

# get spp with multiple prov
vecspp <- unique(morethan1$latbi)

# now color code by dataset ID
vecids <- unique(morethan1$datasetID)

# set color palet
colids <- viridis(length(vecids))

# mach both
tipindex <- match(vecspp, allspp)

# categorize them into splicing and non-splicing for color purpose
status <- ifelse(allspp %in% vecspp, "multiple", "not multiple")
status <- factor(status, levels = c("multiple", "not multiple"))
colprov <- c("blue", "black")[status]

# assign col datasetID with multiple provs
datasetIDs <- unique(morethan1$datasetID)
dataset_colors <- setNames(rainbow(length(datasetIDs)), datasetIDs)

# Match species to tip labels in the tree
tip_labels <- egretTree$tip.label
tip_datasetID <- rep(NA, length(tip_labels))
species_in_df <- unique(morethan1$latbi)

# fill tip_datasetID with datasetIDs from df
for (i in 1:nrow(morethan1)) {
  species <- species_in_df[i]
  if (species %in% tip_labels) {
    tip_index <- which(tip_labels == species)
    tip_datasetID[tip_index] <- morethan1$datasetID[i]
  }
}

# Create color vector for tip dots based on datasetID
colids <- ifelse(!is.na(tip_datasetID), dataset_colors[tip_datasetID], "transparent")

# Plot tree and color-coded tip dots
pdf("figures/egretTreeXprovenance.pdf", width = 20, height = 80)
plot(egretTree, cex = 1.5, tip.color = colprov)
tiplabels(pch = 19, col = colids, adj = 105, cex = 3)
legend("topright", legend = names(dataset_colors), col = dataset_colors, pch = 19, cex = 0.8)
dev.off()
