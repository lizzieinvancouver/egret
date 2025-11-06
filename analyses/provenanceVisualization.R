# Started on 30 August 2024
# By Christophe

# goal of this scrip is to check how many studies have more than one provenance, and other visualization on where the studies were conducted

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdre", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# packages
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(viridisLite)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ape)
library(scales)

# read egret clean
d <- read.csv("output/egretclean.csv")

# === === === === === === === === === === === === === === === === === ===
# Get a subset of data with multiple provenances ####
# === === === === === === === === === === === === === === === === === ===

#start by subsetting down to the studies that have provenances
d$idspp <- paste(d$datasetIDstudy, d$latbi, sep = "_")

provnona <- subset(d, provLatLon != "NA NA")

# how many provenances per study
provcount <- aggregate(provnona["provLatLon"], provnona[c("idspp")], 
                       function(x) length(unique(x)))

# check how many datasetIDs don't have provenance data
subby <- unique(d[, c("idspp", "provLatLon")])

# subset down and get the ones with no provenance data
subNA <- subset(subby, provLatLon == "NA NA")

# replace NA NA with NA
subNA$provLatLon[which(subNA$provLatLon == "NA NA")] <- 0

# rbind both dfs
provcount2 <- rbind(provcount, subNA) 

# change colnames
colnames(provcount2) <- c("idspp", "countProvLatLon") 

# keep only 1 entry per datasetIDstudy since we want to know how many provenance/datasetIDstudy
provcountnodup <- provcount2[!duplicated(provcount2$idspp),]
provcountnodup$countProvLatLon <- as.numeric(provcountnodup$countProvLatLon)

# === === === === === === === === === === === === === === === === === ===
# Make some plots and maps! ####
# === === === === === === === === === === === === === === === === === ===

# add column to fit colors in the plot
provcountnodup$color <- NA
provcountnodup$color[which(provcountnodup$countProvLatLon < 1)] <- "NA provenance"
provcountnodup$color[which(provcountnodup$countProvLatLon == 1)] <- "1 provenance"
provcountnodup$color[which(provcountnodup$countProvLatLon > 1)] <- "More than 1 provenance"

# === === === === === === === === === === === ===  #
##### Hist n of idspp with multiple provenances #####
# === === === === === === === === === === === ===  #

# plotting the number of studies with more than 1 provenance, 1 prov AND NAs
count <- ggplot(provcountnodup, aes(x = countProvLatLon, fill = color)) +
  geom_histogram(binwidth = 1) +
  labs(title = "", x = "Number of provenances", y= "count datasetID X study X spp")+
  scale_color_manual() +
  theme_minimal() 
count
ggsave("figures/provenance/provenanceCount.jpeg", count)

# === === === === === === === === === #
##### Map for treatment x respVar #####
# === === === === === === === === === #
### uncleaned col of treatments
treatment_counts <- table(d$treatmentOverview)

# make df
treatment_df <- data.frame(
  treatmentOverview = names(treatment_counts), 
  Frequency = as.numeric(treatment_counts))
treatment_df <- treatment_df[order(-treatment_df$Frequency), ]

# taking a subset of the 6 most common treatments
vect <- treatment_df$treatmentOverview[1:6]

# Subset down by these treatments
subbytreat <- d[d$treatmentOverview %in% vect, ]

### check which are the most common resp var
respvar_counts <- table(d$responseVar)

# make df
respvar_df <- data.frame(
  respvar = names(respvar_counts), 
  Frequency = as.numeric(respvar_counts))
respvar_df <- respvar_df[order(-respvar_df$Frequency), ]

# take a subset of the 8 most common response variables
vecr <- respvar_df$respvar[1:8]

# subset for the 8 most common respvar and by the 6 most common treatments
subbyrespvar <- subbytreat[subbytreat$responseVar %in% vecr, ]

# get rid of nas
subbyrespvarnona <- subbyrespvar[!is.na(subbyrespvar$provenance.lat), ]

# Select only 1 entry per provenance
respvar4map <- subbyrespvarnona[!duplicated(subbyrespvarnona$provenance.lat), ]

# set color
colors <- c("#66C2A5", "#E5C494", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#FC8D62", "#B3B3B3")

# make interactive
fig <- plot_ly(
  data = respvar4map,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  marker = list(size = 5, opacity = 0.8),
  color = ~responseVar,
  colors = colors,
  text = ~paste("Dataset ID:", datasetID, "<br>ResponseVar:", responseVar),
  hoverinfo = "text"
) %>% 
  layout(
  title = "Locations of study color coded by responsVar",
  geo = list(
    projection = list(type = "natural earth"),
    showland = TRUE,
    landcolor = "rgb(243, 243, 243)"
  )
  )

fig

# === === === === === === === === === === === === === #
##### Treatment count per idspp with >1 provenance #####
# === === === === === === === === === === === === === #
# Get a subset of idspp with only >1 provenance
morethan1 <- subset(provcountnodup, countProvLatLon > 1)
vec <- morethan1$idspp
idmultipleprov <- subset(d, idspp %in% vec)

# save this csv
write.csv(idmultipleprov, "output/moreThan1Prov.csv", row.names = FALSE)

treatXprov <- aggregate(treatmentOverview ~ idspp + provLatLon,
          idmultipleprov,
          FUN = function(x) length(unique(x)))

#open graph device
jpeg("figures/provenance/treatmentOverview.jpeg", width=800, height=600, units = "px", quality=300)
hist(treatXprov$treatmentOverview, ,
     xlab = "Number of unique treatments",
     ylab = "Count", 
     main = "N of unique treatment per idspp of multiple provenances")
dev.off()

# grab the idspp with more than 1 treatment
vec2 <- unique(treatXprov$idspp[which(treatXprov$treatmentOverview > 1)])
treatdf <- subset(d, idspp %in% vec2)

treatdfnodup <- treatdf[!duplicated(treatdf$idspp),]
ntreatperstudy <- aggregate(idspp ~ treatmentOverview, 
                            treatdfnodup,
          FUN = function(x) length(unique(x)))
# plot!
ggplot(ntreatperstudy, aes(x = treatmentOverview, y = idspp)) +
  geom_col() +
  labs(x = "Treatment", y = "Dataset ID Study value") +
  coord_flip()
ggsave("figures/provenance/nidsppPerTreatment.pdf", width = 5, height = 20)

# === === === === === === === === === === === #
#### Map color coded by perc germ ####
# === === === === === === === === === === === #
respvarmap <- TRUE
if(respvarmap){
  
# subset down to studies with perc germ 
PG_lang <- subset(d, responseVar == "percent.germ")

# remove rows with provenance lat NA
PG_lang2 <- PG_lang[!is.na(PG_lang$provenance.lat), ]

# aggregate by paperstudyID + Species
PG_lang$responseValue <- as.numeric(PG_lang$responseValue)
PG_langag <- aggregate(x=PG_lang$responseValue, 
                 by=list(PG_lang$idspp, PG_lang$provenance.lat, PG_lang$provenance.long), 
                 FUN=mean, na.action=na.omit)

# change colnames
colnames(PG_langag) <- c("idspp", "provenance.lat", "provenance.long","responseValue")

# remove NAs 
PG_langag2map <- PG_langag[!is.na(PG_langag$responseValue), ]

# add only datasetID
PG_langag2map$datasetID <- sub("exp.*", "", PG_langag2map$idspp)

# interactive map
fig <- plot_ly(
  data = PG_langag2map,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  marker = list(size = 5, opacity = 0.8),
  color = ~responseValue,
  colors = "Oranges",
  text = ~paste("idspp:", idspp, "<br>ResponseValue:", responseValue),
  hoverinfo = "text"
) %>% 
  layout(
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
#### Map for warm stratification ####
# === === === === === === === === === === === #
vec <- unique(d$treatmentOverview[grepl("warm", d$treatmentOverview)])
warmstrat <- subset(d, treatmentOverview %in% vec)

# source victor's file 
source("analyseSeedCues/summarizeStrat.R")

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

# get rid of nas
stratnona <- subset(strat, provLatLon != "NA NA")

# change NA to no strat because it messes up aggregate function below
stratnona$stratSequence_condensed[which(is.na(stratnona$stratSequence_condensed))] <- "nostrat"

# double check that all no strat + cold or warm are within strat studies
sub <- stratnona$datasetID[which(stratnona$stratSequence_condensed == "nostrat")]

# collapse to keep only one start sequence per datasetID
aggrprov <- aggregate(stratSequence_condensed ~ datasetID + provenance.lat + provenance.long, 
                      data = stratnona, 
                      FUN = function(i) paste0(i, collapse="_")) 

# create a third column that simplifies the previous one
aggrprov$stratclass2 <- aggrprov$stratSequence_condensed

aggrprov$stratclass2[which(grepl("warm", aggrprov$stratSequence_condensed) & 
                             grepl("cold", aggrprov$stratSequence_condensed))] <- "both"
aggrprov$stratclass2[grepl("warm", aggrprov$stratSequence_condensed, ignore.case = TRUE) &
                       !grepl("cold", aggrprov$stratSequence_condensed, ignore.case = TRUE)] <- "warm"
aggrprov$stratclass2[grepl("cold", aggrprov$stratSequence_condensed, ignore.case = TRUE) &
                       !grepl("warm", aggrprov$stratSequence_condensed, ignore.case = TRUE)] <- "cold"
aggrprov$stratclass2[grepl("nostrat", aggrprov$stratSequence_condensed, ignore.case = TRUE) &
                       !grepl("cold", aggrprov$stratSequence_condensed, ignore.case = TRUE) &
                       !grepl("warm", aggrprov$stratSequence_condensed, ignore.case = TRUE)] <- "nostrat"
aggrprov$stratclass2[grepl("nostrat", aggrprov$stratSequence_condensed, ignore.case = TRUE) &
                       !grepl("cold", aggrprov$stratSequence_condensed, ignore.case = TRUE) &
                       !grepl("warm", aggrprov$stratSequence_condensed, ignore.case = TRUE)] <- "nostrat"
aggrprov$stratclass2[grep("undefined", aggrprov$stratSequence_condensed)] <- "undefined"

color_map <- c(
  "nostrat" = "#aaaaaa",
  "cold"    = "#0066ff",
  "both"    = "purple",
  "warm"    = "#ff3300",
  "undefined" = "darkgreen"
)

# Now plot
stratmap <- plot_ly(
  data = aggrprov,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  text = ~paste("datasetID:", datasetID, "<br>Strat:", stratclass2),
  hoverinfo = "text",
  color = ~stratclass2,          
  colors = color_map,              
  marker = list(
    size = 5,
    sizemode = "area"
    # opacity = ~opacityprov
  )
) %>%
  layout(
    title = "Loc of studies of warm, cold, both and no strat treatments",
    geo = list(
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = "rgb(243, 243, 243)"
    ),
    legend = list(
      title = list(text = "<b>Strat Classes</b>"),
      bgcolor = "#FFFFFF",
      bordercolor = "#CCCCCC",
      borderwidth = 1
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
    color = ~I(color_map)
    # colorscale = unique(stratnona$colmap)
  )
) %>% layout(
  title = "Loc of studies of warm strat treatments",
  geo = list(
    projection = list(type = "natural earth"),
    showland = TRUE,
    landcolor = "rgb(243, 243, 243)"
  )
)
warmstratmap

# === === === === === === === === === === #
#### Map for provenance trials ####
# === === === === === === === === === === #
# get all the datasetIDs that have multiple provenances in at least 1 of their study
morethan1ids <- unique(morethan1$idspp)
dfmorethan1 <- subset(d, idspp %in% morethan1ids)

# remove duplicated locations
morethan1nona <- dfmorethan1[!duplicated(dfmorethan1$provLatLon),]
morethan1nona$provenance.lat <- as.numeric(morethan1nona$provenance.lat)
morethan1nona$provenance.long <- as.numeric(morethan1nona$provenance.long)

# grab the cols I want
provbycolor <- morethan1nona[,c("idspp", "provenance.lat", "provenance.long", "provLatLon")]

# first average provenance per dataset ID and size by number of different source.population
provcountlat <- aggregate(provbycolor["provenance.lat"], provbycolor["idspp"], 
                          function(x) mean(x))
provcountlong <- aggregate(provbycolor["provenance.long"], provbycolor["idspp"], 
                           function(x) mean(x))

# count number of provenance per idspp
provbycolor$provcount <- 1
count <- aggregate(provbycolor["provcount"], provbycolor["idspp"], 
                   function(x) sum(x))

### need to merge them together, but not now
merged1 <- merge(provcountlat, provcountlong, by = "idspp")
merged2 <- merge(merged1, count, by = "idspp")

# get rid of nas
provbysize <- merged2[!is.na(merged2$provenance.lat), ]

# set colors
nbysize <- nrow(provbysize)
colors <- colorRampPalette(brewer.pal(12, "Paired"))(nbysize)
provbysize$color <- colors[as.numeric(as.factor(provbysize$idspp))]

# scale count larger
provbysize$countscaled <- provbysize$provcount/3


# Get a world map
world <- ne_countries(scale = "medium", returnclass = "sf")

ggmapprov <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "white") +
  geom_point(data = provbysize,
             aes(x = provenance.long, 
                 y = provenance.lat, 
                 size = provcount,
                 color = idspp),
             alpha = 0.8) +
  scale_size_continuous(
    name = "Provenance Count",
    range = c(1, 5),
    breaks = pretty(provbysize$provcount, n = 4)
  ) +
  scale_color_viridis_d(name = "Dataset ID") +
  labs(
    title = "Averaged Provenance by idspp",
    x = "", y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box = "vertical",           
    legend.text = element_text(size = 8),  
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.5, "lines"), 
    panel.grid.major = element_line(color = "grey90")
  ) +
  guides(
    color = guide_legend(ncol = 1) 
  )
ggmapprov
ggsave("figures/provenance/provenancemapXscaledbysize.jpeg", ggmapprov, width = 12, height = 8, dpi = 300)

# scale count larger
provbysize$countscaled <- provbysize$provcount*1.1
# set colors
n <- length(unique(provbysize$idspp))
colors <- colorRampPalette(brewer.pal(12, "Paired"))(n)
# provbysize$color <- colors[as.numeric(as.factor(provbysize$datasetID))]

plotlymapprov <- plot_ly(
  data = provbysize,
  type = 'scattergeo',
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  color = ~idspp,
  colors = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(provbysize$idspp))),
  marker = list(
    size = ~countscaled,
    sizemode = "area",
    sizemin = 2,
    opacity = 0.8
  ),
  text = ~paste("idspp:", idspp, "<br>Provenance count:", provcount),
  hoverinfo = "text"
) %>%
  layout(
    title = "Locations of study using warm strat treatments",
    geo = list(
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = "rgb(243, 243, 243)"
    )
  )
plotlymapprov

# === === === === === === === === === === === === === #
##### Map for all provenances and color coded by dataset ID #####
# === === === === === === === === === === === === === #
# assign colors to each of the datasetID
# Create 48 unique colors from a qualitative palette
n <- length(unique(provbycolor))
colors <- colorRampPalette(brewer.pal(12, "Paired"))(n)
provbycolor$color <- colors[as.numeric(as.factor(provbycolor$idspp))]

# GG!
ggmapprovbycolor <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "white") +
  geom_point(data = provbycolor,
             aes(x = provenance.long, 
                 y = provenance.lat, 
                 
                 color = idspp),
             size = 2,
             alpha = 0.8) +
  scale_color_viridis_d(name = "idspp") +
  labs(
    title = "Provenances coloured by idspp",
    x = "", y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box = "vertical",           
    legend.text = element_text(size = 8),  
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.5, "lines"), 
    panel.grid.major = element_line(color = "grey90")
  ) +
  guides(
    color = guide_legend(ncol = 1) 
  )
ggmapprovbycolor
ggsave("figures/provenance/provenancemapcoloredIDstudy.jpeg", ggmapprovbycolor, width = 12, height = 8, dpi = 300)

# plotly!
provbycolormap <- plot_ly(
  data = provbycolor,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  marker = list(size = 6, opacity = 0.8),
  color = ~idspp,
  colors = colors,
  text = ~paste("Dataset ID:", idspp),
  hoverinfo = "text"
) %>% 
  layout(
  title = "All provenance color-coded by idspp",
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
provcount4phy <- aggregate(provnona["provLatLon"], 
                           provnona[c("datasetID", "idspp", "latbi")], 
                           function(x) length(unique(x)))

# select only rows with more than 1 provenance
morethan1phy <- subset(provcount4phy, provLatLon != "1")

egretTree <- read.tree("output/egretPhylogenyFull.tre")

# get a vector of egretnames
allspp <- egretTree$tip.label

# get spp with multiple prov
vecspp <- unique(morethan1phy$latbi)

# now color code by dataset ID
vecids <- unique(morethan1phy$datasetID)

# set color palet
colids <- viridis(length(vecids))

# mach both
tipindex <- match(vecspp, allspp)

# categorize them into splicing and non-splicing for color purpose
status <- ifelse(allspp %in% vecspp, "multiple", "not multiple")
status <- factor(status, levels = c("multiple", "not multiple"))
colprov <- c("blue", "black")[status]

# assign col datasetID with multiple provs
datasetIDs <- unique(morethan1phy$datasetID)
dataset_colors <- setNames(turbo(length(datasetIDs)), datasetIDs)

# Match species to tip labels in the tree
tip_labels <- egretTree$tip.label
tip_datasetID <- rep(NA, length(tip_labels))
species_in_df <- unique(morethan1phy$latbi)

# fill tip_datasetID with datasetIDs from df
for (i in 1:nrow(morethan1phy)) {
  species <- species_in_df[i]
  if (species %in% tip_labels) {
    tip_index <- which(tip_labels == species)
    tip_datasetID[tip_index] <- morethan1phy$datasetID[i]
  }
}

# Create color vector for tip dots based on datasetID
colids <- ifelse(!is.na(tip_datasetID), dataset_colors[tip_datasetID], "transparent")

# Plot tree and color-coded tip dots
pdf("figures/provenance/egretTreeXprovenance.pdf", width = 20, height = 80)
plot(egretTree, cex = 1.5, tip.color = colprov)
tiplabels(pch = 19, col = colids, adj = 105, cex = 3)
legend("topright", legend = names(dataset_colors), col = dataset_colors, pch = 19, cex = 0.8)
dev.off()

# === === === === === === === === === === === === === === #
# Scarification: idspp with multiple provs ####
# === === === === === === === === === === === === === === #
vec <- idmultipleprov$idspp
morethan1all <- subset(d, idspp %in% vec)

# subset down to the idspp that don't have NAs in neither of those two columns
scarif <- morethan1all[which(!is.na(morethan1all$scarifTypeSpe) & 
                                 !is.na(morethan1all$scarifTypeGen)),
                       c("idspp", 
                         "provenance.lat", 
                         "provenance.long", 
                         "scarifTypeSpe", 
                         "scarifTypeGen")]

# count how many unique scarifTypeSpe there are per idspp using aggregate
scarifcount <- aggregate(scarifTypeSpe ~ idspp, scarif, 
                         function(x) length(unique(x)))

### for now, only one study has more than 1 scarifTypeSpe

# Histogram!
scarifcount_plot <- ggplot(scarifcount, aes(x = scarifTypeSpe)) +
  geom_histogram(binwidth = 1) +
  labs(title = "", 
       x = "Number of different scarifications", 
       y= "count idspp X scariftype")+
  scale_x_continuous(
    breaks = seq(min(scarifcount$scarifTypeSpe), max(scarifcount$scarifTypeSpe), by = 1),
    labels = label_number(format = 0)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal() 
scarifcount_plot
ggsave("figures/provenance/scarifcount.jpeg", scarifcount_plot, width = 4, height = 4, dpi = 300)

# === === === === === === === === === === === === === === #
##### Stratification: idspp with multiple provs ##### 
# === === === === === === === === === === === === === === #
strat <- morethan1all[which(!is.na(morethan1all$stratSequence_condensed)), 
                      c("idspp", 
                        "provenance.lat", 
                        "provenance.long", 
                        "stratDur_condensed", 
                        "stratSequence_condensed", 
                        "stratTemp_condensed", 
                        "warmStratTemp")]

# count how many temps of strat by idspp
strattempcount <- aggregate(stratTemp_condensed ~ 
                              idspp + provenance.lat + provenance.long, strat, 
                            function(x) length(unique(x)))
# subset only for the ones >1
strattempcountmorethan1 <- subset(strattempcount, stratTemp_condensed > 1)

# Plot stratification temperatures!
strattempcount_plot <- ggplot(strattempcount, aes(x = stratTemp_condensed)) +
  geom_histogram(binwidth = 1) +
  labs(title = "", 
       x = "Number of different strat temp", 
       y= "count idspp X stratTemp_condensed") +
  scale_x_continuous(
    breaks = seq(
      min(strattempcount$stratTemp_condensed), 
      max(strattempcount$stratTemp_condensed), 
      by = 1),
    labels = label_number(format = 0)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal() 
strattempcount_plot
ggsave("figures/provenance/strattempcount.jpeg", strattempcount_plot, width = 4, height = 4, dpi = 300)

# Map strat temp!
strattempcount$countscaled <- strattempcount$stratTemp_condensed*3

# set colors
n <- length(unique(strattempcount$idspp))
stratTemp_plotly <- plot_ly(
  data = strattempcount,
  type = 'scattergeo',
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  color = ~idspp,
  colors = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(strattempcount$idspp))),
  marker = list(
    size = ~countscaled,
    sizemode = "area",
    sizemin = 2,
    opacity = 0.8
  ),
  text = ~paste("idspp:", idspp, "<br>Strat temp count:", stratTemp_condensed),
  hoverinfo = "text"
) %>%
  layout(
    title = "Locations of multiple prov X multiple spp X multiple strat temp",
    geo = list(
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = "rgb(243, 243, 243)"
    )
  )
stratTemp_plotly

# count how many sequences of strat by idspp
stratseqcount <- aggregate(stratSequence_condensed ~ idspp, strat, 
                           function(x) length(unique(x)))
# subset only for the ones >1
stratseqcountmorethan1 <- subset(stratseqcount, stratSequence_condensed > 1)

# Plot stratification sequences!
stratseqcount_plot <- ggplot(stratseqcount, aes(x = stratSequence_condensed)) +
  geom_histogram(binwidth = 1) +
  labs(title = "", 
       x = "Number of different strat temp", 
       y= "count idspp X stratSequence_condensed") +
  scale_x_continuous(
    breaks = seq(
      min(stratseqcount$stratSequence_condensed), 
      max(stratseqcount$stratSequence_condensed), 
      by = 1),
    labels = label_number(format = 0)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal() 
stratseqcount_plot
ggsave("figures/provenance/stratseqcount.jpeg", stratseqcount_plot, width = 4, height = 4, dpi = 300)

# === === === === === === === === === === === === === === #
##### Manipulated germ conditions: idspp with multiple provenances #####
# === === === === === === === === === === === === === === #

# check manipulated germ temp cols
germtemp <- morethan1all[which(!is.na(morethan1all$germTemp)), 
                      c("idspp", 
                        "provenance.lat", 
                        "provenance.long", 
                        "germTemp", 
                        "germDuration")]

# count how many durations of strat by idspp
germTempcount <- aggregate(germTemp ~ idspp + provenance.lat + provenance.long, germtemp, 
                           function(x) length(unique(x)))

# size scale
germTempcount$countscaled <- germTempcount$germTemp*10
# set colors
n <- length(unique(germTempcount$idspp))
colors <- colorRampPalette(brewer.pal(12, "Paired"))(n)

germTemp_plotly <- plot_ly(
  data = germTempcount,
  type = 'scattergeo',
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  color = ~idspp,
  colors = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(germTempcount$idspp))),
  marker = list(
    size = ~countscaled,
    sizemode = "area",
    sizemin = 2,
    opacity = 0.8
  ),
  text = ~paste("idspp:", idspp, "<br>Germ temp count:", germTemp),
  hoverinfo = "text"
) %>%
  layout(
    title = "Locations of multiple prov X multiple spp X multiple germ temp",
    geo = list(
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = "rgb(243, 243, 243)"
    )
  )
germTemp_plotly

# === === === === === === === === === === === === === === #
##### Combo: strat temp, duration and germ temperature #####
# === === === === === === === === === === === === === === #
# check manipulated germ temp cols
comb <- morethan1all[which(!duplicated(morethan1all$idspp)), 
                         c("idspp", 
                           "provenance.lat", 
                           "provenance.long", 
                           "stratTemp_condensed",
                           "stratDur_condensed",
                           "germTemp")]
comb

# empty dataframe to store the number of different start temp and germ temp
df <- data.frame(
  idspp = comb$idspp
)
df1 <- merge(df, 
             strattempcount[, c("idspp", 
                                "provenance.lat", "provenance.long",
                                "stratTemp_condensed")], 
             by = "idspp", all.x = TRUE)

strattempgerm <- merge(df1, 
             germTempcount[, c("idspp", "germTemp")], 
             by = "idspp")

strattempgerm$totalcount <- strattempgerm$stratTemp_condensed + strattempgerm$germTemp

# size scale
strattempgerm$countscaled <- strattempgerm$totalcount*10
# set colors
n <- length(unique(strattempgerm$idspp))
colors <- colorRampPalette(brewer.pal(12, "Paired"))(n)

strattempdurgerm_plotly <- plot_ly(
  data = strattempgerm,
  type = 'scattergeo',
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  color = ~idspp,
  colors = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(strattempgerm$idspp))),
  marker = list(
    size = ~countscaled,
    sizemode = "area",
    sizemin = 0.5,
    opacity = 0.8
  ),
  text = ~paste("idspp:", idspp, "<br>Start temp and germ temp count:", totalcount),
  hoverinfo = "text"
) %>%
  layout(
    title = "Locations of multiple prov per spp X multiple strat temp + germ temp",
    geo = list(
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = "rgb(243, 243, 243)"
    )
  )
strattempdurgerm_plotly

