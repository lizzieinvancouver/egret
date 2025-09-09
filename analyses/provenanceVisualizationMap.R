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

#start by subsetting down to the studies that have provenances
d$idspp <- paste(d$datasetIDstudy, d$latbi, sep = "_")

provnona <- subset(d, provLatLon != "NA NA")

# how many provenances per study
provcount <- aggregate(provnona["provLatLon"], provnona[c("idspp")], function(x) length(unique(x)))

# look for nrow non duplicated
nrow(provcount[!duplicated(provcount$datasetIDstudy),])

#make some checks
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

# keep only 1 entry per datasetIDstudy since we want to know how many provenance/datasetIDstudy
provcountnodup <- provcount2[!duplicated(provcount2$idspp),]
provcountnodup$provLatLon <- as.numeric(provcountnodup$provLatLon)

# add column to fit colors in the plot
provcountnodup$color <- NA
provcountnodup$color[which(provcountnodup$provLatLon < 1)] <- "NA provenance"
provcountnodup$color[which(provcountnodup$provLatLon == 1)] <- "1 provenance"
provcountnodup$color[which(provcountnodup$provLatLon > 1)] <- "More than 1 provenance"

# plotting the number of studies with more than 1 provenance, 1 prov AND NAs
count <- ggplot(provcountnodup, aes(x = provLatLon, fill = color)) +
  geom_histogram(binwidth = 1) +
  labs(title = "", x = "Number of provenances", y= "count datasetID X study X spp")+
  scale_color_manual() +
  theme_minimal() 
count
ggsave("figures/provenance/provenanceCount.jpeg", count)

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

##### Variation of treatments across #####
morethan1 <- subset(provcountnodup, provLatLon > 1)
vec <- morethan1$idspp
idmultipleprov <- subset(d, idspp %in% vec)

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

# grab the datasetIDstudy with more than 1 treatment
vec2 <- unique(treatXprov$idspp[which(treatXprov$treatmentOverview > 1)])
treatdf <- subset(d, idspp %in% vec2)

treatdfnodup <- treatdf[!duplicated(treatdf$idspp),]
ntreatperstudy <- aggregate(idspp ~ treatmentOverview, 
          treatdf,
          FUN = function(x) length(unique(x)))
# plot!
ntreatperstudyplot <- ggplot(ntreatperstudy, aes(x = treatmentOverview, y = idspp)) +
  geom_col() +
  labs(x = "Treatment", y = "Dataset ID Study value") +
  coord_flip()
ntreatperstudyplot
# save as jpeg!
ggsave("figures/provenance/ntreatperstudy.pdf", ntreatperstudyplot, width = 5, height = 20)

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
vec <- unique(d$treatmentOverview[grepl("warm", d$treatmentOverview)])
warmstrat <- subset(d, treatmentOverview %in% vec)
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

# convert prov to numeric
strat$provenance.lat <- as.numeric(strat$provenance.lat)
strat$provenance.long <- as.numeric(strat$provenance.long)

# get rid of nas
stratnona <- subset(strat, provLatLon != "NA NA")

# drop rows when there is no strat for SOME prov, but that they have either warm or cold
nrow(stratnona)
# work around with a small df
test <- unique(stratnona[,c("stratSequence_condensed", "datasetID", "provenance.lat", "provenance.long", "provLatLon")])

# change NA to no strat because it messes up aggregate function below
test$stratSequence_condensed[which(is.na(test$stratSequence_condensed))] <- "nostrat"

# double check that all no strat + cold or warm are within strat studies
sub <- test$datasetID[which(test$stratSequence_condensed == "nostrat")]

# collapse to keep only one start sequence per datasetID
aggrprov <- aggregate(stratSequence_condensed ~ datasetID + provenance.lat + provenance.long, data = test, FUN = function(i) paste0(i, collapse="_")) 

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

color_map <- c(
  "nostrat" = "#aaaaaa",
  "cold"    = "#0066ff",
  "both"    = "purple",
  "warm"    = "#ff3300",
  "undefined" = "darkgreen"
)

# make a quick check because nnow I have 419 rows in aggrprov, but there are 408 single provenances #### TO CHECK
vec <- aggrprov$datasetID[which(duplicated(aggrprov$provenance.long) & duplicated(aggrprov$provenance.lat)) ]

aggrprov$stratclass2
# Now plot
stratmap <- plot_ly(
  data = aggrprov,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  text = ~paste("datasetID:", datasetID, "<br>Strat:", stratclass2),
  hoverinfo = "text",
  color = ~stratclass2,           # key step: map color to variable
  colors = color_map,              # use your predefined hex color mapping
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
morethan1ids <- unique(morethan1$idspp)
dfmorethan1 <- subset(d, idspp %in% morethan1ids)

# remove duplicated locations
morethan1nona <- dfmorethan1[!duplicated(dfmorethan1$provLatLon),]
morethan1nona$provenance.lat <- as.numeric(morethan1nona$provenance.lat)
morethan1nona$provenance.long <- as.numeric(morethan1nona$provenance.long)

# grab the cols I want
provbycolor <- morethan1nona[,c("idspp", "provenance.lat", "provenance.long", "provLatLon")]

# first average provenance per dataset ID and size by number of different source.population
provcountlat <- aggregate(provbycolor["provenance.lat"], provbycolor["idspp"], function(x) mean(x))
provcountlong <- aggregate(provbycolor["provenance.long"], provbycolor["idspp"], function(x) mean(x))

# count number of provenance per idspp
provbycolor$provcount <- 1
count <- aggregate(provbycolor["provcount"], provbycolor["idspp"], function(x) sum(x))

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
##### All provenances and color code by dataset ID #####
# === === === === === === === === === === === === === #
# assign colors to each of the datasetID
# Create 48 unique colors from a qualitative palette
n <- length(unique(provbycolor))
colors <- colorRampPalette(brewer.pal(12, "Paired"))(n)
provbycolor$color <- colors[as.numeric(as.factor(provbycolor$datasetID))]

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
provcount4phy <- aggregate(provnona["provLatLon"], provnona[c("datasetID", "idspp", "latbi")], function(x) length(unique(x)))

# select only rows with more than 1 provenance
morethan1 <- subset(provcount4phy, provLatLon != "1")

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
dataset_colors <- setNames(turbo(length(datasetIDs)), datasetIDs)

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
pdf("figures/provenance/egretTreeXprovenance.pdf", width = 20, height = 80)
plot(egretTree, cex = 1.5, tip.color = colprov)
tiplabels(pch = 19, col = colids, adj = 105, cex = 3)
legend("topright", legend = names(dataset_colors), col = dataset_colors, pch = 19, cex = 0.8)
dev.off()


# === === === === === === === === === === === === === === #
##### Figure multiple provenances of multiple species#####
# === === === === === === === === === === === === === === #
# subset for duplicated idspp since they will give me if there is multiple provenances of multiple species
manysppprov <- morethan1[duplicated(morethan1$idspp),]
manysppprov <- morethan1[duplicated(morethan1$datasetID),]

# === === === === === === === === === === === === === === #
# How many idspp of multiple provenances have multiple treats ####
# === === === === === === === === === === === === === === #


###### Scarification ######

# for this, grab all the idspp that have multiple provs
vec <- unique(morethan1$idspp)

# grab a subset of the whole egret dataset
morethan1all <- subset(d, idspp %in% vec)

# look at the 2 cleaned scrarification cols
unique(morethan1all$scarifTypeGen)
unique(morethan1all$scarifTypeSpe)

# subset down to the idspp that don't have NAs in neither of those two columns
scarif <- morethan1all[which(!is.na(morethan1all$scarifTypeSpe) & !is.na(morethan1all$scarifTypeGen)), 
                       c("idspp", 
                         "provenance.lat", 
                         "provenance.long", 
                         "scarifTypeSpe", 
                         "scarifTypeGen")]

# count how many unique scarifTypeSpe there are per idspp using aggregate
scarifcount <- aggregate(scarifTypeSpe ~ idspp, scarif, function(x) length(unique(x)))

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


##### Stratification ##### 
# look at all the stratification cols
unique(morethan1all$stratDur_condensed)
unique(morethan1all$stratSequence_condensed)
unique(morethan1all$stratTemp_condensed)
unique(morethan1all$warmStratTemp)

strat <- morethan1all[which(!is.na(morethan1all$stratSequence_condensed)), 
                      c("idspp", 
                        "provenance.lat", 
                        "provenance.long", 
                        "stratDur_condensed", 
                        "stratSequence_condensed", 
                        "stratTemp_condensed", 
                        "warmStratTemp")]

# count how many durations of strat by idspp
stratdurcount <- aggregate(stratDur_condensed ~ idspp + provenance.lat + provenance.long, strat, function(x) length(unique(x)))

# subset only for the ones >1
stratdurcountmorethan1 <- subset(stratdurcount, stratDur_condensed > 1)

# Plot stratification durations!
stratdurcount_plot <- ggplot(stratdurcount, aes(x = stratDur_condensed)) +
  geom_histogram(binwidth = 1) +
  labs(title = "", 
       x = "Number of different strat durations", 
       y= "count idspp X stratDur_condensed") +
  scale_x_continuous(
    breaks = seq(
      min(stratdurcount$stratDur_condensed), 
      max(stratdurcount$stratDur_condensed), 
      by = 1),
    labels = label_number(format = 0)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal() 
stratdurcount_plot
ggsave("figures/provenance/stratdurcount.jpeg", stratdurcount_plot, width = 6, height = 4, dpi = 300)

# MAP strat durations per location
# scale count larger
stratdurcount$countscaled <- stratdurcount$stratDur_condensed*3
# set colors
n <- length(unique(stratdurcount$idspp))
colors <- colorRampPalette(brewer.pal(12, "Paired"))(n)
# provbysize$color <- colors[as.numeric(as.factor(provbysize$datasetID))]

stratDur_plotly <- plot_ly(
  data = stratdurcount,
  type = 'scattergeo',
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  color = ~idspp,
  colors = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(stratdurcount$idspp))),
  marker = list(
    size = ~countscaled,
    sizemode = "area",
    sizemin = 2,
    opacity = 0.8
  ),
  text = ~paste("idspp:", idspp, "<br>Strat duration count:", stratDur_condensed),
  hoverinfo = "text"
) %>%
  layout(
    title = "Locations of multiple prov X multiple spp X multiple strat durations",
    geo = list(
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = "rgb(243, 243, 243)"
    )
  )
stratDur_plotly

# count how many temps of strat by idspp
strattempcount <- aggregate(stratTemp_condensed ~ idspp + provenance.lat + provenance.long, strat, function(x) length(unique(x)))
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
colors <- colorRampPalette(brewer.pal(12, "Paired"))(n)
# provbysize$color <- colors[as.numeric(as.factor(provbysize$datasetID))]
stratTemp_plotly <- plot_ly(
  data = strattempcount,
  type = 'scattergeo',
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  color = ~idspp,
  colors = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(stratdurcount$idspp))),
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
stratseqcount <- aggregate(stratSequence_condensed ~ idspp, strat, function(x) length(unique(x)))
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

##### Manipulated germ conditions #####
# check cols of interest
unique(morethan1all$germTemp)
unique(morethan1all$germDuration)

# check manipulated germ temp cols
germtemp <- morethan1all[which(!is.na(morethan1all$germTemp)), 
                      c("idspp", 
                        "provenance.lat", 
                        "provenance.long", 
                        "germTemp", 
                        "germDuration")]

# count how many durations of strat by idspp
germTempcount <- aggregate(germTemp ~ idspp + provenance.lat + provenance.long, germtemp, function(x) length(unique(x)))

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

