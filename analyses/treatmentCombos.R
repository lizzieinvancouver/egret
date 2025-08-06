# Started on 30 July 2025
# By Mao

# Check on the combinations of different treatments (scarification, storage, stratification) and dormancy class for the egret and usda species, so we can know if the species in the same clade are getting the similar treatments

# Package
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(viridisLite)
library(grDevices)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("C:/PhD/Project/egret/analyses")

d <- read.csv("output/egretclean.csv", header = TRUE)

# Add dormancy class to the data
baskegret <- read.csv("output/baskinegretclean.csv")
baskegret <- baskegret[, c("Genus_species","Dormancy.Class")]
d$Genus_species <- paste(d$genus, d$species, sep = " ")
dormancy <- merge(d,baskegret,by = "Genus_species", all.x = TRUE)
dormancy <- dormancy[!duplicated(dormancy$latbi), c("latbi","Dormancy.Class")]
dormancy$Dormancy.Class[is.na(dormancy$Dormancy.Class)] <- "No information"

# Subset species with scarification
scar <- subset(d, scarification == "Y")
scarsp <- scar[!duplicated(scar$latbi), c("latbi","scarification","scarifTypeGen","treatment")]

scarsp$scarifTypeGen[is.na(scarsp$scarifTypeGen)] <- "No information"

# Subset species with stratification
source("analyseSeedCues/summarizeStrat.R")
strat <- subset(d, !is.na(d$stratSequence_condensed))
stratsp <- strat[, c("latbi","stratSequence_condensed")]
stratsp <- stratsp[!duplicated(stratsp[c("latbi", "stratSequence_condensed")]), ]

# Combine similar strat
fluctuating <- c(
  "cold then warm then cold",
  "cold then warm then cold then warm",
  "cold then warm then cold then warm then cold then warm then cold then warm",
  "cold then warm then cold then warm then cold then warm then cold then warm then cold then warm then cold then warm then cold then warm then cold"
)
stratsp$stratSequence_condensed[stratsp$stratSequence_condensed %in% fluctuating] <- "fluctuating temperature"

# Subset species with storage
storage <- subset(d, !is.na(d$storage.type))
storagesp <- storage[!duplicated(storage$latbi), c("latbi","storage.type")]

# Load tree
egretTree <- read.tree("output/egretPhylogenyFull.tre")
allspp <- egretTree$tip.label

# Assign colors to each dormancy class
dorm_classes <- unique(dormancy$Dormancy.Class)
dorm_colors <- setNames(viridis(length(dorm_classes)), dorm_classes)

dormancy$color <- dorm_colors[dormancy$Dormancy.Class]

# Make a named vector for tip coloring
tip_colors <- setNames(dormancy$color, dormancy$latbi)

tip_label_colors <- tip_colors[allspp] 

# Match species to tips
tipindex_storage <- match(storagesp$latbi, allspp)

# Assign a single color and shape
storage_col <- "yellowgreen"
storage_pch <- 21

# Match species to tips
tipindex <- match(scarsp$latbi, allspp)

# Generate color palette for scarifTypeGen
vecids <- unique(scarsp$scarifTypeGen)
colids <- viridis(length(vecids))
names(colids) <- vecids

# Set symbol types (shapes) for scarifTypeGen
shapes <- 21:24
shape_map <- setNames(shapes[1:length(vecids)], vecids)

# Match species to tips
tipindex1 <- match(stratsp$latbi, allspp)

# Generate color palette for stratSequence
vecids1 <- unique(stratsp$stratSequence_condensed)
colids1 <- viridis(length(vecids1))
names(colids1) <- vecids1
shapes1 <- c(21,22,23,1,24,25) 
shape_map1 <- setNames(shapes1[1:length(vecids1)], vecids1)


pdf("figures/egretTreatmentTree.pdf", width = 20, height = 60)
# Plot the tree
plot(egretTree, cex = 1, tip.color = tip_label_colors)
# Add symbols at tips
for (i in seq_along(tipindex_storage)) {
  tip <- tipindex_storage[i]
  tiplabels(pch = storage_pch, tip = tip,
            adj = 70,
            col = "black", bg = storage_col, cex = 1.2)
}

# Add symbols at tips
for (i in seq_along(scarsp$latbi)) {
  sp <- scarsp$latbi[i]
  tip <- tipindex[i]
  type <- scarsp$scarifTypeGen[i]
  
  # Get color and shape
  col <- "palevioletred2"
  pch <- shape_map[type]
  
  # Add tip label symbol
  tiplabels(pch = pch, tip = tip,  adj = 80, col = "black", bg = col, cex = 1.2)
}

# Create legend labels, shapes, and colors
legend_labels <- names(colids)
legend_shapes <- shape_map[legend_labels]

# Count how many treatments per species (for offsetting symbols)
stratsp$rep_count <- as.numeric(ave(stratsp$latbi, stratsp$latbi, FUN = seq_along))

# Base offset for stacking
base_offset <- 5

# Plot strat symbols
for (i in seq_len(nrow(stratsp))) {
  tip <- tipindex1[i]
  type1 <- stratsp$stratSequence_condensed[i]
  rep_id <- stratsp$rep_count[i]
  
  pch <- shape_map1[type1]
  col <- "lightseagreen"
  
  # Adjust adj for stacking
  adj_val <- 90 + (rep_id - 1) * base_offset
  
  tiplabels(pch = pch, tip = tip, adj = adj_val, col = "black", bg = col, cex = 1.2)
}

# scarification legend
legend(x =20, y=20,
       legend = legend_labels,
       pt.bg = "palevioletred2",     
       pch = legend_shapes,        
       pt.cex = 1.2,               
       cex = 1.8,                  
       title = "Scarification Type",
       bty = "n")
# stratification legend
legend(x=20, y=40,
       legend = names(colids1),
       pt.bg = "lightseagreen",
       pch = shape_map1[names(colids1)],
       pt.cex = 1.2,
       cex = 1.8,
       bty = "n",
       text.width = max(strwidth(names(colids1))),
       title = "Stratification Type")
# storage legend
legend(x=20, y=50,
       legend = "storage",
       pt.bg = storage_col,
       pch = storage_pch,
       pt.cex = 1.2,
       cex = 1.8,
       title = "Storage Type",
       bty = "n")
# Dormancy legend
legend("topleft", legend = names(dorm_colors), col = dorm_colors, pch = 15,
       title = "Dormancy Class", pt.cex = 1.5, cex = 1.8, bty = "n")


dev.off()

