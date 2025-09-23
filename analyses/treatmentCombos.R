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
library(ape)
library(viridis)

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
stratsp <- stratsp[!duplicated(stratsp[c("latbi", "stratSequence_condensed")]), ]

# Subset species with storage
storage <- subset(d, !is.na(d$storage.type))
storagesp <- storage[!duplicated(storage$latbi), c("latbi","storage.type")]

# Subset species with germ day temperature
germDay <- subset(d, !is.na(d$germTempDay))
germDaysp <- germDay[!duplicated(germDay$latbi), c("latbi","germTempDay")]

# Subset species with germ night temperature
germNight <- subset(d, !is.na(d$germTempNight))
germNightsp <- germNight[!duplicated(germNight$latbi), c("latbi","germTempNight")]

# Combine germDay and germNight into a single dataframe
germTemps <- merge(germDaysp, germNightsp, by = "latbi", all = TRUE)

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
storage_col <- "#586085"
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

# Create color palette
temp_palette <- colorRampPalette(c("white", "#D65B5A"))

# Combine all temps to get a unified range for consistent coloring
all_temps <- c(germTemps$germTempDay, germTemps$germTempNight)
all_temps <- all_temps[!is.na(all_temps)]

# Map temp to color
n_colors <- 50
temp_colors <- temp_palette(n_colors)

# Function to map temp to color
map_temp_to_color <- function(temp) {
  if (is.na(temp)) return(NA)
  index <- round((temp - min(all_temps)) / diff(range(all_temps)) * (n_colors - 1)) + 1
  temp_colors[index]
}

# Assign colors
germTemps$color_day <- sapply(germTemps$germTempDay, map_temp_to_color)
germTemps$color_night <- sapply(germTemps$germTempNight, map_temp_to_color)

# Match species to tree tips
germTemps$tip_index <- match(germTemps$latbi, allspp)

pdf("figures/egretTreatmentTree.pdf", width = 20, height = 20)
# Plot the tree
plot(egretTree, type = 'fan', label.offset = 0.2,
     cex = 0.6, no.margin = TRUE, tip.color = tip_label_colors)
# Add symbols at tips
for (i in seq_along(tipindex_storage)) {
  tip <- tipindex_storage[i]
  tiplabels(pch = storage_pch, tip = tip,
            offset =  70,
            col = "black", bg = storage_col, cex = 1.2)
}

# Add symbols at tips
for (i in seq_along(scarsp$latbi)) {
  sp <- scarsp$latbi[i]
  tip <- tipindex[i]
  type <- scarsp$scarifTypeGen[i]
  
  # Get color and shape
  col <- "#659794"
  pch <- shape_map[type]
  
  # Add tip label symbol
  tiplabels(pch = pch, tip = tip,  offset=75, col = "black", bg = col, cex = 1.2)
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
  col <- "#F5C98E"
  
  # Adjust adj for stacking
  off_val <- 80 + (rep_id - 1) * base_offset
  
  tiplabels(pch = pch, tip = tip, offset =  off_val, col = "black", bg = col, cex = 1.2)
}


# Plot germTempDay symbols
for (i in seq_len(nrow(germTemps))) {
  tip <- germTemps$tip_index[i]
  if (!is.na(tip) && !is.na(germTemps$color_day[i])) {
    tiplabels(pch = 21, tip = tip, offset = 95,
              col = "black", bg = germTemps$color_day[i], cex = 1.2)
  }
}

# Plot germTempNight symbols 
for (i in seq_len(nrow(germTemps))) {
  tip <- germTemps$tip_index[i]
  if (!is.na(tip) && !is.na(germTemps$color_night[i])) {
    tiplabels(pch = 21, tip = tip, offset = 100,
              col = "black", bg = germTemps$color_night[i], cex = 1.2)
  }
}

# scarification legend
legend(x=-70, y=-70,
       legend = legend_labels,
       pt.bg = "#659794",     
       pch = legend_shapes,        
       pt.cex = 1.2,               
       cex = 1.8,                  
       title = "Scarification Type",
       bty = "n")
# stratification legend
legend(x=0, y=140,
       legend = names(colids1),
       pt.bg = "#F5C98E",
       pch = shape_map1[names(colids1)],
       pt.cex = 1.2,
       cex = 1.8,
       bty = "n",
       text.width = max(strwidth(names(colids1))),
       title = "Stratification Type")
# storage legend
legend(x=-130, y=140,
       legend = "storage",
       pt.bg = storage_col,
       pch = storage_pch,
       pt.cex = 1.2,
       cex = 1.8,
       title = "Storage Type",
       bty = "n")
# Dormancy legend
legend(x = -130, y =70, legend = names(dorm_colors), col = dorm_colors, pch = 15,
       title = "Dormancy Class", pt.cex = 1.5, cex = 1.8, bty = "n")
# temperature gradient legend
legend_gradient <- colorRampPalette(c("white", "#D65B5A"))(10)
legend_temp_vals <- round(seq(min(all_temps), max(all_temps), length.out = 10), 1)

legend("bottomright",
       legend = legend_temp_vals,
       fill = legend_gradient,
       border = NA,
       title = "Germ Temp (°C)",
       cex = 1.5,
       bty = "n")
dev.off()

