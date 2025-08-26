# Started on 30 August 2024
# By Christophe

# goal of this scrip is to check how many studies have more than one provenance

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 150) 

# Package
library(ggplot2)
library(plotly)
library(RColorBrewer)

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

# read csv
d <- read.csv("output/egretclean.csv", header = TRUE, sep = ",")

# how many provenances per study
nrow(subset(d, is.na(provenance.lat)))
provenance_count <- aggregate(provLatLon  ~ datasetID, data = d, function(x) length(unique(x)))
# how many have more than one provenances
suby <- subset(provenance_count, provLatLon > 1)
# vector of datasetID with multiple provenances
vec <- suby$datasetID
# subset datasetID with multiple provenances
subset_df <- d[d$datasetID %in% vec, ]
# plotting the number of studies with more than 1 provenance
count <- ggplot(suby, aes(x = provLatLon)) +
  geom_histogram(binwidth = 1, color = "black", fill = "salmon") +
  labs(title = "", x = "Number of provenances", y= "DatasetID count") +
  theme_minimal()
count

#### make map with color subsetting by most common treatments ####

# quick check of which treatment happens the most often (for now need to run clean treatments!)
treatment_counts <- table(d$treatment)
treatment_df <- data.frame(treatment = names(treatment_counts), Frequency = as.numeric(treatment_counts))
treatment_df <- treatment_df[order(-treatment_df$Frequency), ]
head(treatment_df) 

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

# map! 

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

#### Make a map and color code by perc germ ####
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
