# Started on 30 August 2024
# By Christophe

# goal of this scrip is to check how many studies have more than one provenance
# Package
library(ggplot2)


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

# make map with color subsetting by most common treatments

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
library(plotly)
library(RColorBrewer)

# transform lat long to numeric (will be deleted once clean coordinate is finished)
subbyrespvar$provenance.lat <- as.numeric(subbyrespvar$provenance.lat)
subbyrespvar$provenance.long <- as.numeric(subbyrespvar$provenance.long)
# get rid of nas
no.na.values <- subbyrespvar[!is.na(subbyrespvar$provenance.lat), ]
# Select only 1 entry per provenance
df.4.map <- no.na.values[!duplicated(no.na.values$provenance.lat), ]
# clean columns not necessary
df.4.map <- df.4.map[, c("datasetID", "provenance.lat", "provenance.long", "continent", "responseVar", "treatment")]
head(df.4.map)
dput(df.4.map)

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



