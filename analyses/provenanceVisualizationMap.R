# Started on 30 August 2024
# By Christophe

# goal of this scrip is to check how many studies have more than one provenance, and other visualization on where the studies were conducted

# Package
library(ggplot2)
library(plotly)
library(RColorBrewer)

# how many provenances per study
provcount <- aggregate(d["provLatLon"], d[c("datasetID", "study", "latbi")], function(x) length(unique(x)))
# check how many datasetIDs don't have provenance data
subby <- unique(d[, c("datasetID", "study", "latbi", "provLatLon")])
# subset down and get the ones with no provenance data
subNA <- subset(subby, provLatLon == "NA NA")
# replace NA NA with NA
subNA$provLatLon[which(subNA$provLatLon == "NA NA")] <- 0.001

# rbind both dfs
provcount2 <- rbind(provcount, subNA) # recovered appropriate n of datasetIDs

unique(provcount2$provLatLon)
vec <- unique(provcount2$provLatLon)[2:length(unique(provcount2$provLatLon))]

# how many have more than one provenances including NAs
countsabove1 <- subset(provcount2, provLatLon %in% vec)
morethan1 <- subset(provcount2, provLatLon > 1)
suby$provLatLon <- as.numeric(suby$provLatLon)

# add column to fit colors in the plot
suby$color <- NA
suby$color[which(suby$provLatLon <1)] <- "NA"
suby$color[which(suby$provLatLon > 1)] <- "Nb of studies"

# plotting the number of studies with more than 1 provenance AND NAs
count <- ggplot(suby, aes(x = provLatLon, fill = color)) +
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
#


warmstratbychilltemp <- subset(d, chillTemp >19)

# transform lat long to numeric (will be deleted once clean coordinate is finished)
warmstrat$provenance.lat <- as.numeric(subbyrespvar$provenance.lat)
warmstrat$provenance.long <- as.numeric(subbyrespvar$provenance.long)
# get rid of nas
warmstratnona <- warmstrat[!is.na(warmstrat$provenance.lat), ]
# Select only 1 entry per provenance
warmstratFormap <- warmstratnona[!duplicated(warmstratnona$provenance.lat), ]
# clean columns not necessary
warmstratFormap2 <- warmstratFormap[, c("datasetID", "provenance.lat", "provenance.long", "continent", "responseVar", "treatment")]

color <- "red"
fig <- plot_ly(
  data = warmstratFormap2,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  marker = list(size = 5, opacity = 0.8),
  color = ~responseVar,
  colors = color,
  text = ~paste("Dataset ID:", datasetID, "<br>ResponseVar:", responseVar),
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


# === === === === === === === === === === #
#### Make a map for provenance trials ####
# === === === === === === === === === === #
# get all the datasetIDs that have multiple provenances in at least 1 of their study
morethan1ids <- unique(morethan1$datasetID)
dfmorethan1 <- subset(d, datasetID %in% morethan1ids)

# remove duplicated locations
f <- dfmorethan1[!duplicated(dfmorethan1$provLatLon),]

# first average provenance  per dataset ID and size by number of different source.population

# then plot all source.population and color code by datasetID 
warmstrat$provenance.lat <- as.numeric(subbyrespvar$provenance.lat)
warmstrat$provenance.long <- as.numeric(subbyrespvar$provenance.long)
# get rid of nas
warmstratnona <- warmstrat[!is.na(warmstrat$provenance.lat), ]
# Select only 1 entry per provenance
warmstratFormap <- warmstratnona[!duplicated(warmstratnona$provenance.lat), ]
# clean columns not necessary
warmstratFormap2 <- warmstratFormap[, c("datasetID", "provenance.lat", "provenance.long", "continent", "responseVar", "treatment")]

color <- "red"
fig <- plot_ly(
  data = warmstratFormap2,
  type = 'scattergeo', 
  mode = 'markers',
  lat = ~provenance.lat,
  lon = ~provenance.long,
  marker = list(size = 5, opacity = 0.8),
  color = ~responseVar,
  colors = color,
  text = ~paste("Dataset ID:", datasetID, "<br>ResponseVar:", responseVar),
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


