#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Preliminary Examination on OEGRES Cleaning
# By Hoai Huong -- March 16
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Housekeeping
setwd("C:/Users/17782/Documents/GitHub/oegres")
rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

### Libraries
library("readxl")
library(sf)
library(mapview)
library(ggplot2)

### Import data##############################
folder <- list.dirs(path = "data", full.names = FALSE, recursive = TRUE)
folder <- folder[3:9] # Obtain the folders needed
#Match cols:
  #oegres_DL.xlsx <- read_xlsx("data/oegres_DL.xlsx", sheet = "data_detailed")
  #oegres_TA.xlsx <- read_xlsx(paste("data/", "oegres_TA", "/", "oegres_TA.xlsx", sep = ""), sheet = "data_detailed")
  #oegres_SC.xlsx <- read_xlsx(paste("data/", "oegres_SC", "/", "oegres_SC.xlsx", sep = ""), sheet = "data_detailed")
#oegres <- rbind(oegres_DL.xlsx, oegres_TA.xlsx, oegres_SC.xlsx)
oegres <- data.frame(matrix(data=NA, nrow = 0, ncol = 45))
folder <- folder[1:5]
for(i in 1:length(folder)) {
  file <- list.files(paste("data/", folder[i], sep = ""), pattern = NULL, all.files = TRUE, full.names = FALSE)
  file <- file[3]
  dataframe <- read_xlsx(paste("data/", folder[i], "/", file, sep = ""), sheet = "data_detailed")
  #colnames(oegres) <- colnames(dataframe)
  oegres <- rbind(oegres, dataframe)
  assign(file, dataframe)
}

### Inspecting the genus - species - variety + woody/crop##########
  # Make a reference list of all the names
unique(oegres$genus) # ~94
unique(oegres$species) # ~ 137
unique(oegres$variety) # Rarely specified
  # Crop
unique(oegres$datasetID[which(oegres$crop == "Y")]) # 1 paper -- Should we reject it?
unique(oegres$datasetID[is.na(oegres$crop)]) # 0 papers
  # Woody
unique(oegres$datasetID[which(oegres$woody == "Y")]) # 46 papers
unique(oegres$datasetID[which(oegres$woody == "N")]) # 52 papers
unique(oegres$datasetID[is.na(oegres$woody)]) # 0 papers

### Geography and Time##############################################
  # Germination Year -- Can have more data from Year Collected and Storage Time
unique(oegres[is.na(oegres$provenance.lat) | is.na(oegres$provenance.long),c("datasetID")]) # 16 studies
oegres_map <- oegres
oegres_map$provenance.lat <- as.numeric(oegres_map$provenance.lat)
oegres_map$provenance.long <- as.numeric(oegres_map$provenance.long)
oegres_map <- oegres_map[!is.na(oegres_map$provenance.lat) & !is.na(oegres_map$provenance.long),]
oegres_map <- oegres_map[which(oegres_map$provenance.lat != "NA" & oegres_map$provenance.long != "NA"),]
oegres_map <- st_as_sf(oegres_map, coords = c("provenance.long", "provenance.lat"),  crs = 4326)
oegres_map$year.collected[which(oegres_map$year.collected == "1979.0")] <- "1979"
oegres_map$year.collected[which(oegres_map$year.collected == "1987.0")] <- "1987"
oegres_map$year.collected[which(oegres_map$year.collected == "2019.0")] <- "2019"
oegres_map$year.collected[which(oegres_map$year.collected == "2011.0")] <- "2011"
oegres_map$year.collected[which(oegres_map$year.collected == "2017.0")] <- "2017"
oegres_map$year.collected[which(oegres_map$year.collected == "2013.0")] <- "2013"
oegres_map$year.collected[which(oegres_map$year.collected == "2010.0")] <- "2010"
oegres_map$year.collected[which(oegres_map$year.collected == "2018.0")] <- "2018"
mapview(oegres_map, map.types = "Stamen.Watercolor", zcol = "year.collected") 

### Storage
unique(oegres$storage.type) # Format sync + Differentiate between strat & stored + Is container necessary? -- Missing a lot of info while scraping

### Treatment
unique(oegres$treatment)
unique(oegres$other.treatment) # A lot of parameters -- Fix this; alternating temp
unique(oegres[, c("treatment","other.treatment")]) # A lot of second strat -- Check

### Other columns
unique(oegres[which(oegres$no.indiv.collected != "NA" & !is.na(oegres$no.indiv.collected)),c("datasetID","no.indiv.collected")]) # Only 10 papers
unique(oegres$respvar) # Format sync + Check definitions
unique(oegres[which(is.na(oegres$respvar)),]) # HH's papers -- Will check
unique(oegres$germ.tim.zero) # Can we normalize this?