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
library(dplyr)
library(tidyr)
library(utils)

### Import data##############################
folder <- list.dirs(path = "data", full.names = FALSE, recursive = TRUE)
folder <- folder[3:9] # Obtain the folders needed
#Match cols:
  # Deidre
  oegres_DL.xlsx <- read_xlsx("data/oegres_DL.xlsx", sheet = "data_detailed")
  oegres_DL.xlsx$germ.tim.zero <- oegres_DL.xlsx$germ.time.zero
  oegres_DL.xlsx <- subset(oegres_DL.xlsx, select = c("datasetID", "study", "entered.by", "genus", "species", "variety",
                                                      "woody", "crop", "source.population", "provenance.lat", "provenance.long", "provenance.altitude",
                                                      "continent", "no.indiv.collected", "year.collected", "year.germination", "storage.type", "storage.time",
                                                      "storage.humidity", "storage.temp", "treatment", "chill.temp", "chill.duration", "germ.temp",
                                                      "other.treatment", "photoperiod", "chemical", "chemcial.concent", "trt.duration", "scarification",
                                                      "scarif.type", "soaking", "soaked.in", "soaking.duration", "seed.mass.given", "respvar",
                                                      "response", "error.type", "resp.error", "reps", "n.per.rep", "germ.duration",
                                                      "germ.tim.zero", "figure", "Notes"))
  # Tolu
  oegres_TA.xlsx <- read_xlsx(paste("data/", "oegres_TA", "/", "oegres_TA.xlsx", sep = ""), sheet = "data_detailed")
  oegres_TA.xlsx$Notes <- oegres_TA.xlsx$notes
  oegres_TA.xlsx <- subset(oegres_TA.xlsx, select = c("datasetID", "study", "entered.by", "genus", "species", "variety",
                                                      "woody", "crop", "source.population", "provenance.lat", "provenance.long", "provenance.altitude",
                                                      "continent", "no.indiv.collected", "year.collected", "year.germination", "storage.type", "storage.time",
                                                      "storage.humidity", "storage.temp", "treatment", "chill.temp", "chill.duration", "germ.temp",
                                                      "other.treatment", "photoperiod", "chemical", "chemcial.concent", "trt.duration", "scarification",
                                                      "scarif.type", "soaking", "soaked.in", "soaking.duration", "seed.mass.given", "respvar",
                                                      "response", "error.type", "resp.error", "reps", "n.per.rep", "germ.duration",
                                                      "germ.tim.zero", "figure", "Notes"))
  
  # Sophia
  oegres_SC.xlsx <- read_xlsx(paste("data/", "oegres_SC", "/", "oegres_SC.xlsx", sep = ""), sheet = "data_detailed")
  oegres_SC.xlsx$germ.tim.zero <- oegres_SC.xlsx$germ.time.zero
  oegres_SC.xlsx$Notes <- oegres_SC.xlsx$notes
  oegres_SC.xlsx <- subset(oegres_SC.xlsx, select = c("datasetID", "study", "entered.by", "genus", "species", "variety",
                                                      "woody", "crop", "source.population", "provenance.lat", "provenance.long", "provenance.altitude",
                                                      "continent", "no.indiv.collected", "year.collected", "year.germination", "storage.type", "storage.time",
                                                      "storage.humidity", "storage.temp", "treatment", "chill.temp", "chill.duration", "germ.temp",
                                                      "other.treatment", "photoperiod", "chemical", "chemcial.concent", "trt.duration", "scarification",
                                                      "scarif.type", "soaking", "soaked.in", "soaking.duration", "seed.mass.given", "respvar",
                                                      "response", "error.type", "resp.error", "reps", "n.per.rep", "germ.duration",
                                                      "germ.tim.zero", "figure", "Notes"))
oegres <- rbind(oegres_DL.xlsx, oegres_TA.xlsx, oegres_SC.xlsx)
#oegres <- data.frame(matrix(data=NA, nrow = 0, ncol = 45))
folder <- folder[1:5]
for(i in 1:length(folder)) {
  file <- list.files(paste("data/", folder[i], sep = ""), pattern = NULL, all.files = TRUE, full.names = FALSE)
  file <- file[3]
  dataframe <- read_xlsx(paste("data/", folder[i], "/", file, sep = ""), sheet = "data_detailed")
  #colnames(oegres) <- colnames(dataframe)
  oegres <- rbind(oegres, dataframe)
  assign(file, dataframe)
}

### Inspecting how much data we have for each columns##########
oegres_data <- data.frame(matrix(data=NA, nrow = 0, ncol = 45))
colnames(oegres_data) <- colnames(oegres)
oegres_data <- select(oegres_data, -c("datasetID","figure","Notes", "entered.by", "study"))
column_names <- colnames(oegres_data)
for(i in 1:length(column_names)) {
  oegres_check <- oegres[,c("datasetID", column_names[i])]
  oegres_check <- oegres_check[!is.na(oegres_check[2]),]
  oegres_check <- oegres_check[which(oegres_check[2] != "NA"),]
  oegres_data[1,column_names[i]] <- length(unique(oegres_check$datasetID))
}
oegres_data <- gather(oegres_data, column, num_paper)
oegres_data$Important <- "YES"
oegres_data$Important[which(oegres_data$column %in% c("provenance.lat", "provenance.long", "provenance.altitude",
                                                      "year.germination", "storage.humidity", "treatment", "germ.tim.zero"))] <- "NO"
data_inspection <- ggplot(oegres_data, aes(x = column, y = num_paper, fill = Important))+
    geom_col(aes(num_paper, column, fill = Important), width = 0.8)+
    scale_fill_manual(values = c("#000000", "#00CCC6"))+
    geom_vline(xintercept = length(unique(oegres$datasetID)), color = "red", size = 1.5)+
    theme(
      # Set background color to white
      panel.background = element_rect(fill = "white"),
      # Set the color and the width of the grid lines for the horizontal axis
      panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
      # Remove tick marks by setting their length to 0
      axis.ticks.length = unit(0, "mm"),
      # Remove the title for both axes
      axis.title = element_blank(),
      # Only left line of the vertical axis is painted in black
      axis.line.y.left = element_line(color = "black"),
      # Remove labels from the vertical axis
      axis.text.y = element_blank(),
      # But customize labels for the horizontal axis
      axis.text.x = element_text(size = 16)
    )+
    geom_text(
      data = oegres_data,
      aes(0, y = column, label = column),
      hjust = 0,
      nudge_x = 0.3,
      colour = "white",
      size = 5
    )
data_inspection
# ggsave(plot=data_inspection, filename="cleaning/preliminary_HH/overview.png", width = 15, height = 10, limitsize = FALSE)

### Inspecting the genus - species - variety + woody/crop##########
  # Make a reference list of all the names
unique(oegres$genus) # ~138
unique(oegres$species) # ~ 236
unique(oegres$variety) # Rarely specified (~32)
  # Crop
unique(oegres$datasetID[which(oegres$crop == "Y")]) # 1 paper -- Should we reject it?
unique(oegres$datasetID[is.na(oegres$crop)]) # 9 papers
  # Woody
unique(oegres$datasetID[which(oegres$woody == "Y")]) # 81 papers
unique(oegres$datasetID[which(oegres$woody == "N")]) # 77 papers
unique(oegres$datasetID[is.na(oegres$woody)]) # 10 papers

### Geography and Time##############################################
  # Germination Year -- Can have more data from Year Collected and Storage Time
  # altitude -- varies within a study
unique(oegres[is.na(oegres$provenance.lat) | is.na(oegres$provenance.long),c("datasetID")]) # 57 studies
oegres_map <- oegres
oegres_map$provenance.lat[which(oegres_map$provenance.lat == "~34-34.666667")] <- "34.3"
oegres_map$provenance.long[which(oegres_map$provenance.long == "~105.5-106.5")] <- "106"
oegres_map$provenance.long[which(oegres_map$provenance.long == "107.373333 - 107.861389")] <- "106"
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
unique(oegres$storage.type) # Format sync + Differentiate between strat & stored + Is container necessary? -- Missing a lot of info while scraping, also room temp
unique(oegres$storage.time) # Need cleaning -- what to do when there are 2 storage schemes?
unique(oegres$storage.temp) # Need cleaning -- what to do when there are 2 storage schemes? How to account for fluctuation?
unique(oegres$storage.humidity) # Need cleaning

### Chill
unique(oegres$chill.temp) # Split
unique(oegres$chill.duration)

### Germination
unique(oegres$germ.temp) #Germ during strat -- So is chill.temp or germ.temp?
unique(oegres$germ.duration) # A few negative number
unique(oegres$photoperiod) # Small fix for alternating

### Chemical
unique(oegres$chemical)
unique(oegres$chemcial.concent)
unique(oegres$trt.duration)

### Scarification
unique(oegres[which(oegres$scarification == "Y"), c("datasetID")]) # Only around 36 papers
unique(oegres[which(oegres$scarification == "N"), c("datasetID")]) # ~131 papers
unique(oegres$scarif.type) # Sync

### Soaking
unique(oegres[which(oegres$soaking == "Y"), c("datasetID")]) # ~67 papers
unique(oegres[which(oegres$soaking == "N"), c("datasetID")]) # ~112 papers
unique(oegres$soaked.in) # Differentiate between soaking and chem
unique(oegres$soaking.duration)

### Treatment
unique(oegres$treatment)
unique(oegres$other.treatment) # A lot of parameters -- Fix this; alternating temp
unique(oegres[, c("treatment","other.treatment")]) # A lot of second strat -- Check

### Other columns
unique(oegres[which(oegres$no.indiv.collected != "NA" & !is.na(oegres$no.indiv.collected)),c("datasetID","no.indiv.collected")]) # Only 21 papers
unique(oegres$respvar) # Format sync + Check definitions
unique(oegres[which(is.na(oegres$respvar)),]) # HH's papers -- Will check
unique(oegres$germ.tim.zero) # Can we normalize this?