#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Preliminary Examination on OEGRES Cleaning
# By Hoai Huong -- March 16
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Housekeeping
setwd("~/GitHub/oegres")
rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

### Libraries
library(sf)
library(mapview)
library(ggplot2)
library(dplyr)
library(tidyr)
library(utils)

### Import data##############################
oegres <- read.csv("input/raw_oegres.csv")

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
#ggsave(plot=data_inspection, filename="cleaning/preliminary_HH/overview.png", width = 15, height = 10, limitsize = FALSE)

### #Studies
studies <- unique(oegres[,c("datasetID","entered.by")]) #~174
check <- unique(oegres[,c("datasetID","species")]) #~271

### Inspecting the genus - species - variety + woody/crop##########
  # Make a reference list of all the names
unique(oegres$genus) # ~137
unique(paste(oegres$genus, oegres$species)) # ~ 243-245
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

### Table of treatments (raw)
chemical <- unique(oegres$chemical)
chemical <- chemical[!(chemical %in% c("NA ClO - 10min", "NA ClO - 5min", "NA ClO - 60min", "NA ClO - 90min", "NA ClO - 120min", "NA ClO - 180min", "control"))]
chemical <- chemical[!is.na(chemical)]
chemical <- append(chemical, c("NaClO"))
chemical_df <- data.frame(treatment = "chemical", range = chemical)
chill <- unique(oegres$chill.temp)
chill <- chill[!(chill %in% c("45050"))]
chill <- chill[!is.na(chill)]
chill <- append(chill, c("4-5"))
chill_df <- data.frame(treatment = "stratification", range = chill)
germ <- unique(oegres$germ.temp)
germ <- germ[!(germ %in% c("44696","44854","44727","44859","4.8170000000000002","1.77","9.7330000000000005","14.714", "19.957999999999998",
                           "24.873999999999999", "29.855", "34.835999999999999", "3.1379999999999999", "5.4450000000000003",
                           "7.6109999999999998", "9.8119999999999994", "11.552", "16.876999999999999", "19.256", "21.279",
                           "23.303000000000001", "25.184999999999999", "26.747", "28.486000000000001", "1.768",
                           "4.0250000000000004", "6.3680000000000003", "8.3559999999999999", "10.593", "13.185", "15.741",
                           "18.404","20.213999999999999", "26.001000000000001", "27.954000000000001", "didn't mention"))] #Check MN
germ <- germ[!is.na(germ)]
germ <- append(germ, c("25/15","30/15","30/20","20/10","15/5","15/6","25/10"))
germ_df <- data.frame(treatment = "germination", range = germ)
photo <- unique(oegres$photoperiod)
photo <- photo[!(photo %in% c("0.3444444444444445"))]
photo <- photo[!is.na(photo)]
photo <- append(photo, c("alternating 8/16"))
photo_df <- data.frame(treatment = "photoperiod", range = photo)
treatment <- rbind(chemical_df, chill_df, germ_df, photo_df)
write.csv(treatment, "cleaning/preliminary_HH/treatment.csv", row.names = FALSE)