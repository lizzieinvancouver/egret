# aim of this code is to combine the cleaning work of on Dinara and Hoai Huong 

# Also to determine what datasets are still missing
setwd("~Documents/GitHub/oegres")
rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

### Libraries
library("readxl") # To read Excel files
library(taxize) # To clean species names

### Import data##############################
folder <- list.dirs(path = "data", full.names = FALSE, recursive = TRUE)
folder <- folder[3:9] # Obtain the folders needed

uration### Merge Data##############################
#Match cols:
# DL
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
# TA
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

# SC
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
folder <- folder[1:5]
for(i in 1:length(folder)) {
  file <- list.files(paste("data/", folder[i], sep = ""), pattern = NULL, all.files = TRUE, full.names = FALSE)
  file <- file[3]
  dataframe <- read_xlsx(paste("data/", folder[i], "/", file, sep = ""), sheet = "data_detailed")
  oegres <- rbind(oegres, dataframe)
  assign(file, dataframe)
}
oegres$datasetID <- tolower(oegres$datasetID)
oegres$datasetID[which(oegres$datasetID == "acosta12")] <- "acosta13"
oegres$datasetID[which(oegres$datasetID == "brandel2005")] <- "brandel05"
oegres$datasetID[which(oegres$datasetID == "airi2009")] <- "airi09"
oegres$datasetID[which(oegres$datasetID == "alptekin2002")] <- "alptekin02"
oegres$datasetID[which(oegres$datasetID == "amini2018")] <- "amini18"
oegres$datasetID[which(oegres$datasetID == "pipinus12")] <- "pipinis12"
oegres$datasetID[which(oegres$datasetID == "picciau18")] <- "picciau19"

oegres <- data.frame(oegres)

# General chekcs:

#1. fix typos and minor issues:
(sort(unique(oegres$genus)))
oegres$genus[which(oegres$genus == "Deginia")] <- "Degenia"

unique(oegres$species)

oegres$species[which(oegres$species == "acutifolius L.")] <- "acutifolius"
oegres$species[which(oegres$species == "ammoniacum D.")] <- "ammoniacum"
oegres$species[which(oegres$species == "Amurensis")] <- "amurensis"
oegres$species[which(oegres$species == "aviculare L.")] <- "aviculare"
oegres$species[which(oegres$species == "longiflora var.\r\ntubiformis")] <- "longiflora"
oegres$species[which(oegres$species == "Pagoda")] <- "pagoda"
oegres$species[which(oegres$species == "Sylvestris L.")] <- "sylvestris"

oegres$sp.name <- paste(oegres$genus, oegres$species, sep = "_")

unique(oegres$study)
oegres$study <- gsub(" ","", oegres$study)

unique(oegres$variety) # only 18

unique(oegres$woody) # N, Y, NA, "rhizomatus shrub/bamboo", "succulent" 

unique(oegres$source.population) #192 

unique(oegres$provenance.lat) # character bc includes 3 ranges and some wild decimals
unique(oegres$provenance.long)

unique(oegres$continent)
oegres$continent[which(oegres$continent == "USA")] <- "North America" # 8 and some NA

unique(oegres$no.indiv.collected) # only 13 reported values

unique(oegres$year.collected)
#character bc 4 values entered as ranges, also from 1951-2019

unique(oegres$year.germination)
oegres$year.germination[which(oegres$year.germination == "n/a")] <- "NA" #only 19 report this
#TO CHECK - TRUE? But what is the value?

unique(oegres$storage.type)
oegres$storage.type[which(oegres$storage.type == "laboratory")] <- "room temp"
oegres$storage.type[which(oegres$storage.type == "room conditions")] <- "room temp"
oegres$storage.type[which(oegres$storage.type == "glass bottles, laboratory conditions")] <- "room temp"
oegres$storage.type[which(oegres$storage.type == "dry/refrigerated")] <- "dry refrigeration"
oegres$storage.type[which(oegres$storage.type == "dry refrigerator")] <- "dry refrigeration"

#TO CHECK:
# natural as in outdoors?
# paper bags in dark - room temp?
# controlled environment - not a room?
# is wet and undried the same?
# if just says dry or wet, should we assume at room temp?


unique(oegres$storage.humidity)

unique(oegres$storage.time)
oegres$storage.time[which(oegres$storage.time == "didn't mention")] <- "NA"
# TO CHECK:
#"38 5 h" "38 25h" , days + h or missing decimal?

unique(oegres$storage.temp)
# many ranges or with variance

unique(oegres$chill.temp)
# many ranges or with variance
#TO CHECK: 
# 38, seems high
# what does (25/10)/5/0) mean

unique(oegres$chill.duration)
oegres$chill.duration[which(oegres$chill.duration == "unknown")] <- "NA"
#TO CHECK
# what does "90/30/90" mean
#"Continuous cold stratification" kept in cold whole experiment? How long was experiment?
# is 0 a true zero? 

unique(oegres$germ.temp)
oegres$germ.temp[which(oegres$germ.temp == "unknown")] <- "NA"
oegres$germ.temp[which(oegres$germ.temp == "didn't mention")] <- "NA"
# TO CHECK
# 44696, 44854, 44727,44859
# open air - is this a field study?

unique(oegres$other.treatment)

unique(oegres$photoperiod)
# TO CHECK
#0.3444444444444445
# 0 a true zero? ie 100% dark?
# 0.25

unique(oegres$chemical)
oegres$chemical[which(oegres$chemical == "water")] <- "NA"
# is GA, GA3, GA4 all the same thing?
# 36 diff chemicals

unique(oegres$chemcial.concent)
# some issues with units, g included in some, ppm, nmol/L

unique(oegres$trt.duration)
unique(oegres$scarification)
unique(oegres$scarif.type)

oegres$scarif.type[which(oegres$scarif.type == "sandpaper")] <- "mechanical - sandpaper"
oegres$scarif.type[which(oegres$scarif.type == "mechanical with razor")] <- "mechanical - razor"
oegres$scarif.type[which(oegres$scarif.type == "H2SO4")] <- "chemical - H2SO4"

unique(oegres$soaking)
unique(oegres$soaked.in)
oegres$scarif.type[which(oegres$scarif.type == "water")] <- "H20"
oegres$scarif.type[which(oegres$scarif.type == "Water")] <- "H20"

oegres$scarif.type[which(oegres$scarif.type == "hot H2O - 100C")] <- "H2O 100C"
oegres$scarif.type[which(oegres$scarif.type == "100C water")] <- "H2O 100C"

oegres$scarif.type[which(oegres$scarif.type == "water 35ºC")] <- "H20 35C"
oegres$scarif.type[which(oegres$scarif.type == "35C water")] <- "H20 35C"

oegres$scarif.type[which(oegres$scarif.type == "60C water")] <- "H20 60C"
oegres$scarif.type[which(oegres$scarif.type == "80C water")] <- "H20 80C"
oegres$scarif.type[which(oegres$scarif.type == "5 C water")] <- "H20 5C"

oegres$scarif.type[which(oegres$scarif.type == "water 35ºC")] <- "H20 35C"

#TO CHECK
# what is N, 0 KNO3 - sounds like a control,
# battaglia93	and 97, what is MPa water and a water solution?

unique(oegres$seed.mass.given)
oegres$seed.mass.given[which(oegres$seed.mass.given == "yes")] <- "Y"
oegres$seed.mass.given[which(oegres$seed.mass.given == "no")] <- "N"
oegres$seed.mass.given[which(oegres$seed.mass.given == "Yes")] <- "Y"
oegres$seed.mass.given[which(oegres$seed.mass.given == "No")] <- "N"
oegres$seed.mass.given[which(oegres$seed.mass.given == "1200")] <- "Y"
oegres$seed.mass.given[which(oegres$seed.mass.given == "FALSE")] <- "N"

unique(oegres$respvar)
oegres$respvar[which(oegres$respvar == "germ.speed")] <- "germ.rate"
oegres$respvar[which(oegres$respvar == "rates.germ")] <- "germ.rate"
oegres$respvar[which(oegres$respvar == "germ.rate (days)")] <- "germ.rate"
oegres$respvar[which(oegres$respvar == "germ.proportion")] <- "prop.germ"
oegres$respvar[which(oegres$respvar == "mtg")] <- "mgt"
oegres$respvar[which(oegres$respvar == "mean.germ.time")] <- "mgt"
oegres$respvar[which(oegres$respvar == "MGT")] <- "mgt"

# TO CHECK
# is germ.rt germ rate?
# what is germ.prob

oegres$response <- as.numeric(oegres$response)
range(oegres$response, na.rm =T)

# I assume NG should be NA
oegres$response[which(oegres$response == "NG")] <- "NA"
oegres$response[which(oegres$response == "NA*")] <- "NA"

#TO CHECK what is "NA*"

unique(oegres$error.type)
oegres$error.type[which(oegres$error.type == "mean+/-SE")] <- "SE"
oegres$error.type[which(oegres$error.type == "mean+/-SD")] <- "SD"
oegres$error.type[which(oegres$error.type == "not specified")] <- "not.specified"

# TO CHECK what is xz

unique(oegres$resp.error)

unique(oegres$reps)

unique(oegres$n.per.rep)
unique(oegres$germ.duration)
# TO CHECK
# why are there so many with decimal places? should it not be in days?

unique(oegres$germ.tim.zero)
# TO CHECK - "TRUE"

