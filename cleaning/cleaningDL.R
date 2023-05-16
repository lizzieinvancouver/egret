# aim of this code is to combine the cleaning work of on Dinara and Hoai Huong 

# Also to determine what datasets are still missing
if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/oegres")
} else if(length(grep("Lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/projects/others/deirdre/Synchrony")
} else{
  setwd("/home/deirdre/Synchrony") # for midge
}

rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

### Libraries
library("readxl") # To read Excel files
library(taxize) # To clean species names

### Import data##############################
folder <- list.dirs(path = "data", full.names = FALSE, recursive = TRUE)
folder <- folder[3:9] # Obtain the folders needed

### Merge Data##############################
#Match cols:
# DL
egret_DL.xlsx <- read_xlsx("data/egret_DL.xlsx", sheet = "data_detailed")
egret_DL.xlsx$germ.tim.zero <- egret_DL.xlsx$germ.time.zero
egret_DL.xlsx <- subset(egret_DL.xlsx, select = c("datasetID", "study", "entered.by", "genus", "species", "variety",
                                                    "woody", "crop", "source.population", "provenance.lat", "provenance.long", "provenance.altitude",
                                                    "continent", "no.indiv.collected", "year.collected", "year.germination", "storage.type", "storage.time",
                                                    "storage.humidity", "storage.temp", "treatment", "chill.temp", "chill.duration", "germ.temp",
                                                    "other.treatment", "photoperiod", "chemical", "chemcial.concent", "trt.duration", "scarification",
                                                    "scarif.type", "soaking", "soaked.in", "soaking.duration", "seed.mass.given", "respvar",
                                                    "response", "error.type", "resp.error", "reps", "n.per.rep", "germ.duration",
                                                    "germ.tim.zero", "figure", "Notes"))
# TA
egret_TA.xlsx <- read_xlsx(paste("data/", "oegres_TA", "/", "oegres_TA.xlsx", sep = ""), sheet = "data_detailed")
egret_TA.xlsx$Notes <- egret_TA.xlsx$notes
egret_TA.xlsx <- subset(egret_TA.xlsx, select = c("datasetID", "study", "entered.by", "genus", "species", "variety",
                                                    "woody", "crop", "source.population", "provenance.lat", "provenance.long", "provenance.altitude",
                                                    "continent", "no.indiv.collected", "year.collected", "year.germination", "storage.type", "storage.time",
                                                    "storage.humidity", "storage.temp", "treatment", "chill.temp", "chill.duration", "germ.temp",
                                                    "other.treatment", "photoperiod", "chemical", "chemcial.concent", "trt.duration", "scarification",
                                                    "scarif.type", "soaking", "soaked.in", "soaking.duration", "seed.mass.given", "respvar",
                                                    "response", "error.type", "resp.error", "reps", "n.per.rep", "germ.duration",
                                                    "germ.tim.zero", "figure", "Notes"))

# SC
egret_SC.xlsx <- read_xlsx(paste("data/", "oegres_SC", "/", "oegres_SC.xlsx", sep = ""), sheet = "data_detailed")
egret_SC.xlsx$germ.tim.zero <- egret_SC.xlsx$germ.time.zero
egret_SC.xlsx$Notes <- egret_SC.xlsx$notes
egret_SC.xlsx <- subset(egret_SC.xlsx, select = c("datasetID", "study", "entered.by", "genus", "species", "variety",
                                                    "woody", "crop", "source.population", "provenance.lat", "provenance.long", "provenance.altitude",
                                                    "continent", "no.indiv.collected", "year.collected", "year.germination", "storage.type", "storage.time",
                                                    "storage.humidity", "storage.temp", "treatment", "chill.temp", "chill.duration", "germ.temp",
                                                    "other.treatment", "photoperiod", "chemical", "chemcial.concent", "trt.duration", "scarification",
                                                    "scarif.type", "soaking", "soaked.in", "soaking.duration", "seed.mass.given", "respvar",
                                                    "response", "error.type", "resp.error", "reps", "n.per.rep", "germ.duration",
                                                    "germ.tim.zero", "figure", "Notes"))
egret <- rbind(egret_DL.xlsx, egret_TA.xlsx, egret_SC.xlsx)
folder <- folder[1:5]
for(i in 1:length(folder)) {
  file <- list.files(paste("data/", folder[i], sep = ""), pattern = NULL, all.files = TRUE, full.names = FALSE)
  file <- file[3]
  dataframe <- read_xlsx(paste("data/", folder[i], "/", file, sep = ""), sheet = "data_detailed")
  egret <- rbind(egret, dataframe)
  assign(file, dataframe)
}
egret$datasetID <- tolower(egret$datasetID)
egret$datasetID[which(egret$datasetID == "acosta12")] <- "acosta13"
egret$datasetID[which(egret$datasetID == "brandel2005")] <- "brandel05"
egret$datasetID[which(egret$datasetID == "airi2009")] <- "airi09"
egret$datasetID[which(egret$datasetID == "alptekin2002")] <- "alptekin02"
egret$datasetID[which(egret$datasetID == "amini2018")] <- "amini18"
egret$datasetID[which(egret$datasetID == "pipinus12")] <- "pipinis12"
egret$datasetID[which(egret$datasetID == "picciau18")] <- "picciau19"

egret <- data.frame(egret)

# General chekcs:

#1. fix typos and minor issues:
### Clean Species ##############################
egret$species <- tolower(egret$species)
egret_species <- unique(paste(egret$genus, egret$species))
# Use taxize package to inspect
# ref <- gnr_datasources() # Full list of databases available
# fix_names <- gnr_resolve(sci = egret_species, with_canonical_ranks = T)
# egret_species_fix <- unique(fix_names$matched_name2)
# names_changed <- setdiff(egret_species, egret_species_fix)
# names_changed
# Fix#########################################
egret$species[which(egret$genus == "Colutea" & egret$species == "bohsei")] <- "buhsei"
egret$species[which(egret$genus == "Abies" & egret$species == "amabils")] <- "amabilis"
egret$species[which(egret$genus == "Lathyrus" & egret$species == "sativa")] <- "sativus"
egret$species[which(egret$genus == "Carex" & egret$species == "crytolepis")] <- "cryptolepis"
egret$species[which(egret$genus == "Vicia" & egret$species == "bythinica")] <- "bithynica"
egret$species[which(egret$genus == "Penstemon" & egret$species == "commarhenus")] <- "comarrhenus"
egret$species[which(egret$genus == "Asparagus" & egret$species == "acutifolius l.")] <- "acutifolius"
egret$species[which(egret$genus == "Pinus" & egret$species == "sylvestris l.")] <- "sylvestris"
egret$species[which(egret$genus == "Polygonum" & egret$species == "aviculare l.")] <- "aviculare"
egret$species[which(egret$genus == "Dorema" & egret$species == "ammoniacum d.")] <- "ammoniacum"
egret$species[which(egret$genus == "Tradescantia" & egret$species == "ohioensis")] <- "ohiensis"
egret$species[which(egret$genus == "Betula" & egret$species == "albo-sinensis")] <- "albosinensis"

egret$genus[which(egret$genus == "Pterocaryafra" & egret$species == "fraxinifolia")] <- "Pterocarya"
egret$genus[which(egret$genus == "Leontice\r\n" & egret$species == "incerta")] <- "Leontice"
egret$genus[which(egret$genus == "Aanigozanthos" & egret$species == "flavidus")] <- "Anigozanthos"
egret$genus[which(egret$genus == "Deginia" & egret$species == "velebitica")] <- "Degenia"
egret$genus[which(egret$genus == "Lingularia" & egret$species == "sibirica")] <- "Ligularia"
egret$genus[which(egret$genus == "Eucalytpus" & egret$species == "delegatensis")] <- "Eucalyptus"

egret$genus[which(egret$genus == "Jasminus" & egret$species == "fruiticans")] <- "Jasminum"
egret$species[which(egret$genus == "Jasminum" & egret$species == "fruiticans")] <- "fruticans"

egret$variety[which(egret$genus == "Pedicularis" & egret$species == "longiflora var.\r\ntubiformis")] <- "tubiformis"
egret$species[which(egret$genus == "Pedicularis" & egret$species == "longiflora var.\r\ntubiformis")] <- "longiflora"
# Confirm####################################
egret_species <- unique(paste(egret$genus, egret$species))
fix_names <- gnr_resolve(sci = egret_species, with_canonical_ranks = T)
egret_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(egret_species, egret_species_fix) # Confirm this is of length 0
names_changed
### TO CHECK: Echinacea angustifolia, purpurea, pallida -- 3 species in 1 study

egret$species[which(egret$species == "Amurensis")] <- "amurensis"
egret$species[which(egret$species == "aviculare L.")] <- "aviculare"
egret$species[which(egret$species == "longiflora var.\r\ntubiformis")] <- "longiflora"
egret$species[which(egret$species == "Pagoda")] <- "pagoda"
egret$species[which(egret$species == "Sylvestris L.")] <- "sylvestris"

egret$sp.name <- paste(egret$genus, egret$species, sep = "_")

unique(egret$study)
egret$study <- gsub(" ","", egret$study)

unique(egret$variety) # only 18

unique(egret$woody) # N, Y, NA, "rhizomatus shrub/bamboo", "succulent" 

unique(egret$source.population) #192 

unique(egret$provenance.lat) # character bc includes 3 ranges and some wild decimals
unique(egret$provenance.long)

unique(egret$continent)
egret$continent[which(egret$continent == "USA")] <- "North America" # 8 and some NA

unique(egret$no.indiv.collected) # only 13 reported values

unique(egret$year.collected)
#character bc 4 values entered as ranges, also from 1951-2019

unique(egret$year.germination)
egret$year.germination[which(egret$year.germination == "n/a")] <- "NA" #only 19 report this
#TO CHECK - TRUE? But what is the value?

unique(egret$storage.type)
egret$storage.type[which(egret$storage.type == "laboratory")] <- "room temp"
egret$storage.type[which(egret$storage.type == "room conditions")] <- "room temp"
egret$storage.type[which(egret$storage.type == "glass bottles, laboratory conditions")] <- "room temp"
egret$storage.type[which(egret$storage.type == "dry/refrigerated")] <- "dry refrigeration"
egret$storage.type[which(egret$storage.type == "dry refrigerator")] <- "dry refrigeration"

#TO CHECK:
# natural as in outdoors?
# paper bags in dark - room temp?
# controlled environment - not a room?
# is wet and undried the same?
# if just says dry or wet, should we assume at room temp?


unique(egret$storage.humidity)

unique(egret$storage.time)
egret$storage.time[which(egret$storage.time == "didn't mention")] <- "NA"
# TO CHECK:
#"38 5 h" "38 25h" , days + h or missing decimal?

unique(egret$storage.temp)
# many ranges or with variance

unique(egret$chill.temp)
# many ranges or with variance
#TO CHECK: 
# 38, seems high
# what does (25/10)/5/0) mean

unique(egret$chill.duration)
egret$chill.duration[which(egret$chill.duration == "unknown")] <- "NA"
#TO CHECK
# what does "90/30/90" mean
#"Continuous cold stratification" kept in cold whole experiment? How long was experiment?
# is 0 a true zero? 

unique(egret$germ.temp)
egret$germ.temp[which(egret$germ.temp == "unknown")] <- "NA"
egret$germ.temp[which(egret$germ.temp == "didn't mention")] <- "NA"
# TO CHECK
# 44696, 44854, 44727,44859
# open air - is this a field study?

unique(egret$other.treatment)

unique(egret$photoperiod)
# TO CHECK
#0.3444444444444445
# 0 a true zero? ie 100% dark?
# 0.25

unique(egret$chemical)
# egret$chemical[which(egret$chemical == "water")] <- "NA"
# is GA, GA3, GA4 all the same thing?
# 36 diff chemicals

unique(egret$chemcial.concent)
# some issues with units, g included in some, ppm, nmol/L

unique(egret$trt.duration)
unique(egret$scarification)
unique(egret$scarif.type)

egret$scarif.type[which(egret$scarif.type == "sandpaper")] <- "mechanical - sandpaper"
egret$scarif.type[which(egret$scarif.type == "sand paper (Np. 150)")] <- "mechanical - sandpaper"

egret$scarif.type[which(egret$scarif.type == "mechanical with razor")] <- "mechanical - razor"
egret$scarif.type[which(egret$scarif.type == "H2SO4")] <- "chemical - H2SO4"

#TO CHECK
# acid - what kinds?
#scapel - removing what
# soaking in water?
#cold

unique(egret$soaking)
unique(egret$soaked.in)
egret$soaked.in[which(egret$soaked.in == "water")] <- "H20"
egret$soaked.in[which(egret$soaked.in == "Water")] <- "H20"

egret$soaked.in[which(egret$soaked.in == "hot H2O - 100C")] <- "H2O 100C"
egret$soaked.in[which(egret$soaked.in == "100C water")] <- "H2O 100C"

egret$soaked.in[which(egret$soaked.in == "water 35ºC")] <- "H20 35C"
egret$soaked.in[which(egret$soaked.in == "35C water")] <- "H20 35C"

egret$soaked.in[which(egret$soaked.in == "60C water")] <- "H20 60C"
egret$soaked.in[which(egret$soaked.in == "80C water")] <- "H20 80C"
egret$soaked.in[which(egret$soaked.in == "5 C water")] <- "H20 5C"

egret$soaked.in[which(egret$soaked.in == "water 35ºC")] <- "H20 35C"

#TO CHECK
# what is N, 0 KNO3 - sounds like a control,
# battaglia93	and 97, what is MPa water and a water solution?

unique(egret$seed.mass.given)
egret$seed.mass.given[which(egret$seed.mass.given == "yes")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "no")] <- "N"
egret$seed.mass.given[which(egret$seed.mass.given == "Yes")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "No")] <- "N"
egret$seed.mass.given[which(egret$seed.mass.given == "1200")] <- "Y"
egret$seed.mass.given[which(egret$seed.mass.given == "FALSE")] <- "N"

unique(egret$respvar)
egret$respvar[which(egret$respvar == "germ.speed")] <- "germ.rate"
egret$respvar[which(egret$respvar == "rates.germ")] <- "germ.rate"
egret$respvar[which(egret$respvar == "germ.rate (days)")] <- "germ.rate"
egret$respvar[which(egret$respvar == "germ.proportion")] <- "prop.germ"
egret$respvar[which(egret$respvar == "mtg")] <- "mgt"
egret$respvar[which(egret$respvar == "mean.germ.time")] <- "mgt"
egret$respvar[which(egret$respvar == "MGT")] <- "mgt"

# TO CHECK
# is germ.rt germ rate?
# what is germ.prob
# D50 same as T50

egret$response <- as.numeric(egret$response)
range(egret$response, na.rm =T)

# I assume NG should be NA
egret$response[which(egret$response == "NG")] <- "NA"
egret$response[which(egret$response == "NA*")] <- "NA"

#TO CHECK what is "NA*"

unique(egret$error.type)
egret$error.type[which(egret$error.type == "mean+/-SE")] <- "SE"
egret$error.type[which(egret$error.type == "mean+/-SD")] <- "SD"
egret$error.type[which(egret$error.type == "not specified")] <- "not.specified"

# TO CHECK what is xz

unique(egret$resp.error)

unique(egret$reps)

unique(egret$n.per.rep)
unique(egret$germ.duration)
# TO CHECK
# why are there so many with decimal places? should it not be in days?

unique(egret$germ.tim.zero)
# TO CHECK - "TRUE"

