#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merging OEGRES data and preliminary cleaning (species cleaned)
# Started by Hoai Huong -- April 3
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Housekeeping
setwd("~/GitHub/oegres")
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

### Clean Species ##############################
oegres$species <- tolower(oegres$species)
oegres_species <- unique(paste(oegres$genus, oegres$species))
  # Use taxize package to inspect
ref <- gnr_datasources() # Full list of databases available
fix_names <- gnr_resolve(sci = oegres_species, with_canonical_ranks = T)
oegres_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(oegres_species, oegres_species_fix)
names_changed
  # Fix#########################################
oegres$species[which(oegres$genus == "Colutea" & oegres$species == "bohsei")] <- "buhsei"
oegres$species[which(oegres$genus == "Abies" & oegres$species == "amabils")] <- "amabilis"
oegres$species[which(oegres$genus == "Lathyrus" & oegres$species == "sativa")] <- "sativus"
oegres$species[which(oegres$genus == "Carex" & oegres$species == "crytolepis")] <- "cryptolepis"
oegres$species[which(oegres$genus == "Vicia" & oegres$species == "bythinica")] <- "bithynica"
oegres$species[which(oegres$genus == "Penstemon" & oegres$species == "commarhenus")] <- "comarrhenus"
oegres$species[which(oegres$genus == "Asparagus" & oegres$species == "acutifolius l.")] <- "acutifolius"
oegres$species[which(oegres$genus == "Pinus" & oegres$species == "sylvestris l.")] <- "sylvestris"
oegres$species[which(oegres$genus == "Polygonum" & oegres$species == "aviculare l.")] <- "aviculare"
oegres$species[which(oegres$genus == "Dorema" & oegres$species == "ammoniacum d.")] <- "ammoniacum"
oegres$species[which(oegres$genus == "Tradescantia" & oegres$species == "ohioensis")] <- "ohiensis"
oegres$species[which(oegres$genus == "Betula" & oegres$species == "albo-sinensis")] <- "albosinensis"

oegres$genus[which(oegres$genus == "Pterocaryafra" & oegres$species == "fraxinifolia")] <- "Pterocarya"
oegres$genus[which(oegres$genus == "Leontice\r\n" & oegres$species == "incerta")] <- "Leontice"
oegres$genus[which(oegres$genus == "Aanigozanthos" & oegres$species == "flavidus")] <- "Anigozanthos"
oegres$genus[which(oegres$genus == "Deginia" & oegres$species == "velebitica")] <- "Degenia"
oegres$genus[which(oegres$genus == "Lingularia" & oegres$species == "sibirica")] <- "Ligularia"
oegres$genus[which(oegres$genus == "Eucalytpus" & oegres$species == "delegatensis")] <- "Eucalyptus"

oegres$genus[which(oegres$genus == "Jasminus" & oegres$species == "fruiticans")] <- "Jasminum"
oegres$species[which(oegres$genus == "Jasminum" & oegres$species == "fruiticans")] <- "fruticans"

oegres$variety[which(oegres$genus == "Pedicularis" & oegres$species == "longiflora var.\r\ntubiformis")] <- "tubiformis"
oegres$species[which(oegres$genus == "Pedicularis" & oegres$species == "longiflora var.\r\ntubiformis")] <- "longiflora"
  # Confirm####################################
oegres_species <- unique(paste(oegres$genus, oegres$species))
fix_names <- gnr_resolve(sci = oegres_species, with_canonical_ranks = T)
oegres_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(oegres_species, oegres_species_fix) # Confirm this is of length 0
names_changed
### To check: Echinacea angustifolia, purpurea, pallida -- 3 species in 1 study


### Export data
write.csv(oegres, file = "input/raw_oegres.csv", row.names = FALSE)