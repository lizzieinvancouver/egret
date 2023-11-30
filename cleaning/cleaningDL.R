# aim of this code is to combine the cleaning work of on Dinara and Hoai Huong 

# Also to determine what datasets are still missing
if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret")
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
library(stringr)

### Import data##############################
folder <- list.dirs(path = "data", full.names = FALSE, recursive = TRUE)
folder <- folder[3:9] # Obtain the folders needed

### Merge Data##############################
#Deirdre 
egret_DL.xlsx <- read_xlsx("data/egret_DL.xlsx", sheet = "data_detailed")
length(unique(egret_DL.xlsx$datasetID)) #7

# Tolu
egret_TA.xlsx <- read_xlsx(paste("data/", "egret_TA", "/", "egret_TA.xlsx", sep = ""), sheet = "data_detailed")
#colnames(egret_TA.xlsx)[colnames(egret_TA.xlsx) == "germ.tim.zero"] <- "germ.time.zero"
colnames(egret_TA.xlsx)[colnames(egret_TA.xlsx) == "notes"] <- "Notes"
length(unique(egret_TA.xlsx$datasetID)) #38

# Sophia C
egret_SC.xlsx <- read_xlsx(paste("data/", "egret_SC", "/", "egret_SC.xlsx", sep = ""), sheet = "data_detailed")
#colnames(egret_SC.xlsx)[colnames(egret_SC.xlsx) == "germ.tim.zero"] <- "germ.time.zero"
colnames(egret_SC.xlsx)[colnames(egret_SC.xlsx) == "notes"] <- "Notes"
egret_SC.xlsx <- egret_SC.xlsx[complete.cases(egret_SC.xlsx$datasetID),]
length(unique(egret_SC.xlsx$datasetID)) #35

#Christophe
egret_CRD.xlsx <- read_xlsx("data/egret_CRD.xlsx", sheet = "data_detailed")
#colnames(egret_CRD.xlsx)[colnames(egret_CRD.xlsx) == "germ.tim.zero"] <- "germ.time.zero"
length(unique(egret_CRD.xlsx$datasetID)) #4

# Justin
egret_JN.xlsx <- read_xlsx(paste("data/", "egret_JN", "/", "egret_JN_personal_2023.11.26.xlsx", sep = ""), sheet = "data_detailed")
colnames(egret_JN.xlsx)[colnames(egret_JN.xlsx) == "germ.tim.zero"] <- "germ.time.zero"
length(unique(egret_JN.xlsx$datasetID)) #58

# Britany
egret_BW.xlsx <- read_xlsx("data/egret_BW.xlsx", sheet = "data_detailed")
#colnames(egret_BW.xlsx)[colnames(egret_BW.xlsx) == "germ.tim.zero"] <- "germ.time.zero"
length(unique(egret_BW.xlsx$datasetID)) #7

egret_MN.xlsx <- read_xlsx(paste("data/", "egret_MN", "/", "egret_MN.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_MN.xlsx$datasetID)) #50

egret_HHN.xlsx <- read_xlsx(paste("data/", "egret_HHN", "/", "egret_HHN.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_HHN.xlsx$datasetID)) #26

egret_DK.xlsx <- read_xlsx(paste("data/", "egret_DK", "/", "egret_DK.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_DK.xlsx$datasetID)) #10

egret_DM.xlsx <- read_xlsx(paste("data/", "egret_DM", "/", "egret_DM.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_DM.xlsx$datasetID)) #27

egret_GG.xlsx <- read_xlsx(paste("data/", "egret_GG", "/", "egret_GG.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_GG.xlsx$datasetID)) #12

egret_AZ.xlsx <- read_xlsx(paste("data/", "egret_AZ", "/", "egret_AZ.xlsx", sep = ""), sheet = "data_detailed")
length(unique(egret_AZ.xlsx$datasetID)) #12

egret_JS.xlsx <- read_xlsx(paste("data/", "egret_JS", "/", "egret_JS.xlsx", sep = ""), sheet = "data_detailed")
egret_JS.xlsx <- egret_JS.xlsx[complete.cases(egret_JS.xlsx$datasetID),] # tons of empty rows!  # 757 rows actual data
length(unique(egret_JS.xlsx$datasetID)) #9

egret1 <- rbind(egret_TA.xlsx, egret_BW.xlsx, egret_CRD.xlsx, egret_MN.xlsx, egret_HHN.xlsx, egret_DK.xlsx,
                egret_JS.xlsx, egret_DM.xlsx, egret_AZ.xlsx, egret_GG.xlsx)
colnames(egret1)[colnames(egret1) == "germ.tim.zero"] <- "germ.time.zero"
egret2 <- rbind(egret_DL.xlsx, egret_SC.xlsx, egret1)

colnames(egret2)[colnames(egret2) == "chemcial.concent"] <- "chemical.concent"
egret <- rbind(egret_JN.xlsx, egret2)

egret$datasetID <- tolower(egret$datasetID)
egret$datasetID[which(egret$datasetID == "acosta12")] <- "acosta13"
egret$datasetID[which(egret$datasetID == "brandel2005")] <- "brandel05"
egret$datasetID[which(egret$datasetID == "airi2009")] <- "airi09"
egret$datasetID[which(egret$datasetID == "alptekin2002")] <- "alptekin02"
egret$datasetID[which(egret$datasetID == "amini2018")] <- "amini18"
egret$datasetID[which(egret$datasetID == "pipinus12")] <- "pipinis12"
egret$datasetID[which(egret$datasetID == "picciau18")] <- "picciau19"

egret <- data.frame(egret)

# Full dataset: 30448      45 as of Nov 24 2023
dim(egret)

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
sort(egret_species)

# Remove training spaces:
egret$genus <- str_trim(egret$genus)
egret$species <- str_trim(egret$species)

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
egret$species[which(egret$genus == "Ferula" & egret$species == "assa foetida")] <- "assa-foetida"
egret$species[which(egret$genus == "Astrgalus" & egret$species == "cyclophyllon")] <- "cyclophyllus"
egret$species[which(egret$genus == "Astrgalus" & egret$species == "cyclophyllu")] <- "cyclophyllus"
egret$species[which(egret$species == "Amurensis")] <- "amurensis"
egret$species[which(egret$species == "aviculare L.")] <- "aviculare"
egret$species[which(egret$species == "longiflora var.\r\ntubiformis")] <- "longiflora"
egret$species[which(egret$species == "Pagoda")] <- "pagoda"
egret$species[which(egret$species == "Sylvestris L.")] <- "sylvestris"


# Confirm ####################################
egret_species <- unique(paste(egret$genus, egret$species))
fix_names <- gnr_resolve(sci = egret_species, with_canonical_ranks = T)
egret_species_fix <- unique(fix_names$matched_name2)
names_changed <- setdiff(egret_species, egret_species_fix) # Confirm this is of length 0
names_changed

### TO CHECK: Echinacea angustifolia, purpurea, pallida -- 3 species in 1 study
egret$sp.name <- paste(egret$genus, egret$species, sep = "_")
#View(sort(unique(egret$sp.name)))
unique(egret$study)
egret$study <- gsub(" ","", egret$study)

unique(egret$variety) # 49

unique(egret$woody) # N, Y, NA, "rhizomatus shrub/bamboo", "succulent" 
temp <- subset(egret, woody == "O") # Magnolia_ingrata aka Magnolia_fulva, yes woody
egret$woody[which(egret$woody == "O")] <- "Y"

sort(unique(egret$source.population)) #410

unique(egret$provenance.lat) # character bc includes 3 ranges and some wild decimals
unique(egret$provenance.long)

unique(egret$continent)
egret$continent[which(egret$continent == "USA")] <- "North America" 
egret$continent[which(egret$continent == "North Ameria")] <- "North America" 
# to check:
#South Africa -- africa or south america?
# Central america
# America --- North america?
# "NA, possibly Asia"

unique(egret$no.indiv.collected) # only 13 reported values
# some ranges -- some NA possibly...
unique(egret$year.collected)
egret$year.collected[which(egret$year.collected == "N/A")] <- "NA"
#character bc 4 values entered as ranges, also from 1951-2019

unique(egret$year.germination)
egret$year.germination[which(egret$year.germination == "n/a")] <- "NA" #only 19 report this
#TO CHECK - TRUE? But what is the value?

unique(egret$storage.type) # 75 options
egret$storage.type[which(egret$storage.type == "N/A")] <- "NA"

# Main categories: 
#room temp
egret$storage.type[which(egret$storage.type == "laboratory")] <- "room temp"
egret$storage.type[which(egret$storage.type == "room conditions")] <- "room temp"
egret$storage.type[which(egret$storage.type == "coin envelope (room temperature)")] <- "room temp"
egret$storage.type[which(egret$storage.type == "ambient")] <- "room temp"

egret$storage.type[which(egret$storage.type == "glass bottles, laboratory conditions")] <- "room temp air tight container"

#Air tight container - no temp specified 
egret$storage.type[which(egret$storage.type == "air-tight")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "air-tight containers")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "airtight plastic bags")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "airtight polyethylene bags")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "glass container")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "plastic bags")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "plastic bag")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "sealed containers")] <- "air-tight no temp spec"
egret$storage.type[which(egret$storage.type == "sealed glass bottle")] <- "air-tight no temp spec"

#Dry
#Dry - cold
egret$storage.type[which(egret$storage.type == "dry/refrigerated")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "dry refrigerator")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "dry, refrigerator")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "cold dry")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "cold, dry")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "cold/dry")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "dry/cold")] <- "dry cold"
egret$storage.type[which(egret$storage.type == "dry/refrigerated")] <- "dry cold"

# mosit -  cold
egret$storage.type[which(egret$storage.type == "cold/wet")] <- "moist cold"
egret$storage.type[which(egret$storage.type == "cold/moist")] <- "moist cold"

#Dark
egret$storage.type[which(egret$storage.type == "darkness")] <- "dark"
egret$storage.type[which(egret$storage.type == "in darkness")] <- "dark"

#TO CHECK:
# natural as in outdoors?
# paper bags in dark - room temp?
# controlled environment - not a room?
# is wet and undried the same?
# if just says dry or wet, should we assume at room temp?
# Cold start?


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

#write.csv(egret, "analyses/output/egretData.csv", row.names = FALSE)

sort(unique(egret$datasetID))

# how many studies do we have that time curves?

curved <- egret[, c("datasetID", "sp.name", "germ.duration")]
curved <- unique(curved)

none <- c("N/A", NA, "NA (<35)", "NA")
curved <- curved[!curved$germ.duration %in% none, ]

curved$count <- 1
noDurations <- aggregate(curved["count"], curved[c("datasetID", "sp.name")], FUN = sum)

temp <- subset(noDurations, count >5)

curvedStudy <- unique(temp$datasetID)

curvy <- egret[egret$datasetID %in% curvedStudy, ]
unique(curvy$respvar)
