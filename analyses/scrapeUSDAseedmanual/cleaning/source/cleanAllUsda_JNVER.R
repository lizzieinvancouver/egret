# ####source file for cleaning USDA seed manual
# #made by Dan July 9, 2024
# 
# rm(list=ls())
# options(stringsAsFactors = FALSE)
# options(mc.cores = parallel::detectCores())
# #rstan_options(auto_write = TRUE)
# graphics.off()
# 
# # setwd("~/Documents/git/egret/analyses/scrapeUSDAseedmanual/cleaning/")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# USDA MANUAL GERMINATION MERGED
# Started by Dan, continued by Justin
# 10 JULY 2024
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


# Removing apostrophe across all cells
d[] <- lapply(d, gsub, pattern="'", replacement="")

# Removing all hashtags
d[] <- lapply(d, gsub, pattern="#", replacement="")

# Removing all dollar sign
d[] <- lapply(d, gsub, pattern="\\$", replacement="")

# Giving each row a unique number so I can fix the issue where species and variety name are the same easily
d <- d %>%
  rowid_to_column("rowID")

# Removing Genus abbreviation
d$species_name <- sub(".*?\\. ","",d$species_name)

# Removing * from all cells
d[] <- lapply(d, gsub, pattern = "\\*", replacement = "")

# Removing misc. symbols from d$species_name
d$species_name <- gsub("\\+","",d$species_name)

# Removing minus only if encountered on its own
d <- mutate_all(d, ~ gsub("^-$", "", .))
# Read in the backbone dataset
runworldflora <- FALSE
if(runworldflora){
backbone <- read.csv("C:/PhD/Project/classification.csv",head = TRUE, sep="\t")
d$species_name <- tolower(d$species_name)
d$genus_name <- str_trim(d$genus_name)
d$species_name <- str_trim(d$species_name)
d_species <- unique(paste(d$genus_name, d$species_name))
checks<-WFO.match(spec.data=d_species, WFO.data=backbone, counter=1, verbose=TRUE)
d_species_fix <- unique(checks$scientificName)
names_changed <- setdiff(d_species, d_species_fix)
names_changed
}

d$seed_source[which(d$species_name == "macrophyllum source i")] <- "Source 1"
d$seed_source[which(d$species_name == "acrophyllum source 2")] <- "Source 2"
d$seed_source[which(d$species_name == "acrophyllum source 3")] <- "Source 3"
d$seed_source[which(d$species_name == "rubrump low elevation (u)")] <- "low elevation (u)"
d$seed_source[which(d$species_name == "rubrump low elevation (s)")] <- "low elevation (s)"
d$seed_source[which(d$species_name == "rubrump high elevation (u)")] <- "high elevation (u)"
d$seed_source[which(d$species_name == "rubrump high elevation (s)")] <- "high elevation (s)"
d$seed_source[which(d$species_name == "glutinosa (pennsylvania)")] <- "Pennsylvania"
d$seed_source[which(d$species_name == "glutinosa (finland)")] <- "Finland"
d$seed_source[which(d$species_name == "incana (europe)")] <- "Europe"
d$seed_source[which(d$species_name == "incana (finland)")] <- "Finland"
d$seed_type[which(is.na(d$seed_type) & d$species_name == "incana ssp. tenuifolia fresh seeds")] <- "fresh seed"

# Fixing species names
d$species_name[which(d$genus_name == "Acer" & d$species_name == "ginnala")] <-"tataricum subsp. ginnala"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "macrophyllum source i")] <-"macrophyllum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "macrophyllum source 2")] <-"macrophyllum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "macrophyllum source 3")] <-"macrophyllum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "pensylvanicumt")] <-"pensylvanicum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "")] <-"sp."
d$species_name[which(d$genus_name == "Acer" & d$species_name == "rubrump")] <-"rubrum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "rubrump low elevation (u)")] <-"rubrum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "rubrump low elevation (s)")] <-"rubrum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "rubrump high elevation (u)")] <-"rubrum"
d$species_name[which(d$genus_name == "Acer" & d$species_name == "rubrump high elevation (s)")] <-"rubrum"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "glutinosa (pennsylvania)")] <-"glutinosa"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "glutinosa (finland)")] <-"glutinosa"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "incana (europe)")] <-"incana"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "incana (finland)")] <-"incana"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "i. ssp. tenuifolia fresh seeds")] <-"incana subsp. tenuifolia"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "i. ssp. tenuifolia fresh seeds")] <-"incana subsp. tenuifolia"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "viridis")] <-"alnobetula subsp. fruticosa"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "viridis ssp. crispa")] <-"alnobetula subsp. crispa"
d$species_name[which(d$genus_name == "Alnus" & d$species_name == "viridis ssp. sinuata")] <-"alnobetula subsp. sinuata"
d$species_name[which(d$genus_name == "Aesculus" & d$species_name == "arguta")] <-"glabra var. arguta"
d$species_name[which(d$genus_name == "Cotoneaster" & d$species_name == "lucidus")] <-"acutifolius"
d$species_name[which(d$genus_name == "Corylus" & d$species_name == "cornuta var. californica")] <-"cornuta subsp. californica"
d$species_name[which(d$genus_name == "Crataegus" & d$species_name == "anomala")] <-"holmesiana"
d$species_name[which(d$genus_name == "Ceanothus" & d$species_name == "greggii")] <-"pauciflorus"
d$species_name[which(d$genus_name == "Ceanothus" & d$species_name == "sorediatus")] <-"arboreus"
d$species_name[which(d$genus_name == "Carya" & d$species_name == "illinoensis")] <-"illinoinensis"
d$species_name[which(d$genus_name == "Cercis" & d$species_name == "canadensis var. texensis")] <-"canadensis subsp. texensis"
d$species_name[which(d$genus_name == "Celtis" & d$species_name == "laevigata var. reticulata")] <-"reticulata"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "americana")] <-"americanum"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "atropurpurea")] <-"atropurpureus"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "bungeana")] <-"maackii"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "europaea")] <-"europaeus"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "hamiltoniana ssp. maackii")] <-"maackii"
d$species_name[which(d$genus_name == "Euonymus" & d$species_name == "verrucosa")] <-"verrucosus"
d$species_name[which(d$genus_name == "Fraxinus" & d$species_name == "veluting")] <-"velutina"
d$genus_name[which(d$genus_name == "Acacia" & d$species_name == "farnesiana")] <-"Vachellia"
d$genus_name[which(d$genus_name == "Acacia" & d$species_name == "nilotica")] <-"Vachellia"
d$genus_name[which(d$genus_name == "Mahonia" & d$species_name == "aquifolium")] <-"Berberis"
d$genus_name[which(d$genus_name == "Mahonia" & d$species_name == "fremontii")] <-"Berberis"
d$genus_name[which(d$genus_name == "Mahonia" & d$species_name == "nevinii")] <-"Berberis"
d$genus_name[which(d$genus_name == "Mahonia" & d$species_name == "repens")] <-"Berberis"
d$species_name[which(d$genus_name == "Malus" & d$species_name == "bacatta")] <-"baccata"
d$species_name[which(d$genus_name == "Malus" & d$species_name == "ioensist")] <-"ioensis"
d$species_name[which(d$genus_name == "Nyssa" & d$species_name == "sylvatica var. sylvatica")] <-"sylvatica"
d$species_name[which(d$genus_name == "Sambucus" & d$species_name == "nigra ssp. canadensis")] <-"canadensis"
d$species_name[which(d$genus_name == "Sambucus" & d$species_name == "nigra spp. cerulea")] <-"cerulea"
d$species_name[which(d$genus_name == "Sambucus" & d$species_name == "racemosa var. racemosa")] <-"racemosa subsp. racemosa"
d$species_name[which(d$genus_name == "Syringa" & d$species_name == "reticulata var. amurensis")] <-"reticulata subsp. amurensis"
d$species_name[which(d$genus_name == "Taxodium" & d$species_name == "ascendens")] <-"distichum var. imbricarium"
d$species_name[which(d$genus_name == "Ribes" & d$species_name == "oxyacanthoides ssp. irriguum")] <-"oxyacanthoides var. irriguum"
d$species_name[which(d$genus_name == "Ribes" & d$species_name == "rotundifolia")] <-"rotundifolium"
d$species_name[which(d$genus_name == "Rubus" & d$species_name == "idaeust glen cova")] <-"idaeus"
d$species_name[which(d$genus_name == "Rubus" & d$species_name == "chamemorus")] <-"chamaemorus"
d$species_name[which(d$genus_name == "Quercus" & d$species_name == "durmosa")] <-"dumosa"
d$species_name[which(d$genus_name == "Quercus" & d$species_name == "prinus")] <-"michauxii"
d$species_name[which(d$genus_name == "Quercus" & d$species_name == "vaccinifolia")] <-"vacciniifolia"
d$species_name[which(d$genus_name == "Quercus" & d$species_name == "wislizenii")] <-"wislizeni"
d$species_name[which(d$genus_name == "Prunus" & d$species_name == "alleghaniensis")] <-"umbellata"
d$species_name[which(d$genus_name == "Prunus" & d$species_name == "caroliana")] <-"caroliniana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "latifolia")] <-"engelmannii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "murrayana")] <-"contorta var. murrayana"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "densa")] <-"elliottii var. densa"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "elliotti")] <-"elliottii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "")] <-"sp."
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "thunbergiana")] <-"thunbergii"
d$species_name[which(d$genus_name == "Pinus" & d$species_name == "scopulorum")] <-"scopulorum"
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "")] <-"sp."
d$species_name[which(d$genus_name == "Pseudotsuga" & d$species_name == "glauca")] <-"menziesii var. glauca"
d$genus_name[which(d$genus_name == "Prosopis" & d$species_name == "juliflora")] <-"Neltuma"

# Giving each genus a unique number identifier
unique(d$genus_name)

assign_category_numbers <- function(data, column) {
  data %>%
    dplyr::mutate(genus_ID = match({{ column }}, unique({{ column }})))
}

d <- d %>%
  assign_category_numbers(genus_name) 


# Removing misc. symbols from some numeric columns
itarget <- c("10":"35")
d[itarget] <- lapply(d[itarget], gsub, pattern ="I", replacement = "")
d[itarget] <- lapply(d[itarget], gsub, pattern ="\\|", replacement = "")
d[itarget] <- lapply(d[itarget], gsub, pattern ="\\+", replacement = "")

# Removing - if it is not succeeded by a number
d <- mutate_all(d, ~ gsub("-$", "", .))

# Removing duplicate rows
d <- d %>%
  distinct()

# Fine scale cleaning of misc. values
d$day_temp_celsius[which(d$day_temp_celsius == "5(§41)")] <- "5"
d$night_temp_celsius[which(d$night_temp_celsius == "5(§41)")] <- "5"

# Going through all the columns individually to make sure the observations are consistently phrased
colnames(d)

# unique(d$seed_type)
d$seed_type[which(d$seed_type == "fresh seeds")] <- "fresh seed"
d$seed_type[which(d$seed_type == "dried seeds")] <- "dried seed"
d$seed_type[which(d$seed_type == "stored seeds")] <- "stored seed"

# unique(d$medium)
d$seed_type[which(d$seed_type == "")] <- ""

# unique(d$pregermination_treatment_hot_water_soak_C)

# unique(d$stratification_temp_C)

unique(d$cold_stratification_days) #one entry says 1803 days

# unique(d$temp_unspecified_time_of_day_celsius)
d$temp_unspecified_time_of_day_celsius <- gsub("-"," to ",d$temp_unspecified_time_of_day_celsius)
d$temp_unspecified_time_of_day_celsius <- gsub(" to 2","-2",d$temp_unspecified_time_of_day_celsius)
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "21-28")] <- "21 to 28"

# unique(d$germination_time_days)
d$germination_time_days <- gsub("-"," to ",d$germination_time_days)

# unique(d$avg_germination_percent) #why are there ranges?
d$avg_germination_percent <- gsub("-"," to ",d$avg_germination_percent)

# unique(d$germination_rate)
d$germination_rate <- gsub("-"," to ",d$germination_rate)

# unique(d$germinative_energy_percent)
d$germinative_energy_percent <- gsub("-"," to ",d$germinative_energy_percent)

# unique(d$germinative_capacity)

# unique(d$percent_germination_20degC_incubated)

unique(d$seed_source) #how to standardize these?

# unique(d$pregermination_treatment_time_minutes)
d$pregermination_treatment_time_minutes <- gsub("-"," to ",d$pregermination_treatment_time_minutes)
d$pregermination_treatment_time_minutes[which(d$pregermination_treatment_time_minutes == " to 5")] <- "-5" #how can there be negative 5 minutes?
d$pregermination_treatment_time_minutes[which(d$pregermination_treatment_time_minutes == "None")] <- NA 

unique(d$pretreatment) #Should we standardize abrasion and nicking to mechanical?
# d$pretreatment[which(d$pretreatment == "Mech.")] <- "Mechanical"
# d$pretreatment[which(d$pretreatment == "Mech")] <- "Mechanical"
d$pretreatment[which(d$pretreatment == "None")] <- NA
d$pretreatment[which(d$pretreatment == "Fresh seeds")] <- "Fresh seed"
d$pretreatment <- gsub("-mon"," month",d$pretreatment)
d$pretreatment <- gsub("-"," to ",d$pretreatment)

# Making new columns for pretreatment
d$pretreatmentFeces <- d$pretreatment
d$pretreatmentChill <- d$pretreatment
d$pretreatmentChillDuration <- d$pretreatment
d$pretreatmentScarifType <- d$pretreatment
d$pretreatmentScarifTypeGen <- d$pretreatment

d$pretreatmentFeces[!grepl("feces", d$pretreatmentFeces)] <- NA
d$pretreatmentChill[!grepl("chill|stratification", d$pretreatmentChill)] <- NA
d$pretreatmentChillDuration[!grepl("chill|stratification", d$pretreatmentChillDuration)] <- NA
d$pretreatmentScarifType[!grepl("scarification|Scarification", d$pretreatmentScarifType)] <- NA
d$pretreatmentScarifTypeGen[!grepl("scarification|Scarification", d$pretreatmentScarifTypeGen)] <- NA

d$pretreatmentChill[which(grepl("chill|stratification",d$pretreatmentChill))] <- "Y"
d$pretreatmentChill[which(!grepl("chill|stratification",d$pretreatment))] <- "N"
d$pretreatmentChill[which(d$pretreatment == "No stratification")] <- "N"

d$pretreatmentChillDuration[which(d$pretreatmentChill == "N")] <- NA
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "wet_prechill_3_to_5_C")] <- NA
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "Moist chill")] <- NA
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "Scarification & 2 month stratification")] <- 60.8334
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "Scarification & 4 month stratification")] <- 121.667
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "Scarification & 6 month stratification")] <- 182.5
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "4 month stratification")] <- 121.667
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "6 month stratification")] <- 182.5
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "3 to 5 month stratification")] <- "91.2501 to 152.083"
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "6 to 9 month stratification")] <- "182.5 to 273.75"
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "10 to 13 month stratification")] <- "304.167 to 395.417"

d$pretreatmentScarifType[which(d$pretreatment == "H2SO4")] <- "H2SO4"
d$pretreatmentScarifType[which(d$pretreatment == "Mech")] <- "Mechanical"
d$pretreatmentScarifType[which(d$pretreatment == "Mech.")] <- "Mechanical"
d$pretreatmentScarifType[which(d$pretreatment == "Heat")] <- "Heat"
d$pretreatmentScarifType[which(d$pretreatment == "Hot water")] <- "Hot water"
d$pretreatmentScarifType[which(d$pretreatment == "Warm water")] <- "Warm water"
d$pretreatmentScarifType[which(d$pretreatment == "Abrasion")] <- "Abrasion"
d$pretreatmentScarifType[which(d$pretreatment == "Acid soak")] <- "Acid soak"
d$pretreatmentScarifType[which(d$pretreatment == "Nicking")] <- "Nicking"
d$pretreatmentScarifType[which(d$pretreatment == "No scarification")] <- NA

d$pretreatmentScarifTypeGen[which(grepl("water|Water", d$pretreatmentScarifType))] <- "Moisture"
d$pretreatmentScarifTypeGen[which(grepl("H2SO4|Acid|acid", d$pretreatmentScarifType))] <- "Chemical"
d$pretreatmentScarifTypeGen[which(grepl("Mechanical|Abrasion|Nicking", d$pretreatmentScarifType))] <- "Mechanical"
d$pretreatmentScarifTypeGen[which(grepl("Heat", d$pretreatmentScarifType))] <- "Thermal"

# Rubus spectabilis has general scarification, will find
d$pretreatmentScarifType[which(d$pretreatmentScarifType == "Scarification & 2 month stratification")] <- "Acid scarification"
d$pretreatmentScarifType[which(d$pretreatmentScarifType == "Scarification & 4 month stratification")] <- "Acid scarification"
d$pretreatmentScarifType[which(d$pretreatmentScarifType == "Scarification & 6 month stratification")] <- "Acid scarification"
d$pretreatmentScarifTypeGen[which(d$pretreatmentScarifType == "Acid scarification")] <- "Chemical"

# unique(d$warm_stratification_days)

# unique(d$dailyl_light_hours)
d$dailyl_light_hours <- gsub("-"," to ",d$dailyl_light_hours)

# unique(d$day_temp_celsius)
d$day_temp_celsius <- gsub("-"," to ",d$day_temp_celsius)

# unique(d$night_temp_celsius)
d$night_temp_celsius <- gsub("-"," to ",d$night_temp_celsius)

# unique(d$test_duration_in_days)
d$test_duration_in_days <- gsub("-"," to ",d$test_duration_in_days)

# unique(d$total_germination_percent)
d$total_germination_percent <- gsub("-"," to ",d$total_germination_percent)

# unique(d$samples)
d$samples <- gsub("-"," to ",d$samples)

# unique(d$avg_germinative_energy_percent)
d$avg_germinative_energy_percent <- gsub("-"," to ",d$avg_germinative_energy_percent)

# unique(d$avg_germinative_capacity)
d$avg_germinative_capacity <- gsub("-"," to ",d$avg_germinative_capacity)

# unique(d$percent_germination_15degC_incubated)

# unique(d$genus_ID)

unique(d$mean_dark) #empty column; delete
unique(d$dark_range) #empty column; delete
unique(d$mean_light) #empty column; delete
unique(d$light_range) #empty column; delete

# Replacing all blanks with NA
d <- d %>% mutate_all(~ na_if(.x, ""))

# # Remove empty rows or columns
d <- d %>%
  dplyr::select(-21,-23,-20,-22)

# CHANGING COLUMN NAMES AND PIVOTING WIDER
#d <- read.csv("..//output/earlyIterationDataSheets/germinationCleaned_official.csv")
colnames(d)

# Changing column names
colnames(d) <- c("rowID",
                 "filePath",
                 "pdfPageNumber",
                 "scrapedTableNumber",
                 "pdfTableNumber",
                 "genus",
                 "species",
                 "seedType",
                 "seedSource",
                 "medium",
                 "pregermTreatment",
                 "pregermTreatmentHotWater",
                 "pretreatment",
                 "stratification",
                 "warmStratificationDuration",
                 "coldStratificationDuration",
                 "photoperiod",
                 "tempDay",
                 "tempNight",
                 "darkRange",
                 "testDuration",
                 "germTime",
                 "germPercentTotal",
                 "germPercentAvg",
                 "samples",
                 "germRate",
                 "germEnergyPercentAvg",
                 "germEnergyPercent",
                 "germCapacityAvg",
                 "germCapacity",
                 "germPercent15degIncubated",
                 "germPercent20degIncubated",
                 "genusID",
                 "pretreatmentFeces",
                 "pretreatmentChill",
                 "pretreatmentChillDuration",
                 "pretreatmentScarifType",
                 "pretreatmentScarifTypeGen")


# Checking class to see if any conflicts in variable type
sapply(d, class)

# Converting integer to character for pivot_longer()
d$germCapacity <- as.character(d$germCapacity)
d$germPercent15degIncubated <- as.character(d$germPercent15degIncubated)
d$germPercent20degIncubated <- as.character(d$germPercent20degIncubated)

# Converting the data into long format
d <- d %>%
  group_by(species) %>%
  pivot_longer(cols = c("germTime","germPercentTotal","germPercentAvg",
                        "germRate","germEnergyPercentAvg","germEnergyPercent",
                        "germCapacityAvg","germCapacity","germPercent15degIncubated",
                        "germPercent20degIncubated"),
               names_to = "responseType",
               values_to = "responseValue")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Making MEAN AVERAGE Columns
# Replacing - in cold strat duration with "to" since I missed it first time
d$coldStratificationDuration <- gsub("-"," to ",d$coldStratificationDuration)

# Finding which columns have ranges of numeric values
colnames(d)[apply(d, 2, function(col) any(str_detect(col, " to ")))]

# Cleaning up some of the response value data
d$responseValue[which(d$responseValue == "16:")] <- "16"
d$responseValue[which(d$responseValue == "<")] <- NA

# Making min and max columns for each column that has a range
# First using strsplit() to separate values into new columns
breakbyto <- strsplit(d$pregermTreatment, " to ", fixed=TRUE)
d$pregermTrtMin <- unlist(lapply(breakbyto, function(x) x[1]))
d$pregermTrtMax <- unlist(lapply(breakbyto, function(x) x[2]))

breakbyto2 <- strsplit(d$coldStratificationDuration, " to ", fixed=TRUE)
d$coldStratDurMin <- unlist(lapply(breakbyto2, function(x) x[1]))
d$coldStratDurMax <- unlist(lapply(breakbyto2, function(x) x[2]))

breakbyto3 <- strsplit(d$photoperiod, " to ", fixed=TRUE)
d$photoperiodMin <- unlist(lapply(breakbyto3, function(x) x[1]))
d$photoperiodMax <- unlist(lapply(breakbyto3, function(x) x[2]))

breakbyto4 <- strsplit(d$tempDay, " to ", fixed=TRUE)
d$tempDayMin <- unlist(lapply(breakbyto4, function(x) x[1]))
d$tempDayMax <- unlist(lapply(breakbyto4, function(x) x[2]))

breakbyto5 <- strsplit(d$tempNight, " to ", fixed=TRUE)
d$tempNightMin <- unlist(lapply(breakbyto5, function(x) x[1]))
d$tempNightMax <- unlist(lapply(breakbyto5, function(x) x[2]))

breakbyto6 <- strsplit(d$testDuration, " to ", fixed=TRUE)
d$testDurMin <- unlist(lapply(breakbyto6, function(x) x[1]))
d$testDurMax <- unlist(lapply(breakbyto6, function(x) x[2]))

breakbyto7 <- strsplit(d$samples, " to ", fixed=TRUE)
d$samplesMin <- unlist(lapply(breakbyto7, function(x) x[1]))
d$samplesMax <- unlist(lapply(breakbyto7, function(x) x[2]))

breakbyto8 <- strsplit(d$pretreatmentChillDuration, " to ", fixed=TRUE)
d$pretrtChillDurMin <- unlist(lapply(breakbyto8, function(x) x[1]))
d$pretrtChillDurMax <- unlist(lapply(breakbyto8, function(x) x[2]))

breakbyto9 <- strsplit(d$responseValue, " to ", fixed=TRUE)
d$responseValueMin <- unlist(lapply(breakbyto9, function(x) x[1]))
d$responseValueMax <- unlist(lapply(breakbyto9, function(x) x[2]))

# Making mean average columns for the above columns
# unique(d$responseValueMin)
d$responseValueMin[which(d$responseValueMin == "69 (18")] <- "69" #GIT ISSUE 20 IN EGRET; Keeping the range of values in the original data but leaving just the given mean for the responseValueAvg column
d$responseValueMin[which(d$responseValueMin == "93 (84")] <- "93"
d$responseValueMin[which(d$responseValueMin == "60 (40")] <- "60"
d$responseValueMin[which(d$responseValueMin == "I")] <- "1"
# unique(d$responseValueMax)
d$responseValueMax[which(d$responseValueMax == "94)")] <- NA
d$responseValueMax[which(d$responseValueMax == "96)")] <- NA
d$responseValueMax[which(d$responseValueMax == "88)")] <- NA

# Converting to integer
d$responseValueMin <- as.integer(d$responseValueMin) #NAs introduced by coercion
d$responseValueMax <- as.integer(d$responseValueMax)

# using rowMeans() conserves the rows where just one value is present so that it doesn't try to make a mean out of a valid value in the Min column and NA in the Max column
d$responseValueAvg <- rowMeans(d[, c("responseValueMin", "responseValueMax")], na.rm = TRUE)
d$responseValueAvg[which(is.nan(d$responseValueAvg))] <- NA

# First looking at which values are weird, converting them to numeric placeholders in the Min column, then turning them back to their original value in the Avg column
# Pregermination treatment
# unique(d$pregermTrtMin)
d$pregermTrtMin[which(d$pregermTrtMin == "ttc")] <- 99991
d$pregermTrtMin[which(d$pregermTrtMin == "Overnight")] <- 99992
# unique(d$pregermTrtMax)

d$pregermTrtMin <- as.integer(d$pregermTrtMin)
d$pregermTrtMax <- as.integer(d$pregermTrtMax)
d$pregermTrtAvg <- rowMeans(d[, c("pregermTrtMin", "pregermTrtMax")], na.rm = TRUE)
d$pregermTrtAvg[which(is.nan(d$pregermTrtAvg))] <- NA

d$pregermTrtAvg[which(d$pregermTrtMin == 99991)] <- "ttc (time to cool to room temperature), varied from  several hours to overnight"
d$pregermTrtAvg[which(d$pregermTrtMin == 99992)] <- "Overnight"
d$pregermTrtMin[which(d$pregermTrtMin == 99991)] <- "ttc (time to cool to room temperature), varied from  several hours to overnight"
d$pregermTrtMin[which(d$pregermTrtMin == 99992)] <- "Overnight"

# cold stratification duration
unique(d$coldStratDurMin)
d$coldStratDurMin[which(d$coldStratDurMin == "CSG")] <- 99991
d$coldStratDurMin[which(d$coldStratDurMin == "Var.")] <- 99992
d$coldStratDurMin[which(d$coldStratDurMin == "1803")] <- 180
d$coldStratDurMin[which(d$coldStratDurMin == "l")] <- 1
# unique(d$coldStratDurMax)

d$coldStratDurMin <- as.integer(d$coldStratDurMin)
d$coldStratDurMax <- as.integer(d$coldStratDurMax)
d$coldStratDurAvg <- rowMeans(d[, c("coldStratDurMin", "coldStratDurMax")], na.rm = TRUE)
d$coldStratDurAvg[which(is.nan(d$coldStratDurAvg))] <- NA

d$coldStratDurAvg[which(d$coldStratDurMin == 99991)] <- "Stratification and germination as a continuum under the same conditions"
d$coldStratDurAvg[which(d$coldStratDurMin == 99992)] <- "Variable"
d$coldStratDurMin[which(d$coldStratDurMin == 99991)] <- "Stratification and germination as a continuum under the same conditions"
d$coldStratDurMin[which(d$coldStratDurMin == 99992)] <- "Variable"

# Photoperiod
# unique(d$photoperiodMin)
d$photoperiodMin[which(d$photoperiodMin == "NDL")] <- 99991
d$photoperiodMin[which(d$photoperiodMin == "<16")] <- 99992
d$photoperiodMin[which(d$photoperiodMin == ">8")] <- 99993
d$photoperiodMin[which(d$photoperiodMin == "ND")] <- 99994
d$photoperiodMin[which(d$photoperiodMin == "Dark")] <- 0
# unique(d$photoperiodMax)

d$photoperiodMin <- as.integer(d$photoperiodMin)
d$photoperiodMax <- as.integer(d$photoperiodMax)
d$photoperiodAvg<- rowMeans(d[, c("photoperiodMin", "photoperiodMax")], na.rm = TRUE)
d$photoperiodAvg[which(is.nan(d$photoperiodAvg))] <- NA

d$photoperiodAvg[which(d$photoperiodMin == 99991)] <- "Natural daylength in a greenhouse"
d$photoperiodAvg[which(d$photoperiodMin == 99992)] <- "<16"
d$photoperiodAvg[which(d$photoperiodMin == 99993)] <- ">8"
d$photoperiodAvg[which(d$photoperiodMin == 99994)] <- "Natural daylength in a greenhouse"
d$photoperiodMin[which(d$photoperiodMin == 99991)] <- "Natural daylength in a greenhouse"
d$photoperiodMin[which(d$photoperiodMin == 99992)] <- "<16"
d$photoperiodMin[which(d$photoperiodMin == 99993)] <- ">8"
d$photoperiodMin[which(d$photoperiodMin == 99994)] <- "Natural daylength in a greenhouse"

# Temperature Day
# unique(d$tempDayMin)
d$tempDayMin[which(d$tempDayMin == "al")] <- -1 #This was the A1 that Selena mentioned in ISSUE 20 titled June 24 updates
# unique(d$tempDayMax)

d$tempDayMin <- as.integer(d$tempDayMin)
d$tempDayMax <- as.integer(d$tempDayMax)
d$tempDayAvg <- rowMeans(d[, c("tempDayMin", "tempDayMax")], na.rm = TRUE)
d$tempDayAvg[which(is.nan(d$tempDayAvg))] <- NA

# Temperature Night
# unique(d$tempNightMin)
d$tempNightMin[which(d$tempNightMin == "a7")] <- -7 #This was the A7 that Selena mentioned in ISSUE 20 titled June 24 updates
# unique(d$tempNightMax)

d$tempNightMin <- as.integer(d$tempNightMin)
d$tempNightMax <- as.integer(d$tempNightMax)
d$tempNightAvg <- rowMeans(d[, c("tempNightMin", "tempNightMax")], na.rm = TRUE)
d$tempNightAvg[which(is.nan(d$tempNightAvg))] <- NA

# Test Duration
# unique(d$testDurMin)
d$testDurMin[which(d$testDurMin == "NP")] <- 99991
d$testDurMin[which(d$testDurMin == ">60")] <- 99992
# unique(d$testDurMax)

d$testDurMin <- as.integer(d$testDurMin)
d$testDurMax <- as.integer(d$testDurMax)
d$testDurAvg <- rowMeans(d[, c("testDurMin", "testDurMax")], na.rm = TRUE)
d$testDurAvg[which(is.nan(d$testDurAvg))] <- NA

d$testDurAvg[which(d$testDurMin == 99991)] <- "NP"
d$testDurAvg[which(d$testDurMin == 99992)] <- ">60"
d$testDurMin[which(d$testDurMin == 99991)] <- "NP"
d$testDurMin[which(d$testDurMin == 99992)] <- ">60"

# Samples
# unique(d$samplesMin)
d$samplesMin[which(d$samplesMin == ">7")] <- 99991
d$samplesMin[which(d$samplesMin == "7t")] <- "7"
# unique(d$samplesMax)

d$samplesMin <- as.integer(d$samplesMin)
d$samplesMax <- as.integer(d$samplesMax)
d$samplesAvg <- rowMeans(d[, c("samplesMin", "samplesMax")], na.rm = TRUE)
d$samplesAvg[which(is.nan(d$samplesAvg))] <- NA

d$testDurAvg[which(d$testDurMin == 99991)] <- ">7"
d$testDurMin[which(d$testDurMin == 99991)] <- ">7"

# Pretreatment Chill Duration
# unique(d$pretrtChillDurMin)
# unique(d$pretrtChillDurMax)

d$pretrtChillDurMin <- as.integer(d$pretrtChillDurMin)
d$pretrtChillDurMax <- as.integer(d$pretrtChillDurMax)
d$pretrtChillDurAvg <- rowMeans(d[, c("pretrtChillDurMin", "pretrtChillDurMax")], na.rm = TRUE)
d$pretrtChillDurAvg[which(is.nan(d$pretrtChillDurAvg))] <- NA

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Changing column names to better fit EGRET 
colnames(d)
colnames(d) <- c("speciesID",
                 "filePath",
                 "pdfPageNumber",
                 "scrapedTableNumber",
                 "pdfTableNumber",
                 "genus",
                 "species",
                 "seed.type",
                 "source.population",
                 "medium",
                 "pretreatment.duration",
                 "pretreatment.hotwater",
                 "pretreatment",
                 "stratification.temp",
                 "warm.stratification.duration",
                 "cold.stratification.duration",
                 "photoperiod",
                 "tempDay",
                 "tempNight",
                 "darkRange",
                 "germ.duration",
                 "samples",
                 "genusID",
                 "pretreatmentFeces",
                 "chilling",
                 "chill.duration",
                 "scarifType",
                 "scarifTypeGen",
                 "responseVar",
                 "responseValue",
                 "pretreatmentMin",
                 "pretreatmentMax",
                 "cold.strat.dur.Min",
                 "cold.strat.dur.Max",
                 "photoperiodMin",
                 "photoperiodMax",
                 "tempDayMin",
                 "tempDayMax",
                 "tempNightMin",
                 "tempNightMax",
                 "germ.dur.Min",
                 "germ.dur.Max",
                 "samplesMin",
                 "samplesMax",
                 "chill.dur.Min",
                 "chill.dur.Max",
                 "responseValueMin",
                 "responseValueMax",
                 "responseValueAvg",
                 "pretreatmentAvg",
                 "cold.strat.dur.Avg",
                 "photoperiodAvg",
                 "tempDayAvg",
                 "tempNightAvg",
                 "germ.dur.Avg",
                 "samplesAvg",
                 "chill.dur.Avg")

# Changing the responseVar to better fit EGRET
# colnames(d)[23]<-"responseVar"
unique(d$responseVar)
d$responseVar[which(d$responseVar == "germTime")] <- "mgt"
d$responseVar[which(d$responseVar == "germPercentTotal")] <- "percent.germ.total"
d$responseVar[which(d$responseVar == "germPercentAvg")] <- "percent.germ"
d$responseVar[which(d$responseVar == "germRate")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germEnergyPercentAvg")] <- "mean.percent.germ.energy"
d$responseVar[which(d$responseVar == "germEnergyPercent")] <- "percent.germ.energy"
d$responseVar[which(d$responseVar == "germCapacityAvg")] <- "mean.germ.capacity"
d$responseVar[which(d$responseVar == "germCapacity")] <- "germ.capacity"
d$responseVar[which(d$responseVar == "germPercent15degIncubated")] <- "percent.germ.15degC.incubated"
d$responseVar[which(d$responseVar == "germPercent20degIncubated")] <- "percent.germ.20degC.incubated"

# Removing darkRange as I missed it when removing empty columns
unique(d$darkRange)
d <- d %>%
  dplyr::select(-20)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Combining cold stratification duration and chilling duration into one column since they represent the same thing
unique(d$chill.duration)
unique(d$cold.stratification.duration)
# I don't want to touch these original values right?
# So then I'm only going to transfer things in the min/max/avg columns over...I think
unique(d$cold.strat.dur.Min)
unique(d$cold.strat.dur.Max)
unique(d$cold.strat.dur.Avg)

# # First I need to check if there are any rows where values are present in BOTH chill.dur.XXX and cold.strat.dur.XXX
# dcopy <- d %>%
#   dplyr::select(genus, species,cold.strat.dur.Avg,chill.dur.Avg)
# dcopy$cold.strat.dur.Avg <- as.numeric(dcopy$cold.strat.dur.Avg) 
# dcopy$chill.dur.Avg <- as.numeric(dcopy$chill.dur.Avg)
# #NAS introduced by coercion because of the character class values, can ignore for now
# 
# # Adding the columns together to see if any rows actually sum up to a new value
# dcopy$newsum <- rowMeans(dcopy[, c("cold.strat.dur.Avg", "chill.dur.Avg")], na.rm = TRUE)
# unique(dcopy$newsum)
# 
# # Making an indicator column that will tell me if there's ever a time where the two columns averaged together DON'T equal the original columns' values, i.e. that there were both cold.strat.dur and chill.dur
# dcopy$Indicator <- ifelse(dcopy$newsum != dcopy$chill.dur.Avg | dcopy$newsum != dcopy$cold.strat.dur.Avg, "Not Equal", "")
# unique(dcopy$Indicator)
# # There aren't any overlaps!

# Then now I can just move the data from cold.strat.dur.XXX into chill.dur.XXX
# I think I should make this a new column to preserve old columns thought
d$chill.dur.Avg.comb <- d$chill.dur.Avg
d$chill.dur.Min.comb <- d$chill.dur.Min
d$chill.dur.Max.comb <- d$chill.dur.Max

d$chill.dur.Avg.comb[!is.na(d$cold.strat.dur.Avg)] <- d$cold.strat.dur.Avg[!is.na(d$cold.strat.dur.Avg)]
d$chill.dur.Min.comb[!is.na(d$cold.strat.dur.Min)] <- d$cold.strat.dur.Min[!is.na(d$cold.strat.dur.Min)]
d$chill.dur.Max.comb[!is.na(d$cold.strat.dur.Max)] <- d$cold.strat.dur.Max[!is.na(d$cold.strat.dur.Max)]

# # Making sure that the transfer worked
# unique(d$chill.dur.Avg)
# unique(d$chill.dur.Avg.comb)
# 
# dcopy <- d %>%
#   dplyr::select(genus, species,cold.strat.dur.Avg,chill.dur.Avg,chill.dur.Avg.comb)
# 
# # If I subtract chill.dur.Avg from chill.dur.Avg.comb, I should get cold.strat.dur.Avg right?
# dcopy$cold.strat.dur.Avg <- as.numeric(dcopy$cold.strat.dur.Avg)
# dcopy$chill.dur.Avg <- as.numeric(dcopy$chill.dur.Avg)
# dcopy$chill.dur.Avg.comb <- as.numeric(dcopy$chill.dur.Avg.comb)
# dcopy <- dcopy %>%
#   dplyr::mutate(matching = chill.dur.Avg.comb - chill.dur.Avg)
# identical(dcopy$matching,dcopy$cold.strat.dur.Avg) #why is it FALSE...
# # Oh it's because subtraction into an NA will give NA
# 
# # Trying again
# dcopy <- dcopy %>%
#   dplyr::mutate(matching2 = ifelse(is.na(chill.dur.Avg), chill.dur.Avg.comb, chill.dur.Avg.comb - chill.dur.Avg))
# unique(dcopy$matching2)
# unique(dcopy$cold.strat.dur.Avg)
# identical(dcopy$matching2,dcopy$cold.strat.dur.Avg) #still FALSE...???
# 
# # Trying with the ifelse()
# dcopy$Indicator2 <- ifelse(dcopy$matching2 != dcopy$cold.strat.dur.Avg, "NOT EQUAL","")
# 
# dcopy$newsum <- rowMeans(dcopy[, c("cold.strat.dur.Avg", "chill.dur.Avg")], na.rm = TRUE)  
# dcopy$Indicator <- ifelse(dcopy$newsum != dcopy$chill.dur.Avg | dcopy$newsum != dcopy$cold.strat.dur.Avg, "Not Equal", "")
# unique(dcopy$Indicator2) #I wonder if identical() doesn't work if it's a different class?
# # So long as my indicator column contains no "NOT EQUAL", then I think it's safe to assume that the transfer worked properly...

# Will remove the cold.strat.dur.XXX columns if we feel necessary

