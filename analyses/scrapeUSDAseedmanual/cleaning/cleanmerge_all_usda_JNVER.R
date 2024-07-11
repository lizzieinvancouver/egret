####source file for cleaning USDA seed manual
#made by Dan July 9, 2024

rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
graphics.off()

# setwd("~/Documents/git/egret/analyses/scrapeUSDAseedmanual/cleaning/")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# USDA MANUAL GERMINATION MERGED
# Started by Dan, continued by Justin
# 10 JULY 2024
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list=ls())
library(tidyverse)
library(xlsx)
library(tibble)
library(taxize)
getwd()
d <- read_csv("germination_master_spreadsheet.csv", na = c("", "NA"))

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

# Fixing species names
d$species_name[which(d$species_name == "idaeust Glen Cova")] <- "idaeus 'Glen Cova'"
d$species_name[which(d$species_name == "macrophyllum Source I")] <- "macrophyllum Source 1"
d$species_name[which(d$species_name == "pensylvanicumt")] <- "pensylvanicum"
d$species_name[which(d$species_name == "chamemorus")] <- "chamaemorus"
d$species_name <- gsub("rubrump","rubrum",d$species_name)
d$species_name[which(d$species_name == "i. ssp. tenuifolia fresh seeds")] <- "incana ssp. tenuifolia fresh seeds"
d$species_name[which(d$species_name == "arguta")] <- "glabra var. arguta"
d$species_name[which(d$species_name == "latifolia" & d$genus_name == "Pinus")] <- "contorta var. latifolia"
d$species_name[which(d$species_name == "murrayana" & d$genus_name == "Pinus")] <- "contorta var. murrayana"
d$species_name[which(d$species_name == "densa" & d$genus_name == "Pinus")] <- "elliottii var. densa"
d$species_name[which(d$species_name == "scopulorum" & d$genus_name == "Pinus")] <- "ponderosa var. scopulorum"
d$species_name[which(d$species_name == "glauca" & d$genus_name == "Pseudotsuga")] <- "menziesii var. glauca"
d$species_name[which(d$species_name == "ioensist" & d$genus_name == "Malus")] <- "ioensis"
d$species_name[which(d$rowID == "500|501|502")] <- "elliottii var. elliotii"
d$species_name[which(d$rowID == "611")] <- "menziesii var. menziesii"

# Removing the blank space after some specific epithets
d$species_name <- sub(" ","",d$species_name)

# Adding back in the space for any places where it was removed for var. or ssp. etc.
d$species_name <- sub("var"," var",d$species_name)
d$species_name <- sub("ssp"," ssp",d$species_name)
d$species_name <- sub("Xprunifolia","X prunifolia",d$species_name)
d$species_name <- sub("Xrobusta","X robusta",d$species_name)
d$species_name <- sub(" variabilis","variabilis",d$species_name)
d$species_name <- sub("\\("," \\(",d$species_name)
d$species_name <- sub("Source"," Source",d$species_name)
d$species_name <- sub("Low"," Low",d$species_name)
d$species_name <- sub("High"," High",d$species_name)
d$species_name <- sub("  (U)"," (U)",d$species_name)
d$species_name <- sub("  (S)"," (S)",d$species_name)

# Filling in blank d$species_name
# Replacing all blanks with NA
d <- d %>% mutate_all(na_if,"")
d <- d %>% 
  fill(species_name)

# Mass checking species nomenclature with package taxize
# d_species <- unique(paste(d$genus_name, d$species_name))
# 
# ref <- gnr_datasources() # Full list of dabases available
# fix_names <- gnr_resolve(sci = d_species, with_canonical_ranks = T)
# d_species_fix <- unique(fix_names$matched_name2)
# names_changed <- setdiff(d_species, d_species_fix)
# names_changed

d$species_name[which(d$species_name == "X prunifolia")] <- "x prunifolia"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "macrophyllum Source 1")] <- "Source 1"
d$species_name[which(d$species_name == "macrophyllum Source 1")] <- "macrophyllum"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "macrophyllum Source 2")] <- "Source 2"
d$species_name[which(d$species_name == "macrophyllum Source 2")] <- "macrophyllum"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "macrophyllum Source 3")] <- "Source 3"
d$species_name[which(d$species_name == "macrophyllum Source 3")] <- "macrophyllum"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "rubrum Low elevation  (U)")] <- "Low elevation (U)"
d$species_name[which(d$species_name == "rubrum Low elevation  (U)")] <- "rubrum"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "rubrum Low elevation  (S)")] <- "Low elevation (S)"
d$species_name[which(d$species_name == "rubrum Low elevation  (S)")] <- "rubrum"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "rubrum High elevation  (U)")] <- "High elevation (U)"
d$species_name[which(d$species_name == "rubrum High elevation  (U)")] <- "rubrum"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "rubrum High elevation  (S)")] <- "High elevation (S)"
d$species_name[which(d$species_name == "rubrum High elevation  (S)")] <- "rubrum"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "glutinosa (Pennsylvania)")] <- "Pennsylvania"
d$species_name[which(d$species_name == "glutinosa (Pennsylvania)")] <- "glutinosa"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "glutinosa (Finland)")] <- "Finland"
d$species_name[which(d$species_name == "glutinosa (Finland)")] <- "glutinosa"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "incana (Europe)")] <- "Europe"
d$species_name[which(d$species_name == "incana (Europe)")] <- "incana"

d$seed_source[which(is.na(d$seed_source) & d$species_name == "incana (Finland)")] <- "Finland"
d$species_name[which(d$species_name == "incana (Finland)")] <- "incana"

d$seed_type[which(is.na(d$seed_type) & d$species_name == "incana ssp. tenuifolia fresh seeds")] <- "fresh seed"
d$species_name[which(d$species_name == "incana ssp. tenuifolia fresh seeds")] <- "incana ssp. tenuifolia"

d$species_name[which(d$species_name == "veluting" & d$genus_name == "Fraxinus")] <- "velutina"
d$species_name[which(d$species_name == "bacatta" & d$genus_name == "Malus")] <- "baccata"
d$species_name[which(d$species_name == "X robusta" & d$genus_name == "Malus")] <- "x robusta"
d$species_name[which(d$species_name == "nigraspp. cerulea" & d$genus_name == "Sambucus")] <- "nigra ssp. cerulea"
d$species_name[which(d$species_name == "rotundifolia" & d$genus_name == "Ribes")] <- "rotundifolium"
d$species_name[which(d$species_name == "idaeus'Glen Cova'" & d$genus_name == "Rubus")] <- "idaeus 'Glen Cova'"
d$species_name[which(d$species_name == "durmosa" & d$genus_name == "Quercus")] <- "dumosa"
d$species_name[which(d$species_name == "caroliana" & d$genus_name == "Prunus")] <- "caroliniana"
d$species_name[which(d$species_name == "ponderosa var. ponderosa " & d$genus_name == "Pinus")] <- "ponderosa var. ponderosa"

# # Final check of species names
# d_species2 <- unique(paste(d$genus_name, d$species_name))
# 
# ref <- gnr_datasources() # Full list of databases available
# fix_names2 <- gnr_resolve(sci = d_species2, with_canonical_ranks = T)
# d_species_fix2 <- unique(fix_names2$matched_name2)
# names_changed2 <- setdiff(d_species2, d_species_fix2)
# names_changed2 # ALL GOOD!!

# Giving each genus a unique number identifier
unique(d$genus_name)

assign_category_numbers <- function(data, column) {
  data %>%
    mutate(genus_ID = match({{ column }}, unique({{ column }})))
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
  select(-21,-23,-20,-22)

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
                 "chill.duraton",
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
  select(-20)
