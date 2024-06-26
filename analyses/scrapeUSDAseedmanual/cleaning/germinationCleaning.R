# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# USDA MANUAL GERMINATION CLEANING SCRIPT
# Started by Justin
# 17 June 2024
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
 
library(tidyverse)
library(xlsx)
library(tibble)
library(taxize)
d <- read_csv("cleaning/germination_master_spreadsheet.csv", na = c("", "NA"))

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

# Save as excel file since CSV to excel converts numbers to dates
# install.packages("xlsx")
write.xlsx(d, "germinationCleaned.xlsx")

unique(d$)

