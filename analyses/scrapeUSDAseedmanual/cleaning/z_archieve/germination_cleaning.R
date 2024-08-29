# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# USDA MANUAL GERMINATION CLEANING SCRIPT
# Started by Justin
# 17 June 2024
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
 
#library(tidyverse)
#library(xlsx)
#d <- read_csv("cleaning/germination_master_spreadsheet.csv", na = c("", "NA"))

# Removing apostrophe across all cells
d[] <- lapply(d, gsub, pattern="'", replacement="")

# Removing all hashtags
d[] <- lapply(d, gsub, pattern="#", replacement="")

# Removing all dollar sign
d[] <- lapply(d, gsub, pattern="\\$", replacement="")

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

# These species below have the same subspecies as specific epithet, change them manually
# d$species_name[which(d$species_name == "menziesii" $ d$genus_name == "Pseudotsuga")] <- "menziesii var. menziesii"
# d$species_name[which(d$species_name == "elliottii" $ d$genus_name == "Pinus")] <- "elliottii var. elliottii"

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
d <- d %>% 
  fill(species_name)

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

# unique(d$pretreatment) #Should we standardize abrasion and nicking to mechanical?
d$pretreatment[which(d$pretreatment == "Mech.")] <- "Mechanical"
d$pretreatment[which(d$pretreatment == "Mech")] <- "Mechanical"
d$pretreatment[which(d$pretreatment == "None")] <- NA
d$pretreatment[which(d$pretreatment == "Fresh seeds")] <- "Fresh seed"
d$pretreatment <- gsub("-mon"," month",d$pretreatment)
d$pretreatment <- gsub("-"," to ",d$pretreatment)

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

# Remove empty rows or columns
d <- d %>%
  select(-21,-23,-20,-22)

# Save as excel file since CSV to excel converts numbers to dates
# install.packages("xlsx")
#write.xlsx(d, "germinationCleaned.xlsx")

