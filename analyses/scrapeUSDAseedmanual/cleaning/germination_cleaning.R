# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# USDA MANUAL GERMINATION CLEANING SCRIPT
# Started by Justin
# 17 June 2024
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
 
library(tidyverse)
d <- read_csv("cleaning/germination_master_spreadsheet.csv", na = c("", "NA"))

# Removing apostrophe across all cells
d[] <- lapply(d, gsub, pattern="'", replacement="")

# Removing all hashtags
d[] <- lapply(d, gsub, pattern="#", replacement="")

# Removing all dollar sign
d[] <- lapply(d, gsub, pattern="\\$", replacement="")

# Removing Genus abbreviation
d$species_name <- sub(".*?\\. ","",d$species_name)

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

# Replacing all blanks with NA
# d[] <- lapply(d, gsub, pattern = "", replacement = NA)
d <- d %>% mutate_all(~ na_if(.x, ""))

# Filling in blank d$species_name
d <- d %>% 
  fill(species_name)

# Removing duplicate rows
d <- d %>%
  distinct()

# Remove empty rows or columns
# install.packages("janitor")
library(janitor)
d < d %>%
  remove_empty()

# Fine scale cleaning of misc. values
d$day_temp_celsius[which(d$day_temp_celsius == "5(§41)")] <- "5"
d$night_temp_celsius[which(d$night_temp_celsius == "5(§41)")] <- "5"

# Giving each genus a unique number identifier
unique(d$genus_name)

assign_category_numbers <- function(data, column) {
  data %>%
    mutate(genus_ID = match({{ column }}, unique({{ column }})))
}
d <- d %>%
  assign_category_numbers(genus_name)

# Save as excel file since CSV to excel converts numbers to dates
# install.packages("xlsx")
library(xlsx)
write.xlsx(d, "germination_cleaned.xlsx")
