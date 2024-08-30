#Phenology Data Cleaning Script
# By Selena Shew
# August 13, 2024
# For the UBC Temporal Ecology Lab

# Description:
# This script is to clean the phenology & seed datasets
# from the USDA Seed Manual. The script does the following:

# 1) cleans the datasets, removing weird symbols, filling in blanks/species names, etc.
# 2) Converts headers to camelCase
# 3) merges duplicate columns together (different column names but have same type of data)
# 4) fix column values that got converted into dates when they should be ranges
# 5) fix any weird individual issues that I spot
# 6) outputs the final cleaned versions to csv files in the output folder

# Load libraries
rm(list = ls())
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
#install.packages("janitor")
library(janitor) #has the clean_names() function to make column names camelCase

#---------------------------------------------------------------------------------------------------
# Set Working directory (change as needed)
setwd("C:\\Users\\User\\my_repo_dir\\egret\\analyses\\scrapeUSDAseedmanual\\output")

#setwd("C:\\Users\\User\\my_repo_dir\\egret\\analyses\\scrapeUSDAseedmanual\\input\\test_folder")

#---------------------------------------------------------------------------------------------------
# Read in test data

# phen_data_test <- read.csv('phenCom_added_missing_pagenum_genus_test.csv', colClasses = "character")
# head(phen_data_test)
# 
# seed_data_test <- read.csv('seedCom_added_missing_pagenum_genus_test.csv', colClasses = "character")
# head(seed_data_test)

#---------------------------------------------------------------------------------------------------------
## Code
# 1) Clean the phen_data

# test
# phen_data_test <- phen_data_test %>%
#   mutate(across(everything(), ~ na_if(.x, ""))) %>%
#   mutate(across(everything(), ~ str_replace_all(.x, "NOT_SELECTED|NOT_SELECTED - |\\*|\\Â§", ""))) %>%
#   fill(Species, .direction = "down") %>%
#   mutate(Species = str_remove_all(Species, "\\b[A-Z]\\. ")) %>%
#   mutate(across(everything(), ~ ifelse(.x == ".", NA, .x))) %>%
#   mutate(across(everything(), ~ str_replace_all(.x, "\\|", "1")))

phen_data <- read.csv('phenComAddedPageNumGenus', colClasses = "character")

phen_data <- phen_data %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ str_replace_all(.x, "NOT_SELECTED|NOT_SELECTED - |\\*|\\Â§|\\§|\\+", ""))) %>%
  fill(Species, .direction = "down") %>%
  mutate(Species = str_remove_all(Species, "\\b[A-Z]\\. ")) %>%
  mutate(across(everything(), ~ ifelse(.x == ".", NA, .x))) %>%
  mutate(across(everything(), ~ ifelse(.x == "I", "1", .x))) %>%
  mutate(across(everything(), ~ str_replace_all(.x, "\\bI\\b", "1"))) %>%
  mutate(across(everything(), ~ ifelse(str_detect(.x, "\\|"), 1, .x))) %>%
  mutate(across(everything(), ~ str_replace_all(.x, "-$", "")))


#convert headers to camelCase

#get rid of metrics in the name (mm, lb, kg, yrs)
phen_data <- rename_with(phen_data, ~ gsub("mm", "", .x, fixed = TRUE)) 
phen_data <- rename_with(phen_data, ~ gsub("lb", "", .x, fixed = TRUE)) 
phen_data <- rename_with(phen_data, ~ gsub("kg", "", .x, fixed = TRUE)) 
phen_data <- rename_with(phen_data, ~ gsub("yrs", "", .x, fixed = TRUE))

colnames(phen_data)

#need to convert "." to "_" for clean_names() to work
phen_data <- rename_with(phen_data, ~ gsub(".", "_", .x, fixed = TRUE)) 
phen_data <- rename_with(phen_data, ~ gsub("__", "_", .x, fixed = TRUE)) 
phen_data <- rename_with(phen_data, ~ gsub("___", "_", .x, fixed = TRUE)) 

#convert to camelCase
phen_data <- clean_names(phen_data, "lower_camel")
colnames(phen_data)

#merge duplicate columns together (eg. locationElevation & locationElevationM)
phen_data <- phen_data %>% 
  unite(col = "matureHeight", c("treeHtM", "heightAtMaturityM", "heightOfMatureTreesM"), sep = "") %>%
  unite(col = "colorRipeFruit", c("colorOfRipeFruit", "colorOfRipeFruits", "colorOfRipeFruit1"), sep = "") %>%
  unite(col = "locationElevation", c("locationAltitude", "locationElevation", "locationElevationM"), sep = "") %>%
  unite(col = "seedDispersalTime", c("seedDispersal", "seedRipeningDispersal", "dispersal"), sep = "")

colnames(phen_data)

#get rid of NANA and NANANA after merging
phen_data <- phen_data %>% 
  mutate(across(everything(), ~ str_replace_all(.x, "NA|NANA|NANANA", ""))) %>%
  mutate(across(everything(), ~ na_if(.x, "")))

#rename weird names to my liking
phen_data <- rename(phen_data, 
                    cleanedSeedWtLb = seedsWt,
                    fruitDropMonth = fruitDrop,
                    cleanedSeedWtKg = cleaned,
                    minSeedBearingAge = age,
                    yearsBetweenSeedProd = interval,
                    fruitSeedRipeningTime = fruitSeedRipening,
                    floweringTime = flowering,
                    fruitRipeningTime = fruitRipening,
                    coneRipeningTime = coneRipening,
                    seedLength = seedSize
                    )
colnames(phen_data)

# Fix species column to include the species name before ssp, spp, var entries
last_species <- NA

phen_data <- phen_data %>%
  rowwise() %>%
  mutate(New_Species = ifelse(grepl("^(ssp|spp|var)\\.", species), 
                              paste(last_species, species),
                              {last_species <<- species; species})) %>%
  ungroup()

phen_data <- select(phen_data, -species)

phen_data <- rename(phen_data, species = New_Species)

#fix column values that got converted to dates back into ranges of values (eg. 5-May should be 5-5)
phen_data <- phen_data %>%
  mutate(across(
    c(yearsBetweenSeedProd, minSeedBearingAge, seedLength, matureHeight), 
    ~ if_else(
      # Check if the value is a decimal, range, single number, starts with > or <, or contains no letters
      str_detect(., "^>|^<|\\d+\\.\\d+|^(\\d+-\\d+|\\d+)$") | !str_detect(., "[a-zA-Z]"), 
      ., 
      # Convert date-like strings to numeric ranges with the month first
      paste0(
        case_when(
          str_detect(., "Jan") ~ 1,
          str_detect(., "Feb") ~ 2,
          str_detect(., "Mar") ~ 3,
          str_detect(., "Apr") ~ 4,
          str_detect(., "May") ~ 5,
          str_detect(., "Jun") ~ 6,
          str_detect(., "Jul") ~ 7,
          str_detect(., "Aug") ~ 8,
          str_detect(., "Sep") ~ 9,
          str_detect(., "Oct") ~ 10,
          str_detect(., "Nov") ~ 11,
          str_detect(., "Dec") ~ 12,
          str_detect(., "I") ~ 1,
          str_detect(., "\\|") ~ 1,
          TRUE ~ NA_real_
        ),
        "-",
        str_extract(., "\\d+")
      )
    )
  ))

#fixing weird values I spotted in the data; manually cross-checked correct values in the pdf
phen_data$floweringTime[which(phen_data$species == "cerasifera" & phen_data$genusName == "Prunus")] <- "May 12"
phen_data$origin[which(phen_data$species == "vulgaris" & phen_data$genusName == "Berberus")] <- "Europe"


#-----------------------------------------------------------------------------------------------------------
# 2) Clean the seed_data
seed_data <- read.csv('seedComAddedPageNumGenus.csv', colClasses = "character")

# test
# seed_data_test <- seed_data_test %>%
#   mutate(across(everything(), ~ na_if(.x, ""))) %>%
#   mutate(across(everything(), ~ str_replace_all(.x, "NOT_SELECTED|NOT_SELECTED - |\\*|\\Â§|\\+", ""))) %>%
#   fill(Species, .direction = "down") %>%
#   mutate(Species = str_remove_all(Species, "\\b[A-Z]\\. ")) %>%
#   mutate(across(everything(), ~ ifelse(.x == ".", NA, .x))) %>%
#   mutate(across(everything(), ~ str_replace_all(.x, "\\|", "1")))

seed_data <- seed_data %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ str_replace_all(.x, "NOT_SELECTED|NOT_SELECTED - |\\*|\\Â§|\\+", ""))) %>%
  fill(Species, .direction = "down") %>%
  mutate(Species = str_remove_all(Species, "\\b[A-Z]\\. ")) %>%
  mutate(across(everything(), ~ ifelse(.x == ".", NA, .x))) %>%
  mutate(across(everything(), ~ ifelse(str_detect(.x, "\\|"), 1, .x))) %>%
  mutate(across(everything(), ~ ifelse(.x == "I", 1, .x))) %>%
  mutate(across(everything(), ~ ifelse(.x == "II", 11, .x))) %>%
  mutate(across(everything(), ~ ifelse(.x == "Yearly|yearly", 1, .x))) %>%
  mutate(across(everything(), ~ ifelse(str_detect(.x, "LC|Long|Long ago"), "Long cultivated", .x)))%>%
  mutate(across(everything(), ~ str_replace_all(.x, "-$", "")))

#convert headers to camelCase

#get rid of metrics in the name (cm, m, yr, yrs)
seed_data <- rename_with(seed_data, ~ gsub("cm", "", .x, fixed = TRUE)) 
seed_data <- rename_with(seed_data, ~ gsub(".m.", "", .x, fixed = TRUE)) 
seed_data <- rename_with(seed_data, ~ gsub("yr.s", "", .x, fixed = TRUE)) 
seed_data <- rename_with(seed_data, ~ gsub("yr", "", .x, fixed = TRUE))
colnames(seed_data)

#need to convert "." to "_" for clean_names() to work
seed_data <- rename_with(seed_data, ~ gsub(".", "_", .x, fixed = TRUE)) 

#convert to camelCase
seed_data <- clean_names(seed_data, "lower_camel")
colnames(seed_data)

#merge duplicate columns together (eg. heightAtMaturity & heightAtMaturity_2)
seed_data <- seed_data %>% 
  unite(col = "matureHeight", c("heightAtMaturity", "matureTreeHeight", "matureHeight", "heightAtMaturity_2", "heightAtMaturity_3", "x"), sep = "") %>%
  unite(col = "minSeedBearingAge", c("minimumSeedBearingAge", "minimumSeedBearingAge1", "minimumSeedBearingAge_2", "age"), sep = "") %>%
  unite(col = "onsetSeedBearingAge", c("ageAtOnsetSeedBearing_2", "ageAtOnsetSeedBearing"), sep = "") %>%
  unite(col = "yearsBetweenLargeSeedCrops", c("yearsBetweenLargeSeedCrops", "yearsBetweenLargeSeedcrops", "crops", "yearsBetweenLargeCrops", "yearsLargeTime"), sep = "") %>%
  unite(col = "preripeFruitColor", c("fruitPreripeColor", "fruitRipenessPreripeColor", "fruitPreripe"), sep = "") %>%
  unite(col = "ripeFruitColor", c("ripenessCriteriaRipeColor", "ripeFruitColor", "colorRipe"), sep = "")

colnames(seed_data)

#get rid of NANA and NANANA after merging
seed_data <- seed_data %>% 
  mutate(across(everything(), ~ str_replace_all(.x, "NA|NANA|NANANA", ""))) %>%
  mutate(across(everything(), ~ na_if(.x, "")))

#rename weird names to my liking
seed_data <- rename(seed_data, fruitDiameter = diameter)
seed_data <- rename(seed_data, fruitLength = length)
seed_data <- rename(seed_data, yearCultivated = cultivated)
seed_data <- rename(seed_data, seedCropLocation = betweenSeedcropsLocation)
colnames(seed_data)

# Fix species column to include the species name before ssp, spp, var entries
last_species <- NA

seed_data <- seed_data %>%
  rowwise() %>%
  mutate(New_Species = ifelse(grepl("^(ssp|spp|var)\\.", species), 
                              paste(last_species, species),
                              {last_species <<- species; species})) %>%
  ungroup()

seed_data <- select(seed_data, -species)

seed_data <- rename(seed_data, species = New_Species)

#fix column values that got converted to dates back into ranges of values (eg. 5-May should be 5-5)
seed_data <- seed_data %>%
  mutate(across(
    c(minSeedBearingAge, yearsBetweenLargeSeedCrops, yearsBetweenConeCrops, yearsBetweenSeedcrops, onsetSeedBearingAge, fruitLength, coneLength, fruitDiameter), 
    ~ if_else(
      # Check if the value is a decimal, range, single number, starts with > or <, or contains no letters
      str_detect(., "^>|^<|\\d+\\.\\d+|^(\\d+-\\d+|\\d+)$") | !str_detect(., "[a-zA-Z]"), 
      ., 
      # Convert date-like strings to numeric ranges with the month first
      paste0(
        case_when(
          str_detect(., "Jan") ~ 1,
          str_detect(., "Feb") ~ 2,
          str_detect(., "Mar") ~ 3,
          str_detect(., "Apr") ~ 4,
          str_detect(., "May") ~ 5,
          str_detect(., "Jun") ~ 6,
          str_detect(., "Jul") ~ 7,
          str_detect(., "Aug") ~ 8,
          str_detect(., "Sep") ~ 9,
          str_detect(., "Oct") ~ 10,
          str_detect(., "Nov") ~ 11,
          str_detect(., "Dec") ~ 12,
          str_detect(., "I") ~ 1,
          str_detect(., "\\|") ~ 1,
          TRUE ~ NA_real_
        ),
        "-",
        str_extract(., "\\d+")
      )
    )
  ))

#fixing weird values I spotted in the data; manually cross-checked correct values in the pdf
seed_data$minSeedBearingAge[which(seed_data$species == "dulcis" & seed_data$genusName == "Prunus")] <- "6-7"
seed_data$preripeFruitColor[which(seed_data$species == "angustifolia" & seed_data$genusName == "Elaeagnus")] <- "Whitish to silvery"
seed_data$ripeFruitColor[which(seed_data$species == "angustifolia" & seed_data$genusName == "Elaeagnus")] <- "Silver-gray outer; lemon-yellow inside"
seed_data$preripeFruitColor[which(seed_data$species == "umbellata" & seed_data$genusName == "Elaeagnus")] <- "Silvery"

# 3) Output the cleaned data to a new CSV

# test
# write_csv(phen_data_test, "cleaned_phen_data_test.csv")
# write_csv(seed_data_test, "cleaned_seed_data_test.csv")

write_csv(phen_data, "phenCleanedFinalMaster.csv")
write_csv(seed_data, "seedCleanedFinalMaster.csv")
