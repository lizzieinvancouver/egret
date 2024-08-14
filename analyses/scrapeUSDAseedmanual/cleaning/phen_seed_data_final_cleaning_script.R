#Phenology Data Cleaning Script
# By Selena Shew
# August 13, 2024
# For the UBC Temporal Ecology Lab

# Description:
# This script is to clean the phenology & seed datasets
# from the USDA Seed Manual. The script does the following:

# For Species column:
# -Fill empty cells with the value above it
# -If the cell value starts with character.space, remove it

# For all columns:
# -remove NOT_SELECTED -  or NOT_SELECTED
# -remove . if that's the entire cell value
# -Remove  Â§ symbol
# -Replace | with 1
# -Fill empty cells with NA
# -remove *

# Load libraries
rm(list = ls())
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

# Set Working directory (change as needed)
# setwd("C:\\Users\\User\\my_repo_dir\\egret\\analyses\\scrapeUSDAseedmanual\\output")

setwd("C:\\Users\\User\\my_repo_dir\\egret\\analyses\\scrapeUSDAseedmanual\\input\\test_folder")

# Read in data
# phen_data <- read_csv('phenCom_added_missing_pagenum_genus.csv')
# head(phen_data)
# 
# seed_data <- read_csv('seedCom_added_missing_pagenum_genus.csv')
# head(seed_data)

phen_data_test <- read.csv('phenCom_added_missing_pagenum_genus_test.csv', colClasses = "character")
head(phen_data_test)

seed_data_test <- read.csv('seedCom_added_missing_pagenum_genus_test.csv', colClasses = "character")
head(seed_data_test)



# 1) Clean the phen_data

phen_data_test <- phen_data_test %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(everything(), ~ str_replace_all(.x, "NOT_SELECTED|NOT_SELECTED - |\\*|\\Â§", ""))) %>%
  fill(Species, .direction = "down") %>%
  mutate(Species = str_remove_all(Species, "\\b[A-Z]\\. ")) %>%
  mutate(across(everything(), ~ ifelse(.x == ".", NA, .x))) %>%
  mutate(across(everything(), ~ str_replace_all(.x, "\\|", "1")))
  
# 2) Output the cleaned phen_data to a new CSV

write_csv(phen_data_test, "cleaned_phen_data_test.csv")

