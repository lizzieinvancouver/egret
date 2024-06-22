# Phenology Data Preparation for Compilation Script
# By Selena Shew
# June 17, 2024
# For the UBC Temporal Ecology Lab

# Description:
# This script is to automate the preparation of the phenology data ahead of
# compiling all of the data into a single CSV file. The Data Preparation script
# includes removing all of the confidence scores, quotation marks, adding
# 2 new columns: pdf_table_number and genus_name, 
# removes the first row of the CSV in order to access the column names.

# Load Libraries
rm(list = ls())
library(tidyverse)
library(dplyr)
library(stringr)

# Set directory
if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("~/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
}
# Code

library(tidyverse)

prep_phen_data <- function(directory, output_directory) {
  # Get a list of all CSV files in the directory
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Define a function to process each CSV file
  process_csv <- function(file_path) {
    # Read the CSV file
    data <- read_csv(file_path, col_names = FALSE)
    
    # Convert all columns to character type to handle mixed data types
    data <- data %>% mutate(across(everything(), as.character))
    # Remove columns where all values are NA
    data <- data %>% select(where(~ !all(is.na(.))))
    # Remove quotation marks from all cells
    data <- data %>% mutate(across(everything(), ~ str_replace_all(.x, "'", "")))
    data <- data %>% mutate(across(everything(), ~ str_replace_all(.x, "^\\-", "")))
    data <- data %>% mutate(across(everything(), ~ replace_na(.x, "")))
    # Find rows with all empty cells
    empty_rows <- which(apply(data, 1, function(row) all(row == "")))
    # If there are empty rows, truncate the dataset to keep only rows before the first empty row
    if (length(empty_rows) > 0) {
      data <- data[seq_len(empty_rows[1] - 1), ]
    }
    
    # Find the row index where the header contains "Table"
    table_row <- which(str_detect(data[[1]], "Table"))
    number_row <- which(str_detect(data[1], "[0-9]"))
    if (length(table_row) > 0 || length(number_row) > 0) {
      # Get the header row as character vector
      if (length(table_row) > 0) {
        header_row <- as.character(data[table_row, ])
        match <- str_match(header_row, "Table ([0-9]+)-(.*?)(,|$)")
      } else {
        header_row <- as.character(data[number_row, ])
        match <- str_match(header_row, "([0-9]+)-(.*?)(,|$)")
      }
      
      if (!is.na(match[1, 1])) {
        pdf_table_number <- match[1, 2]  # Captured group 1
        genus_name <- match[1, 3]  # Captured group 2
      } else {
        pdf_table_number <- NA
        genus_name <- NA
      }
    } else {
      pdf_table_number <- NA
      genus_name <- NA
    }
    
    # Find the row indices containing "SUBGENUS"
    subgenus_rows <- which(str_detect(data[[1]], "SUBGENUS"))
    
    # Remove rows containing "SUBGENUS"
    if (length(subgenus_rows) > 0) {
      data <- data[-subgenus_rows, ]
    }
    # Find the row which contains the word "species"
    species_row <- which(str_detect(data[[1]], "Species"))
    
    if (length(species_row) > 0) {
      
      # Use this row as the column names
      column_names <- as.character(data[species_row, ])
      # Create a new data frame excluding the header row
      data <- data[-(1:species_row), ]
      # Set the column names
      colnames(data) <- column_names
      
      # Add pdf_table_number and genus_name columns
      data <- data %>%
        mutate(pdf_table_number = pdf_table_number,
               genus_name = genus_name)
    } else {
      # If "species" header is not found, add empty columns
      data$pdf_table_number <- pdf_table_number
      data$genus_name <- genus_name
    }
    output_file_path <- file.path(output_directory, basename(file_path))
    # Write the modified data to output
    write_csv(data, output_file_path)
  }
  
  # Process each CSV file in the directory
  walk(csv_files, process_csv)
  
  cat("All CSV files processed successfully!\n")
}

# Run Tests
prep_phen_data("scrapeUSDAseedmanual/input/phenology_data_test_folder", 
               "scrapeUSDAseedmanual/output/phenology_test_output")

# Run code on entire phenology dataset
prep_phen_data("scrapeUSDAseedmanual/input/copy_of_phenology_data_only",
               "scrapeUSDAseedmanual/output/phenology_copy_output")

###########testing
file_path <- "scrapeUSDAseedmanual/input/phenology_data_test_folder/phenology_phenology_table-2 (2).csv"
data <- read_csv(file_path, col_names = FALSE)

# Convert all columns to character type to handle mixed data types
data <- data %>% mutate(across(everything(), as.character))
# Remove columns where all values are NA
data <- data %>% select(where(~ !all(is.na(.))))
# Remove quotation marks from all cells
data <- data %>% mutate(across(everything(), ~ str_replace_all(.x, "'", "")))
data <- data %>% mutate(across(everything(), ~ str_replace_all(.x, "^\\-", "")))
data <- data %>% mutate(across(everything(), ~ replace_na(.x, "")))
# Find rows with all empty cells
empty_rows <- which(apply(data, 1, function(row) all(row == "")))
# If there are empty rows, truncate the dataset to keep only rows before the first empty row
if (length(empty_rows) > 0) {
  data <- data[seq_len(empty_rows[1] - 1), ]
}

# Find the row index where the header contains "Table"
table_row <- which(str_detect(data[[1]], "Table"))
number_row <- which(str_detect(data[1], "[0-9]"))
if (length(table_row) > 0 || length(number_row) > 0) {
  # Get the header row as character vector
  if (length(table_row) > 0) {
    header_row <- as.character(data[table_row, ])
    match <- str_match(header_row, "Table ([0-9]+)-(.*?)(,|$)")
  } else {
    header_row <- as.character(data[number_row, ])
    match <- str_match(header_row, "([0-9]+)-(.*?)(,|$)")
  }
  
  if (!is.na(match[1, 1])) {
    pdf_table_number <- match[1, 2]  # Captured group 1
    genus_name <- match[1, 3]  # Captured group 2
  } else {
    pdf_table_number <- NA
    genus_name <- NA
  }
} else {
  pdf_table_number <- NA
  genus_name <- NA
}
# Find the row which contains the word "species"
species_row <- which(str_detect(data[[1]], "Species"))

if (length(species_row) > 0) {

  # Use this row as the column names
  column_names <- as.character(data[species_row, ])
  # Create a new data frame excluding the header row
  data <- data[-(1:species_row), ]
  # Set the column names
  colnames(data) <- column_names

  # Add pdf_table_number and genus_name columns
  data <- data %>%
    mutate(pdf_table_number = pdf_table_number,
           genus_name = genus_name)
} else {
  # If "species" header is not found, add empty columns
  data$pdf_table_number <- pdf_table_number
  data$genus_name <- genus_name
}
