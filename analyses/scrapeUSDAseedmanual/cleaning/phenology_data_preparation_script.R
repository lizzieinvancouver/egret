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

prep_phen_data <- function(directory) {
  # Get a list of all CSV files in the directory
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Define a function to process each CSV file
  process_csv <- function(file_path) {
    # Read the CSV file
    data <- read_csv(file_path, col_names = FALSE)
    
    # Convert all columns to character type to handle mixed data types
    data <- data %>% mutate(across(everything(), as.character))
    
    # Find rows with all empty cells
    empty_rows <- which(apply(data, 1, function(row) all(row == "")))
    
    # If there are empty rows, truncate the dataset to keep only rows before the first empty row
    if (length(empty_rows) > 0) {
      data <- data[seq_len(empty_rows[1] - 1), ]
    }
    
    # Remove quotation marks from all cells
    data <- data %>% mutate(across(everything(), ~ str_replace_all(.x, "'", "")))
    
    # Find the row index where the header contains "Table"
    table_row <- which(str_detect(data[[1]], "Table"))
    
    if (length(table_row) > 0) {
      # Get the header row as character vector
      header_row <- as.character(data[table_row, ])
      
      # Use str_match to extract pdf_table_number and genus_name
      match <- str_match(header_row, "Table ([0-9]+)-(.*?)(,|$)")
      
      if (!is.na(match[1, 1])) {
        pdf_table_number <- match[1, 2]  # Captured group 1
        genus_name <- match[1, 3]  # Captured group 2
        
        # Repeat pdf_table_number and genus_name for each row in the dataframe
        pdf_table_number <- rep(pdf_table_number, nrow(data))
        genus_name <- rep(genus_name, nrow(data))
        # Add pdf_table_number and genus_name as new columns
        data <- data %>% 
          mutate(pdf_table_number = pdf_table_number,
                 genus_name = genus_name)
      } else {
        # If match is not found, set pdf_table_number and genus_name to NA
        data$pdf_table_number <- NA
        data$genus_name <- NA
      }
    } else {
      # If "Table" header is not found, set pdf_table_number and genus_name to NA
      data$pdf_table_number <- NA
      data$genus_name <- NA
    }
    
    # Write the modified data back to the CSV file
    write_csv(data, file_path)
  }
  
  # Process each CSV file in the directory
  walk(csv_files, process_csv)
  
  cat("All CSV files processed successfully!\n")
}

# Run Tests
prep_phen_data("scrapeUSDAseedmanual/input/test_folder")

# Run code on entire phenology dataset

#testing
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

data <- read_csv(csv_files[1], col_names = FALSE)

# Convert all columns to character type to handle mixed data types
data <- data %>% mutate(across(everything(), as.character))

# Find rows with all empty cells
empty_rows <- which(apply(data, 1, function(row) all(row == "")))

# If there are empty rows, truncate the dataset to keep only rows before the first empty row
if (length(empty_rows) > 0) {
  data <- data[seq_len(empty_rows[1] - 1), ]
}

# Remove quotation marks from all cells
data <- data %>% mutate(across(everything(), ~ str_replace_all(.x, "'", "")))

# Find the row index where the header contains "Table"
table_row <- which(str_detect(data[[1]], "Table"))

if (length(table_row) > 0) {
  # Get the header row as character vector
  header_row <- as.character(data[table_row, ])
  
  # Use str_match to extract pdf_table_number and genus_name
  match <- str_match(header_row, "Table ([0-9]+)-(.*?)(,|$)")
  
  if (!is.na(match[1, 1])) {
    pdf_table_number <- match[1, 2]  # Captured group 1
    genus_name <- match[1, 3]  # Captured group 2
    
    # Repeat pdf_table_number and genus_name for each row in the dataframe
    pdf_table_number <- rep(pdf_table_number, nrow(data))
    genus_name <- rep(genus_name, nrow(data))
    # Add pdf_table_number and genus_name as new columns
    data <- data %>% 
      mutate(pdf_table_number = pdf_table_number,
             genus_name = genus_name)
  } else {
    # If match is not found, set pdf_table_number and genus_name to NA
    data$pdf_table_number <- NA
    data$genus_name <- NA
  }
} else {
  # If "Table" header is not found, set pdf_table_number and genus_name to NA
  data$pdf_table_number <- NA
  data$genus_name <- NA
}
