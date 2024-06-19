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

# Code

library(tidyverse)

prep_phen_data <- function(directory) {
  # Get a list of all CSV files in the directory
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Define a function to process each CSV file
  process_csv <- function(file_path) {
    # Read the CSV file
    data <- read_csv(file_path)
    
    # Convert all columns to character type to handle mixed data types
    data <- data %>% mutate(across(everything(), as.character))
    
    # Find rows with all empty cells
    empty_rows <- which(apply(data, 1, function(row) all(row == "")))
    
    # If there are empty rows, truncate the dataset to keep only rows before the first empty row
    if (length(empty_rows) > 0) {
      data <- data[seq_len(empty_rows[1] - 1), ]
    }
    
    
    # Write the modified data back to the CSV file
    write_csv(data, file_path)
  }
  
  # Process each CSV file in the directory
  walk(csv_files, process_csv)
  
  cat("All CSV files processed successfully!\n")
}

# Run Tests
prep_phen_data("C:\\Users\\User\\my_repo_dir\\egret\\analyses\\scrapeUSDAseedmanual\\input\\phenology_data_test_folder\\test_folder_1")

# Run code on entire phenology dataset
