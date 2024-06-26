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
    tryCatch({
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
      empty_rows <- which(apply(data[-1, ], 1, function(row) all(row == ""))) + 1
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
        if (length(table_row) > 0 && species_row > table_row) {
          # Check if there are rows between table_row and species_row
          if (species_row - table_row > 1) {
            between_rows <- data[(table_row + 1):(species_row - 1), ]
            species_colnames <- data[species_row, ]
            # Combine rows into a single character vector
            combined_names <- apply(rbind(between_rows, species_colnames), 2, function(col) paste(col, collapse = " "))      
            # Set the column names
            colnames(data) <- combined_names
          } else {
            # No rows between table_row and species_row, use species_row as column names
            colnames(data) <- as.character(data[species_row, ])
          }
        } else {
          # Use species_row as the column names
          colnames(data) <- as.character(data[species_row, ])
        }
        
        # Create a new data frame excluding the header row
        data <- data[-(1:species_row), ]
        
        # Add pdf_table_number and genus_name columns
        data <- data %>%
          mutate(pdf_table_number = pdf_table_number,
                 genus_name = genus_name)
      } else {
        # If "species" header is not found, use the first non-empty cell in the first column
        first_non_empty_row <- which(data[[1]] != "")[which(which(data[[1]] != "") > table_row)[1]]
        
        between_rows <- data[(table_row+1):(first_non_empty_row-1), ]
        
        combined_names <- apply(between_rows, 2, function(col) paste(col, collapse = " "))
        # Set the first non-empty row as the first row of the new dataframe and "species" as column name
        combined_names[1] <- "Species"
        # Create a new data frame excluding the rows before the first non-empty row
        data <- data[-(1:(first_non_empty_row - 1)), ]
        
        # Set the column names
        colnames(data) <- combined_names
        
        # Add pdf_table_number and genus_name columns
        data <- data %>%
          mutate(pdf_table_number = pdf_table_number,
                 genus_name = genus_name)
      }
      
      output_file_path <- file.path(output_directory, basename(file_path))
      # Write the modified data to output
      write_csv(data, output_file_path)
      
    }, error = function(e) {
      cat("Error processing file:", file_path, "\n")
      message(e)
      # Skip the file by returning early
      return(NULL)
    })
  }
  
  # Process each CSV file in the directory
  walk(csv_files, process_csv)
  
  cat("All CSV files processed successfully!\n")
}


# Run code on entire phenology dataset
prep_phen_data("scrapeUSDAseedmanual/input/copy_of_phenology_data_only", 
               "scrapeUSDAseedmanual/output/phenology_copy_output")

# Run code on entire seed dataset
prep_phen_data("scrapeUSDAseedmanual/input/copy_of_all_seed_data_only",
               "scrapeUSDAseedmanual/output/seed_copy_output")

