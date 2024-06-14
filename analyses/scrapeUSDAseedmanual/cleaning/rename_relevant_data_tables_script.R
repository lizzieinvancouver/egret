# Script to Automate Renaming Relevant Data Tables
# By Selena Shew
# June 5, 2024



#Load packages
library(tidyverse)
library(tidyr)
library(tidyselect)
library(dplyr)
library(stringr)
library(readr)


#Function Code

rename_relevant_data_tables <- function(root_directory) {
  # Get a list of all files in the directory and subdirectories
  all_files <- list.files(root_directory, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  
  # Loop through each file
  for (file_path in all_files) {
    tryCatch({
      # Read in the CSV file
      df <- suppressWarnings(read_csv(file_path, col_types = cols(.default = "c")))
      
      # Create a temp df by temporarily converting columns to character for string matching
      temp_df <- df %>% mutate(across(everything(), as.character))
      
      # Phenology of Flowering & Fruiting tables
      # Check if the table is a phenology table (will contain the words Flowering or Fruit ripening in the table cells)
      if (any(sapply(temp_df, function(col) any(str_detect(col, "Flowering"), na.rm = TRUE))) && 
          any(sapply(temp_df, function(col) any(str_detect(col, "Fruit ripening"), na.rm = TRUE)))) {
        
        # Get the current file path and file name
        dir_name <- dirname(file_path)
        file_name <- basename(file_path)
        
        # Specify the new file name and file path if relevant table
        new_file_name <- paste0("phenology_", file_name)
        new_file_path <- file.path(dir_name, new_file_name)
        
        # Rename the file
        file.rename(file_path, new_file_path)
        cat("Renamed:", file_path, "to", new_file_path, "\n")
      }
      
      # Germination Test Conditions & Results tables
      # Check if the table is a germination table (should contain the words Germination and test in the cells)
      if (any(sapply(temp_df, function(col) any(str_detect(col, "Germination"), na.rm = TRUE))) && 
          any(sapply(temp_df, function(col) any(str_detect(col, "test"), na.rm = TRUE)))) {
        
        # Get the current file path and file name
        dir_name <- dirname(file_path)
        file_name <- basename(file_path)
        
        # Specify the new file name and file path if relevant table
        new_file_name <- paste0("germination_", file_name)
        new_file_path <- file.path(dir_name, new_file_name)
        
        # Rename the file
        file.rename(file_path, new_file_path)
        cat("Renamed:", file_path, "to", new_file_path, "\n")
      }
      
      # Height, Seed-Bearing Age, Seedcrop Frequency tables
      # Check if the table is a seedcrop table (should contain the words Height and Minimum in the cells)
      if (any(sapply(temp_df, function(col) any(str_detect(col, "Height"), na.rm = TRUE))) && 
          any(sapply(temp_df, function(col) any(str_detect(col, "Minimum"), na.rm = TRUE)))) {
        
        # Get the current file path and file name
        dir_name <- dirname(file_path)
        file_name <- basename(file_path)
        
        # Specify the new file name and file path if relevant table
        new_file_name <- paste0("seed_", file_name)
        new_file_path <- file.path(dir_name, new_file_name)
        
        # Rename the file
        file.rename(file_path, new_file_path)
        cat("Renamed:", file_path, "to", new_file_path, "\n")
      }
    }, error = function(e) {
      cat("Error processing file", file_path, ":", e$message, "\n")
    })
  }
}

# Test
#root_directory <- "C:\\Users\\User\\my_repo_dir\\egret\\analyses\\scrapeUSDAseedmanual\\input\\test_folder"  # Test folder
#rename_relevant_data_tables(root_directory)

# Call function on entire folder
root_directory <- "C:\\Users\\User\\my_repo_dir\\egret\\analyses\\scrapeUSDAseedmanual\\input"  # Entire dataset
rename_relevant_data_tables(root_directory)