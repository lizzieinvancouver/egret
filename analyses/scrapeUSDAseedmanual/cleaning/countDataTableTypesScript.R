# Data Table Type Count
# By Selena Shew
# June 5, 2024

# Description
# This script checks whether the csv file name starts with
# phenology, germination, or seed
# and tallies them up accordingly.
# Returns a count of each of the three table types.


# Load libraries
library(stringr)
library(tools)

# Function code

count_data_table_types <- function(root_directory) {
  # Get a list of all files in the directory and subdirectories
  all_files <- list.files(root_directory, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  
  # Initialize counts of each data table type
  phenology_count <- 0
  germination_count <- 0
  seed_count <- 0
  
  # Loop through each file
  for (file_path in all_files) {
    
    # Extract just the file name from the entire file path & without the file type (AKA .csv)
    file_name <- file_path_sans_ext(basename(file_path))
    
    # Check if the file name starts with specific words: phenology, germination, or seed
    if (str_detect(file_name, "^phenology", negate = FALSE)) {
      phenology_count <- phenology_count + 1 #if it does, increase count by 1
    } else if (str_detect(file_name, "^germination", negate = FALSE)) {
      germination_count <- germination_count + 1
    } else if (str_detect(file_name, "^seed", negate = FALSE)) {
      seed_count <- seed_count + 1
    }
  }
  
  # Print final counts of each data table type
  cat("Number of CSV files starting with 'phenology':", phenology_count, "\n")
  cat("Number of CSV files starting with 'germination':", germination_count, "\n")
  cat("Number of CSV files starting with 'seed':", seed_count, "\n")
}

# Test
#root_directory <- "C:/Users/User/my_repo_dir/egret/analyses/scrapeUSDAseedmanual/input/test_folder"  # Test directory
#count_data_table_types(root_directory)

# Call function on entire folder
root_directory <- "C:\\Users\\User\\my_repo_dir\\egret\\analyses\\scrapeUSDAseedmanual\\input"  # Entire dataset
count_data_table_types(root_directory)