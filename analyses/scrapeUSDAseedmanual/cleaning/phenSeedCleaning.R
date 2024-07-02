## Updated 29 June 2024 ##
## By Britany ##
## merging phenology and seed data into one csv

#house keeping
rm(list = ls())
setwd("~/Documents/ubc/year5/TemporalEcologyLab/egret/analyses/scrapeUSDAseedmanual/")

library(dplyr)


# Define a function to read all CSV files in a directory and append them into one data frame
append_csv_files <- function(directory) {
  # Get a list of all CSV files in the directory
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty list to store the data frames
  data_list <- list()
  
  # Loop through each CSV file
  for (file in csv_files) {
    # Read the CSV file
    data <- read.csv(file)
    
    data <- data %>% mutate(across(everything(), as.character))
    
    # Append the data frame to the list
    data_list[[length(data_list) + 1]] <- data
  }
  
  # Combine all data frames in the list into one large data frame
  combined_data <- bind_rows(data_list)
  
  return(combined_data)
}

# Specify the directory containing the CSV files
pheno_directory <- "output/phenology_copy_output"
seed_directory <- "output/seed_copy_output"

# Call the function to read and append all CSV files in the directory
c_pheno <- append_csv_files(pheno_directory)
c_seed <- append_csv_files(seed_directory)

# Fix column names
p_colnames <- colnames(c_pheno)
p_colnames <- gsub("\\.", " ", p_colnames)
p_colnames <- gsub("  ", " ", p_colnames)
p_colnames <- gsub("_", " ", p_colnames)
p_colnames <- gsub(" m ", " (m)", p_colnames)
p_colnames <- gsub(" kg |kg", " (kg)", p_colnames)
p_colnames <- gsub(" mm ", " (mm)", p_colnames)
p_colnames <- gsub(" lb |lb", " (lb)", p_colnames)
p_colnames <- gsub(" yrs ", " (yrs)", p_colnames)
p_colnames <- gsub(" $", "", p_colnames)
p_colnames

s_colnames <- colnames(c_seed)
s_colnames <- gsub("\\.", " ", s_colnames)
s_colnames <- gsub("  ", " ", s_colnames)
s_colnames <- gsub("_", " ", s_colnames)
s_colnames <- gsub(" m ", " (m)", s_colnames)
s_colnames <- gsub(" kg |kg", " (kg)", s_colnames)
s_colnames <- gsub(" mm ", " (mm)", s_colnames)
s_colnames <- gsub(" lb ", " (lb)", s_colnames)
s_colnames <- gsub(" yrs ", " (yrs)", s_colnames)
s_colnames <- gsub(" $", "", s_colnames)
s_colnames <- gsub("yr", "(yr)", s_colnames)
s_colnames <- gsub("cm", "(cm)", s_colnames)
s_colnames

# Write files
write.csv(c_pheno, "output/phenologyCombined.csv")
write.csv(c_seed, "output/seedCombined.csv")

