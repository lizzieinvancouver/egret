# started Nov 30, 2023 by Deirdre and Lizzie

# aim is read in the csv files from everyone's data that will include both sourced cleaning code and write out a raw file

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("Buni", getwd()) > 0)) {
  setwd("~/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
}

# 1. Get the data (reads in a dataframe called `egret')
source("cleaning/source/mergedata.R")

# 2. Clean up datasetID issues
source("cleaning/source/cleandatasetID.R")

# 3. Clean species names
source("cleaning/source/cleanspecies.R")

# 4. Clean chill duration and temperature
source("cleaning/source/clean_chill_temp_duration.R")

# 5. Clean germination temperature
source("cleaning/source/clean_germination_temp_duration.R")

# 6. Clean germination scarification
source("cleaning/source/cleanScarification.R")

# 7. Clean Clean chemical---yes/no column---clean chemcial name col
source("cleaning/source/clean_chemical.R")

# 8. Clean storage type
source("cleaning/source/clean_storage.R")

# 9. Clean response variables and response
source("cleaning/source/clean_response_var.R")

# 10. Clean coordinates seed provinance
source("cleaning/source/clean_coordinates.R")

# XX. Clean year of germination (some)
source("cleaning/source/clean_yeargermination.R")