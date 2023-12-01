# started Nov 30, 2023 by Deirdre and Lizzie

# aim is read in the csv files from everyone's data that will include both sourced cleaning code and write out a raw file

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else{
  setwd("boomdittyboom") 
}

# 1. Get the data (reads in a dataframe called `egret')
source("cleaning/source/mergedata.R")

# 2. Clean up datasetID issues
source("cleaning/source/cleandatasetID.R")

# 3. Clean species names
source("cleaning/source/cleanspecies.R")




# XX. Clean year of germination (some)
source("cleaning/source/clean_yeargermination.R")