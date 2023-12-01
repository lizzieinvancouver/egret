# setwd
if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/data")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/data")
} else{
  setwd("boomdittyboom") # for midge
}

# housekeeping
rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

files <- list.files(pattern="^egret_.*\\.csv$")

# Initialize an empty dataframe
d <- data.frame()

# Loop through the files and append them to the dataframe
for (file in files) {
  dtemp <- read.csv(file)
  d <- rbind(d, dtemp)
}