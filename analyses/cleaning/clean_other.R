## Updated 30 November 2023 ##
## By Lizzie and Deirdre ##

## This contains miscellaneous cleaning of specific entries ##
## Original file called coordinate_cleaning_JS.R ##

# setwd
if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else{
  setwd("boomdittyboom") # for midge
}

# housekeeping
rm(list = ls()) # Clear whatever is already in R's memory
options(stringsAsFactors=FALSE)# Make sure words are read in as characters rather than factors

## load packages ##
library(leaflet)
library(sp)
library(sf)

# grab the data 
egret <- read.csv("input/egretData.csv")




unique(egret$year.germination)
egret$year.germination[which(egret$year.germination == "n/a")] <- NA #only 19 report this
#TO CHECK - TRUE? But what is the value?
germyeartru <- egret[which(egret$year.germination == "TRUE"),] 
#hawkins19 found to have germination year of 2010 
egret$year.germination[which(egret$year.germination == "TRUE")] <- "2010" 
# rechecking all of the NA values
germ_year <- egret[which(is.na(egret$year.germination)),]
unique(germ_year$datasetID) #80 papers where there is no germ year 




