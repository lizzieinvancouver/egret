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




