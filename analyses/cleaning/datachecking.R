## Started 30 November 2023 ##
## by Deirdre

## Making notes of our methods --- have values summarizing the data we would like to reference

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(stringr)
library(plyr)
library(ggplot2)
library(viridis)

#if(length(grep("deirdreloughnan", getwd()) > 0)) {
#  setwd("~/Documents/github/egret")
#} else if(length(grep("frederik", getwd()) > 0)) {
#} else{
#  setwd("/Users/Lizzie/Documents/git/projects/egret/analyses") # for Lizzie
###  setwd("~/github/egret")
#}

# Grab de data!
d <- read.csv("..//analyses/output/egretData.csv")

noStudy <- length(unique(d$datasetID))
noSpp <- length(unique(d$sp.name))
nRow <- nrow(d)
