## Started 29 November 2023 ##
## By Lizzie ##

## The first EGRET retreat rocks on! Now we tackle respvar ##
## Goal here is a sense of which variables we have a lot of useful data on ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(stringr)
library(plyr)
library(ggplot2)
library(viridis)

setwd("/Users/Lizzie/Documents/git/projects/egret/analyses")

# Grab de data!
d <- read.csv("input/egretData.csv")
d$latbi <- paste(d$genus, d$species)
d$datasetIDstudy <- paste(d$datasetID, d$study)

## Let's get a df of things we especially care about 
dsubset <- subset(d, select=c("datasetID", "study","datasetIDstudy", "genus" , "species", "latbi", 
	"chill.temp", "chill.duration" , "germ.temp", "respvar", "response"))
dsm <- dsubset[!duplicated(dsubset), ]

## summarizing data
summaryhere <- data.frame(respvar=character(), ndatasets=numeric(), nspecies=numeric(), nchilldurations=numeric())
for(i in unique(dsm$respvar)){
	subby <- subset(dsm, respvar==i)
	summaryhereadd <- data.frame(respvar=i, 
		ndatasets=length(unique(subby$datasetIDstudy)), 
		nspecies=length(unique(subby$latbi)), 
		nchilldurations=length(unique(subby$chill.duration)))
	summaryhere <- rbind(summaryhere, summaryhereadd)
}

summaryhere <- summaryhere[with(summaryhere, order(-ndatasets, -nspecies, -nchilldurations)), ]
