## Started 28 November 2023 ##
## By Lizzie ##

## Looking at a subset of the EGRET data (that overlaps with OSPREE spp) ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(ggplot2)
library(gridExtra)
library(viridis)

setwd("/Users/Lizzie/Documents/git/projects/egret/analyses")
d <- read.csv("output/ospree8studies.csv")

## Looking at some basics 
table(paste(d$datasetID, d$study))
sort(table(d$respvar))
table(d$datasetID, d$germ.duration)

# Let's just start with ONE study ...
fs  <- subset(d, genus=="Fagus")
unique(fs$study)
unique(fs$respvar)
unique(fs$germ.duration)

