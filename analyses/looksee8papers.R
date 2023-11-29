## Started 28 November 2023 ##
## By Lizzie ##

## Looking at a subset of the EGRET data (that overlaps with OSPREE spp) ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(stringr)
library(ggplot2)
library(gridExtra)
library(viridis)

setwd("/Users/Lizzie/Documents/git/projects/egret/analyses")
d <- read.csv("output/ospree8studies.csv")
d$latbi <- paste(d$genus, d$species)

## Looking at some basics 
table(paste(d$datasetID, d$study))
sort(table(d$respvar))

table(d$datasetID, d$germ.duration)

# Let's just start with ONE study ...
fs  <- subset(d, genus=="Fagus")
unique(fs$study)
unique(fs$respvar)
unique(fs$germ.duration)

## Okay ... let's find some common responses
table(d$respvar, d$datasetID)
# a through D_lag-50.min are all one study (which also has per.germ.cumulative) so skip them ... 
# how to convert germ.days? My want to fit a model and extract something common to other studies
d$respvaradj <- d$respvar
d$respvaradj[which(d$respvar=="TMGR")] <- "germ.time and mgt" # looked at it and it appeared to be mean-ish
d$respvaradj[which(d$respvar=="germ.time")] <- "germ.time and mgt"
d$respvaradj[which(d$respvar=="log10(mgt)")] <- "germ.time and mgt"
d$response[which(d$respvar=="log10(mgt)")]  <- 10^log(d$response[which(d$respvar=="log10(mgt)")])

dsm <- d[which(d$respvaradj %in% c("germ.time and mgt", "per.germ")),]

# Stuff to deal with, but that I am ignoring for now
table(dsm$soaked.in, dsm$datasetID)
table(dsm$other.treatment, dsm$datasetID)
table(dsm$scarification, dsm$datasetID) # thomsen did both Y/N scarring

# Columns to clean 
table(dsm$chill.temp, dsm$datasetID)
table(dsm$chill.duration, dsm$datasetID)
table(dsm$germ.temp, dsm$datasetID)

dsm$chill.tempclean <- as.numeric(dsm$chill.temp)
dsm$chill.tempclean[which(dsm$chill.temp=="3-5")] <- 4
dsm$chill.tempclean[which(dsm$chill.temp=="4+/-0.5")] <- 1.75

dsm$chill.durationclean <- as.numeric(dsm$chill.duration)
dsm$chill.durationextra <- 0
dsm$chill.durationclean[grep("(cold)", dsm$chill.duration)] <- 
as.numeric(regmatches(dsm$chill.duration[grep("(cold)", dsm$chill.duration)], 
	gregexpr("\\d+(?= days \\(cold\\))", dsm$chill.duration[grep("(cold)", dsm$chill.duration)], perl = TRUE)))
dsm$chill.durationextra[grep("(warm)", dsm$chill.duration)] <-  
	as.numeric(regmatches(dsm$chill.duration[grep("(warm)", dsm$chill.duration)], 
	gregexpr("\\d+(?= days \\(warm\\))", dsm$chill.duration[grep("(warm)", dsm$chill.duration)], perl = TRUE)))
dsm$chill.durationclean[which(is.na(dsm$chill.durationclean)==TRUE)] <- 0 

dsm$germ.tempclean <- as.numeric(dsm$germ.temp)
dsm$germ.tempclean[which(dsm$germ.temp=="20/30")] <- 25
dsm$germ.tempclean[which(dsm$germ.temp=="30/20")] <- 25
dsm$germ.tempclean[which(dsm$germ.temp=="13+/-0.5")] <- 6.75
dsm$germ.tempclean[which(dsm$germ.temp=="20°C (6h dark) + 25°C (18h light)")] <- 23.75

ggplot(subset(dsm, respvaradj=="per.germ"), aes(y=response, x=chill.durationclean, 
	color=germ.temp)) +
	geom_point() + 
	facet_wrap(latbi~., scales="free") 

ggplot(subset(dsm, respvaradj=="germ.time and mgt"), aes(y=response, x=chill.durationclean, 
	color=germ.temp, shape=other.treatment)) +
	geom_point() + 
	facet_wrap(latbi~., scales="free") 

ggplot(subset(dsm, respvaradj=="per.germ"), aes(y=response, x=chill.temp, 
	color=germ.temp)) +
	geom_point() + 
	facet_wrap(latbi~., scales="free") 

ggplot(subset(dsm, respvaradj=="per.germ"), aes(y=response, x=chill.temp, 
	color=germ.temp, shape=other.treatment)) +
	geom_point() + 
	facet_wrap(latbi~., scales="free") 