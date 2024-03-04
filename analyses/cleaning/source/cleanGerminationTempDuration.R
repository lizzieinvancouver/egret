## Updated 3 March 2024 ##
## By Justin ##

## This contains cleaning of germination temperature ##
## Original code taken from file called cleaningDL.R ##

# Trying to figure out how the code from cleanall.R transfers over without using the setwd? Or we do the setwd() anyway?

source("cleaning/source/mergedata.R")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# my first look through with tidyverse() for now:
library(tidyverse)
options(max.print=1000000)

# Some Qs MAKE A NEW ISSUE AND PUT IN EXAMPLES OF THESE ISSUES
# Some of them say +/-, should we just take the central value? 
# For alternating germ.temps, should we keep the alternation or take the mean? ESTIMATE how often this comes up so we can think of a decision to make
# Some of them are in date format, like 20-Oct; this is probably 20/10 warm/cold cycle.
# Some have dashes instead of slash, probably meaning slash; 3-20 certainly should not mean a broad unknown temperature range spanning 17 degrees. IDENTIFY INPUT PERSON and then ADD COMMENT TO EXPLAIN WHY I MADE A CHOICE
# ^Commas probably mean the same thing.
# Those that have the photoperiod include, should check the photoperiod column to make sure the information is also present there.
# remove "varying".
# If it's NA but has a secondary comment, take it out and place that information elsewhere.
# Why are some of these values so random in their decimals? PROBABLY FINE TO KEEP
# Why do some of them say "greenhouse" as the germ.temp? CONVERT TO "AMBIENT"
# Take out rows containing "open field". DONT TAKE THEM OUT, JUST CONVERT TO "AMBIENT" put in issue, and find frequency of its occurrence
# What to do with unregulated? Probably do NA, since "unknown/didn't mention" are also changed to NA.



# Bizarre cases
# d %>% filter(germ.temp == 45219) #it's tang10b
# d %>% filter(datasetID == "tang10b") #instead of 45219 it should be 20/10
# d %>% filter(germ.temp == 100) #basbag09
# Upon confirmation with the paper, they really did expose the seeds to 100 degC, so no amendments necessary

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#1. germination temperature
unique(d$germ.temp)

#d <- within(d, forcetemp[datasetID== 'falusi96' & study == 'exp3'] <- 24)
# ALERT: these changes should be in a NEW column, not overwriting the current column. 

# d$germ.temp[which(d$germ.temp == "unknown")] <- "NA"
# d$germ.temp[which(d$germ.temp == "didn't mention")] <- "NA"

#Check unusual values:
# open field---does this qualify for this study ie controlled environment?
d$germTemp[which(d$germTemp == "unregulated: 6-27")] <- "ambient"
d$germTemp[which(d$germTemp == "controlled greenhouse")] <- "ambient"
d$germTemp[which(d$germTemp == "open air")] <- "ambient"
d$germTemp[which(d$germTemp == "open field")] <- "ambient"
d$germTemp[which(d$germTemp == "greenhouse")] <- "ambient"

# Values that are transformed i.e. averaged or rounded:
d$germ.tempCor <- d$germ.temp

# Now make new column with heavy duty cleaning
d$germTemp  <- d$germ.temp

d$germTemp[which(d$germTemp == "45219")] <- "20/10"
d$germTemp[which(d$germTemp == "unknown")] <- "NA"
d$germTemp[which(d$germTemp == "NA (germ during strat)")] <- "NA"
d$germTemp[which(d$germTemp == "15-May")] <- "15/5"
d$germTemp[which(d$germTemp == "20-Oct")] <- "20/10"
d$germTemp[which(d$germTemp == "15-Jun")] <- "15/6"
d$germTemp[which(d$germTemp == "25-Oct")] <- "25/10"
d$germTemp[which(d$germTemp == "8,10")] <- "8/10"
d$germTemp[which(d$germTemp == "8, 10")] <- "8/10"
d$germTemp[which(d$germTemp == "4, 7, 10")] <- "4/7/10"
d$germTemp[which(d$germTemp == "15,5")] <- "15/5"
d$germTemp[which(d$germTemp == "25,15")] <- "25/15"
d$germTemp[which(d$germTemp == "20,30")] <- "20/30"
d$germTemp[which(d$germTemp == "10,20")] <- "10/20"
d$germTemp[which(d$germTemp == "5,15")] <- "5/15"
d$germTemp[which(d$germTemp == "25, 20, 15")] <- "25/20/15"
d$germTemp[which(d$germTemp == "15, 20, 25")] <- "15/20/25"
d$germTemp[which(d$germTemp == "20,15,20, 25")] <- "20/15/20/25"
d$germTemp[which(is.na(d$germTemp))] <- "NA"

# unique(d$germTemp)

# 2. germ.duration
unique(d$germ.duration)

#This variable is important---if NA or unknown or negative please double check the paper


