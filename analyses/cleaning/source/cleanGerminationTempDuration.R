## Updated February 10 2024 ##
## By Justin ##

## This contains cleaning of germination temperature ##
## Original code taken from file called cleaningDL.R ##

# Trying to figure out how the code from cleanall.R transfers over without using the setwd? Or we do the setwd() anyway?

setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
source("cleaning/source/mergedata.R")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# my first look through with tidyverse() for now:
library(tidyverse)
d2 <- d %>%
  filter(germ.temp == 45219) #it's tang10b
d3 <- d %>%
  filter(datasetID == "tang10b") #instead of 45219 it should be 20/10

unique(d$germ.temp)

# Some Qs
# Some of them say +/-, should we just take the central value?
# For alternating germ.temps, should we keep the alternation or take the mean?
# Some of them are in date format, like 20-Oct; this is probably 20/8 warm/cold cycle.
# Some have dashes instead of slash, probably meaning slash; 3-20 certainly should not mean a broad unknown temperature range spanning 17 degrees.
# ^Commas probably mean the same thing.
# Those that have the photoperiod include, should check the photoperiod column to make sure the information is also present there.
# remove "varying".
# If it's NA but has a secondary comment, take it out and place that information elsewhere.
# Why are some of these values so random in their decimals?
# Why do some of them say "greenhouse" as the germ.temp?
# Take out rows containing "open field".
# What to do with unregulated? Probably do NA, since "unknown/didn't mention" are also changed to NA.


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



#1. germination temperature
unique(d$germ.temp)

#d <- within(d, forcetemp[datasetID== 'falusi96' & study == 'exp3'] <- 24)
# ALERT: these changes should be in a NEW column, not overwriting the current column. 

# d$germ.temp[which(d$germ.temp == "unknown")] <- "NA"
# d$germ.temp[which(d$germ.temp == "didn't mention")] <- "NA"

# Shouldn't the two lines above follow the same format as the example? Otherwise we are changing the data within the same column?
# e.g.;
# d <- within(d, forcetemp[datasetID == "datasetID" & study == "study"] <- value)


#Check unusual values:
# open field---does this qualify for this study ie controlled environment?

# Values that are transformed ie averaged or rounded:
d$germ.tempCor <- d$germ.temp

# Now make new column with heavy duty cleaning
d$germTemp  <- d$germ.temp

# 2. germ.duration
unique(d$germ.duration)

#This variable is important---if NA or unknown or negative please double check the paper


