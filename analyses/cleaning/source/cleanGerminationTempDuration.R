## Updated 7 April 2024 ##
## By Justin ##

# !!! Don't forget to run cleanall up to line 22 to get the data file "d" !!!

## This contains cleaning of germination temperature ##
## Original code taken from file called cleaningDL.R ##



# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# my first look through with tidyverse() for now:
# library(tidyverse)
# options(max.print=1000000)

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

# Some overview for the git issue
# Figuring out how many papers have alternating temperature regimes
# unique(d$germ.temp)
# d.alt <- d %>%
#   filter(grepl(",|/|alternating|night|-",germTemp)) %>%
#   filter(!grepl("+/-",germTemp)) %>%
#   select(datasetID,study,germTemp)
# n_distinct(d_alt$datasetID) 
# There are 122 papers in which alternating temperatures occur

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#1. germination temperature
unique(d$germ.temp)

#d <- within(d, forcetemp[datasetID== 'falusi96' & study == 'exp3'] <- 24)
# ALERT: these changes should be in a NEW column, not overwriting the current column. 

# d$germ.temp[which(d$germ.temp == "unknown")] <- "NA"
# d$germ.temp[which(d$germ.temp == "didn't mention")] <- "NA"


# Values that are transformed i.e. averaged or rounded:
d$germ.tempCor <- d$germ.temp

# Now make new column with heavy duty cleaning
d$germTemp  <- d$germ.temp

d$germTemp[which(d$germTemp == "45219")] <- "20/10"
d$germTemp[which(d$germTemp == "unknown")] <- "NA"
d$germTemp[which(d$germTemp == "unknown ")] <- "NA"
d$germTemp[which(d$germTemp == "didn't mention")] <- "NA"
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
d$germTemp[which(d$germTemp == "22.2//20/29.4")] <- "22.2/20/29.4"
d$germTemp[which(is.na(d$germTemp))] <- "NA"


#Check unusual values:
# open field---does this qualify for this study ie controlled environment?
# ____________________________________
# d.open <- d %>% 
#   filter(grepl("unregulated: 6-27|open air|open field|greenhouse",germ.temp))
# xd %>% filter(grepl("greenhouse",germTemp))
# unique(d_open$datasetID)
# # parmenter96, deb17, parvin15, and olmez08
# # parvin15 is controlled greenhouse, so it's fine
# # upon checking parmenter98, it seems there are indeed estimates for temperature; checking parmenter96
# d.parm <- d %>% 
#   filter(datasetID == "parmenter96")
# # it's either 10-30 (ok) or unregulated (6-27)
# # deb17 is open air, so disqualify
# d.olmez <- d %>%
#   filter(datasetID == "olmez08")
# ____________________________________
# Some of the plants here are open field, so we'll keep the other ones that are listed as "greenhouse"

d$germTemp[which(d$germTemp == "greenhouse")] <- "ambient"
d$germTemp[which(d$germTemp == "controlled greenhouse")] <- "ambient"
d$germTemp[which(d$germTemp == "open air")] <- "ambient"
d$germTemp[which(d$germTemp == "open field")] <- "ambient"
d$germTemp[which(d$germTemp == "unregulated: 6-27")] <- "ambient"

# We should disqualify deb17 and some of olmez08 based on their open field status
# SCRUM: olmez08 stratified their seeds indoors in a controlled environment, so keep as ambient; deb17 also has controlled environment in treatment so keep it as ambient too
# d %>% filter(datasetID == "Deb17")
# d %>% filter(datasetID == "olmez08")
# d %>% filter(germTemp == "unregulated: 6-27")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Making new columns for temperature regime class (constant or alternating), temperature 1, and temperature 2
# To make things easier turn the +/- temperature regimes into just their median value in germTemp column
# unique(d$germTemp)
# d.pom <- d %>%
#   filter(grepl("+/-",germTemp)) %>%
#   select(datasetID,study,germTemp)
# d.pom <- sub("\\+.*","\\+",d.pom$germTemp)
# d.pom <- as.data.frame(d.pom)

# sandbox dataframe to mess around with three new columns to get a feel
# d.calcomanie <- d %>%
#   mutate(tempClass = ifelse(grepl(",|/|alternating|night|-",germTemp) & !grepl("+/-",germTemp),"alternating","constant"))

# In base R
# d.calcomania <- d
# d.calcomania$germTemp <- sub("\\+.*","",d.calcomania$germTemp)
# d.calcomania$tempClass <- ifelse(grepl(",|/|alternating|night|-",d.calcomania$germTemp) & !grepl("+/-",d.calcomania$germTemp),"alternating","constant")
# unique(d.calcomania$germTemp)
# 
# d.calcomania$temp1 <- d.calcomania$germTemp
# d.calcomania$temp1 <- sub("\\/.*","",d.calcomania$temp1) #removing forslash
# d.calcomania$temp1 <- sub("\\-.*","",d.calcomania$temp1) #removing dash
# d.calcomania$temp1 <- sub("alternating temperature ","", d.calcomania$temp1)
# d.calcomania$temp1 <- sub("alternating ","", d.calcomania$temp1)
# d.calcomania$temp1 <- sub("\\s.*","",d.calcomania$temp1) #removing to (order of this matters; if you remove the space first, it messes up the two lines above)
# 
# d.calcomania$temp2 <- d.calcomania$germTemp
# d.calcomania$temp2 <- sub("alternating temperature ","", d.calcomania$temp2)
# d.calcomania$temp2 <- sub("alternating ","", d.calcomania$temp2)
# d.calcomania$temp2 <- gsub(".*/","",d.calcomania$temp2) #removing backslash
# d.calcomania$temp2 <- gsub(".*-","",d.calcomania$temp2) #removing backslash


d$germTemp <- sub("\\+.*","",d$germTemp)
d$tempClass <- ifelse(grepl(",|/|alternating|night|-",d$germTemp) & !grepl("+/-",d$germTemp),"alternating","constant")

d$germTemp[which(d$germTemp == "10-20")] <- "10/20"
d$germTemp[which(d$germTemp == "11-17")] <- "11/17"
d$germTemp[which(d$germTemp == "11-20")] <- "11/20"
d$germTemp[which(d$germTemp == "18-20")] <- "18/20"
d$germTemp[which(d$germTemp == "15-10")] <- "15/10"
d$germTemp[which(d$germTemp == "15-5")] <- "15/5"
d$germTemp[which(d$germTemp == "20-10")] <- "20/10"
d$germTemp[which(d$germTemp == "20-22")] <- "20/22"
d$germTemp[which(d$germTemp == "3-15")] <- "3/15"
d$germTemp[which(d$germTemp == "3-20")] <- "3/20"
d$germTemp[which(d$germTemp == "3-25")] <- "3/25"
d$germTemp[which(d$germTemp == "20-30")] <- "20/30"
d$germTemp[which(d$germTemp == "16-22")] <- "16/22"
d$germTemp[which(d$germTemp == "10-20")] <- "10/20"
d$germTemp[which(d$germTemp == "5-10")] <- "5/10"
d$germTemp[which(d$germTemp == "20-23")] <- "20/23"
d$germTemp[which(d$germTemp == "15-25")] <- "15/25"
d$germTemp[which(d$germTemp == "18.5-21.5")] <- "18.5/21.5"
d$germTemp[which(d$germTemp == "5 to 15")] <- "5/15"
d$germTemp[which(d$germTemp == "10 to 15")] <- "10/15"
d$germTemp[which(d$germTemp == "25 to 15")] <- "25/15"
d$germTemp[which(d$germTemp == "21/18 day/night")] <- "21/18"
d$germTemp[which(d$germTemp == "20°C (6h dark) ")] <- "20/25"
d$germTemp[which(d$germTemp == "15 - 25")] <- "15/25"
d$germTemp[which(d$germTemp == "24/30 (varying)")] <- "24/30"

d$germDuration <- d$germ.duration
d$germDuration[which(d$germDuration == "14(7)")] <- "14"
d$germDuration[which(d$germDuration == "21(7)")] <- "21"
d$germDuration[which(d$germDuration == "28(7)")] <- "28"

d$germTemp <- sub("alternating temperature ","", d$germTemp)
d$germTemp <- sub("alternating ","", d$germTemp)


d$tempClass[which(d$germTemp == "25/20/15")] <- "other"
d$tempClass[which(d$germTemp == "22.2/20/29.4")] <- "other"
d$tempClass[which(d$germTemp == "15/20/25")] <- "other"
d$tempClass[which(d$germTemp == "20/15/20/25")] <- "other"

breakbyslash <- strsplit(as.character(d$germTemp), "/", fixed=TRUE)
d$temp1 <- unlist(lapply(breakbyslash, function(x) x[1]))
d$temp2 <- unlist(lapply(breakbyslash, function(x) x[2]))
# There's a weird one where the temp is 27-29/6-18

#those with triple or quadruple temp regimes are not going to have their temp1/temp2 columns populated
d$temp1[which(d$tempClass == "other")] <- "" 
d$temp2[which(d$tempClass == "other")] <- ""

# d %>% filter(germTemp == "27-29/6-18") #Dehgan84
# This paper says it's 27-29 in the day and 16-18 at night; which value should we take for each part of photoperiod?
# SCRUM: take the mean of each period as our alternating; 28 for day and 17 at night
d$temp1[which(d$datasetID == "Dehgan84" & d$temp1 == "27-29")] <- "28"
d$temp2[which(d$datasetID == "Dehgan84" & d$temp2 == "16-18")] <- "17"
d$temp2[which(d$datasetID == "Dehgan84" & d$temp2 == "6-18")] <- "17" 
d$temp2[which(d$datasetID == "Scocco98" & d$temp2 == "30 (varying)")] <- "30" 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# 2. germ.duration
#This variable is important---if NA or unknown or negative please double check the paper
# unique(d$germ.duration)
# d$germDuration <- d$germ.duration
# 
# dneg <- d %>% filter(grepl("-",germ.duration))
# unique(dneg$datasetID)
# # jensen97, schutz02, gremer20, ren15 have NEGATIVE or a RANGE in germ duration
# duno <- d %>% filter(grepl("unknown",germ.duration))
# dna <- d %>% filter(grepl("NA",germ.duration))
# dnatrue <- d %>% filter(germ.duration = NA)
# unique(dna$datasetID)
# # kato11 and marcello15 have UNKNOWN or NA germ.duration

d$germDuration[which(d$datasetID == "jensen97" & d$germ.time.zero == "when incubation begins")] <- "30"
d$germDuration[which(d$datasetID == "gremer20" & d$germ.duration == "30-31")] <- "7"
d$germDuration[which(d$datasetID == "ren15" & d$germ.duration == "30-31")] <- "30"
d$germDuration[which(d$datasetID == "Marcello15" & d$germ.duration == "NA (<35)")] <- "35"
d$germDuration[which(d$germDuration == "~30")] <- "30"
d$germDuration[which(d$datasetID == "Schutz02" & d$germ.duration == "30-50")] <- "50"

d[, 'germDurComment'] = NA
d$germDurComment[which(d$datasetID == "Schutz02" & d$germDuration == "50")] <- "Paper says 30-50 as germDuration due to end of germination = 1 week since last observed germinant"
d$germDurComment[which(d$datasetID == "kato11" & d$germDuration == "unknown")] <- "Looked into the paper and found nothing except for germination counted every 3 days"

# unique(d$germDuration)
# dna2 <- d %>% select(germDuration)
# It says there's an NA when I do unique() but it doesn't actually exist in the dataset

# library(tidyverse)
# # unique(d$germ.temp)
# #
# dtempchange <- d
# pattern <- c("alternating","day","dark","+/-")
# pattern2 <- c("-","to","/")
# 
# dtempchange <- d %>%
#   filter(!grepl(paste(pattern,collapse="|"),germ.temp))
# unique(dtempchange$germ.temp)
# #
# dtempchange2 <- dtempchange %>%
#   filter(grepl(paste(pattern2,collapse = "|"),germ.temp))
# unique(dtempchange2$germ.temp)
# n_distinct(dtempchange2$datasetID)
# print(unique(dtempchange2$datasetID))
# #
# # getwd()
# # setwd("C:/Users/sapph/Documents/ubc things/work/egret/data")
# #
# # egretmaster <- read_csv("egret.csv")
# #
# # setwd("C:/Users/sapph/Documents/ubc things/work/egret/analyses")
# #
# # ids <- as.list(unique(dtempchange2$datasetID))
# # dflagged <- egretmaster %>%
# #   filter(studyID %in% ids)
# # runif(6,min = 1, max = 77)
# 
# d %>% filter(datasetID == "zhang21") %>%
#   select(germ.temp,species)
#
library(tidyverse)

missingpdf <- read_csv("cleaning/missingPdf.csv")
missing <- missingpdf %>%
  filter(assigned.to == "Justin")
write.csv(missing,file="missing.csv")

missingpdf$pdf.in.folder[which(missingpdf$assigned.to == "Justin")] <- "Y"
missingpdf$ILL.needed[which(missingpdf$assigned.to == "Justin")] <- "N"

missingpdf$pdf.in.folder[which(missingpdf$datasetID == "chien10")] <- "N"
missingpdf$ILL.needed[which(missingpdf$datasetID == "chien10")] <- "Y"

missingpdf$pdf.in.folder[which(missingpdf$datasetID == "cousins10")] <- "N"
missingpdf$ILL.needed[which(missingpdf$datasetID == "cousins10")] <- "Y"

missingpdf$pdf.in.folder[which(missingpdf$datasetID == "crank92")] <- "N"
missingpdf$ILL.needed[which(missingpdf$datasetID == "crank92")] <- "Y"

missingpdf$pdf.in.folder[which(missingpdf$datasetID == "geszprych02")] <- "N"
missingpdf$ILL.needed[which(missingpdf$datasetID == "geszprych02")] <- "Y"
