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
d$tempDay <- unlist(lapply(breakbyslash, function(x) x[1]))
d$tempNight <- unlist(lapply(breakbyslash, function(x) x[2]))
# There's a weird one where the temp is 27-29/6-18

#those with triple or quadruple temp regimes are not going to have their temp1/temp2 columns populated
d$tempDay[which(d$tempClass == "other")] <- "" 
d$tempNight[which(d$tempClass == "other")] <- ""

# d %>% filter(germTemp == "27-29/6-18") #Dehgan84
# This paper says it's 27-29 in the day and 16-18 at night; which value should we take for each part of photoperiod?
# SCRUM: take the mean of each period as our alternating; 28 for day and 17 at night
d$tempDay[which(d$datasetID == "Dehgan84" & d$tempDay == "27-29")] <- "28"
d$tempNight[which(d$datasetID == "Dehgan84" & d$tempNight == "16-18")] <- "17"
d$tempNight[which(d$datasetID == "Dehgan84" & d$tempNight == "6-18")] <- "17" 
d$tempNight[which(d$datasetID == "Scocco98" & d$tempNight == "30 (varying)")] <- "30" 
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

# Fixing the day/night conundrum
d[ , 'photoperiodNote'] = NA
d$photoperiodNote[which(d$datasetID == "Albrecht20" & d$study == "exp1" & d$photoperiod == "12")] <- "constant light" # change photoperiod to 24
d$photoperiodNote[which(d$datasetID == "Albrecht20" & d$study == "exp1" & d$photoperiod == "0")] <- "constant darkness"
d$photoperiodNote[which(d$datasetID == "Albrecht20" & d$study == "exp3" & d$photoperiod == "0")] <- "constant darkness"

d$photoperiodNote[which(d$datasetID == "cicek08" & d$species == "fraxinifolia ")] <- "just alternating temperature no photoperiod" # Change photoperiod to NA

d$photoperiodNote[which(d$datasetID == "han10" & d$species == "ingrata" & d$photoperiod == "12")] <- "assumed day is warmer temperature"

d$photoperiodNote[which(d$datasetID == "li11" & d$species == "centralasiatica" & d$photoperiod == "24")] <- "constant light"

d$photoperiodNote[which(d$datasetID == "meyer95" & d$genus == "Penstemon" & d$photoperiod == "24")] <- "constant light"

d$germTemp[which(d$datasetID == "herron01" & d$genus == "Melicytus")] <- "20" # it's not 18.5-21.5, that was a range
d$tempClass[which(d$datasetID == "herron01" & d$genus == "Melicytus")] <- "constant"
d$tempDay[which(d$datasetID == "herron01" & d$genus == "Melicytus")] <- "20"
d$tempNight[which(d$datasetID == "herron01" & d$genus == "Melicytus")] <- "NA"

d$germTemp[which(d$datasetID == "langlois17" & d$genus == "Carex")] <- "ambient" # it's not 18.5-21.5, that was a range
d$tempClass[which(d$datasetID == "langlois17" & d$genus == "Carex")] <- "constant"
d$tempDay[which(d$datasetID == "langlois17" & d$genus == "Carex")] <- "ambient"
d$tempNight[which(d$datasetID == "langlois17" & d$genus == "Carex")] <- "NA"

d$photoperiodNote[which(d$datasetID == "ochuodho08" & d$species == "capense")] <- "just alternating temperature no photoperiod"

d$photoperiodNote[which(d$datasetID == "povoa09" & d$species == "euaptoria" & d$photoperiod == "0")] <- "constant darkness"
d$photoperiodNote[which(d$datasetID == "povoa09" & d$species == "euaptoria" & d$photoperiod == "12")] <- "assumed day is warmer temperature"

d$photoperiodNote[which(d$datasetID == "roh08" & d$genus == "Corylopsis")] <- "just alternating temperature no photoperiod"

d$photoperiodNote[which(d$datasetID == "tylkowski07" & d$species == "catharticus")] <- "assumed day is warmer temperature"
d$photoperiodNote[which(d$datasetID == "tylkowski09" & d$species == "communis")] <- "assumed day is warmer temperature"
d$photoperiodNote[which(d$datasetID == "tylkowski10" & d$species == "rhamnoides")] <- "assumed day is warmer temperature"
d$photoperiodNote[which(d$datasetID == "tylkowski91" & d$species == "mas")] <- "assumed day is warmer temperature"

d$photoperiodNote[which(d$datasetID == "zhang21" & d$species == "koraiensis" & d$other.treatment == "200 μmol/m^2/s light")] <- "constant light"
d$photoperiodNote[which(d$datasetID == "zhang21" & d$species == "koraiensis" & d$other.treatment == "20 μmol/m^2/s light")] <- "constant light"
d$photoperiodNote[which(d$datasetID == "zhang21" & d$species == "koraiensis" & d$other.treatment == "0 μmol/m^2/s light")] <- "constant darkness"

d$photoperiodNote[which(d$datasetID == "Chien10" & d$species == "glaucescens")] <- "assumed day is warmer temperature"

d$photoperiodNote[which(d$datasetID == "brenchley98" & d$species == "capricorni")] <- "two stage temperature regime no photoperiod"

d$germTemp[which(d$datasetID == "markovic20" & d$genus == "Liriodendron")] <- "21.5" 
d$tempClass[which(d$datasetID == "markovic20" & d$genus == "Liriodendron")] <- "constant"
d$tempDay[which(d$datasetID == "markovic20" & d$genus == "Liriodendron")] <- "21.5"
d$tempNight[which(d$datasetID == "markovic20" & d$genus == "Liriodendron")] <- "NA"

d$germTemp[which(d$datasetID == "momonoki79" & d$genus == "Bupleurum" & d$germTemp == "15/25")] <- "25" 
d$tempClass[which(d$datasetID == "momonoki79" & d$genus == "Bupleurum" & d$germTemp == "15/25")] <- "constant"
d$tempDay[which(d$datasetID == "momonoki79" & d$genus == "Bupleurum" & d$germTemp == "15/25")] <- "25"
d$tempNight[which(d$datasetID == "momonoki79" & d$genus == "Bupleurum" & d$germTemp == "15/25")] <- "NA"

d$germTemp[which(d$datasetID == "panayotova15" & d$genus == "Betonica" & d$germTemp == "18/20")] <- "19" 
d$tempClass[which(d$datasetID == "panayotova15" & d$genus == "Betonica" & d$germTemp == "18/20")] <- "constant"
d$tempDay[which(d$datasetID == "panayotova15" & d$genus == "Betonica" & d$germTemp == "18/20")] <- "19"
d$tempNight[which(d$datasetID == "panayotova15" & d$genus == "Betonica" & d$germTemp == "18/20")] <- "NA"

d$germTemp[which(d$datasetID == "prknova15" & d$genus == "Sorbus")] <- "21" 
d$tempClass[which(d$datasetID == "prknova15" & d$genus == "Sorbus")] <- "constant"
d$tempDay[which(d$datasetID == "prknova15" & d$genus == "Sorbus")] <- "21"
d$tempNight[which(d$datasetID == "prknova15" & d$genus == "Sorbus")] <- "NA"

d$germTemp[which(d$datasetID == "yaqoob17" & d$genus == "Ferula")] <- "ambient"
d$tempClass[which(d$datasetID == "yaqoob17" & d$genus == "Ferula")] <- "constant"
d$tempDay[which(d$datasetID == "yaqoob17" & d$genus == "Ferula")] <- "ambient"
d$tempNight[which(d$datasetID == "yaqoob17" & d$genus == "Ferula")] <- "NA"

d$photoperiodNote[which(d$datasetID == "geszprych02" & d$genus == "Rhaponticum")] <- "just alternating temperature no photoperiod"

d$photoperiodNote[which(d$datasetID == "winstead71" & d$genus == "Liquidambar")] <- "photoperiod 15 but alternating temperature at 12 hr interval"

# Swapping night and day for papers in which night came first when stating their photoperiod
# We don't want to overwrite the original columns, so making copy template columns
d$tempDayCopy <- d$tempDay
d$tempNightCopy <- d$tempNight

d$tempNight[which(d$datasetID == "jiro10" & d$species == "ermanii")] <- d$tempDayCopy[which(d$datasetID == "jiro10" & d$species == "ermanii")]
d$tempDay[which(d$datasetID == "jiro10" & d$species == "ermanii")] <- d$tempNightCopy[which(d$datasetID == "jiro10" & d$species == "ermanii")]

d$tempNight[which(d$datasetID == "jiro10" & d$species == "platyphylla")] <- d$tempDayCopy[which(d$datasetID == "jiro10" & d$species == "platyphylla")]
d$tempDay[which(d$datasetID == "jiro10" & d$species == "platyphylla")] <- d$tempNightCopy[which(d$datasetID == "jiro10" & d$species == "platyphylla")]

d$tempNight[which(d$datasetID == "kato11" & d$species == "sinensis")] <- d$tempDayCopy[which(d$datasetID == "kato11" & d$species == "sinensis")]
d$tempDay[which(d$datasetID == "kato11" & d$species == "sinensis")] <- d$tempNightCopy[which(d$datasetID == "kato11" & d$species == "sinensis")]

d$tempNight[which(d$datasetID == "lee06" & d$species == "sinicus ")] <- d$tempDayCopy[which(d$datasetID == "lee06" & d$species == "sinicus ")]
d$tempDay[which(d$datasetID == "lee06" & d$species == "sinicus ")] <- d$tempNightCopy[which(d$datasetID == "lee06" & d$species == "sinicus ")]

d$tempNight[which(d$datasetID == "liu13" & d$species == "glauca")] <- d$tempDayCopy[which(d$datasetID == "liu13" & d$species == "glauca")]
d$tempDay[which(d$datasetID == "liu13" & d$species == "glauca")] <- d$tempNightCopy[which(d$datasetID == "liu13" & d$species == "glauca")]

d$tempNight[which(d$datasetID == "meyer94" & d$genus == "Penstemon")] <- d$tempDayCopy[which(d$datasetID == "meyer94" & d$genus == "Penstemon")]
d$tempDay[which(d$datasetID == "meyer94" & d$genus == "Penstemon")] <- d$tempNightCopy[which(d$datasetID == "meyer94" & d$genus == "Penstemon")]

d$tempNight[which(d$datasetID == "parmenter96" & d$species == "angustifolia")] <- d$tempDayCopy[which(d$datasetID == "parmenter96" & d$species == "angustifolia")]
d$tempDay[which(d$datasetID == "parmenter96" & d$species == "angustifolia")] <- d$tempNightCopy[which(d$datasetID == "parmenter96" & d$species == "angustifolia")]

d$tempNight[which(d$datasetID == "parmenter96" & d$species == "purpurea")] <- d$tempDayCopy[which(d$datasetID == "parmenter96" & d$species == "purpurea")]
d$tempDay[which(d$datasetID == "parmenter96" & d$species == "purpurea")] <- d$tempNightCopy[which(d$datasetID == "parmenter96" & d$species == "purpurea")]

d$tempNight[which(d$datasetID == "pipinis09" & d$species == "fruiticans")] <- d$tempDayCopy[which(d$datasetID == "pipinis09" & d$species == "fruiticans")]
d$tempDay[which(d$datasetID == "pipinis09" & d$species == "fruiticans")] <- d$tempNightCopy[which(d$datasetID == "pipinis09" & d$species == "fruiticans")]

d$tempNight[which(d$datasetID == "pipinis20" & d$species == "avellana")] <- d$tempDayCopy[which(d$datasetID == "pipinis20" & d$species == "avellana")]
d$tempDay[which(d$datasetID == "pipinis20" & d$species == "avellana")] <- d$tempNightCopy[which(d$datasetID == "pipinis20" & d$species == "avellana")]

d$tempNight[which(d$datasetID == "ren08" & d$genus == "Pedicularis")] <- d$tempDayCopy[which(d$datasetID == "ren08" & d$genus == "Pedicularis")]
d$tempDay[which(d$datasetID == "ren08" & d$genus == "Pedicularis")] <- d$tempNightCopy[which(d$datasetID == "ren08" & d$genus == "Pedicularis")]

d$tempNight[which(d$datasetID == "tang10a" & d$species == "tristaniaecarpa")] <- d$tempDayCopy[which(d$datasetID == "tang10a" & d$species == "tristaniaecarpa")]
d$tempDay[which(d$datasetID == "tang10a" & d$species == "tristaniaecarpa")] <- d$tempNightCopy[which(d$datasetID == "tang10a" & d$species == "tristaniaecarpa")]

# Deleting the template columns
d$tempDayCopy <- NULL
d$tempNightCopy <- NULL
