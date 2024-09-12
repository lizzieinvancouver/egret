## Upded 30 January 2024 ##
## Started by Deirdre ##
## Followed by Justin's (Ngo) efforts
## Starting 1 May 2024
## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


# Making a new column for storageDetails
# Original "typeCor" will become storageType

unique(d$storage.type)

# in order of DRYNESS / TEMPERATURE / LIGHT
d$storageType <- d$storage.type
d$storageDetails <- d$storage.type

# Just dry
d$storageType[which(d$storageType == "dry")] <- "dry"
d$storageType[which(d$storageType == "air dried at 25+/-2C for 7 days, then brown paper bags")] <- "dry"
d$storageType[which(d$storageType == "air dry")] <- "dry"
d$storageType[which(d$storageType == "sun dry")] <- "dry"
d$storageType[which(d$storageType == "dry, airy")] <- "dry"
d$storageType[which(d$storageType == "dry, in paper bags")] <- "dry"
d$storageType[which(d$storageType == "dry ")] <- "dry"

# Dry cold NA
d$storageType[which(d$storageType == "freeze/dry")] <- "dry/cold"
d$storageType[which(d$storageType == "dry/cold")] <- "dry/cold"
d$storageType[which(d$storageType == "cold, dry")] <- "dry/cold"
d$storageType[which(d$storageType == "dry/refrigerated")] <- "dry/cold"
d$storageType[which(d$storageType == "dry refrigeration")] <- "dry/cold"
d$storageType[which(d$storageType == "dry refrigerator")] <- "dry/cold"
d$storageType[which(d$storageType == "dry, refrigeration")] <- "dry/cold"
d$storageType[which(d$storageType == "cold/dry")] <- "dry/cold"
d$storageType[which(d$storageType == "cold dry")] <- "dry/cold"

# Dry room NA
d$storageType[which(d$storageType == "dry, room temp")] <- "dry/room"
d$storageType[which(d$storageType == "dry room temp")] <- "dry/room"
d$storageType[which(d$storageType == "room temp dry")] <- "dry/room"

# Just room temp
d$storageType[which(d$storageType == "room conditions")] <- "room"
d$storageType[which(d$storageType == "room temp")] <- "room"
d$storageType[which(d$storageType == "room temperature")] <- "room"
d$storageType[which(d$storageType == "coin envelope (room temperature)")] <- "room"
d$storageType[which(d$storageType == "room temp sand")] <- "room"

# # Ambient, any
# d$storageType[which(d$storageType == "laboratory")] <- "ambient"
# d$storageType[which(d$storageType == "controlled environment")] <- "ambient"
# d$storageType[which(d$storageType == "ground at 3mm")] <- "ambient"
# d$storageType[which(d$storageType == "natural environment")] <- "ambient"
# d$storageType[which(d$storageType == "ambient")] <- "ambient"
# d$storageType[which(d$storageType == "naked storage")] <- "ambient"

# Just moist
d$storageType[which(d$storageType == "moist")] <- "moist"
d$storageType[which(d$storageType == "undried")] <- "moist"
d$storageType[which(d$storageType == "in tap water")] <- "moist"
d$storageType[which(d$storageType == "partly dry")] <- "moist"
d$storageType[which(d$storageType == "wet")] <- "moist"
d$storageType[which(d$storageType == "wet; water")] <- "moist"
d$storageType[which(d$storageType == "damp")] <- "moist"

# Just cold
d$storageType[which(d$storageType == "cold")] <- "cold"
d$storageType[which(d$storageType == "vapor of liquid nitrogen")] <- "cold"
d$storageType[which(d$storageType == "cold sand")] <- "cold"
d$storageType[which(d$storageType == "in liquid nitrogen")] <- "cold"

# Moist cold NA
d$storageType[which(d$storageType == "cold/wet")] <- "moist/cold"

# Just warm
d$storageType[which(d$storageType == "oven")] <- "warm"

# Just dark
d$storageType[which(d$storageType == "dark")] <- "dark"
d$storageType[which(d$storageType == "paper bags in dark")] <- "dark"
d$storageType[which(d$storageType == "darkness")] <- "dark"
d$storageType[which(d$storageType == "in darkness")] <- "dark"

# Photoperiod
d$storageType[which(d$storageType == "cool-white alternating 12/12")] <- "12-12 photoperiod"
d$storageType[which(d$storageType == "moist, in light/dark at 12/12h")] <- "moist 12-12 photoperiod"

# dry NA dark
d$storageType[which(d$storageType == "silica gel, dark")] <- "dry/dark"
d$storageType[which(d$storageType == "dry, in dark")] <- "dry/dark"
d$storageType[which(d$storageType == "dry shade")] <- "dry/dark"

# NA cold dark
d$storageType[which(d$storageType == "dark refrigeration")] <- "cold/dark"

# NA room dark
d$storageType[which(d$storageType == "dark room temp")] <- "room/dark"
d$storageType[which(d$storageType == "room temp dark")] <- "room/dark"

# airtight
d$storageType[which(d$storageType == "sealed glass bottle")] <- "airtight"
d$storageType[which(d$storageType == "glass container")] <- "airtight"
d$storageType[which(d$storageType == "sealed containers")] <- "airtight"
d$storageType[which(d$storageType == "plastic bags")] <- "airtight"
d$storageType[which(d$storageType == "plastic bag")] <- "airtight"
d$storageType[which(d$storageType == "airtight plastic bags ")] <- "airtight"
d$storageType[which(d$storageType == "glass bottles, laboratory conditions")] <- "airtight"
d$storageType[which(d$storageType == "air-tight containers")] <- "airtight"
d$storageType[which(d$storageType == "airtight polyethylene bags")] <- "airtight"
d$storageType[which(d$storageType == "air-tight")] <- "airtight"

# airflow
d$storageType[which(d$storageType == "air-flow")] <- "airflow"

# NA
d$storageType[which(d$storageType == "no storage")] <- "NA"
d$storageType[which(d$storageType == "NA (unstored)")] <- "NA"
d$storageType[which(d$storageType == "NA (control)")] <- "NA"
d$storageType[which(d$storageType == "NA")] <- NA

# Weird ones
# library(tidyverse)

# d %>% filter(storageType == "moisutre controlled") 
# yang08 Litsea coreana
# yang08 <- d %>% filter(datasetID == "yang08")
# The seeds were desiccated to varying degrees during storage
d$storageType[which(d$storageType == "moisutre controlled")] <- "moisture-controlled"

# d %>% filter(storageType == "dry + cold/dry")
# washitani85 Geranium carolinianum
# ALL seeds were in DRY ROOM storage
# ALL Seeds EXCEPT destined for stratification were then placed into DRY COLD
# washitani <- d %>% filter(datasetID == "Washitani85")

d$storageType[which(d$storageType == "dry + cold/dry" & d$treatment == "dry-chilling pretreatment")] <- "dry"
d$storageType[which(d$storageType == "dry + cold/dry" & d$treatment == "moist-chilling pretreatment")] <- "dry"
d$storageType[which(d$storageType == "dry + cold/dry")] <- "dry + dry/cold"

# d %>% filter(storageType == "cold start")
# li17 Distylium chinense
# li17 <- d %>% filter(datasetID == "li17" & genus == "Distylium")
# It's a typo from cold strat but the actual storage conditions were just cold

d$storageType[which(d$storageType == "cold start")] <- "cold"
d$storageDetails[which(d$storageType == "cold start")] <- "cold"


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Storage Details

unique(d$storageDetails)

# Just dry
d$storageDetails[which(d$storageDetails == "air dry")] <- "air-dry"
d$storageDetails[which(d$storageDetails == "sun dry")] <- "sun-dry"
d$storageDetails[which(d$storageDetails == "dry, airy")] <- "air-dry"
d$storageDetails[which(d$storageDetails == "dry, in paper bags")] <- "dry paper bag"

# Dry cold
d$storageDetails[which(d$storageDetails == "freeze/dry")] <- "dry frozen"
d$storageDetails[which(d$storageDetails == "dry/cold")] <- "dry cold"
d$storageDetails[which(d$storageDetails == "cold, dry")] <- "dry cold"
d$storageDetails[which(d$storageDetails == "dry/refrigerated")] <- "dry refrigerated"
d$storageDetails[which(d$storageDetails == "dry refrigeration")] <- "dry refrigerated"
d$storageDetails[which(d$storageDetails == "dry refrigerator")] <- "dry refrigerated"
d$storageDetails[which(d$storageDetails == "dry, refrigeration")] <- "dry refrigerated"
d$storageDetails[which(d$storageDetails == "cold/dry")] <- "dry cold"
d$storageDetails[which(d$storageDetails == "cold dry")] <- "dry cold"

# Dry room temp
d$storageDetails[which(d$storageDetails == "dry, room temp")] <- "dry room temp"
d$storageDetails[which(d$storageDetails == "room temp dry")] <- "dry room temp"

# Just room temp
d$storageDetails[which(d$storageDetails == "room conditions")] <- "room temp"
d$storageDetails[which(d$storageDetails == "room temp")] <- "room temp"
d$storageDetails[which(d$storageDetails == "room temperature")] <- "room temp"
d$storageDetails[which(d$storageDetails == "coin envelope (room temperature)")] <- "room temp envelope"
d$storageDetails[which(d$storageDetails == "room temp sand")] <- "room temp sand"

# Ambient, any
# d$storageDetails[which(d$storageDetails == "laboratory")] <- "ambient"
# d$storageDetails[which(d$storageDetails == "controlled environment")] <- "ambient"
# d$storageDetails[which(d$storageDetails == "ground at 3mm")] <- "ambient"
# d$storageDetails[which(d$storageDetails == "natural environment")] <- "ambient"
# d$storageDetails[which(d$storageDetails == "ambient")] <- "ambient"
# d$storageDetails[which(d$storageDetails == "naked storage")] <- "ambient"

# Just moist
d$storageDetails[which(d$storageDetails == "undried")] <- "moist"
d$storageDetails[which(d$storageDetails == "in tap water")] <- "water"
d$storageDetails[which(d$storageDetails == "partly dry")] <- "moist"
d$storageDetails[which(d$storageDetails == "wet; water")] <- "water"
d$storageDetails[which(d$storageDetails == "damp")] <- "moist"

# Just cold
d$storageDetails[which(d$storageDetails == "vapor of liquid nitrogen")] <- "liquid nitrogen vapour"
d$storageDetails[which(d$storageDetails == "in liquid nitrogen")] <- "liquid nitrogen submerged"

# Moist cold NA
d$storageDetails[which(d$storageDetails == "cold/wet")] <- "moist cold"

# Just warm
d$storageDetails[which(d$storageDetails == "oven")] <- "oven"

# Just dark
d$storageDetails[which(d$storageDetails == "paper bags in dark")] <- "dark paper bag"
d$storageDetails[which(d$storageDetails == "darkness")] <- "dark"
d$storageDetails[which(d$storageDetails == "in darkness")] <- "dark"

# Photoperiod
d$storageDetails[which(d$storageDetails == "cool-white alternating 12/12")] <- "cool-white light 12-12 photoperiod"
d$storageDetails[which(d$storageDetails == "moist, in light/dark at 12/12h")] <- "moist 12-12 photoperiod"

# dry NA dark
d$storageDetails[which(d$storageDetails == "silica gel, dark")] <- "dark in silica gel"
d$storageDetails[which(d$storageDetails == "dry, in dark")] <- "dry dark"
d$storageDetails[which(d$storageDetails == "dry shade")] <- "dry shade"

# NA cold dark
d$storageDetails[which(d$storageDetails == "dark refrigeration")] <- "dark refrigerated"

# NA room dark
d$storageDetails[which(d$storageDetails == "room temp dark")] <- "dark room temp"

# airtight
d$storageDetails[which(d$storageDetails == "sealed glass bottle")] <- "glass container"
d$storageDetails[which(d$storageDetails == "glass container")] <- "glass container"
d$storageDetails[which(d$storageDetails == "sealed containers")] <- "container"
d$storageDetails[which(d$storageDetails == "plastic bags")] <- "plastic bag"
d$storageDetails[which(d$storageDetails == "plastic bag")] <- "plastic bag"
d$storageDetails[which(d$storageDetails == "airtight plastic bags ")] <- "plastic bag"
d$storageDetails[which(d$storageDetails == "glass bottles, laboratory conditions")] <- "glass container"
d$storageDetails[which(d$storageDetails == "air-tight containers")] <- "container"
d$storageDetails[which(d$storageDetails == "airtight polyethylene bags")] <- "plastic bag"

# airflow
d$storageDetails[which(d$storageDetails == "air-flow")] <- "air-flow"

# NA
d$storageDetails[which(d$storageDetails == "no storage")] <- "NA"
d$storageDetails[which(d$storageDetails == "NA (unstored)")] <- "NA"
d$storageDetails[which(d$storageDetails == "NA (control)")] <- "NA"
d$storageDetails[which(d$storageDetails == "NA")] <- NA

# Washitani85
d$storageDetails[which(d$storageDetails == "dry + cold/dry")] <- "dry first then dry/cold later"
d$storageDetails[which(d$storageDetails == "dry first then dry/cold later" & d$datasetID == "Washitani85" & d$storageType == "dry")] <- "NA"

## cleaning storage temp and time values
d$storageTemp <- d$storage.temp
d$storageDuration <- d$storage.time

unique(d$storage.temp)
unique(d$storage.time)
unique(d$storageTemp)
unique(d$storageDuration)

idx <- which(d$storage.time == unique(d$storageDuration)[58])
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "species", "storage.type", "storage.temp",
                                        "storage.time", "storage.humidity", "treatment", "chillTemp",
                                        "chillDuration", "storageType", "storageDetails",
                                        "respvar", "response", "figure"))

idx <- which(d$datasetID == "bytnerowicz14")
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "species", "storage.type", "storage.temp",
                                        "storage.time", "storage.humidity", "treatment", "chill.temp",
                                        "chill.duration", "chillTemp", "chillDuration", "storageType",
                                        "storageDetails", "figure"))

##cleaned remaining storage duration

#bhatt00 - estimate
d$storageTemp[d$datasetID == "pipinis12"] <- 26
d$storageDuration[d$datasetID == "bhatt00"] <- NA

#aldridge1992 - 5, paper pending

#bytnerowicz - 5/12, a lot of storage data need rescraping
d$storageDuration[d$datasetID == "bytnerowicz14" & d$storage.time == "5/12"] <- NA

#middleton96 - 1, 2-3, >3 years 
temp <- rep(c(365, 913, NA), 2)
d$storageDuration[d$datasetID == "middleton96" & !is.na(d$storage.time)] <- temp

#pipinis12 - 1 month
d$storageDuration[d$datasetID == "pipinis12"] <- 30

#tang21 - 0-3
d$storageDuration[d$datasetID == "tang21" & d$storage.time == "0-3"] <- 1.5

#tylkowski07 - 28-49, have to fix chilling
d$storagetemp[d$datasetID == "tylkowski07"] <- 0
d$storageDuration[d$datasetID == "tylkowski07"] <- 0

#yan16 - 35-75
d$storageDuration[d$datasetID == "yan16" & d$storage.time == "35-75"] <- 55

#feng18 - very short, only for transportation
d$storageDuration[d$datasetID == "feng18"] <- NA

#werner13 - 1 to 2 years
d$storageDuration[d$datasetID == "werner13" & d$figure == "table 2"] <- 730
d$storageDuration[d$datasetID == "werner13" & d$figure == "table 3"] <- 365

#washitani89 - 60 to 90
d$storageDuration[d$datasetID == "washitani89"] <- 75

#zulfiqar15 - ~ 90
d$storageDuration[d$datasetID == "zulfiqar15"] <- 91

#bibby53 - 14-28, paper pending
#d$storageDuration[d$datasetID == "bibby53" & d$storage.time == "14-28"] <- 21

#ren08 - 30-31
d$storageDuration[d$datasetID == "ren08"] <- 30

#gimenez-benavides13 - 4-5 months
d$storageDuration[d$datasetID == "gimenez-benavides13"] <- 123

#guo20 - ~*
temp <- c(rep(1095, 4), rep(730, 4), rep(365, 4), rep(0, 4),
          rep(1095, 2), rep(730, 2), rep(365, 2), rep(0, 2),
          rep(1095, 2), rep(730, 2), rep(365, 2), rep(0, 2),
          rep(1095, 2), rep(730, 2), rep(365, 2), rep(0, 2))
d$storageTemp[which(d$datasetID == "guo20" & d$study != "exp1")] <- 5
d$storageDuration[which(d$datasetID == "guo20" & d$study != "exp1")] <- temp

#all vague durations
d$storageDuration[d$storage.time == "didn't mention" |
                    d$storage.time == "few days" |
                    d$storage.time == "until constant weight"] <- NA

##cleaned storage temp

#acosta12 - 18-20
d$storageTemp[d$datasetID == "acosta12"] <- 19
d$storageDuration[d$datasetID == "acosta12"] <- 120

#brandel2005 - 15-20
d$storageTemp[d$datasetID == "brandel2005"] <- 17.5

#vahdati12 - 15-20
d$storageTemp[d$datasetID == "vahdati12"] <- 17.5

#albrecht20 - 21-23
d$storageTemp[d$datasetID == "albrecht20"] <- 22
d$storageDuration[d$datasetID == "albrecht20"] <- NA

#boscagli01 - 18-22
d$storageTemp[d$datasetID == "boscagli01"] <- 20

#basaran12 - 18+/-2, paper pending
#d$storageTemp[d$datasetID == "basaran12"] <- 18

#jusung16 - 4+/-1
d$storageTemp[d$datasetID == "jusung16"] <- 4

#karlsson08 - natural, fluctuating temperatures
d$storageTemp[d$datasetID == "karlsson08"] <- NA

#kazaz10 - 20-24
d$storageTemp[d$datasetID == "kazaz10"] <- 22

#meyer94 - 20-22
d$storageTemp[d$datasetID == "meyer94"] <- 21

#meyer95 - 20-22
d$storageTemp[d$datasetID == "meyer95"] <- 21

#schutz02 - 20-22
d$storageTemp[d$datasetID == "schutz02"] <- 21
d$storageTemp[d$datasetID == "schutz02"] <- NA

#ordonez-salanueva15 - 20+/-1
d$storageTemp[d$datasetID == "ordonez-salanueva15"] <- 20

#parmenter96 - 2-3
d$storageTemp[d$datasetID == "parmenter96"] <- 2.5

#alptekin2002 - 105
d$storageTemp[d$datasetID == "alptekin2002"] <- NA
d$storageDuration[d$datasetID == "alptekin2002"] <- NA

#naseri18 - 2 - 4
d$storageTemp[d$datasetID == "naseri18"] <- 3

#pipinis09 - 2 - 4
d$storageTemp[d$datasetID == "pipinis09"] <- 3

#sacande04 - 103
d$storageTemp[d$datasetID == "sacande04"] <- 5
d$storageDuration[d$datasetID == "sacande04"] <- 183

#all room temp
d$storageTemp[d$storage.temp == "room temp" | d$storage.temp == "room temperature"] <- "NA"

#lee21 - room temp/4
d$storageTemp[d$datasetID == "lee21"] <- "NA then 4"
d$storageDuration[d$datasetID == "lee21"] <- "30 then NA"

#lee06 - 20-25
d$storageTemp[d$datasetID == "lee06" & d$storage.temp == "20-25"] <- 22.5

#li21 - 20 - 25
d$storageTemp[d$datasetID == "li21" & d$storage.temp == "20 - 25"] <- 22.5

#pipinis20 - multiple values
d$storageTemp[d$datasetID == "pipinis20"] <- 4

#pritchard93 - 11-16, 16/4
d$storageTemp[d$datasetID == "pritchard93" & d$storage.temp == "11-16"] <- 13.5
d$storageTemp[d$datasetID == "pritchard93" & d$storage.temp == "16/4"] <- "16 then 4"
d$storageDuration[d$datasetID == "pritchard93" & d$storage.time == "7-9"] <- 8
d$storageDuration[d$datasetID == "pritchard93" & d$storage.time == "10-14"] <- 12
d$storageDuration[d$datasetID == "pritchard93" & d$storage.time == "21-35"] <- 28
d$storageDuration[d$datasetID == "pritchard93" & d$storage.time == "2 wk (16°C) + 4 wk (2°C )"] <- "14 then 14"

#prknova15 - multiple values
d$storageTemp[d$datasetID == "prknova15"] <- "multiple"

#arslan11 - 18 - 20
d$storageTemp[d$datasetID == "arslan11"] <- 19

#downie91 - 2 - 4
d$storageTemp[d$datasetID == "downie91"] <- 3

#marcello15 - 22+-2
d$storageTemp[d$datasetID == "marcello15"] <- 18

#esmaeili09
d$storageTemp[d$datasetID == "esmaeili09"] <- 20

#mamut20 - 23 - 26
d$storageTemp[d$datasetID == "mamut20" & is.na(d$chill.temp)] <- 24.5
d$storageDuration[d$datasetID == "mamut20" & !is.na(d$chill.temp)] <- NA
d$storageDuration[d$datasetID == "mamut20" & d$storage.time == "0 (control)"] <- 0

#washitani85 - room temp + 4
d$storageTemp[d$datasetID == "washitani85" & is.na(d$chill.temp)] <- "NA then 4"
d$storageDuration[d$datasetID == "washitani85" & is.na(d$chill.temp)] <- "153 then 61"
d$storageTemp[d$datasetID == "washitani85" & !is.na(d$chill.temp)] <- NA
d$storageDuration[d$datasetID == "washitani85" & !is.na(d$chill.temp)] <- 214

#zhou08 - 5,25, room temp
d$storageTemp[d$datasetID == "zhou08" & d$storage.temp == "room temp"] <- 17.5
d$storageTemp[d$datasetID == "zhou08" & d$storage.temp != "room temp"] <- NA
d$storageDuration[d$datasetID == "zhou08" & d$storage.temp != "room temp"] <- NA

#yang08 - 4:65, 30/20, 4
d$storageTemp[d$datasetID == "yang08" & d$figure == "Figure 3"] <- 4
d$storageTemp[d$datasetID == "yang08" & d$storage.temp == "30/20, 4"] <- NA
d$storageDuration[d$datasetID == "yang08" & d$storage.temp == "30/20, 4"] <- NA

#li17 - avg17, avg18, paper pending
#d$storageTemp[d$storage.temp == "avg 17"] <- 17
#d$storageTemp[d$storage.temp == "avg 18"] <- 18

#ghimeray14 - 22+-22
d$storageTemp[d$storage.temp == "22+-22"] <- 22
d$storageDuration[d$datasetID == "ghimeray14" & d$storage.time == "30-31"] <- 30
d$storageDuration[d$datasetID == "ghimeray14" & d$storage.time == "60-62"] <- 61

#ren04 - 23-26
d$storageTemp[d$datasetID == "ren04"] <- 24.5

#rezvani14 - 20+-5
d$storageTemp[d$datasetID == "rezvani14"] <- 20

#kolodziejek18 - 21.5-22
d$storageTemp[d$datasetID == "kolodziejek18"] <- 21.75
d$storageTemp[d$datasetID == "kolodziejek18" & !is.na(d$chill.temp)] <- NA
d$storageDuration[d$datasetID == "kolodziejek18" & !is.na(d$chill.temp)] <- NA

#ahola99 - 2.1-5.9
d$storageTemp[d$datasetID == "ahola99" & d$species == "pendula"] <- "-18 then NA"
d$storageTemp[d$datasetID == "ahola99" & d$species == "abies"] <- "-3 then NA"
d$storageTemp[d$datasetID == "ahola99" & d$species == "sylvestris"] <- "-3 then NA"
d$storageDuration[d$datasetID == "ahola99"] <- "NA then NA"

#veatch-blohm11 - 21-25
d$storageTemp[d$datasetID == "veatch-blohm11"] <- 23

#veiga-barbosa14 - ambient
d$storageTemp[d$datasetID == "veiga-barbosa14"] <- 23
d$storageDuration[d$datasetID == "veiga-barbosa14"] <- 123

