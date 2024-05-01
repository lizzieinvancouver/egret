## Upded 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#1. Storage
# unique(d$storage.type)

# d$storage.typeCor <- d$storage.type
# d$storage.typeCor[which(d$storage.typeCor == "laboratory")] <- "room temp"
# d$storage.typeCor[which(d$storage.typeCor == "room conditions")] <- "room temp"
# d$storage.typeCor[which(d$storage.typeCor == "coin envelope (room temperature)")] <- "room temp"
# d$storage.typeCor[which(d$storage.typeCor == "ambient")] <- "room temp"
# 
# d$storage.typeCor[which(d$storage.typeCor == "glass bottles, laboratory conditions")] <- "room temp air tight container"
# 
# #Air tight container - no temp specified 
# d$storage.typeCor[which(d$storage.typeCor == "air-tight")] <- "air-tight no temp spec"
# d$storage.typeCor[which(d$storage.typeCor == "air-tight containers")] <- "air-tight no temp spec"
# d$storage.typeCor[which(d$storage.typeCor == "airtight plastic bags")] <- "air-tight no temp spec"
# d$storage.typeCor[which(d$storage.typeCor == "airtight polyethylene bags")] <- "air-tight no temp spec"
# d$storage.typeCor[which(d$storage.typeCor == "glass container")] <- "air-tight no temp spec"
# d$storage.typeCor[which(d$storage.typeCor == "plastic bags")] <- "air-tight no temp spec"
# d$storage.typeCor[which(d$storage.typeCor == "plastic bag")] <- "air-tight no temp spec"
# d$storage.typeCor[which(d$storage.typeCor == "sealed containers")] <- "air-tight no temp spec"
# d$storage.typeCor[which(d$storage.typeCor == "sealed glass bottle")] <- "air-tight no temp spec"
# 
# #Dry
# #Dry - cold
# d$storage.typeCor[which(d$storage.typeCor == "dry/refrigerated")] <- "dry cold"
# d$storage.typeCor[which(d$storage.typeCor == "dry refrigerator")] <- "dry cold"
# d$storage.typeCor[which(d$storage.typeCor == "dry, refrigerator")] <- "dry cold"
# d$storage.typeCor[which(d$storage.typeCor == "cold dry")] <- "dry cold"
# d$storage.typeCor[which(d$storage.typeCor == "cold, dry")] <- "dry cold"
# d$storage.typeCor[which(d$storage.typeCor == "cold/dry")] <- "dry cold"
# d$storage.typeCor[which(d$storage.typeCor == "dry/cold")] <- "dry cold"
# d$storage.typeCor[which(d$storage.typeCor == "dry/refrigerated")] <- "dry cold"
# 
# # mosit -  cold
# d$storage.typeCor[which(d$storage.typeCor == "cold/wet")] <- "moist cold"
# d$storage.typeCor[which(d$storage.typeCor == "cold/moist")] <- "moist cold"
# 
# #Dark
# d$storage.typeCor[which(d$storage.typeCor == "darkness")] <- "dark"
# d$storage.typeCor[which(d$storage.typeCor == "in darkness")] <- "dark"




# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Justin's efforts
# 1 May 2024

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

# Ambient, any
d$storageType[which(d$storageType == "laboratory")] <- "ambient"
d$storageType[which(d$storageType == "controlled environment")] <- "ambient"
d$storageType[which(d$storageType == "ground at 3mm")] <- "ambient"
d$storageType[which(d$storageType == "natural environment")] <- "ambient"
d$storageType[which(d$storageType == "ambient")] <- "ambient"
d$storageType[which(d$storageType == "naked storage")] <- "ambient"

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
d$storageDetails[which(d$storageDetails == "dry")] <- "NA"
d$storageDetails[which(d$storageDetails == "air dried at 25+/-2C for 7 days, then brown paper bags")] <- "air-dry/paperbag"
d$storageDetails[which(d$storageDetails == "air dry")] <- "air-dry"
d$storageDetails[which(d$storageDetails == "sun dry")] <- "sun-dry"
d$storageDetails[which(d$storageDetails == "dry, airy")] <- "air-dry"
d$storageDetails[which(d$storageDetails == "dry, in paper bags")] <- "paperbag"
d$storageDetails[which(d$storageDetails == "dry ")] <- "NA"

# Dry cold
d$storageDetails[which(d$storageDetails == "freeze/dry")] <- "dry frozen"
d$storageDetails[which(d$storageDetails == "dry/cold")] <- "NA"
d$storageDetails[which(d$storageDetails == "cold, dry")] <- "NA"
d$storageDetails[which(d$storageDetails == "dry/refrigerated")] <- "refrigerated"
d$storageDetails[which(d$storageDetails == "dry refrigeration")] <- "refrigerated"
d$storageDetails[which(d$storageDetails == "dry refrigerator")] <- "refrigerated"
d$storageDetails[which(d$storageDetails == "dry, refrigeration")] <- "refrigerated"
d$storageDetails[which(d$storageDetails == "cold/dry")] <- "NA"
d$storageDetails[which(d$storageDetails == "cold dry")] <- "NA"

# Dry room temp
d$storageDetails[which(d$storageDetails == "dry, room temp")] <- "NA"
d$storageDetails[which(d$storageDetails == "dry room temp")] <- "NA"
d$storageDetails[which(d$storageDetails == "room temp dry")] <- "NA"

# Just room temp
d$storageDetails[which(d$storageDetails == "room conditions")] <- "NA"
d$storageDetails[which(d$storageDetails == "room temp")] <- "NA"
d$storageDetails[which(d$storageDetails == "room temperature")] <- "NA"
d$storageDetails[which(d$storageDetails == "coin envelope (room temperature)")] <- "envelope"
d$storageDetails[which(d$storageDetails == "room temp sand")] <- "in sand"

# Ambient, any
# d$storageDetails[which(d$storageDetails == "laboratory")] <- "ambient"
# d$storageDetails[which(d$storageDetails == "controlled environment")] <- "ambient"
# d$storageDetails[which(d$storageDetails == "ground at 3mm")] <- "ambient"
# d$storageDetails[which(d$storageDetails == "natural environment")] <- "ambient"
# d$storageDetails[which(d$storageDetails == "ambient")] <- "ambient"
# d$storageDetails[which(d$storageDetails == "naked storage")] <- "ambient"

# Just moist
d$storageDetails[which(d$storageDetails == "moist")] <- "NA"
d$storageDetails[which(d$storageDetails == "undried")] <- "NA"
d$storageDetails[which(d$storageDetails == "in tap water")] <- "water"
d$storageDetails[which(d$storageDetails == "partly dry")] <- "NA"
d$storageDetails[which(d$storageDetails == "wet")] <- "NA"
d$storageDetails[which(d$storageDetails == "wet; water")] <- "water"
d$storageDetails[which(d$storageDetails == "damp")] <- "NA"

# Just cold
d$storageDetails[which(d$storageDetails == "cold")] <- "NA"
d$storageDetails[which(d$storageDetails == "vapor of liquid nitrogen")] <- "liquid nitrogen vapour"
d$storageDetails[which(d$storageDetails == "cold sand")] <- "in sand"
d$storageDetails[which(d$storageDetails == "in liquid nitrogen")] <- "liquid nitrogen submerged"

# Moist cold NA
d$storageDetails[which(d$storageDetails == "cold/wet")] <- "NA"

# Just warm
d$storageDetails[which(d$storageDetails == "oven")] <- "oven"

# Just dark
d$storageDetails[which(d$storageDetails == "dark")] <- "NA"
d$storageDetails[which(d$storageDetails == "paper bags in dark")] <- "paperbag"
d$storageDetails[which(d$storageDetails == "darkness")] <- "NA"
d$storageDetails[which(d$storageDetails == "in darkness")] <- "NA"

# Photoperiod
d$storageDetails[which(d$storageDetails == "cool-white alternating 12/12")] <- "cool white light"
d$storageDetails[which(d$storageDetails == "moist, in light/dark at 12/12h")] <- "moist 12-12 photoperiod"

# dry NA dark
d$storageDetails[which(d$storageDetails == "silica gel, dark")] <- "in silica gel"
d$storageDetails[which(d$storageDetails == "dry, in dark")] <- "NA"
d$storageDetails[which(d$storageDetails == "dry shade")] <- "shade"

# NA cold dark
d$storageDetails[which(d$storageDetails == "dark refrigeration")] <- "refrigerated"

# NA room dark
d$storageDetails[which(d$storageDetails == "dark room temp")] <- "NA"
d$storageDetails[which(d$storageDetails == "room temp dark")] <- "NA"

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
d$storageDetails[which(d$storageDetails == "air-tight")] <- "NA"

# airflow
d$storageDetails[which(d$storageDetails == "air-flow")] <- "NA"

# NA
d$storageDetails[which(d$storageDetails == "no storage")] <- "NA"
d$storageDetails[which(d$storageDetails == "NA (unstored)")] <- "NA"
d$storageDetails[which(d$storageDetails == "NA (control)")] <- "NA"
d$storageDetails[which(d$storageDetails == "NA")] <- NA

# Washitani85
d$storageDetails[which(d$storageDetails == "dry + cold/dry")] <- "dry first then dry/cold later"
d$storageDetails[which(d$storageDetails == "dry first then dry/cold later" & d$datasetID == "Washitani85" & d$storageType == "dry")] <- "NA"

# d %>% filter(datasetID == "Washitani85")

