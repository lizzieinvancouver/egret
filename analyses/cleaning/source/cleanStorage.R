## Upded 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#1. Storage
unique(d$storage.type)

d$storage.typeCor <- d$storage.type
d$storage.typeCor[which(d$storage.typeCor == "laboratory")] <- "room temp"
d$storage.typeCor[which(d$storage.typeCor == "room conditions")] <- "room temp"
d$storage.typeCor[which(d$storage.typeCor == "coin envelope (room temperature)")] <- "room temp"
d$storage.typeCor[which(d$storage.typeCor == "ambient")] <- "room temp"

d$storage.typeCor[which(d$storage.typeCor == "glass bottles, laboratory conditions")] <- "room temp air tight container"

#Air tight container - no temp specified 
d$storage.typeCor[which(d$storage.typeCor == "air-tight")] <- "air-tight no temp spec"
d$storage.typeCor[which(d$storage.typeCor == "air-tight containers")] <- "air-tight no temp spec"
d$storage.typeCor[which(d$storage.typeCor == "airtight plastic bags")] <- "air-tight no temp spec"
d$storage.typeCor[which(d$storage.typeCor == "airtight polyethylene bags")] <- "air-tight no temp spec"
d$storage.typeCor[which(d$storage.typeCor == "glass container")] <- "air-tight no temp spec"
d$storage.typeCor[which(d$storage.typeCor == "plastic bags")] <- "air-tight no temp spec"
d$storage.typeCor[which(d$storage.typeCor == "plastic bag")] <- "air-tight no temp spec"
d$storage.typeCor[which(d$storage.typeCor == "sealed containers")] <- "air-tight no temp spec"
d$storage.typeCor[which(d$storage.typeCor == "sealed glass bottle")] <- "air-tight no temp spec"

#Dry
#Dry - cold
d$storage.typeCor[which(d$storage.typeCor == "dry/refrigerated")] <- "dry cold"
d$storage.typeCor[which(d$storage.typeCor == "dry refrigerator")] <- "dry cold"
d$storage.typeCor[which(d$storage.typeCor == "dry, refrigerator")] <- "dry cold"
d$storage.typeCor[which(d$storage.typeCor == "cold dry")] <- "dry cold"
d$storage.typeCor[which(d$storage.typeCor == "cold, dry")] <- "dry cold"
d$storage.typeCor[which(d$storage.typeCor == "cold/dry")] <- "dry cold"
d$storage.typeCor[which(d$storage.typeCor == "dry/cold")] <- "dry cold"
d$storage.typeCor[which(d$storage.typeCor == "dry/refrigerated")] <- "dry cold"

# mosit -  cold
d$storage.typeCor[which(d$storage.typeCor == "cold/wet")] <- "moist cold"
d$storage.typeCor[which(d$storage.typeCor == "cold/moist")] <- "moist cold"

#Dark
d$storage.typeCor[which(d$storage.typeCor == "darkness")] <- "dark"
d$storage.typeCor[which(d$storage.typeCor == "in darkness")] <- "dark"

#TO CHECK:
# natural as in outdoors?
# paper bags in dark - room temp?
# controlled environment - not a room?
# is wet and undried the same?
# if just says dry or wet, should we assume at room temp?
# Cold start?
