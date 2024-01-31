## Updated 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#1. Storage
unique(dat$storage.type)

dat$storage.typeCor <- dat$storage.type
dat$storage.typeCor[which(dat$storage.typeCor == "laboratory")] <- "room temp"
dat$storage.typeCor[which(dat$storage.typeCor == "room conditions")] <- "room temp"
dat$storage.typeCor[which(dat$storage.typeCor == "coin envelope (room temperature)")] <- "room temp"
dat$storage.typeCor[which(dat$storage.typeCor == "ambient")] <- "room temp"

dat$storage.typeCor[which(dat$storage.typeCor == "glass bottles, laboratory conditions")] <- "room temp air tight container"

#Air tight container - no temp specified 
dat$storage.typeCor[which(dat$storage.typeCor == "air-tight")] <- "air-tight no temp spec"
dat$storage.typeCor[which(dat$storage.typeCor == "air-tight containers")] <- "air-tight no temp spec"
dat$storage.typeCor[which(dat$storage.typeCor == "airtight plastic bags")] <- "air-tight no temp spec"
dat$storage.typeCor[which(dat$storage.typeCor == "airtight polyethylene bags")] <- "air-tight no temp spec"
dat$storage.typeCor[which(dat$storage.typeCor == "glass container")] <- "air-tight no temp spec"
dat$storage.typeCor[which(dat$storage.typeCor == "plastic bags")] <- "air-tight no temp spec"
dat$storage.typeCor[which(dat$storage.typeCor == "plastic bag")] <- "air-tight no temp spec"
dat$storage.typeCor[which(dat$storage.typeCor == "sealed containers")] <- "air-tight no temp spec"
dat$storage.typeCor[which(dat$storage.typeCor == "sealed glass bottle")] <- "air-tight no temp spec"

#Dry
#Dry - cold
dat$storage.typeCor[which(dat$storage.typeCor == "dry/refrigerated")] <- "dry cold"
dat$storage.typeCor[which(dat$storage.typeCor == "dry refrigerator")] <- "dry cold"
dat$storage.typeCor[which(dat$storage.typeCor == "dry, refrigerator")] <- "dry cold"
dat$storage.typeCor[which(dat$storage.typeCor == "cold dry")] <- "dry cold"
dat$storage.typeCor[which(dat$storage.typeCor == "cold, dry")] <- "dry cold"
dat$storage.typeCor[which(dat$storage.typeCor == "cold/dry")] <- "dry cold"
dat$storage.typeCor[which(dat$storage.typeCor == "dry/cold")] <- "dry cold"
dat$storage.typeCor[which(dat$storage.typeCor == "dry/refrigerated")] <- "dry cold"

# mosit -  cold
dat$storage.typeCor[which(dat$storage.typeCor == "cold/wet")] <- "moist cold"
dat$storage.typeCor[which(dat$storage.typeCor == "cold/moist")] <- "moist cold"

#Dark
dat$storage.typeCor[which(dat$storage.typeCor == "darkness")] <- "dark"
dat$storage.typeCor[which(dat$storage.typeCor == "in darkness")] <- "dark"

#TO CHECK:
# natural as in outdoors?
# paper bags in dark - room temp?
# controlled environment - not a room?
# is wet and undried the same?
# if just says dry or wet, should we assume at room temp?
# Cold start?
