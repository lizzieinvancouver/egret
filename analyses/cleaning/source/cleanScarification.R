## Updated 24 February 2024 ##
## By Britany ##

## This contains code to clean scarification and scarif type ##
## Original code taken from file called cleaningDL.R ##
## 24 Feb subset each gen group, can start specific grouping ## 
#1. Scarification
unique(d$scarification)
unique(d$scarif.type)

# clean 
# move hot to soaking and cold to cold trt
d$scarifType <- d$scarif.type

# group more generally --- chemical vs mechanical
d$scarifTypeGen <- d$scarif.type
# mechanical 
d$scarifTypeGen[which(d$scarifTypeGen == "Mechanical")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "sandpaper")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "sand paper")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "sand paper (Np. 150)")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "mechanical - sandpaper")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "mechanical with sandpaper")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "mechanical - razor")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "mechanical with razor")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "coat removal")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "coat removal ")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "pericarp removal")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "tegument cutting")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "seed polishing")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "trimmed pappus")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "pelleted")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "seed coat removed")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "Seed coat removed")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "burned")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "scapel")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "scalpel on lenticular side")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "manual with needle at the cotyledon end.")] <- "mechanical"

# chemical
d$scarifTypeGen[which(d$scarifTypeGen == "Chemical")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "H2SO4")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "H2SO4.98per.0min")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "H2SO4.98per.10min")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "H2SO4.98per.20min")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "H2SO4.98per.30min")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "acid scarification")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "acid")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "NaClO")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "3% H2O2 (2 hrs)")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "chemical, H2O2")] <- "chemical"


# temperature
d$scarifTypeGen[which(d$scarifTypeGen == "use temerpature to permeate seedcoat")] <- "temperature"
d$scarifTypeGen[which(d$scarifTypeGen == "cold")] <- "temperature"


# moisture 
d$scarifTypeGen[which(d$scarifTypeGen == "soaking in water")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot water scarification")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot water 70C")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot water")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot.water.10min")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot.water.20min")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot.water.30min")] <- "moisture"

# other
unique(d$scarifTypeGen)

#look at each general categories 
d.temp <- subset(d,d$scarifTypeGen == "temperature")
d.moisture <- subset(d,d$scarifTypeGen == "moisture") #move water to soaking column??
d.mechanical <- subset(d,d$scarifTypeGen == "mechanical")
d.chemical <- subset(d,d$scarifTypeGen == "chemical")
unique(d.temp$datasetID)
unique(d.moisture$datasetID)
unique(d.mechanical$datasetID)
unique(d.chemical$datasetID)

# group specifically 
d$scarifTypeSpe <- d$scarif.type

d$scarifTypeSpe[which(d$scarifTypeSpe == "sandpaper")] <- "mechanical - sandpaper"
d$scarifTypeSpe[which(d$scarifTypeSpe == "sand paper (Np. 150)")] <- "mechanical - sandpaper"

d$scarifTypeSpe[which(d$scarifTypeSpe == "mechanical with razor")] <- "mechanical - razor"
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4")] <- "chemical - H2SO4"


### Check unusual entries--- 
### Y, cold, seed coat, partly scarified, use temperature to permeate seedcoat,
### partial scarification of seed coat, soaking in water
# soaking---wrong column? is this da in the soaking column?
# remove soaking, check if data for the whole paper is good, if not take it to lab meeting
# is hot water scarification vs soaking---to dicuss as a group