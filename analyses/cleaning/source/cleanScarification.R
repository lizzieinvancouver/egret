## Updated 24 February 2024 ##
## By Britany ##

## This contains code to clean scarification and scarif type ##
## Original code taken from file called cleaningDL.R ##
## 24 Feb subset each gen group, can start specific grouping ## 
## 9 Apr figure out unusual entry and moisture ##
## 17 May clarify questions from git issues ##

## to do:
## rahnama-ghahfarokhi07 specify the scarification column 
## 
#1. Scarification
unique(d$scarification)
unique(d$scarif.type)

# clean 
# move hot to soaking and cold to cold trt

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
d$scarifTypeGen[which(d$scarifTypeGen == "seed coat")] <- "mechanical" #seed coat removed
d$scarifTypeGen[which(d$scarifTypeGen == "partly scarified")] <- "mechanical" #seed coat was partly removed 

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
d$scarifTypeGen[which(d$scarifTypeGen == "use  temerpature to permeate seedcoat")] <- "temperature"
d$scarifTypeGen[which(d$scarifTypeGen == "cold")] <- "temperature" #used both cold treatment and 


# move moisture to other treatment (Al-Absi10, prknova15, rouhi13, Wickens01)
d$scarifTypeGen[which(d$scarifTypeGen == "soaking in water")] <- "moisture" #included in scarification because they did both soaking and water scarification
d$scarifTypeGen[which(d$scarifTypeGen == "hot water scarification")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot water 70C")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot water")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot.water.0min")] <- "NA"
d$scarifTypeGen[which(d$scarifTypeGen == "hot.water.10min")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot.water.20min")] <- "moisture"
d$scarifTypeGen[which(d$scarifTypeGen == "hot.water.30min")] <- "moisture"
# Al-Absi10
# they see GA3 as scarifcation too but in our cleaning we put GA3 in a separate column, hence NA for GA3 scarifications
d$other.treatment[which((d$datasetID == "Al-Absi10" | 
                           d$datasetID == "al-absi10") & 
                          d$scarifTypeGen == "moisture")] <- d$scarifType[which((d$datasetID == "Al-Absi10" | 
                                                                                   d$datasetID == "al-absi10") & 
                                                                                  d$scarifTypeGen == "moisture")]
# prknova15
d$other.treatment[which((d$datasetID == "prknova15") & 
                          d$scarifTypeGen == "moisture")] <- d$scarifType[which((d$datasetID == "prknova15") & 
                                                                                  d$scarifTypeGen == "moisture")]
# rouhi13
d$other.treatment[which((d$datasetID == "rouhi13") & 
                          d$scarifTypeGen == "moisture")] <- d$scarifType[which((d$datasetID == "rouhi13") & 
                                                                                  d$scarifTypeGen == "moisture")]
# Wickens01

# other
unique(d$scarifTypeGen)

#look at each general categories 
d.temp <- subset(d,d$scarifTypeGen == "temperature")
d.moisture <- subset(d,d$scarifTypeGen == "moisture") 
d.mechanical <- subset(d,d$scarifTypeGen == "mechanical")
d.chemical <- subset(d,d$scarifTypeGen == "chemical")
unique(d.temp$datasetID)
unique(d.moisture$datasetID)
unique(d.mechanical$datasetID)
unique(d.chemical$datasetID)

# group specifically 
d$scarifTypeSpe <- d$scarif.type
unique(d$scarifTypeSpe)
d$scarifTypeSpe[which(d$scarifTypeSpe == "sandpaper")] <- "Mechanical - sandpaper"
d$scarifTypeSpe[which(d$scarifTypeSpe == "sand paper (Np. 150)")] <- "Mechanical - sandpaper"
#d$scarifTypeGen[which(d$scarifTypeGen == "Mechanical")] <- "mechanical"
d$scarifTypeSpe[which(d$scarifTypeSpe == "sand paper")] <- "Mechanical - sandpaper"
d$scarifTypeSpe[which(d$scarifTypeSpe == "mechanical with sandpaper")] <- "Mechanical - sandpaper"
d$scarifTypeSpe[which(d$scarifTypeSpe == "mechanical with razor")] <- "Mechanical - razor"
d$scarifTypeSpe[which(d$scarifTypeSpe == "coat removal")] <- "Mechanical - seed coat removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "coat removal ")] <- "Mechanical - seed coat removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "pericarp removal")] <- "Mechanical - pericarp removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "tegument cutting")] <- "Mechanical - tegument cutting"
d$scarifTypeSpe[which(d$scarifTypeSpe == "seed polishing")] <- "Mechanical - seed polishing"
d$scarifTypeSpe[which(d$scarifTypeSpe == "trimmed pappus")] <- "Mechanical - trimmed pappus"
d$scarifTypeSpe[which(d$scarifTypeSpe == "pelleted")] <- "Mechanical - pelleted"
d$scarifTypeSpe[which(d$scarifTypeSpe == "seed coat removed")] <- "Mechanical - seed coat removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Seed coat removed")] <- "Mechanical - seed coat removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "burned")] <- "Mechanical - burned"
d$scarifTypeSpe[which(d$scarifTypeSpe == "scapel")] <- "Mechanical - scapel"
d$scarifTypeSpe[which(d$scarifTypeSpe == "scalpel on lenticular side")] <- "mechanical - scalpel"
d$scarifTypeSpe[which(d$scarifTypeSpe == "manual with needle at the cotyledon end.")] <- "mechanical - needle"

# chemical
d$scarifTypeSpe[which(d$scarifTypeSpe == "Chemical")] <- "Chemical"
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4.98per.0min")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4.98per.10min")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4.98per.20min")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4.98per.30min")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "acid scarification")] <- "chemical - acid"
d$scarifTypeSpe[which(d$scarifTypeSpe == "acid")] <- "chemical - acid"
d$scarifTypeSpe[which(d$scarifTypeSpe == "NaClO")] <- "chemical - NaClO"
d$scarifTypeSpe[which(d$scarifTypeSpe == "3% H2O2 (2 hrs)")] <- "chemical - H2O2"
d$scarifTypeSpe[which(d$scarifTypeSpe == "chemical, H2O2")] <- "chemical - H2O2"

#temperature
d$scarifTypeSpe[which(d$scarifTypeSpe == "use temerpature to permeate seedcoat")] <- "temperature"
d$scarifTypeSpe[which(d$scarifTypeSpe == "cold")] <- "temperature - cold" #used both cold treatment and chilling

# moisture move to other treatments 
d$scarifTypeSpe[which(d$scarifTypeSpe == "soaking in water")] <- "moisture - water" #included in scarification because they did both soaking and water scarification
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot water scarification")] <- "moisture - hot water" #check if there's anything in other treatment 
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot water 70C")] <- "moisture - hot water" #check soaking column
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot water")] <- "moisture - hot water"
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot.water.10min")] <- "moisture - hot water"
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot.water.20min")] <- "moisture - hot water"
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot.water.30min")] <- "moisture - hot water"

### Check unusual entries--- 
### Y, cold, seed coat, partly scarified, use temperature to permeate seedcoat,
### partial scarification of seed coat, soaking in water
# soaking---wrong column? is this da in the soaking column?
# remove soaking, check if data for the whole paper is good, if not take it to lab meeting
