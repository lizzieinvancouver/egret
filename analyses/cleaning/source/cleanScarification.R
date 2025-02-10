## Updated 5 July 2024 ##
## By Britany Wu ##

## This contains code to clean scarification and scarif type ##
## Original code taken from file called cleaningDL.R ##
## 24 Feb subset each gen group, can start specific grouping ## 
## 9 Apr figure out unusual entry and soakin ##
## 17 May clarify questions from git issues ##
## 5 July done ##

## 
#1. Scarification
unique(d$scarification)
unique(d$scarif.type)

# clean 

# fix typos and re-organizing 
d$scarifType <- d$scarif.type
unique(d$scarifType)

d$scarifType[which(d$datasetID == "rizwan18" & d$scarif.type == "Y")] <- NA
d$scarifType[which(d$scarifType == "partial scarification of seed coat")] <- "partially scarified"
d$scarifType[which(d$scarifType == "seed coat")] <- "seed coat removal"
d$scarifType[which(d$scarifType == "coat removal ")] <- "seed coat removal"
d$scarifType[which(d$scarifType == "seed coat removed")] <- "seed coat removal"
d$scarifType[which(d$scarifType == "mechanical with sandpaper")] <- "mechanical - sandpaper"
d$scarifType[which(d$scarifType == "mechanical with razor")] <- "mechanical - razor"
d$scarifType[which(d$scarifType == "use temerpature to permeate seedcoat")] <- NA
d$scarifType[which(d$datasetID == "lee06")] <- NA #stratification not scarification
d$scarifType[which(d$datasetID == "grose57")] <- NA #stratification not scarification
d$scarifType[which(d$datasetID == "maithani90")] <- NA #stratification not scarification
d$scarifType[which(d$datasetID == "rafiq21")] <- NA #stratification not scarification
d$scarifType[which(d$scarifType == "H2SO4.98per.0min")] <- NA #control
d$scarifType[which(d$scarifType == "hot.water.0min")] <- NA #control
d$scarifType[which(d$datasetID == "rahnama-ghahfarokhi07" & d$treatment == "Hot water 70°C for 10 min")] <- "soaking - 70°C hot water" 
d$scarifType[which(d$datasetID == "rahnama-ghahfarokhi07" & d$treatment == "Hot water 90°C for 10 min")] <- "soaking - 90°C hot water"
d$scarifType[which(d$datasetID == "rahnama-ghahfarokhi07" & d$treatment == "H2SO4 (80%), 10 min")] <- "chemical - H2SO4 (80%)"
d$scarifType[which(d$datasetID == "rahnama-ghahfarokhi07" & d$treatment == "H2SO4 (80%), 5 min")] <- "chemical - H2SO4 (80%)"
d$scarifType[which(d$datasetID == "rahnama-ghahfarokhi07" & d$treatment == "Thiourea (3%), 72h")] <- "chemical - Thiourea (3%)"
d$scarifType[which(d$datasetID == "rahnama-ghahfarokhi07" & d$treatment == "KNO3 (0.3%), 72h")] <- "chemical - KNO3 (0.3%)"
d$scarifType[which(d$datasetID == "jacquemart21" & d$chemical == "H2SO4")] <- "chemical - H2SO4"
d$scarifType[which(d$datasetID == "tilki07" & d$chemical == "H2SO4")] <- "chemical - H2SO4"
d$scarifType[which(d$datasetID == "tilki07" & d$chemical == "hydrogen.peroxide")] <- "chemical - hydrogen peroxide"
d$scarifType[which(d$datasetID == "tilki07" & d$chemical == "citric.acid")] <- "chemical - citric acid"
d$scarifType[which(d$datasetID == "tilki07" & d$chemical == "citric.acid/GA3")] <- "chemical - citric acid"
d$scarifType[which(d$datasetID == "olmez08" & d$chemical == "Y")] <- "chemical - H2SO4"
d$scarifType[which(d$datasetID == "tilki07" & d$chemical == "H2SO4")] <- "chemical - H2SO4"
d$scarifType[which(d$datasetID == "moradi12" & d$scarif.type == "acid")] <- "chemical - H2SO4" #Kin and BAP... are they considered scarification?
d$scarifType[which(d$datasetID == "irvani12" & d$scarif.type == "acid")] <- "chemical - H2SO4"
d$scarifType[which(d$datasetID == "tilki07" & d$scarif.type == "mechanical")] <- "mechanical - razor"
d$scarifType[which(d$datasetID == "tilki07" & d$scarif.type == "mechanical")] <- "mechanical - razor"



unique(d$scarifType)
# group more generally --- chemical vs mechanical
d$scarifTypeGen <- d$scarifType
# mechanical 
d$scarifTypeGen[which(d$scarifTypeGen == "Mechanical")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "sandpaper")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "sand paper")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "sand paper (Np. 150)")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "mechanical - sandpaper")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "mechanical - razor")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "coat removal")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "pericarp removal")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "tegument cutting")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "seed polishing")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "trimmed pappus")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "pelleted")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "burned")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "scapel")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "scalpel on lenticular side")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "manual with needle at the cotyledon end.")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "partly scarified")] <- "mechanical" #seed coat was partly removed 
d$scarifTypeGen[which(d$scarifTypeGen == "seed coat removal")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "Seed coat removed")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "mechanical with 220 grit sandpaper")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "Demucilagation")] <- "mechanical"
d$scarifTypeGen[which(d$scarifTypeGen == "soaking - 70°C hot water")] <- "soaking"
d$scarifTypeGen[which(d$scarifTypeGen == "soaking - 90°C hot water")] <- "soaking"

# chemical
d$scarifTypeGen[which(d$scarifTypeGen == "Chemical")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "H2SO4")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "H2SO4.98per.10min")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "H2SO4.98per.20min")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "H2SO4.98per.30min")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "acid scarification")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "NaClO")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "3% H2O2 (2 hrs)")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "chemical, H2O2")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "chemical - H2SO4")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "chemical - hydrogen peroxide")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "chemical - citric acid")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "chemical - H2SO4 (80%)")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "chemical - Thiourea (3%)")] <- "chemical"
d$scarifTypeGen[which(d$scarifTypeGen == "chemical - KNO3 (0.3%)")] <- "chemical"



#soaking (Al-Absi10, prknova15, rouhi13, Wickens01)
d$scarifTypeGen[which(d$scarifTypeGen == "soaking in water")] <- "soaking"
d$scarifTypeGen[which(d$scarifTypeGen == "hot water 70C")] <- "soaking"
d$scarifTypeGen[which(d$scarifTypeGen == "hot water")] <- "soaking"
d$scarifTypeGen[which(d$scarifTypeGen == "hot.water.10min")] <- "soaking"
d$scarifTypeGen[which(d$scarifTypeGen == "hot.water.20min")] <- "soaking"
d$scarifTypeGen[which(d$scarifTypeGen == "hot.water.30min")] <- "soaking"
d$scarifTypeGen[which(d$scarifTypeGen == "mechanical - 70°C hot water")] <- "soaking"
d$scarifTypeGen[which(d$scarifTypeGen == "mechanical - 90°C hot water")] <- "soaking"


# other
unique(d$scarifTypeGen)

#look at each general categories 
d.mechanical <- subset(d,d$scarifTypeGen == "mechanical")
d.chemical <- subset(d,d$scarifTypeGen == "chemical")
unique(d.mechanical$datasetID)
unique(d.chemical$datasetID)

# group specifically 
# papers that only have "mechanical" were checked, they didn't specify how they scarified the seeds
# "chakraborty92" "basaran12"     "javanmard14"   "keshtkar08"    "rostamipoor20"(not in English) "Marcello15"   
d$scarifTypeSpe <- d$scarifType

d$scarifTypeSpe[which(d$scarifTypeSpe == "sandpaper")] <- "mechanical - sandpaper"
d$scarifTypeSpe[which(d$scarifTypeSpe == "sand paper (Np. 150)")] <- "mechanical - sandpaper"
d$scarifTypeSpe[which(d$scarifTypeSpe == "sand paper")] <- "mechanical - sandpaper"
d$scarifTypeSpe[which(d$scarifTypeSpe == "mechanical with 220 grit sandpaper")] <- "mechanical - sandpaper"
d$scarifTypeSpe[which(d$scarifTypeSpe == "mechanical with sandpaper")] <- "mechanical - sandpaper"
d$scarifTypeSpe[which(d$scarifTypeSpe == "mechanical with razor")] <- "mechanical - razor"
d$scarifTypeSpe[which(d$scarifTypeSpe == "coat removal")] <- "mechanical - seed coat removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "coat removal ")] <- "mechanical - seed coat removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "pericarp removal")] <- "mechanical - pericarp removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "tegument cutting")] <- "mechanical - tegument cutting"
d$scarifTypeSpe[which(d$scarifTypeSpe == "seed polishing")] <- "mechanical - seed polishing"
d$scarifTypeSpe[which(d$scarifTypeSpe == "trimmed pappus")] <- "mechanical - trimmed pappus"
d$scarifTypeSpe[which(d$scarifTypeSpe == "pelleted")] <- "mechanical - pelleted"
d$scarifTypeSpe[which(d$scarifTypeSpe == "seed coat removed")] <- "mechanical - seed coat removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Seed coat removed")] <- "mechanical - seed coat removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Demucilagation")] <- "mechanical - demucilagation"
d$scarifTypeSpe[which(d$scarifTypeSpe == "burned")] <- "mechanical - burned"
d$scarifTypeSpe[which(d$scarifTypeSpe == "scapel")] <- "mechanical - scapel"
d$scarifTypeSpe[which(d$scarifTypeSpe == "scalpel on lenticular side")] <- "mechanical - scalpel"
d$scarifTypeSpe[which(d$scarifTypeSpe == "manual with needle at the cotyledon end.")] <- "mechanical - needle"
d$scarifTypeSpe[which(d$scarifTypeSpe == "seed coat removal")] <- "mechanical - seed coat removal"
d$scarifTypeSpe[which(d$scarifTypeSpe == "partially scarified")] <- "mechanical - seed coat partially removed with scalpel"
d$scarifTypeSpe[which(d$scarifTypeSpe == "partly scarified")] <- "mechanical - pericarp partially removed"
d$scarifTypeSpe[which(d$scarifTypeSpe == "Demucilagation")] <- "mechanical - demucilagation"

unique(d$scarifTypeSpe)

# chemical
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4.98per.0min")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4.98per.10min")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4.98per.20min")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "H2SO4.98per.30min")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$scarifTypeSpe == "NaClO")] <- "chemical - NaClO"
d$scarifTypeSpe[which(d$scarifTypeSpe == "3% H2O2 (2 hrs)")] <- "chemical - H2O2"
d$scarifTypeSpe[which(d$scarifTypeSpe == "chemical, H2O2")] <- "chemical - H2O2"
d$scarifTypeSpe[which(d$datasetID == "Arslan11" & d$scarifTypeSpe == "chemical")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$datasetID == "moradi12" & d$scarifTypeSpe == "acid")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$datasetID == "irvani12" & d$scarifTypeSpe == "acid")] <- "chemical - H2SO4"
d$scarifTypeSpe[which(d$datasetID == "Al-Absi10" & d$scarifTypeSpe == "acid scarification")] <- "chemical - H2SO4"

# soakin
d$scarifTypeSpe[which(d$scarifTypeSpe == "soaking in water")] <- "soakin - soaking"
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot water 70C")] <- "soaking - 70°C hot water"
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot water")] <- "soaking - hot water"
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot water scarification")] <- "soaking - hot water"
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot.water.10min")] <- "soaking - hot water 10 min"
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot.water.20min")] <- "soaking - hot water 20 min"
d$scarifTypeSpe[which(d$scarifTypeSpe == "hot.water.30min")] <- "soaking - hot water 30 min"
d$scarifTypeSpe[which(d$scarifTypeSpe == "mechanical - 70°C hot water")] <- "soaking - 70°C hot water"
d$scarifTypeSpe[which(d$scarifTypeSpe == "mechanical - 90°C hot water")] <- "soaking - 90°C hot water"

unique(d$scarifTypeSpe)
