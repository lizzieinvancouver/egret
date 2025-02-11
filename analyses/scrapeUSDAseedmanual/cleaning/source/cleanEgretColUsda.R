## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##

# Changing column names to better fit EGRET 

# Replacing all blanks with NA
d[d==""] <- NA


# Removing redundant columns
d <- subset(d, select = -c(responseVarClean,X)) 

colnames(d)
colnames(d) <- c("speciesID",
                 "filePath",
                 "pdfPageNumber",
                 "scrapedTableNumber",
                 "pdfTableNumber",
                 "genus",
                 "species",
                 "seedType",
                 "source.population",
                 "medium",
                 "pretreatmentDuration",
                 "pretreatmentHotWaterTemp",
                 "pretreatment",
                 "stratificationTemp",
                 "warmStratDuration",
                 "coldStratDuration",
                 "photoperiod",
                 "tempDay",
                 "tempNight",
                 "tempUnspecified",
                 "germDuration",
                 "samples",
                 "latbi",
                 "chilling",
                 "chillDuration",
                 "scarifTypeGen",
                 "scarifTypeSpe",
                 "responseVar",
                 "responseValue",
                 "pretreatmentMin",
                 "pretreatmentMax",
                 "coldStratDurMin",
                 "coldStratDurMax",
                 "photoperiodMin",
                 "photoperiodMax",
                 "tempDayMin",
                 "tempDayMax",
                 "tempNightMin",
                 "tempNightMax",
                 "germDurationMin",
                 "germDurationMax",
                 "samplesMin",
                 "samplesMax",
                 "chillDurationMin",
                 "chillDurationMax",
                 "responseValueMin",
                 "responseValueMax",
                 "responseValueAvg",
                 "pretreatmentAvg",
                 "coldStratDurAvg",
                 "photoperiodAvg",
                 "tempDayAvg",
                 "tempNightAvg",
                 "germDurationAvg",
                 "samplesAvg",
                 "chillDurationAvg")
