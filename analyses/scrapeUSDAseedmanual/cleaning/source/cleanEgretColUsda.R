## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##

# Changing column names to better fit EGRET 

# Replacing all blanks with NA
d[d==""] <- NA

colnames(d)[colnames(d) == "genus_name"] <- "genus"
colnames(d)[colnames(d) == "species_name"] <- "species"
colnames(d)[colnames(d) == "seed_source"] <- "source.population"
colnames(d)[colnames(d) == "cold_stratification_days"] <- "chillDuration"
colnames(d)[colnames(d) == "dailyl_light_hours"] <- "germPhotoperiod"
# colnames(d)[colnames(d) == "day_temp_celsius"] <- "germTempDay"
# colnames(d)[colnames(d) == "night_temp_celsius"] <- "germTempNight"
colnames(d)[colnames(d) == "pretreatment"] <- "treatment"

colnames(d)[colnames(d) == "responseType"] <- "responseVar"
colnames(d)[colnames(d) == "samples"] <- "reps"
colnames(d)[colnames(d) == "pretreatment"] <- "treatment"

# 
# colnames(d)
# colnames(d) <- c(
#   "rowID","pdfPageNumber","pdfTableNumber", "genus", "species","seedType","source.population",
#   "medium", "pretreatmentDuration", "pretreatmentHotWaterTemp","pretreatment",
#   "coldStratTemp",
#   
#   [13] "warm_stratification_days"                  "cold_stratification_days"                 
#   [15] "dailyl_light_hours"                        "day_temp_celsius"                         
#   [17] "night_temp_celsius"                        "temp_unspecified_time_of_day_celsius"     
#   [19] "test_duration_in_days"                     "samples"                                  
#   [21] "warm_stratification_temp_C"                "Notes"                                    
#   [23] "variety"                                   "latbi"                                    
#   [25] "pretreatmentChill"                         "pretreatmentChillDuration"                
#   [27] "pretreatmentScarifTypeGen"                 "pretreatmentScarifTypeSpe"                
#   [29] "avg_germination_percen"                    "responseType"                             
#   [31] "responseValue"                             "responseVarClean"                         
#   [33] "pregermTrtMin"                             "pregermTrtMax"                            
#   [35] "coldStratDurMin"                           "coldStratDurMax"                          
#   [37] "photoperiodMin"                            "photoperiodMax"                           
#   [39] "tempDayMin"                                "tempDayMax"                               
#   [41] "tempNightMin"                              "tempNightMax"                             
#   [43] "testDurMin"                                "testDurMax"                               
#   [45] "samplesMin"                                "samplesMax"                               
#   [47] "pretrtChillDurMin"                         "pretrtChillDurMax"                        
#   [49] "responseValueMin"                          "responseValueMax"                         
#   [51] "coldStratTempMin"                          "coldStratTempMax"                         
#   [53] "warmStratTempMin"                          "warmStratTempMax"                         
#   [55] "responseValueAvg"                          "pregermTrtAvg"                            
#   [57] "coldStratDurAvg"                           "photoperiodAvg"    
#   
#   
#   
#   
#                 
#                  "warmStratDuration",
#                  "coldStratDuration",
#                  "photoperiod",
#                  "tempDay",
#                  "tempNight",
#                  "tempUnspecified",
#                  "germDuration",
#                  "reps",
#                  "warmStratTemp","warmStratTempMax","warmStratTempMin",
#                  "notes",
#                  "latbi",
#                  "chilling",
#                  "chillDuration",
#                  "scarifTypeGen",
#                  "scarifTypeSpe",
#                  "avgGerminationPercen",
#                  "responseType",
#                  "responseValue",
#                  "responseVarClean",     
#                  "preGermTrtMin",
#                  "preGermTrtMax",
#                  "coldStratDurMin",
#                  "coldStratDurMax",
#                  "photoperiodMin",
#                  "photoperiodMax",
#                  "tempDayMin",
#                  "tempDayMax",
#                  "tempNightMin",
#                  "tempNightMax",
#                  "germDurationMin",
#                  "germDurationMax",
#                  "samplesMin",
#                  "samplesMax",
#                  "chillDurationMin",
#                  "chillDurationMax",
#                  "responseValueMin",
#                  "responseValueMax",
#                  "responseValueAvg",
#                  "pretreatmentAvg",
#                  "coldStratDurAvg",
#                  "photoperiodAvg",
#                  "tempDayAvg",
#                  "tempNightAvg",
#                  "germDurationAvg",
#                  "samplesAvg",
#                  "chillDurationAvg")

# d <- d[, c("rowID", "pdfPageNumber", "pdfTableNumber",
#            "genus",
#            "species","latbi",
#            "seedType",
#            "source.population",
#            "medium",
#            "pretreatmentDuration",
#            "pretreatmentHotWaterTemp",
#            "pretreatment",
#            "coldStratTemp",
#            "warmStratTemp", 
#            "warmStratDuration",
#            "coldStratDuration",
#            "photoperiod",
#            "tempDay",
#            "tempNight",
#            "tempUnspecified",
#            "germDuration",
#            "reps","chilling",
#            "chillDuration",
#            "scarifTypeGen",
#            "scarifTypeSpe",
#            "avgGerminationPercen",
#            "responseValue",
#            "preGermTrtMin",
#            "preGermTrtMax",
#            "coldStratDurMin",
#            "coldStratDurMax",
#            "photoperiodMin",
#            "photoperiodMax",
#            "tempDayMin",
#            "tempDayMax",
#            "tempNightMin",
#            "tempNightMax",
#            "germDurationMin",
#            "germDurationMax",
#            "samplesMin",
#            "samplesMax",
#            "chillDurationMin",
#            "chillDurationMax",
#            "responseVarClean",
#            "responseValueMin",
#            "responseValueMax",
#            "responseValueAvg",
#            "pretreatmentAvg",
#            "coldStratDurAvg",
#            "photoperiodAvg",
#            "tempDayAvg",
#            "tempNightAvg",
#            "germDurationAvg",
#            "samplesAvg",
#            "chillDurationAvg","notes")]
