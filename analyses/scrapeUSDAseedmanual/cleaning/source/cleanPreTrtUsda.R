## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##

# unique(d$pregermination_treatment_time_minutes)
d$pregermination_treatment_time_minutes <- gsub("-"," to ",d$pregermination_treatment_time_minutes)
d$pregermination_treatment_time_minutes[which(d$pregermination_treatment_time_minutes == " to 5")] <- "-5" #how can there be negative 5 minutes?
d$pregermination_treatment_time_minutes[which(d$pregermination_treatment_time_minutes == "None")] <- NA 

unique(d$pretreatment) #Should we standardize abrasion and nicking to mechanical?
# d$pretreatment[which(d$pretreatment == "Mech.")] <- "Mechanical"
# d$pretreatment[which(d$pretreatment == "Mech")] <- "Mechanical"
d$pretreatment[which(d$pretreatment == "None")] <- NA
d$pretreatment[which(d$pretreatment == "Fresh seeds")] <- "Fresh seed"
d$pretreatment <- gsub("-mon"," month",d$pretreatment)
d$pretreatment <- gsub("-"," to ",d$pretreatment)

# Making new columns for pretreatment
d$pretreatmentChill <- d$pretreatment
d$pretreatmentChillDuration <- d$pretreatment

d$pretreatmentChill[!grepl("chill|stratification", d$pretreatmentChill)] <- NA
d$pretreatmentChillDuration[!grepl("chill|stratification", d$pretreatmentChillDuration)] <- NA
d$pretreatment[!is.na(d$pregermination_treatment_hot_water_soak_C)] <- "Hot water"

d$pretreatmentChill[which(grepl("chill|stratification",d$pretreatmentChill))] <- "Y"
d$pretreatmentChill[which(!grepl("chill|stratification",d$pretreatment))] <- "N"
d$pretreatmentChill[which(d$pretreatment == "No stratification")] <- "N"

d$pretreatmentChillDuration[which(d$pretreatmentChill == "N")] <- NA
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "wet_prechill_3_to_5_C")] <- NA
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "Moist chill")] <- NA
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "Scarification & 2 month stratification")] <- 60.8334
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "Scarification & 4 month stratification")] <- 121.667
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "Scarification & 6 month stratification")] <- 182.5
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "4 month stratification")] <- 121.667
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "6 month stratification")] <- 182.5
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "3 to 5 month stratification")] <- "91.2501 to 152.083"
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "6 to 9 month stratification")] <- "182.5 to 273.75"
d$pretreatmentChillDuration[which(d$pretreatmentChillDuration == "10 to 13 month stratification")] <- "304.167 to 395.417"

