## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##
## checked 26 July 2025 by Deirdre

############

# aggregate(latbi ~ pretrtChillDurationMax, data = d, FUN = function(x) length(unique(x)))

###need to make a rowmnames column
d$X<-rownames(d)


# Clean CSG
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "americana" & d$responseValue == "132")] <-"132"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "americana" & d$responseValue == "16")] <-"132"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$responseValue == "99")] <-"99"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$responseValue == "94")] <-"99"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$responseValue == "61")] <-"61"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$responseValue == "96")] <-"61"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$cold_stratification_days == "CSG"& d$responseValue == "90")] <-"90"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$responseValue == "93")] <-"90"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$cold_stratification_days == "CSG"& d$test_duration_in_days == "150 to 210")] <-"210"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$responseValue == "420")] <-"420"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$responseValue == "88")] <-"420"


# Get rid of weird values
unique(d$pretreatmentChill)
# d <- subset(d, !(pretrtChillDurAvg %in% c("Variable","Stratification and germination as a continuum under the same conditions")))

# Add in stratification temp
d$cold_stratification_temp_C[which(d$genus_name == "Aronia")] <-"10"
d$cold_stratification_temp_C[which(d$genus_name == "Alnus" & d$cold_stratification_days == "180")] <- "5"
d$cold_stratification_temp_C[which(d$genus_name == "Alnus" & d$cold_stratification_days == "1803")] <- "5"
d$warm_stratification_temp_C[which(d$genus_name == "Alnus" & d$cold_stratification_days == "1803")] <- "20"
d$warm_stratification_days[which(d$genus_name == "Alnus" & d$cold_stratification_days == "1803")] <- "3"
d$cold_stratification_days[which(d$genus_name == "Alnus" & d$cold_stratification_days == "1803")] <- "180"
d$cold_stratification_temp_C[which(d$genus_name == "Alnus" & d$cold_stratification_days != "0")] <- "5"
d$cold_stratification_temp_C[which(d$genus_name == "Berberis")] <-"-1 to 5"
d$cold_stratification_temp_C[which(d$genus_name == "Aesculus" & d$cold_stratification_days != "0")] <- "-0.5 to 5"
d$cold_stratification_temp_C[which(d$genus_name == "Ceanothus")] <- "1 to 5"
d$cold_stratification_temp_C[which(d$genus_name == "Carya")] <- "1 to 4"
d$cold_stratification_temp_C[which(d$genus_name == "Malus")] <- "2 to 5"
d$cold_stratification_temp_C[which(d$genus_name == "Taxus" & d$species_name == "baccata" )] <- "4"
d$warm_stratification_temp_C[which(d$genus_name == "Taxus" & d$species_name == "baccata")] <- "20"
d$cold_stratification_temp_C[which(d$genus_name == "Taxus" & d$species_name == "cuspidata" )] <- "4"
d$warm_stratification_temp_C[which(d$genus_name == "Taxus" & d$species_name == "cuspidata")] <- "20"
d$cold_stratification_temp_C[which(d$genus_name == "Tsuga" & d$cold_stratification_days != "0")] <- "-16 to -15"
d$cold_stratification_temp_C[which(d$genus_name == "Sequoiadendron" & d$cold_stratification_days != "0")] <- "2"
d$cold_stratification_temp_C[which(d$genus_name == "Quercus" & d$cold_stratification_days != "0")] <- "2 to 5"
d$cold_stratification_temp_C[which(d$genus_name == "Sequoiadendron" & d$cold_stratification_days != "0")] <- "2"
d$cold_stratification_temp_C[which(d$genus_name == "Prunus" & d$cold_stratification_days != "0")] <- "0.6 to 5"
d$warm_stratification_temp_C[which(d$genus_name == "Prunus" & d$warm_stratification_days != "0")] <- "20"
d$cold_stratification_temp_C[which(d$genus_name == "Pseudotsuga" & d$cold_stratification_days != "0")] <- "0 to 5"
d$cold_stratification_temp_C[which(d$cold_stratification_temp_C == "3-5")] <- "3 to 5"
d$cold_stratification_temp_C[which(d$cold_stratification_temp_C == "2-4")] <- "2 to 4"
d$cold_stratification_temp_C[which(d$cold_stratification_temp_C == "3-7")] <- "3 to 7"

d$cold_stratification_days[which(d$cold_stratification_days == "l")] <- "1"


breakbyto2 <- strsplit(d$cold_stratification_days, " to ", fixed=TRUE)
d$chillDurationMin <- unlist(lapply(breakbyto2, function(x) x[1]))
d$chillDurationMax <- unlist(lapply(breakbyto2, function(x) x[2]))

breakbyto8 <- strsplit(d$pretreatmentChillDuration, " to ", fixed=TRUE)
d$pretrtChillDurationMin <- unlist(lapply(breakbyto8, function(x) x[1]))
d$pretrtChillDurationMax <- unlist(lapply(breakbyto8, function(x) x[2]))

breakbyto10 <- strsplit(d$cold_stratification_temp_C, " to ", fixed=TRUE)
d$chillTempMin <- unlist(lapply(breakbyto10, function(x) x[1]))
d$chillTempMax <- unlist(lapply(breakbyto10, function(x) x[2]))

breakbyto9 <- strsplit(d$responseValue, " to ", fixed=TRUE)
d$responseValueMin <- unlist(lapply(breakbyto9, function(x) x[1]))
d$responseValueMax <- unlist(lapply(breakbyto9, function(x) x[2]))

# Step 1: Filter rows with non-NA values in all specified columns
filtered_rows <- d[!is.na(d$pretrtChillDurationMin) & !is.na(d$pretrtChillDurationMax) & 
                     !is.na(d$responseValueMin) & !is.na(d$responseValueMax), ]

filtered_spp <- d[!is.na(d$pretrtChillDurationMin) & !is.na(d$pretrtChillDurationMax) & 
                    !is.na(d$responseValueMin) & !is.na(d$responseValueMax), "latbi"]

tmp_X <- filtered_rows$X
spec_X <- filtered_rows$latbi

# Remove all filtered rows from the original data frame--removes 2 rows of data
d <- d[!d$X %in% tmp_X, ]

###add stratification and chilling

# ###add stratification to chilling values #  dl moved this 
# d$pretrtChillDurationMin<-ifelse(!is.na(d$chillDurationMin),d$chillDurationMin,d$pretrtChillDurationMin)
# d$pretrtChillDurationMax<-ifelse(!is.na(d$chillDurationMax),d$chillDurationMax,d$pretrtChillDurationMax)

# d <- d[,1:65]
