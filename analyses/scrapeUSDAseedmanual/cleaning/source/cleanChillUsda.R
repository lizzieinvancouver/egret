## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##
## checked 26 July 2025 by Deirdre

###add stratification and chilling
d$pretreatmentChill<-ifelse(!is.na(d$coldStratDurAvg),"Y",d$pretreatmentChill)
d$pretreatmentChill<-ifelse(!is.na(d$coldStratDurMax),"Y",d$pretreatmentChill)
d$pretreatmentChill<-ifelse(!is.na(d$coldStratDurMin),"Y",d$pretreatmentChill)

###add stratification to chilling values
d$pretrtChillDurMin<-ifelse(!is.na(d$coldStratDurMin),d$coldStratDurMin,d$pretrtChillDurMin)
d$pretrtChillDurMax<-ifelse(!is.na(d$coldStratDurMax),d$coldStratDurMax,d$pretrtChillDurMax)
d$pretrtChillDurAvg<-ifelse(!is.na(d$coldStratDurAvg),d$coldStratDurAvg,d$pretrtChillDurAvg)
d$pretreatmentChillDuration<-ifelse(!is.na(d$cold_stratification_days),d$cold_stratification_days,d$pretreatmentChillDuration)



############

aggregate(latbi ~ pretrtChillDurMax, data = d, FUN = function(x) length(unique(x)))

###need to make a rowmnames column
d$X<-rownames(d)


# Step 1: Filter rows with non-NA values in all specified columns
filtered_rows <- d[!is.na(d$pretrtChillDurMin) & !is.na(d$pretrtChillDurMax) & 
                     !is.na(d$responseValueMin) & !is.na(d$responseValueMax), ]

filtered_spp <- d[!is.na(d$pretrtChillDurMin) & !is.na(d$pretrtChillDurMax) & 
                    !is.na(d$responseValueMin) & !is.na(d$responseValueMax), "latbi"]


tmp_X <- filtered_rows$X
spec_X <- filtered_rows$latbi

# Remove all filtered rows from the original data frame
d <- d[!d$X %in% tmp_X, ]
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
unique(d$pretrtChillDurAvg)
d <- subset(d, !(pretrtChillDurAvg %in% c("Variable","Stratification and germination as a continuum under the same conditions")))

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
d$cold_stratification_days[which(d$cold_stratification_days == "l")] <- "1"

d <- d[,1:58]
