## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##

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
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "americana" & d$germination_time_days == "132")] <-"132"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$germination_time_days == "99")] <-"99"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$germination_time_days == "61")] <-"61"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$germination_time_days == "90")] <-"90"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$test_duration_in_days == "150-210" & d$cold_stratification_days == "CSG")] <-"210"
d$cold_stratification_days[which(d$genus_name == "Sorbus" & d$species_name == "aucuparia" & d$germination_time_days == "420")] <-"420"


# Get rid of weird values
unique(d$pretrtChillDurAvg)
d <- subset(d, !(pretrtChillDurAvg %in% c("Variable","Stratification and germination as a continuum under the same conditions")))
