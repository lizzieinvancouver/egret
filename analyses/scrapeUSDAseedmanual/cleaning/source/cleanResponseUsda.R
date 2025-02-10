## Started 10 JULY 2024 ##
## By Dan, continued by Justin ##


## Updated 4 fEB 2025 by Mao ##

# Converting integer to character for pivot_longer()
d$germinative_capacity <- as.character(d$germinative_capacity)
d$percent_germination_15degC_incubated <- as.character(d$percent_germination_15degC_incubated)
d$percent_germination_20degC_incubated <- as.character(d$percent_germination_20degC_incubated)

# Converting the data into long format
d <- d %>%
  group_by(species_name) %>%
  pivot_longer(cols = c("germination_time_days","total_germination_percent","avg_germination_percent","germination_rate","avg_germinative_energy_percent","germinative_energy_percent","avg_germinative_capacity","germinative_capacity","percent_germination_15degC_incubated","percent_germination_20degC_incubated"),
               names_to = "responseType",
               values_to = "responseValue")


# Cleaning response variables names
d$responseType[which(d$responseType == "germination_time_days")] <- "mgt"
d$responseType[which(d$responseType == "total_germination_percent")] <- "percent.germ.total"
d$responseType[which(d$responseType == "avg_germination_percent")] <- "percent.germ"
d$responseType[which(d$responseType == "germination_rate")] <- "germ.rate"
d$responseType[which(d$responseType == "avg_germinative_energy_percent")] <- "mean.percent.germ.energy"
d$responseType[which(d$responseType == "germinative_energy_percent")] <- "percent.germ.energy"
d$responseType[which(d$responseType == "avg_germinative_capacity")] <- "mean.germ.capacity"
d$responseType[which(d$responseType == "germinative_capacity")] <- "germ.capacity"
d$responseType[which(d$responseType == "percent_germination_15degC_incubated")] <- "percent.germ.15degC.incubated"
d$responseType[which(d$responseType == "percent_germination_20degC_incubated")] <- "percent.germ.20degC.incubated"


#create a new column 
d$responseVarClean <- d$responseType

#how many uniques entries are in usda$responseVar
tapply(d$rowID, d$responseVarClean, function(x) length(!is.na(x)))
aggregate(rowID ~ responseType, data = d, FUN = function(x) length(unique(x)))
d[d$responseVarClean == "percent.germ","responseVarClean"]

#these columns most likely show the same type of percentage, so rename them to "perc.standard"
d$responseVarClean[d$responseVarClean == "percent.germ"] <- "perc.standard"
d$responseVarClean[d$responseVarClean == "percent.germ.total"] <- "perc.standard"
d$responseVarClean[d$responseVarClean == "germ.capacity"] <- "perc.standard"
d$responseVarClean[d$responseVarClean == "mean.germ.capacity"] <- "perc.standard"
d$responseVarClean[d$responseVarClean == "percent.germ.15degC.incubated"] <- "perc.standard"
#another two that are probably the same
d$responseVarClean[d$responseVarClean == "mean.percent.germ.energy"] <- "percent.germ.energy"

#replace the original response variable column with the cleaned one
d$responseType <- d$responseVarClean

