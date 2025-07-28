## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##

# Making MEAN AVERAGE Columns

# Finding which columns have ranges of numeric values
colnames(d)[apply(d, 2, function(col) any(str_detect(col, " to ")))]

# Making min and max columns for each column that has a range
# First using strsplit() to separate values into new columns
breakbyto <- strsplit(d$pregermination_treatment_time_minutes, " to ", fixed=TRUE)
d$pregermTrtMin <- unlist(lapply(breakbyto, function(x) x[1]))
d$pregermTrtMax <- unlist(lapply(breakbyto, function(x) x[2]))

breakbyto2 <- strsplit(d$cold_stratification_days, " to ", fixed=TRUE)
d$chillDurationMin <- unlist(lapply(breakbyto2, function(x) x[1]))
d$chillDurationMax <- unlist(lapply(breakbyto2, function(x) x[2]))

breakbyto3 <- strsplit(d$dailyl_light_hours, " to ", fixed=TRUE)
d$photoperiodMin <- unlist(lapply(breakbyto3, function(x) x[1]))
d$photoperiodMax <- unlist(lapply(breakbyto3, function(x) x[2]))

breakbyto4 <- strsplit(d$day_temp_celsius, " to ", fixed=TRUE)
d$tempDayMin <- unlist(lapply(breakbyto4, function(x) x[1]))
d$tempDayMax <- unlist(lapply(breakbyto4, function(x) x[2]))

breakbyto5 <- strsplit(d$night_temp_celsius, " to ", fixed=TRUE)
d$tempNightMin <- unlist(lapply(breakbyto5, function(x) x[1]))
d$tempNightMax <- unlist(lapply(breakbyto5, function(x) x[2]))

breakbyto6 <- strsplit(d$test_duration_in_days, " to ", fixed=TRUE)
d$testDurMin <- unlist(lapply(breakbyto6, function(x) x[1]))
d$testDurMax <- unlist(lapply(breakbyto6, function(x) x[2]))

breakbyto7 <- strsplit(d$samples, " to ", fixed=TRUE)
d$samplesMin <- unlist(lapply(breakbyto7, function(x) x[1]))
d$samplesMax <- unlist(lapply(breakbyto7, function(x) x[2]))

breakbyto8 <- strsplit(d$pretreatmentChillDuration, " to ", fixed=TRUE)
d$pretrtChillDurationMin <- unlist(lapply(breakbyto8, function(x) x[1]))
d$pretrtChillDurationMax <- unlist(lapply(breakbyto8, function(x) x[2]))

breakbyto9 <- strsplit(d$responseValue, " to ", fixed=TRUE)
d$responseValueMin <- unlist(lapply(breakbyto9, function(x) x[1]))
d$responseValueMax <- unlist(lapply(breakbyto9, function(x) x[2]))

d$cold_stratification_temp_C[which(d$cold_stratification_temp_C == "3-5")] <- "3 to 5"
d$cold_stratification_temp_C[which(d$cold_stratification_temp_C == "2-4")] <- "2 to 4"
breakbyto10 <- strsplit(d$cold_stratification_temp_C, " to ", fixed=TRUE)
d$chillTempMin <- unlist(lapply(breakbyto10, function(x) x[1]))
d$chillTempMax <- unlist(lapply(breakbyto10, function(x) x[2]))

d$warm_stratification_temp_C[which(d$warm_stratification_temp_C == "20-30")] <- "20 to 30"

breakbyto11 <- strsplit(d$warm_stratification_temp_C, " to ", fixed=TRUE)
d$warmTempMin <- unlist(lapply(breakbyto11, function(x) x[1]))
d$warmTempMax <- unlist(lapply(breakbyto11, function(x) x[2]))

breakbyto12 <- strsplit(d$temp_unspecified_time_of_day_celsius, " to ", fixed=TRUE)
d$unspecTempMin <- unlist(lapply(breakbyto12, function(x) x[1]))
d$unspecTempMax <- unlist(lapply(breakbyto12, function(x) x[2]))

d$warm_stratification_days[which(d$warm_stratification_days == "some I suspect (see notes)")] <- "NA"
d$warm_stratification_days[which(d$warm_stratification_days == "60-90")] <- "60 to 90"

breakbyto13 <- strsplit(d$warm_stratification_days, " to ", fixed=TRUE)
d$warmDurationMin <- unlist(lapply(breakbyto13, function(x) x[1]))
d$warmDurationMax <- unlist(lapply(breakbyto13, function(x) x[2]))

# Making mean average columns for the above columns
# unique(d$responseValueMin)
d$responseValueMin[which(d$responseValueMin == "69 (18")] <- "69" #GIT ISSUE 20 IN EGRET; Keeping the range of values in the original data but leaving just the given mean for the responseValueAvg column
d$responseValueMin[which(d$responseValueMin == "93 (84")] <- "93"
d$responseValueMin[which(d$responseValueMin == "60 (40")] <- "60"
# unique(d$responseValueMax)
d$responseValueMax[which(d$responseValueMax == "94)")] <- NA
d$responseValueMax[which(d$responseValueMax == "96)")] <- NA
d$responseValueMax[which(d$responseValueMax == "88)")] <- NA

# Converting to integer
d$responseValueMin <- as.integer(d$responseValueMin) #NAs introduced by coercion
d$responseValueMax <- as.integer(d$responseValueMax)

# using rowMeans() conserves the rows where just one value is present so that it doesn't try to make a mean out of a valid value in the Min column and NA in the Max column
d$responseValueAvg <- rowMeans(d[, c("responseValueMin", "responseValueMax")], na.rm = TRUE)
d$responseValueAvg[which(is.nan(d$responseValueAvg))] <- NA

# First looking at which values are weird, converting them to numeric placeholders in the Min column, then turning them back to their original value in the Avg column
# Pregermination treatment
# unique(d$pregermTrtMin)
d$pregermTrtMin[which(d$pregermTrtMin == "ttc")] <- NA
d$pregermTrtMin[which(d$pregermTrtMin == "Overnight")] <- NA
# unique(d$pregermTrtMax)

d$pregermTrtMin <- as.integer(d$pregermTrtMin)
d$pregermTrtMax <- as.integer(d$pregermTrtMax)
d$pregermTrtAvg <- rowMeans(d[, c("pregermTrtMin", "pregermTrtMax")], na.rm = TRUE)
d$pregermTrtAvg[which(is.nan(d$pregermTrtAvg))] <- NA

# d$pregermTrtAvg[which(d$pregermTrtMin == 99991)] <- "ttc (time to cool to room temperature), varied from  several hours to overnight"
# d$pregermTrtAvg[which(d$pregermTrtMin == 99992)] <- "Overnight"
# d$pregermTrtMin[which(d$pregermTrtMin == 99991)] <- "ttc (time to cool to room temperature), varied from  several hours to overnight"
# d$pregermTrtMin[which(d$pregermTrtMin == 99992)] <- "Overnight"

# cold stratification duration
sort(unique(d$chillDurationMin))
d$chillDurationMin[which(d$chillDurationMin == "CSG")] <- NA # "stratification and germination as a continuum under the same conditions"
d$chillDurationMin[which(d$chillDurationMin == "Var.")] <- NA
d$chillDurationMin[which(d$chillDurationMin == "over winter")] <- NA
d$chillDurationMin[which(d$chillDurationMin == "1803")] <- 180 # 180 +3 in the table
d$chillDurationMin[which(d$chillDurationMin == "l")] <- 1
d$chillDurationMin[which(d$chillDurationMin == "60+")] <- 60
d$chillDurationMin[which(d$chillDurationMin == "90+")] <- 90

# unique(d$chillDurationMax)

d$chillDurationMin <- as.integer(d$chillDurationMin)
d$chillDurationMax <- as.integer(d$chillDurationMax)
d$coldStratDurAvg <- rowMeans(d[, c("chillDurationMin", "chillDurationMax")], na.rm = TRUE)
d$coldStratDurAvg[which(is.nan(d$coldStratDurAvg))] <- NA

#d$coldStratDurAvg[which(d$chillDurationMin == 99991)] <- "Stratification and germination as a continuum under the same conditions"
# d$coldStratDurAvg[which(d$chillDurationMin == 99992)] <- "Variable"
# d$chillDurationMin[which(d$chillDurationMin == 99991)] <- "Stratification and germination as a continuum under the same conditions"
# d$chillDurationMin[which(d$chillDurationMin == 99992)] <- "Variable"

# Photoperiod
d$photoperiodMax[which(d$photoperiodMin == "<16")] <- 16

# unique(d$photoperiodMin)
d$photoperiodMin[which(d$photoperiodMin == "ambient")] <- NA
d$photoperiodMin[which(d$photoperiodMin == "<16")] <- NA
d$photoperiodMin[which(d$photoperiodMin == ">8")] <- 8
d$photoperiodMin[which(d$photoperiodMin == "8<")] <- 8
d$photoperiodMin[which(d$photoperiodMin == "Light")] <- NA
d$photoperiodMin[which(d$photoperiodMin == "Dark")] <- 0

d$photoperiodMin <- as.integer(d$photoperiodMin)
d$photoperiodMax <- as.integer(d$photoperiodMax)
d$photoperiodAvg<- rowMeans(d[, c("photoperiodMin", "photoperiodMax")], na.rm = TRUE)
d$photoperiodAvg[which(is.nan(d$photoperiodAvg))] <- NA

# Temperature Day
# unique(d$tempDayMin)
# unique(d$tempDayMax)

d$tempDayMin <- as.integer(d$tempDayMin)
d$tempDayMax <- as.integer(d$tempDayMax)
d$tempDayAvg <- rowMeans(d[, c("tempDayMin", "tempDayMax")], na.rm = TRUE)
d$tempDayAvg[which(is.nan(d$tempDayAvg))] <- NA


d$tempNightMin <- as.integer(d$tempNightMin)
d$tempNightMax <- as.integer(d$tempNightMax)
d$tempNightAvg <- rowMeans(d[, c("tempNightMin", "tempNightMax")], na.rm = TRUE)
d$tempNightAvg[which(is.nan(d$tempNightAvg))] <- NA

# Test Duration
# unique(d$testDurMin)
d$testDurMin[which(d$testDurMin == ">60")] <- 60
# unique(d$testDurMax)

d$testDurMin <- as.integer(d$testDurMin)
d$testDurMax <- as.integer(d$testDurMax)
d$testDurAvg <- rowMeans(d[, c("testDurMin", "testDurMax")], na.rm = TRUE)
d$testDurAvg[which(is.nan(d$testDurAvg))] <- NA

# Samples
sort(unique(d$samplesMin))
d$samplesMin[which(d$samplesMin == "na")] <- NA

d$samplesMin[which(d$samplesMin == ">7")] <- 7
d$samplesMin[which(d$samplesMin == "7t")] <- "7"
d$samplesMin[which(d$species_name == "orbiculata")] <- "1"

unique(d$samplesMax)

d$samplesMin <- as.integer(d$samplesMin)
d$samplesMax <- as.integer(d$samplesMax)
d$samplesAvg <- rowMeans(d[, c("samplesMin", "samplesMax")], na.rm = TRUE)
d$samplesAvg[which(is.nan(d$samplesAvg))] <- NA

# Pretreatment Chill Duration
# unique(d$pretrtChillDurationMin)
# unique(d$pretrtChillDurationMax)

d$pretrtChillDurationMin <- as.integer(d$pretrtChillDurationMin)
d$pretrtChillDurationMax <- as.integer(d$pretrtChillDurationMax)
d$pretrtChillDurAvg <- rowMeans(d[, c("pretrtChillDurationMin", "pretrtChillDurationMax")], na.rm = TRUE)
d$pretrtChillDurAvg[which(is.nan(d$pretrtChillDurAvg))] <- NA

