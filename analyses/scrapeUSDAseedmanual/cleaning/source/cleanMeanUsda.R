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
d$coldStratDurMin <- unlist(lapply(breakbyto2, function(x) x[1]))
d$coldStratDurMax <- unlist(lapply(breakbyto2, function(x) x[2]))

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
d$pretrtChillDurMin <- unlist(lapply(breakbyto8, function(x) x[1]))
d$pretrtChillDurMax <- unlist(lapply(breakbyto8, function(x) x[2]))

breakbyto9 <- strsplit(d$responseValue, " to ", fixed=TRUE)
d$responseValueMin <- unlist(lapply(breakbyto9, function(x) x[1]))
d$responseValueMax <- unlist(lapply(breakbyto9, function(x) x[2]))

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
sort(unique(d$coldStratDurMin))
d$coldStratDurMin[which(d$coldStratDurMin == "CSG")] <- NA # "stratification and germination as a continuum under the same conditions"
d$coldStratDurMin[which(d$coldStratDurMin == "Var.")] <- NA
d$coldStratDurMin[which(d$coldStratDurMin == "over winter")] <- NA
d$coldStratDurMin[which(d$coldStratDurMin == "1803")] <- 180 # 180 +3 in the table
d$coldStratDurMin[which(d$coldStratDurMin == "l")] <- 1
d$coldStratDurMin[which(d$coldStratDurMin == "60+")] <- 60
d$coldStratDurMin[which(d$coldStratDurMin == "90+")] <- 90

# unique(d$coldStratDurMax)

d$coldStratDurMin <- as.integer(d$coldStratDurMin)
d$coldStratDurMax <- as.integer(d$coldStratDurMax)
d$coldStratDurAvg <- rowMeans(d[, c("coldStratDurMin", "coldStratDurMax")], na.rm = TRUE)
d$coldStratDurAvg[which(is.nan(d$coldStratDurAvg))] <- NA

#d$coldStratDurAvg[which(d$coldStratDurMin == 99991)] <- "Stratification and germination as a continuum under the same conditions"
# d$coldStratDurAvg[which(d$coldStratDurMin == 99992)] <- "Variable"
# d$coldStratDurMin[which(d$coldStratDurMin == 99991)] <- "Stratification and germination as a continuum under the same conditions"
# d$coldStratDurMin[which(d$coldStratDurMin == 99992)] <- "Variable"

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
# unique(d$pretrtChillDurMin)
# unique(d$pretrtChillDurMax)

d$pretrtChillDurMin <- as.integer(d$pretrtChillDurMin)
d$pretrtChillDurMax <- as.integer(d$pretrtChillDurMax)
d$pretrtChillDurAvg <- rowMeans(d[, c("pretrtChillDurMin", "pretrtChillDurMax")], na.rm = TRUE)
d$pretrtChillDurAvg[which(is.nan(d$pretrtChillDurAvg))] <- NA
