## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##

# Making MEAN AVERAGE Columns

# Finding which columns have ranges of numeric values
colnames(d)[apply(d, 2, function(col) any(str_detect(col, " to ")))]

# Cleaning up some of the response value data
d$responseValue[which(d$responseValue == "16:")] <- "16"
d$responseValue[which(d$responseValue == "<")] <- NA

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
d$responseValueMin[which(d$responseValueMin == "I")] <- "1"
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
d$pregermTrtMin[which(d$pregermTrtMin == "ttc")] <- 99991
d$pregermTrtMin[which(d$pregermTrtMin == "Overnight")] <- 99992
# unique(d$pregermTrtMax)

d$pregermTrtMin <- as.integer(d$pregermTrtMin)
d$pregermTrtMax <- as.integer(d$pregermTrtMax)
d$pregermTrtAvg <- rowMeans(d[, c("pregermTrtMin", "pregermTrtMax")], na.rm = TRUE)
d$pregermTrtAvg[which(is.nan(d$pregermTrtAvg))] <- NA

d$pregermTrtAvg[which(d$pregermTrtMin == 99991)] <- "ttc (time to cool to room temperature), varied from  several hours to overnight"
d$pregermTrtAvg[which(d$pregermTrtMin == 99992)] <- "Overnight"
d$pregermTrtMin[which(d$pregermTrtMin == 99991)] <- "ttc (time to cool to room temperature), varied from  several hours to overnight"
d$pregermTrtMin[which(d$pregermTrtMin == 99992)] <- "Overnight"

# cold stratification duration
unique(d$coldStratDurMin)
#d$coldStratDurMin[which(d$coldStratDurMin == "CSG")] <- 99991
d$coldStratDurMin[which(d$coldStratDurMin == "Var.")] <- 99992
d$coldStratDurMin[which(d$coldStratDurMin == "1803")] <- 180
d$coldStratDurMin[which(d$coldStratDurMin == "l")] <- 1
# unique(d$coldStratDurMax)

d$coldStratDurMin <- as.integer(d$coldStratDurMin)
d$coldStratDurMax <- as.integer(d$coldStratDurMax)
d$coldStratDurAvg <- rowMeans(d[, c("coldStratDurMin", "coldStratDurMax")], na.rm = TRUE)
d$coldStratDurAvg[which(is.nan(d$coldStratDurAvg))] <- NA

#d$coldStratDurAvg[which(d$coldStratDurMin == 99991)] <- "Stratification and germination as a continuum under the same conditions"
d$coldStratDurAvg[which(d$coldStratDurMin == 99992)] <- "Variable"
d$coldStratDurMin[which(d$coldStratDurMin == 99991)] <- "Stratification and germination as a continuum under the same conditions"
d$coldStratDurMin[which(d$coldStratDurMin == 99992)] <- "Variable"

# Photoperiod
# unique(d$photoperiodMin)
d$photoperiodMin[which(d$photoperiodMin == "NDL")] <- 99991
d$photoperiodMin[which(d$photoperiodMin == "<16")] <- 99992
d$photoperiodMin[which(d$photoperiodMin == ">8")] <- 99993
d$photoperiodMin[which(d$photoperiodMin == "ND")] <- 99994
d$photoperiodMin[which(d$photoperiodMin == "Dark")] <- 0
# unique(d$photoperiodMax)

d$photoperiodMin <- as.integer(d$photoperiodMin)
d$photoperiodMax <- as.integer(d$photoperiodMax)
d$photoperiodAvg<- rowMeans(d[, c("photoperiodMin", "photoperiodMax")], na.rm = TRUE)
d$photoperiodAvg[which(is.nan(d$photoperiodAvg))] <- NA

d$photoperiodAvg[which(d$photoperiodMin == 99991)] <- "Natural daylength in a greenhouse"
d$photoperiodAvg[which(d$photoperiodMin == 99992)] <- "<16"
d$photoperiodAvg[which(d$photoperiodMin == 99993)] <- ">8"
d$photoperiodAvg[which(d$photoperiodMin == 99994)] <- "Natural daylength in a greenhouse"
d$photoperiodMin[which(d$photoperiodMin == 99991)] <- "Natural daylength in a greenhouse"
d$photoperiodMin[which(d$photoperiodMin == 99992)] <- "<16"
d$photoperiodMin[which(d$photoperiodMin == 99993)] <- ">8"
d$photoperiodMin[which(d$photoperiodMin == 99994)] <- "Natural daylength in a greenhouse"

# Temperature Day
# unique(d$tempDayMin)
d$tempDayMin[which(d$tempDayMin == "al")] <- -1 #This was the A1 that Selena mentioned in ISSUE 20 titled June 24 updates
# unique(d$tempDayMax)

d$tempDayMin <- as.integer(d$tempDayMin)
d$tempDayMax <- as.integer(d$tempDayMax)
d$tempDayAvg <- rowMeans(d[, c("tempDayMin", "tempDayMax")], na.rm = TRUE)
d$tempDayAvg[which(is.nan(d$tempDayAvg))] <- NA

# Temperature Night
# unique(d$tempNightMin)
d$tempNightMin[which(d$tempNightMin == "a7")] <- -7 #This was the A7 that Selena mentioned in ISSUE 20 titled June 24 updates
# unique(d$tempNightMax)

d$tempNightMin <- as.integer(d$tempNightMin)
d$tempNightMax <- as.integer(d$tempNightMax)
d$tempNightAvg <- rowMeans(d[, c("tempNightMin", "tempNightMax")], na.rm = TRUE)
d$tempNightAvg[which(is.nan(d$tempNightAvg))] <- NA

# Test Duration
# unique(d$testDurMin)
d$testDurMin[which(d$testDurMin == "NP")] <- 99991
d$testDurMin[which(d$testDurMin == ">60")] <- 99992
# unique(d$testDurMax)

d$testDurMin <- as.integer(d$testDurMin)
d$testDurMax <- as.integer(d$testDurMax)
d$testDurAvg <- rowMeans(d[, c("testDurMin", "testDurMax")], na.rm = TRUE)
d$testDurAvg[which(is.nan(d$testDurAvg))] <- NA

d$testDurAvg[which(d$testDurMin == 99991)] <- "NP"
d$testDurAvg[which(d$testDurMin == 99992)] <- ">60"
d$testDurMin[which(d$testDurMin == 99991)] <- "NP"
d$testDurMin[which(d$testDurMin == 99992)] <- ">60"

# Samples
# unique(d$samplesMin)
d$samplesMin[which(d$samplesMin == ">7")] <- 99991
d$samplesMin[which(d$samplesMin == "7t")] <- "7"
# unique(d$samplesMax)

d$samplesMin <- as.integer(d$samplesMin)
d$samplesMax <- as.integer(d$samplesMax)
d$samplesAvg <- rowMeans(d[, c("samplesMin", "samplesMax")], na.rm = TRUE)
d$samplesAvg[which(is.nan(d$samplesAvg))] <- NA

d$testDurAvg[which(d$testDurMin == 99991)] <- ">7"
d$testDurMin[which(d$testDurMin == 99991)] <- ">7"

# Pretreatment Chill Duration
# unique(d$pretrtChillDurMin)
# unique(d$pretrtChillDurMax)

d$pretrtChillDurMin <- as.integer(d$pretrtChillDurMin)
d$pretrtChillDurMax <- as.integer(d$pretrtChillDurMax)
d$pretrtChillDurAvg <- rowMeans(d[, c("pretrtChillDurMin", "pretrtChillDurMax")], na.rm = TRUE)
d$pretrtChillDurAvg[which(is.nan(d$pretrtChillDurAvg))] <- NA
