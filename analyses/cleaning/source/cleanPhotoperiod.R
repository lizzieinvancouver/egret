# Started July 11, 2024 by mao


# Take a look at photoperiod column
# d$photoperiod <- as.factor (d$photoperiod)
# unique(d$photoperiod)

# Take a look at treatment and other treatment columns
# d$treatment <- as.factor(d$treatment)
# d$other.treatment <- as.factor(d$other.treatment)
# summary(d$chemical)
# d$chemical <- as.factor (d$chemical)

# Select for row numbers with either light or dark in other treatment column
selected_rows <- grep("light|dark|photoperiod|lux", d$other.treatment, ignore.case = TRUE)

# Select rows
subset_othertreatment <- d[selected_rows, ]

# Select for rows that is not NA and greater than 0 in photoperiod column
photoperiod_row <- which(!is.na(d$photoperiod) & d$photoperiod != 0)

# Select rows with "log2(red/far.red)" in chemical column
far.red <- which(d$chemical == "log2(red/far.red)")

# Select for rows with any word indicating presence of light in other treatment column
light_row1 <- grep("light|photoperiod|lux", d$other.treatment, ignore.case = TRUE)

# Combine all light rows
light_rows <- c(light_row1,photoperiod_row,far.red)
light_rows <- unique(light_rows)

# Select for rows with word dark in other treatment column
dark_row1 <- grep("dark", d$other.treatment, ignore.case = TRUE)

# Select for rows with any photoperiod values equal to 0
dark_row2 <- which(d$photoperiod == 0)

# Combine all dark rows
dark_rows <- unique(c(dark_row1, dark_row2))

# Make an extra column recording light/dark data
d$photoperiodCor <- NA
d$photoperiodCor[light_rows] <- "light"
d$photoperiodCor[dark_rows] <- "dark"

# Justin: taken from cleanGerminationTempDuration "These weren't actually light treatments, it was just when the researchers wanted to check for germination
# Not really photoperiod"
d$photoperiodCor[which(d$photoperiodCor == "light")] <- "dark"
