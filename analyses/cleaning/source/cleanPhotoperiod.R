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
selected_rows <- grep("light|dark", d$other.treatment, ignore.case = TRUE)

# Select rows
subset_othertreatment <- d[selected_rows, ]

# Check by Lizzie ... seems we missed some relevant rows
sort(unique(d$other.treatment))
lux_rows <- grep("lux", d$other.treatment, ignore.case = TRUE)
mol_rows <- grep("μmol/m^2/s light", d$other.treatment, ignore.case = TRUE)
lightintense_rows <- grep("light intensity", d$other.treatment, ignore.case = TRUE)
whitelight_rows <- grep("white light", d$other.treatment, ignore.case = TRUE)

morerowsmaybe <- c(lux_rows, mol_rows, lightintense_rows, whitelight_rows)

# Select for rows that is not NA and greater than 0 in photoperiod column
photoperiod_row <- which(!is.na(d$photoperiod) & d$photoperiod != 0)

# Select rows with "log2(red/far.red)" in chemical column
far.red <- which(d$chemical == "log2(red/far.red)")

# Combine all row numbers
row_light <- c(selected_rows,photoperiod_row,far.red, morerowsmaybe)
# Select for unique rows
light_egret <- d[unique(row_light),]

# Calculate how many datasetIDstudy, and how many sp have photoperiod data
length(unique(light_egret$datasetIDstudy))
length(unique(light_egret$latbi))

length(unique(d$datasetIDstudy))
length(unique(d$latbi))

# Make an extra column recording light/dark data
light_row1 <- grep("light", d$other.treatment, ignore.case = TRUE)
light_rows <- c(light_row1,photoperiod_row,far.red)
light_rows <- unique(light_rows)

dark_row1 <- grep("dark", d$other.treatment, ignore.case = TRUE)
dark_row2 <- which(d$photoperiod == 0)
dark_rows <- unique(c(dark_row1, dark_row2))

d$lightDark <- NA
d$lightDark[light_rows] <- "light"
d$lightDark[dark_rows] <- "dark"
