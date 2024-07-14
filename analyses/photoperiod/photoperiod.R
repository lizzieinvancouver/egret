# Started July 11, 2024 by mao

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("C:/PhD/Project/egret/analyses")

egret <- read.csv ("output/egretclean.csv")

# Take a look at photoperiod column
egret$photoperiod <- as.factor (egret$photoperiod)
unique(egret$photoperiod)

# Take a look at treatment and other treatment columns
egret$treatment <- as.factor(egret$treatment)
egret$other.treatment <- as.factor(egret$other.treatment)
summary(egret$chemical)
egret$chemical <- as.factor (egret$chemical)

# Select for row numbers with either light or dark in other treatment column
selected_rows <- grep("light|dark", egret$other.treatment, ignore.case = TRUE)

# Select rows
subset_othertreatment <- egret[selected_rows, ]

# Select for rows that is not NA and greater than 0 in photoperiod column
subset_photoperiod <- egret[!is.na(egret$photoperiod) & egret$photoperiod != 0, ]
photoperiod_row <- which(!is.na(egret$photoperiod) & egret$photoperiod != 0)

# Select rows with "log2(red/far.red)" in chemical column
far.red <- which(egret$chemical == "log2(red/far.red)")

# Combine all row numbers
row_light <- c(selected_rows,photoperiod_row,far.red)
# Select for unique rows
light_egret <- egret[unique(row_light),]

# Calculate how many datasetIDstudy, and how many sp have photoperiod data
length(unique(light_egret$datasetIDstudy))
length(unique(light_egret$latbi))

length(unique(egret$datasetIDstudy))
length(unique(egret$latbi))

# Make an extra column recording light/dark data
light_row1 <- grep("light", egret$other.treatment, ignore.case = TRUE)
light_rows <- c(light_row1,photoperiod_row,far.red)
light_rows <- unique(light_rows)

dark_row1 <- grep("dark", egret$other.treatment, ignore.case = TRUE)
dark_row2 <- which(egret$photoperiod == 0)
dark_rows <- unique(c(dark_row1, dark_row2))

egret$lightDark <- NA
egret$lightDark[light_rows] <- "light"
egret$lightDark[dark_rows] <- "dark"
