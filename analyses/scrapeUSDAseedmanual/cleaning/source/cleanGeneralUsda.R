## Started 10 JULY 2024 ##
## By Dan, continued by Justin ##


## Updated 26 Jan 2025 by Mao ##
## Updated 26 July 2025 by Deirdre

# Giving each row a unique number so I can fix the issue where species and variety name are the same easily
d <- d %>%
  rowid_to_column("rowID")
# Removing apostrophe across all cells
d[] <- lapply(d, gsub, pattern="'", replacement="")

# Removing all hashtags
d[] <- lapply(d, gsub, pattern="#", replacement="")

# Removing all dollar sign
d[] <- lapply(d, gsub, pattern="\\$", replacement="")

# Removing Genus abbreviation
d$species_name <- sub(".*?\\. ","",d$species_name)

# Removing * from all cells
d[] <- lapply(d, gsub, pattern = "\\*", replacement = "")

# Removing misc. symbols from d$species_name
d$species_name <- gsub("\\+","",d$species_name)

# Removing minus only if encountered on its own
d <- mutate_all(d, ~ gsub("^-$", "", .))

# Removing misc. symbols from some numeric columns
itarget <- c("10":"35")
d[itarget] <- lapply(d[itarget], gsub, pattern ="I", replacement = "")
d[itarget] <- lapply(d[itarget], gsub, pattern ="\\|", replacement = "")
d[itarget] <- lapply(d[itarget], gsub, pattern ="\\+", replacement = "")

# Removing - if it is not succeeded by a number
d <- mutate_all(d, ~ gsub("-$", "", .))

# Removing duplicate rows
d <- d %>%
  distinct()

# Replace all empty cells with NA
d <- replace(d, d=='', NA)

# Remove empty columns
na_columns <- sapply(d, function(x) all(is.na(x)))

d <- d[, !na_columns]

# Remove unwanted columns
unwanted <- c("file_path", "scraped_table_number", "mean_light", "mean_dark", "light_range", "dark_range")
d <- d[ , !(names(d) %in% unwanted)]

# Read in newly scraped data
folder <- "scrapeUSDAseedmanual/scraping/"
fileList <- list.files(path = folder, pattern = "\\.xlsx$", full.names = TRUE)
df <- list()

for (file in fileList) {
  d1 <- read_xlsx(file, sheet = 2)
  df[[file]] <- d1
}

d1 <- do.call(rbind, df)

rownames(d1) <- seq_len(nrow(d1))

head(d)
setdiff(colnames(d), colnames(d1))
setdiff(colnames(d1), colnames(d))
d1$seed_type <- NA
d1$rowID <- NA

# Make new warm stratification column for original dataset
d$warm_stratification_temp_C <- NA

# Separate warm strat cold strat data
d$warm_stratification_temp_C[d$stratification_temp_C == "20"] <- "20"
d$stratification_temp_C[d$stratification_temp_C == "20"] <- NA
colnames(d)[which(names(d) == "stratification_temp_C")] <- "cold_stratification_temp_C"

# Add the new columns from newly scraped data
new <- c("germ_rate_days", "50%_germ", "Notes")

for (col in new) {
  d[[col]] <- NA
}

d <- rbind(d, d1)

d$rowID <- seq_len(nrow(d))
colnames(d)[colnames(d) == "50%_germ"] <- "per50_germ"

# Remove rows with no data at all
d <- d %>%
  filter(!if_all(c(pregermination_treatment_time_minutes,pregermination_treatment_hot_water_soak_C,
                   pretreatment,cold_stratification_temp_C,warm_stratification_temp_C,warm_stratification_days,
                   cold_stratification_days,dailyl_light_hours,day_temp_celsius,night_temp_celsius,
                   temp_unspecified_time_of_day_celsius,test_duration_in_days, germination_time_days, 
                   total_germination_percent, avg_germination_percent, samples, germination_rate, 
                   avg_germinative_energy_percent, germinative_energy_percent, avg_germinative_capacity, 
                   germinative_capacity, percent_germination_15degC_incubated, 
                   percent_germination_20degC_incubated,germ_rate_days,per50_germ,Notes), is.na))
# Clean some columns without specific cleaning code for other stuff
d$samples <- gsub("-"," to ",d$samples)
d$cold_stratification_days <- gsub("-"," to ",d$cold_stratification_days)

# cleaning sample number:
d$samples[which(d$latbi == "Baccharis_halimifolia")] <- "1"
d$samples[which(d$latbi == "Baccharis_pilularis" & d$responseValue == "93")] <- "1"
d$samples[which(d$latbi == "Baccharis_pilularis" & d$responseValue == "92")] <- "1"

# cleaning table numbers:
d$pdf_table_number[which(d$pdf_page_number == "277")] <- "3"
d$pdf_table_number[which(d$pdf_page_number == "287")] <- "3"



