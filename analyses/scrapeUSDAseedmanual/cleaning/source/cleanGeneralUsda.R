## Started 10 JULY 2024 ##
## By Dan, continued by Justin ##


## Updated 26 Jan 2025 by Mao ##

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

# Remove rows with no data at all
d <- d %>%
  filter(!if_all(c(pregermination_treatment_time_minutes,pregermination_treatment_hot_water_soak_C,pretreatment,stratification_temp_C,warm_stratification_days,cold_stratification_days,dailyl_light_hours,day_temp_celsius,night_temp_celsius,temp_unspecified_time_of_day_celsius,test_duration_in_days, germination_time_days, total_germination_percent, avg_germination_percent, samples, germination_rate, avg_germinative_energy_percent, germinative_energy_percent, avg_germinative_capacity, germinative_capacity, percent_germination_15degC_incubated, percent_germination_20degC_incubated), is.na))
# Clean some columns without specific cleaning code for other stuff
d$samples <- gsub("-"," to ",d$samples)
d$cold_stratification_days <- gsub("-"," to ",d$cold_stratification_days)
