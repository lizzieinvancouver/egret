## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##
## Checked 26 July 2025 by Deirdre

d$day_temp_celsius[which(d$day_temp_celsius == "5(§41)")] <- "5"
d$night_temp_celsius[which(d$night_temp_celsius == "5(§41)")] <- "5"

# Looking back at page 855 see that that al for day and and a7 for night, should be 1 and 7
d$day_temp_celsius[which(d$day_temp_celsius == "al")] <- "1"
d$night_temp_celsius[which(d$night_temp_celsius == "a7")] <- "7"

# Ribes aureum var. villosum has day or night temps in the temp_unspecified_time_of_day_celsius column = "20/0 (D/N)"; updating specific day night coln
d$day_temp_celsius[which(d$species_name == "aureum var. villosum")] <- "20"
d$night_temp_celsius[which(d$species_name == "aureum var. villosum")] <- "0"

d$temp_unspecified_time_of_day_celsius <- gsub("-"," to ", d$temp_unspecified_time_of_day_celsius)
d$temp_unspecified_time_of_day_celsius <- gsub(" to 2","-2",d$temp_unspecified_time_of_day_celsius)
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "21-28")] <- "21 to 28"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "-2-2")] <- "-2 - 2"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "0-2")] <- "0 - 2"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "18-21")] <- "18 to 21"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "20-27")] <- "20 to 27"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "20-29")] <- "20 to 29"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "5-25")] <- "5 to 25"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "na")] <- NA
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "NP")] <- NA # means "no pretreatment"

# unique(d$day_temp_celsius)
d$day_temp_celsius <- gsub("-"," to ",d$day_temp_celsius)

# unique(d$night_temp_celsius)
d$night_temp_celsius <- gsub("-"," to ",d$night_temp_celsius)

# cleaning "na" to NA
d$day_temp_celsius[which(d$day_temp_celsius == "na")] <- NA
d$night_temp_celsius[which(d$night_temp_celsius == "na")] <- NA
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "na")] <- NA