## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##
## Checked 26 July 2025 by Deirdre


d$temp_unspecified_time_of_day_celsius <- gsub("-"," to ", d$temp_unspecified_time_of_day_celsius)
d$temp_unspecified_time_of_day_celsius <- gsub(" to 2","-2",d$temp_unspecified_time_of_day_celsius)
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "21-28")] <- "21 to 28"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "-2-2")] <- "-2 to 2"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "0-2")] <- "0 to 2"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "18-21")] <- "18 to 21"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "20-27")] <- "20 to 27"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "20-29")] <- "20 to 29"
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "5-25")] <- "5 to 25"


# unique(d$day_temp_celsius)
d$day_temp_celsius <- gsub("-"," to ",d$day_temp_celsius)

# unique(d$night_temp_celsius)
d$night_temp_celsius <- gsub("-"," to ",d$night_temp_celsius)

# cleaning "na" to NA
d$day_temp_celsius[which(d$day_temp_celsius == "na")] <- NA
d$night_temp_celsius[which(d$night_temp_celsius == "na")] <- NA
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "na")] <- NA
d$temp_unspecified_time_of_day_celsius[which(d$temp_unspecified_time_of_day_celsius == "NP")] <- NA # means "no pretreatment"

