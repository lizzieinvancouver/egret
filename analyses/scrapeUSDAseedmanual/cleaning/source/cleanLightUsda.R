## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##
## checked 26 July 2025 by Deirdre

# unique(d$dailyl_light_hours)
d$dailyl_light_hours <- gsub("-"," to ",d$dailyl_light_hours)
d$dailyl_light_hours[which(d$dailyl_light_hours == "8+")] <- ">8"
d$dailyl_light_hours[which(d$dailyl_light_hours == "8 or fewer")] <- "8<"
d$dailyl_light_hours[which(d$dailyl_light_hours == "Na")] <- NA
d$dailyl_light_hours[which(d$dailyl_light_hours == "ND")] <- "ambient"
d$dailyl_light_hours[which(d$dailyl_light_hours == "NDL")] <- "ambient"
d$dailyl_light_hours[which(d$dailyl_light_hours == "N")] <- "Dark"
d$dailyl_light_hours[which(d$dailyl_light_hours == "Y")] <- "Light"
d$dailyl_light_hours[which(d$dailyl_light_hours == "0")] <- "Dark"


# unique(d$test_duration_in_days)
d$test_duration_in_days <- gsub("-"," to ",d$test_duration_in_days)
d$test_duration_in_days[which(d$test_duration_in_days == "60+")] <- ">60"
d$test_duration_in_days[which(d$test_duration_in_days == "NP")] <- "0" # no light pretreatment

d$dailyl_light_hours[which(d$genus == "Philadelphus")] <- "8"
