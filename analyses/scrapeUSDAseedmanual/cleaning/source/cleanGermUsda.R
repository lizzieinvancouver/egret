## Started 10 July 2024 ##
## By Dan, continued by Justin ##

## Updated 26 Jan 2025 by Mao ##
## Checked 26 July 2025 by Deirdre

# unique(d$germination_time_days)
d$germination_time_days <- gsub("-"," to ",d$germination_time_days)

# unique(d$avg_germination_percent) #why are there ranges?
d$avg_germination_percent <- gsub("-"," to ", d$avg_germination_percent)
d$avg_germination_percen <- gsub("na", NA, d$avg_germination_percen)

# unique(d$germination_rate)
d$germination_rate <- gsub("-"," to ",d$germination_rate)

# unique(d$germinative_energy_percent)
d$germinative_energy_percent <- gsub("-"," to ",d$germinative_energy_percent)
d$germinative_energy_percent <- gsub("na", NA,d$germinative_energy_percent)

# unique(d$total_germination_percent)
d$total_germination_percent <- gsub("-"," to ",d$total_germination_percent)
d$total_germination_percent <- gsub("na", NA,d$total_germination_percent)
# unclear what the values in the bracket are, assume it is the min and max---> not cleaning it here bc it gets fixed in cleanMeanUsda.R
# d$total_germination_percent[which(d$total_germination_percent == "69 (18 to 94)")] <- "69"
# d$total_germination_percent[which(d$total_germination_percent == "93 (84 to 96)")] <- "93"
# d$total_germination_percent[which(d$total_germination_percent == "60 (40 to 88)")] <- "60"

# double checked tables and corrected values
d$total_germination_percent[which(d$total_germination_percent == "<")] <- "<1"
d$total_germination_percent[which(d$total_germination_percent == "18.600000000000001")] <- "18.6"
d$total_germination_percent[which(d$total_germination_percent == "72.599999999999994")] <- "72.6"
d$total_germination_percent[which(d$total_germination_percent == "4 to  to 45")] <- "4 to 45"
d$total_germination_percent[which(d$total_germination_percent == "16:")] <- "16"

# standard error in parenteses
d$total_germination_percent[which(d$total_germination_percent == "28.3 (2.2)")] <- "28.3"
d$total_germination_percent[which(d$total_germination_percent == "41.7 (2.2)")] <- "41.7"
d$total_germination_percent[which(d$total_germination_percent == "53.3 (2.2)")] <- "53.3"
d$total_germination_percent[which(d$total_germination_percent == "61.7 (2.2)")] <- "61.7"
d$total_germination_percent[which(d$total_germination_percent == "66.7 (2.2)")] <- "66.7"
d$total_germination_percent[which(d$total_germination_percent == "70.8 (2.2)")] <- "70.8"
d$total_germination_percent[which(d$total_germination_percent == "65 (2.2)")] <- "65"
d$total_germination_percent[which(d$total_germination_percent == "59.2 (2.2)")] <- "59.2"

# unique(d$avg_germinative_energy_percent)
d$avg_germinative_energy_percent <- gsub("-"," to ",d$avg_germinative_energy_percent)

# unique(d$avg_germinative_capacity)
d$avg_germinative_capacity <- gsub("-"," to ",d$avg_germinative_capacity)

