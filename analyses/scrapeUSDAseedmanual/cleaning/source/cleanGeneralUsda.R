## Started 10 JULY 2024 ##
## By Dan, continued by Justin ##


## Updated 26 Jan 2025 by Mao ##
## Updated 26 July 2025 by Deirdre ##
## Updated 3 Oct 2025 by Mao ##

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

# Clean "-" in specific columns
d$samples <- gsub("-"," to ",d$samples)
d$cold_stratification_days <- gsub("-"," to ",d$cold_stratification_days)


source("scrapeUSDAseedmanual/cleaning/source/cleanSpeciesUsda.R")
#d$genus_name[which(d$pdf_page_number==464 & d$pdf_table_number==4)] <- "Cupressus"
#d$species_name[which(d$pdf_page_number==464 & d$pdf_table_number==4 & d$speciesnames=="nevadensis")] <- "arizonica ssp. nevadensis"
#d$species_name[which(d$pdf_page_number==464 & d$pdf_table_number==4 & d$speciesnames=="pygmaea")] <- "goveniana ssp. pygmaea"

# manual cleaning

# cleaning sample number:
d$samples[which(d$latbi == "Baccharis_halimifolia")] <- "1"
d$samples[which(d$latbi == "Baccharis_pilularis" & d$responseValue == "93")] <- "1"
d$samples[which(d$latbi == "Baccharis_pilularis" & d$responseValue == "92")] <- "1"
d$samples[which(d$pdf_page_number==301 & d$pdf_table_number==4 & d$samples==25)] <- # 7 # actually was "2+5" but I am adding them up, and -- for this table --  not sure for this paper why B. koreana shows up as three rows instead of one row, but otherwise seems okay
  d$samples[which(d$pdf_page_number==377 & d$pdf_table_number==5 & d$samples=="7t")] <- 7 # the t was a footnote meaning 'Best results from a set of tests on each of 7 seedlots.'
d$samples[which(d$pdf_page_number==309 & d$pdf_table_number=="Table 6" & d$cold_stratification_days=="over winter" & d$species_name=="nana")] <- 1 
d$samples[which(d$pdf_page_number==336 & d$pdf_table_number=="5" & d$test_duration_in_days==63 & d$species_name=="aquatica")] <- 1 
d$samples[which(d$pdf_page_number==336 & d$pdf_table_number=="5" & d$medium=="Soil" & d$species_name=="cordiformis")] <- 1
d$samples[which(d$pdf_page_number == "936" & d$species_name == "acutissima")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "agrifolia")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "gambelii")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "kelloggii")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "laurifolia" & d$medium == "Soil")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "lyrata" & d$test_duration_in_days == "160")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "macrocarpa")] <- "11"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "marilandica")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "michauxii"& d$test_duration_in_days == "50")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "pagoda")] <- "11"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "phellos" & d$medium == "Soil")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "rubra" & d$medium == "Sand")] <- "11"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "rubra" & d$medium == "Sand/peat")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "vaccinifolia")] <- "1"
d$samples[which(d$pdf_page_number == "936" & d$species_name == "wislizenii")] <- "1"
d$samples[which(d$genus_name == "Ribes", d$species_name == "oxyacanthoides ssp. irriguum")] <- "11"
d$samples[which(d$pdf_page_number == "1063" & is.na(d$samples) & d$species_name %in% c("aucuparia", "scopulina", "sitchensis"))] <- "1"
d$samples[which(d$pdf_page_number == "1096" & is.na(d$samples))] <- "1"
d$samples[which(d$pdf_page_number == "1134" & d$species_name == "mertensiana" & d$germination_rate == "61")] <- "1"

# cleaning table numbers:
d$pdf_table_number[which(d$pdf_page_number == "277")] <- "3"
d$pdf_table_number[which(d$pdf_page_number == "287")] <- "3"
d$pdf_table_number[which(d$pdf_page_number==309 & d$pdf_table_number=="Table 6")] <- 6
d$pdf_table_number[which(d$pdf_page_number==411 & d$pdf_table_number=="Table 3")] <- 3
d$pdf_table_number[which(d$pdf_page_number==687 & d$pdf_table_number=="Table.5")] <- 5
d$pdf_table_number[which(d$pdf_page_number==692 & d$pdf_table_number=="Table.2")] <- 2

# clean light
d$dailyl_light_hours[which(d$pdf_page_number==336 & d$pdf_table_number==5)] <- 8 # see footnote in table which says "* Daily light period was 8 hours." 
# 'Both the Association of Official Seed Analysts (AOSA 1993) and the International Seed Testing Association (ISTA 1993) 
# recommend the same germination test procedures: germination on top of moist blotters or other paper products for 21 days 
# at temperatures alternating diurnally from 20 °C during a 16-hour dark period to 30°C during an 8-hour light period'
# We thus assume that the table rows with 21 days of 20C/30C applied this photoperiod
d$dailyl_light_hours[which(d$pdf_page_number == 646 & d$pdf_table_number == 8 & d$day_temp_celsius == '30' & d$night_temp_celsius == '20' & d$test_duration_in_days == '21')] <- 8
d$dailyl_light_hours[which(d$pdf_page_number==440 & d$pdf_table_number==3 & d$day_temp_celsius==30)] <- 8 
d$dailyl_light_hours[which(d$pdf_page_number == "1085" & d$species_name == "villosa")] <- "8"
d$dailyl_light_hours[which(d$pdf_page_number == "1148" & d$species_name == "americana" & d$medium == "paper pads")] <- "8"
d$dailyl_light_hours[which(d$pdf_page_number == "899" & d$genus_name == "Pseudotsuga")] <- "8"

# clean medium
d$medium[which(d$pdf_page_number==368 & d$pdf_table_number==5)] <- "sand, a sand–peat mixture, or a sandy loam soil"
d$medium[which(d$pdf_page_number==703 & d$pdf_table_number==4 & d$medium == 'na')] <- NA
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('pe, S', "pe  S"))] <- "Peat, sand or soil"
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('P'))] <- 'Absorbent medium in covered petri dish'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('P, S'))] <- 'Absorbent medium in covered petri dish, sand or soil'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('S'))] <- 'Sand or soil'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c("S  V", 'S, V', 'S V', 'sv'))] <- 'Sand or soil, vermiculite'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('V'))] <- 'Vermiculite'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('pl'))] <- 'Perlite'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('P, pl'))] <- 'Absorbent medium in covered petri dish, perlite'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('pl  S'))] <- 'Perlite, sand or soil'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('A, P'))] <- 'Absorbent paper (filter, blotter), absorbent medium in covered petri dish'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('A'))] <- 'Absorbent paper (filter, blotter)'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('A, P, v'))] <- 'Absorbent paper (filter, blotter), absorbent medium in covered petri dish, vermiculite'
d$medium[which(d$pdf_page_number==837 & d$pdf_table_number==10 & d$medium %in% c('K'))] <- 'Kimpak'
d$medium[which(d$pdf_page_number==432 & d$pdf_table_number==6)] <- "sand"

# clean germination temp
d$day_temp_celsius[which(d$pdf_page_number==432 & d$pdf_table_number==6)] <- 30 # "Temperatures were 30 °C for 8 hours and 20 °C for 16 hours each day. Sand was the medium used on all listed species. "
d$night_temp_celsius[which(d$pdf_page_number==432 & d$pdf_table_number==6)] <- 20
d$day_temp_celsius[which(d$pdf_page_number==622 & d$pdf_table_number==2)]<- 2
d$night_temp_celsius[which(d$pdf_page_number==622 & d$pdf_table_number==2)]<- 2
d$temp_unspecified_time_of_day_celsius[which(d$pdf_page_number == "1063" & d$species_name == "aucuparia" & d$medium == "Peat moss")] <- 1 
d$temp_unspecified_time_of_day_celsius[which(d$pdf_page_number == "1085" & d$species_name == "reticulata var. amurensis")] <- "20"
d$day_temp_celsius[which(d$pdf_page_number == "1085" & d$species_name == "villosa")] <- "20 to 30"
d$day_temp_celsius[which(d$day_temp_celsius == "5(§41)")] <- "5"
d$night_temp_celsius[which(d$night_temp_celsius == "5(§41)")] <- "5"
# Looking back at page 855 see that that al for day and and a7 for night, should be 1 and 7
d$day_temp_celsius[which(d$day_temp_celsius == "al")] <- "1"
d$night_temp_celsius[which(d$night_temp_celsius == "a7")] <- "7"
# Ribes aureum var. villosum has day or night temps in the temp_unspecified_time_of_day_celsius column = "20/0 (D/N)"; updating specific day night coln
d$day_temp_celsius[which(d$genus_name == "Ribes", d$species_name == "aureum")] <- "20"
d$night_temp_celsius[which(d$genus_name == "Ribes", d$species_name == "aureum")] <- "0"
d$temp_unspecified_time_of_day_celsius <- gsub("20/0 (D/N)","NA", d$temp_unspecified_time_of_day_celsius)

# clean germination duration
d$test_duration_in_days[which(d$pdf_page_number == "993" & d$species_name == "idaeus" &  !is.na(d$pretreatment))] <- "30"
d$test_duration_in_days[which(d$pdf_page_number == "1085" & d$species_name == "reticulata var. amurensis")] <- "21"
d$test_duration_in_days[which(d$pdf_page_number == "1091")] <- "28"

# clean stratification
d$cold_stratification_temp_C[which(d$pdf_page_number== 377 & d$pdf_table_number==5 & d$cold_stratification_days %in% c(0,28))] <- 1 
d$cold_stratification_temp_C[which(d$pdf_page_number==464 & d$pdf_table_number==4)] <- 1
d$cold_stratification_days[which(d$pdf_page_number==464 & d$pdf_table_number==4)] <- 30
d$cold_stratification_temp_C[which(d$pdf_page_number == "1063" & d$species_name == "aucuparia" & d$medium == "Peat moss")] <- "1"
d$cold_stratification_days[which(d$pdf_page_number == "1091" & d$species_name == "distichum")] <- "30"
d$cold_stratification_temp_C[which(d$pdf_page_number == "1091" & d$species_name == "distichum")] <- "3 to 5"
d$cold_stratification_days[which(d$pdf_page_number == "1091" & d$species_name == "ascendens")] <- "60 to 90"
d$cold_stratification_days[which(d$pdf_page_number == "1091" & d$species_name == "ascendens")] <- "4"
d$warm_stratification_temp_C[which(d$pdf_page_number == "1096" & is.na(d$warm_stratification_days))] <- NA
d$cold_stratification_days[which(d$pdf_page_number == "1184" & d$species_name == "jujuba")] <- "60 to 90"
d$cold_stratification_temp_C[which(d$pdf_page_number == "1184" & d$species_name == "jujuba")] <- "5"
d$cold_stratification_temp_C[which(d$pdf_page_number == "993" & d$pretreatment == "Stratification")] <- "5"
d$cold_stratification_days[which(d$pdf_page_number == "993" & d$species_name == "idaeus" &  !is.na(d$pretreatment))] <- "60 to 180"
d$cold_stratification_temp_C[which(d$pdf_page_number == "993" & d$species_name == "spectabilis")] <- "2 to 5"
d$cold_stratification_temp_C[which(d$pdf_page_number == "993" & d$species_name == "chamemorus")] <- "1"

# clean pretreatment
d$pretreatment[which(d$pdf_page_number==646 & d$pdf_table_number==8 & d$species_name == 'lyallii')] <- 'USP 3% H2O2' # "seeds were soaked up in USP 3% H2O2 for 24 days in lieu of stratification"
d$pretreatment[which(d$pdf_page_number==612 & d$pdf_table_number==6 & d$pregermination_treatment_time_minutes==240)]<- "1% citric acid"
d$pretreatment[which(d$pdf_page_number==612 & d$pdf_table_number==6 & d$pregermination_treatment_time_minutes==45)]<- "sulfuric acid"
d$pretreatment[which(d$pdf_page_number == "1063" & d$species_name == "sitchensis" & d$cold_stratification_days == "90")] <- "chemical - H2SO4"
d$pretreatment[which(d$pdf_page_number == "1091" & d$species_name == "ascendens")] <- "0.01% citric acid for 24-48h"

d$pregermination_treatment_time_minutes[which(d$pdf_page_number==612 & d$pdf_table_number==6 & d$pregermination_treatment_time_minutes==240)]<- 5760
d$pregermination_treatment_time_minutes[which(d$pdf_page_number == "1063" & d$species_name == "sitchensis" & d$cold_stratification_days == "90")] <- "10"

### clean notes
d$Notes[which(d$pdf_page_number==518 & d$pdf_table_number==8)] <- "The table references that the seeds were stratified, but there is no information about the pre-treatment"
d$Notes[which(d$pdf_page_number==523 & d$pdf_table_number==5)] <- "The table references that the seeds were stratified, but there is no information about the pre-treatment"
d$Notes[which(d$pdf_page_number==542 & d$pdf_table_number==6)] <- "The table references that the seeds were stratified, but there is no information about the pre-treatment"

# Clean seed type
d$seed_type[which(d$seed_type == "fresh seeds")] <- "fresh seed"
d$seed_type[which(d$seed_type == "dried seeds")] <- "dried seed"
d$seed_type[which(d$seed_type == "stored seeds")] <- "stored seed"

d$seed_type[which(d$seed_type == "")] <- ""
