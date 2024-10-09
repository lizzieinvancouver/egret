# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# 
# Cleaning Germination Temperature and Duration
# 
# Updated 16 July 2024
# by Justin
# 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

## This contains cleaning of germination temperature ##
## Original code taken from file called cleaningDL.R ##

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#1. germination temperature

# Values that are transformed i.e. averaged or rounded:
d$germTempGen <- d$germ.temp # This is currently doing nothing

# Now make new column with heavy duty cleaning
d$germTemp  <- d$germ.temp
d$germDuration <- d$germ.duration

d$germTemp[which(d$germTemp == "45219")] <- "20/10" # 45219 corresponds to tang10b which had 20/10 germination temperature regime
d$germTemp[which(d$germTemp == "unknown")] <- "NA"
d$germTemp[which(d$germTemp == "unknown ")] <- "NA"
d$germTemp[which(d$germTemp == "didn't mention")] <- "NA"
d$germTemp[which(d$germTemp == "NA (germ during strat)")] <- "NA"
d$germTemp[which(d$germTemp == "15-May")] <- "15/5"
d$germTemp[which(d$germTemp == "20-Oct")] <- "20/10"
d$germTemp[which(d$germTemp == "15-Jun")] <- "15/6"
d$germTemp[which(d$germTemp == "25-Oct")] <- "25/10"
d$germTemp[which(d$germTemp == "8,10")] <- "8/10"
d$germTemp[which(d$germTemp == "8, 10")] <- "8/10"
d$germTemp[which(d$germTemp == "4, 7, 10")] <- "4/7/10"
d$germTemp[which(d$germTemp == "15,5")] <- "15/5"
d$germTemp[which(d$germTemp == "25,15")] <- "25/15"
d$germTemp[which(d$germTemp == "20,30")] <- "20/30"
d$germTemp[which(d$germTemp == "10,20")] <- "10/20"
d$germTemp[which(d$germTemp == "5,15")] <- "5/15"
d$germTemp[which(d$germTemp == "25, 20, 15")] <- "25/20/15"
d$germTemp[which(d$germTemp == "15, 20, 25")] <- "15/20/25"
d$germTemp[which(d$germTemp == "20,15,20, 25")] <- "20/15/20/25"
d$germTemp[which(d$germTemp == "22.2//20/29.4")] <- "22.2/20/29.4"
d$germTemp[which(is.na(d$germTemp))] <- "NA"

# Anything ambient or greenhouse
d$germTemp[which(d$germTemp == "greenhouse")] <- "ambient"
d$germTemp[which(d$germTemp == "controlled greenhouse")] <- "ambient"
d$germTemp[which(d$germTemp == "open air")] <- "ambient"
d$germTemp[which(d$germTemp == "open field")] <- "ambient"
d$germTemp[which(d$germTemp == "unregulated: 6-27")] <- "ambient"

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Working on figuring the data for the papers that Ken listed as problematic

# Acosta13 - "germination temp missing from Figure 3 data"
d$germTemp[which(d$datasetID == "acosta12" & d$figure == "Fig 3")] <- "25/15"

# Mamut20 - "germ durations seem to be weird, like a number 1 was dragged through excel until 14 and then repeated again and again, even for the first three values for table 2 which should be 24, 56, and 84 days"
    # From looking over Mamut20, germDuration should be 30 days for everything except the experiment where they routinely took out seeds after months-long stratification was over
d$germDuration[which(d$datasetID == "mamut20")]
# This will be addressed in the germDuration section

# Sacande04 - "incubation temp is put as chill temp, might be better to put as germ temp"
for (i in 1:nrow(d)) {
  if (!is.na(d$datasetID[i]) && d$datasetID[i] == "sacande04") { 
    d$germTemp[i] <- d$chill.temp[i]
    d$chill.temp[i] <- NA  
  }
}

# tylkowski91 - "cold stratification is mistakenly coded as germination temp for some data from table 2"
for (i in 1:nrow(d)) {
  if (!is.na(d$datasetID[i]) && d$datasetID[i] == "tylkowski91" & d$figure[i] == "Table 2") {
    d$germTemp[i] <- d$chill.temp[i]
    d$chill.temp[i] <- d$germ.temp[i]
    d$germ.temp[i] <- d$germTemp[i]
  }
}
d$germTemp[which(d$germTemp == "3-15")] <- "15/3"
d$germTemp[which(d$germTemp == "3-20")] <- "20/3"

# yang08 - "germ temp is missing"
# According to the paper, it's 30/20 day/night
d$germTemp[which(d$datasetID == "yang08" & d$genus == "Litsea")] <- "30/20"

# yang18_1 - "chill and germination data for Figures 4 and 5 is complicated because some germination data were taken during stratification so the stratification conditions should actually be the germination data; I think cleaning these columns together would be easier than separately"

# Making an empty column to give numeric values for just this subset of data
d$yang18chill.duration <- NA
d$yang18germ.duration <- NA
d$yang18germDuration <- NA

# For figure 4
for (i in c(1:nrow(d))) {
  if(!is.na(d$datasetID[i]) && d$datasetID[i] == "yang18" && d$genus[i] == "Maackia" && d$figure[i] == "Figure 4") {
    d$yang18chill.duration[i] <- d$chill.duration[i]
    d$yang18germ.duration[i] <- d$germ.duration[i]
    d$yang18germDuration[i] <- d$germDuration[i]
  }
}

# For figure 5
for (i in c(1:nrow(d))) {
  if(!is.na(d$datasetID[i]) && d$datasetID[i] == "yang18" && d$genus[i] == "Maackia" && d$figure[i] == "Figure 5") {
    d$yang18chill.duration[i] <- d$chill.duration[i]
    d$yang18germ.duration[i] <- d$germ.duration[i]
    d$yang18germDuration[i] <- d$germDuration[i]
  }
}

# Converting these temp columns to numeric
d$yang18chill.duration <- as.numeric(d$yang18chill.duration)
d$yang18germ.duration <- as.numeric(d$yang18germ.duration)
d$yang18germDuration <- as.numeric(d$yang18germDuration)

# For Figure 4
for (i in 1:nrow(d)) {
  if (!is.na(d$datasetID[i]) && d$datasetID[i] == "yang18" && d$genus[i] == "Maackia" && d$figure[i] == "Figure 4") {
    if (!is.na(d$yang18germ.duration[i]) && !is.na(d$yang18chill.duration[i]) && d$yang18germ.duration[i] <= d$yang18chill.duration[i]) {
      d$germTemp[i] <- "4"
      d$yang18germDuration[i] <- d$yang18germ.duration[i] - d$yang18chill.duration[i]
    }
    else if (!is.na(d$yang18germ.duration[i]) && !is.na(d$chill.duration[i]) && d$yang18germ.duration[i] > d$yang18chill.duration[i]) {
      d$germTemp[i] <- "30/20"
    }
  }
}

# For Figure 5
for (i in 1:nrow(d)) {
  if (!is.na(d$datasetID[i]) && d$datasetID[i] == "yang18" && d$genus[i] == "Maackia" && d$figure[i] == "Figure 5") {
    if (!is.na(d$yang18germ.duration[i]) && !is.na(d$yang18chill.duration[i]) && d$yang18germ.duration[i] <= d$yang18chill.duration[i]) {
      d$germTemp[i] <- "4"
      d$yang18germDuration[i] <- d$yang18germ.duration[i] - d$yang18chill.duration[i]
    }
    else if (!is.na(d$yang18germ.duration[i]) && !is.na(d$chill.duration[i]) && d$yang18germ.duration[i] > d$yang18chill.duration[i]) {
      d$germTemp[i] <- "30/20"
    }
  }
}

# Assigning germDuration the temporary column values, figure 4
for (i in c(1:nrow(d))) {
  if(!is.na(d$datasetID[i]) && d$datasetID[i] == "yang18" && d$genus[i] == "Maackia" && d$figure[i] == "Figure 4") {
    d$germDuration[i] <- d$yang18germDuration[i]
  }
}

# Assigning germDuration the temporary column values, figure 5
for (i in c(1:nrow(d))) {
  if(!is.na(d$datasetID[i]) && d$datasetID[i] == "yang18" && d$genus[i] == "Maackia" && d$figure[i] == "Figure 5") {
    d$germDuration[i] <- d$yang18germDuration[i]
  }
}

# Removing the temporary columns
d <- d[, -which(names(d) == "yang18chill.duration")]
d <- d[, -which(names(d) == "yang18germ.duration")]
d <- d[, -which(names(d) == "yang18germDuration")]

# yeom21 - "for Figure 4, germination measurement details may be lacking; if lacking, germ temp and duration should be NA, if germination measurement is done at very end of stratification, germ temp and duration should be chill temp and duration"
# yeom21 <- d %>%
#   filter(datasetID == "yeom21" & figure == "Figure 4")
# The highest percent germ occurred for constant 25 degC, lowest for constant 5 degC, and intermediate for  5 -> 25 degC treatment
d$germTemp[which(d$datasetID == "yeom21" & d$figure == "Figure 4" & d$response == "10.866")] <- "25"
d$germTemp[which(d$datasetID == "yeom21" & d$figure == "Figure 4" & d$response == "1.904")] <- "25/5"
d$germTemp[which(d$datasetID == "yeom21" & d$figure == "Figure 4" & d$response == "0")] <- "5"

# zhou08 - "germ temp for figure 1 is missing"
# Should be resolved now that Selena has helped scrape this

# Chen15 the germ.temp was put into photoperiod
# for (i in 1:nrow(d)) {
#   if(!is.na(d$datasetID[i]) && d$datasetID[i] == "chen15" && d$treatment[i] == "incubation temperature"){
#     d$germ.temp[i] <- d$photoperiod[i]
#     d$photoperiod[i] <- 12
#   }
# }

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Making new columns for temperature regime class (constant or alternating), temperature 1, and temperature 2
# To make things easier turn the +/- temperature regimes into just their median value in germTemp column

d$germTemp <- sub("\\+.*",NA,d$germTemp)
d$tempClass <- ifelse(grepl(",|/|alternating|night|-",d$germTemp) & !grepl("+/-",d$germTemp),"alternating","constant")

d$germTemp[which(d$germTemp == "10-20")] <- "10/20"
d$germTemp[which(d$germTemp == "11-17")] <- "11/17"
d$germTemp[which(d$germTemp == "11-20")] <- "11/20"
d$germTemp[which(d$germTemp == "18-20")] <- "18/20"
d$germTemp[which(d$germTemp == "15-10")] <- "15/10"
d$germTemp[which(d$germTemp == "15-5")] <- "15/5"
d$germTemp[which(d$germTemp == "20-10")] <- "20/10"
d$germTemp[which(d$germTemp == "20-22")] <- "20/22"
d$germTemp[which(d$germTemp == "3-15")] <- "3/15"
d$germTemp[which(d$germTemp == "3-20")] <- "3/20"
d$germTemp[which(d$germTemp == "3-25")] <- "3/25"
d$germTemp[which(d$germTemp == "20-30")] <- "20/30"
d$germTemp[which(d$germTemp == "16-22")] <- "16/22"
d$germTemp[which(d$germTemp == "10-20")] <- "10/20"
d$germTemp[which(d$germTemp == "5-10")] <- "5/10"
d$germTemp[which(d$germTemp == "20-23")] <- "20/23"
d$germTemp[which(d$germTemp == "15-25")] <- "15/25"
d$germTemp[which(d$germTemp == "18.5-21.5")] <- "18.5/21.5"
d$germTemp[which(d$germTemp == "5 to 15")] <- "5/15"
d$germTemp[which(d$germTemp == "10 to 15")] <- "10/15"
d$germTemp[which(d$germTemp == "25 to 15")] <- "25/15"
d$germTemp[which(d$germTemp == "21/18 day/night")] <- "21/18"
d$germTemp[which(d$germTemp == "20°C (6h dark) ")] <- "20/25"
d$germTemp[which(d$germTemp == "15 - 25")] <- "15/25"
d$germTemp[which(d$germTemp == "24/30 (varying)")] <- "24/30"
d$germTemp[which(d$germTemp == "19-24")] <- "19/24"
d$germTemp[which(d$germTemp == "15 and 5")] <- "15/5"
d$germTemp[which(d$germTemp == "25 and 15")] <- "25/15"

# d$germDuration <- d$germ.duration
d$germDuration[which(d$germDuration == "14(7)")] <- "14"
d$germDuration[which(d$germDuration == "21(7)")] <- "21"
d$germDuration[which(d$germDuration == "28(7)")] <- "28"

d$germTemp <- sub("alternating temperature ",NA, d$germTemp)
d$germTemp <- sub("alternating ",NA, d$germTemp)

d$tempClass[which(d$germTemp == "25/20/15")] <- "other"
d$tempClass[which(d$germTemp == "22.2/20/29.4")] <- "other"
d$tempClass[which(d$germTemp == "15/20/25")] <- "other"
d$tempClass[which(d$germTemp == "20/15/20/25")] <- "other"
d$tempClass[which(d$germTemp == "15 and 25 then 3")] <- "other"
d$tempClass[which(d$germTemp == "pooled across 3 temp regimes: 15/5, 20/10, 25/76")] <- "other"

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Splitting apart the Night and Day
breakbyslash <- strsplit(as.character(d$germTemp), "/", fixed=TRUE)
d$tempDay <- unlist(lapply(breakbyslash, function(x) x[1]))
d$tempNight <- unlist(lapply(breakbyslash, function(x) x[2]))
# There's a weird one where the temp is 27-29/6-18

#those with triple or quadruple temp regimes are not going to have their temp1/temp2 columns populated
d$tempDay[which(d$tempClass == "other")] <- NA 
d$tempNight[which(d$tempClass == "other")] <- NA

# d %>% filter(germTemp == "27-29/6-18") #Dehgan84
# This paper says it's 27-29 in the day and 16-18 at night; which value should we take for each part of photoperiod?
# SCRUM: take the mean of each period as our alternating; 28 for day and 17 at night
d$tempDay[which(d$datasetID == "dehgan84" & d$tempDay == "27-29")] <- "28"
d$tempNight[which(d$datasetID == "dehgan84" & d$tempNight == "16-18")] <- "17"
d$tempNight[which(d$datasetID == "dehgan84" & d$tempNight == "6-18")] <- "17" 
d$tempNight[which(d$datasetID == "scocco98" & d$tempNight == "30 (varying)")] <- "30" 

d$tempNight[which(d$tempNight == "05")] <- "5"

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Germination Duration
d$germDuration <- d$germ.duration

d$germDuration[which(d$datasetID == "jensen97" & d$germ.time.zero == "when incubation begins")] <- "30"
d$germDuration[which(d$datasetID == "gremer20" & d$germ.duration == "30-31")] <- "7"
d$germDuration[which(d$datasetID == "ren15" & d$germ.duration == "30-31")] <- "30"
d$germDuration[which(d$datasetID == "marcello15" & d$germ.duration == "NA (<35)")] <- "35"
d$germDuration[which(d$germDuration == "~30")] <- "30"
d$germDuration[which(d$datasetID == "schutz02" & d$germ.duration == "30-50")] <- "50"

d[, 'germDurComment'] = NA
d$germDurComment[which(d$datasetID == "schutz02" & d$germDuration == "50")] <- "Paper says 30-50 as germDuration due to end of germination = 1 week since last observed germinant"
d$germDurComment[which(d$datasetID == "kato11" & d$germDuration == "unknown")] <- "Looked into the paper and found nothing except for germination counted every 3 days"

# Fixing the mamut20 issue here that Ken raised in ISSUE 14
# I'm certain that all values should be 30 days incubation...the 24, 56, 84 are referring to to cold/warm stratification, not germination
# Table 2 has mixed warm and cold stratification, so we need to transfer the data on stratification out of germTemp and into the chill or other.treatment columns
d$germDuration[which(d$datasetID == "mamut20")] <- "30"
d$germDuration[which(d$germ.duration == "9 months")] <- "273.75"
d$germDuration[which(d$germ.duration == "6 montns")] <- "273.75"

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fixing the day/night conundrum in the alternating temperature papers
d[ , 'photoperiodNote'] = NA
d$photoperiodNote[which(d$datasetID == "albrecht20" & d$study == "exp1" & d$photoperiod == "12")] <- "constant light" # change photoperiod to 24
d$photoperiodNote[which(d$datasetID == "albrecht20" & d$study == "exp1" & d$photoperiod == "0")] <- "constant darkness"
d$photoperiodNote[which(d$datasetID == "albrecht20" & d$study == "exp3" & d$photoperiod == "0")] <- "constant darkness"

d$photoperiodNote[which(d$datasetID == "cicek08" & d$species == "fraxinifolia ")] <- "just alternating temperature not photoperiod" # Change photoperiod to NA

d$photoperiodNote[which(d$datasetID == "han10" & d$species == "ingrata" & d$photoperiod == "12")] <- "assumed day is warmer temperature"

d$photoperiodNote[which(d$datasetID == "li11" & d$species == "centralasiatica" & d$photoperiod == "24")] <- "constant light"

d$photoperiodNote[which(d$datasetID == "meyer95" & d$genus == "Penstemon" & d$photoperiod == "24")] <- "constant light"

d$germTemp[which(d$datasetID == "herron01" & d$genus == "Melicytus")] <- "20" # it's not 18.5-21.5, that was a range
d$tempClass[which(d$datasetID == "herron01" & d$genus == "Melicytus")] <- "constant"
d$tempDay[which(d$datasetID == "herron01" & d$genus == "Melicytus")] <- "20"
d$tempNight[which(d$datasetID == "herron01" & d$genus == "Melicytus")] <- "NA"

d$germTemp[which(d$datasetID == "langlois17" & d$genus == "Carex")] <- "ambient" # it's not 25/10, that's what the authors reported in the intro as a known-to-be-successful germ temperature
# Langlois17 has a part that isn't scraped; it's light intensity
d$germTemp[which(d$datasetID == "langlois17" & d$figure == "Fig 4")] <- "20/18" #These are the light intensity treatments
d$photoperiod[which(d$datasetID == "langlois17" & d$figure == "Fig 4")] <- "14/10"

d$tempClass[which(d$datasetID == "langlois17" & d$genus == "Carex")] <- "constant"
d$tempDay[which(d$datasetID == "langlois17" & d$genus == "Carex")] <- "ambient"
d$tempNight[which(d$datasetID == "langlois17" & d$genus == "Carex")] <- "NA"

d$photoperiodNote[which(d$datasetID == "ochuodho08" & d$species == "capense")] <- "just alternating temperature not photoperiod"

d$photoperiodNote[which(d$datasetID == "povoa09" & d$species == "euaptoria" & d$photoperiod == "0")] <- "constant darkness"
d$photoperiodNote[which(d$datasetID == "povoa09" & d$species == "euaptoria" & d$photoperiod == "12")] <- "assumed day is warmer temperature"

d$photoperiodNote[which(d$datasetID == "roh08" & d$genus == "Corylopsis")] <- "just alternating temperature not photoperiod"

d$photoperiodNote[which(d$datasetID == "tylkowski07" & d$species == "catharticus")] <- "assumed day is warmer temperature"
d$photoperiodNote[which(d$datasetID == "tylkowski09" & d$species == "communis")] <- "assumed day is warmer temperature"
d$photoperiodNote[which(d$datasetID == "tylkowski10" & d$species == "rhamnoides")] <- "assumed day is warmer temperature"
d$photoperiodNote[which(d$datasetID == "tylkowski91" & d$species == "mas")] <- "assumed day is warmer temperature"

d$photoperiodNote[which(d$datasetID == "zhang21" & d$species == "koraiensis" & d$other.treatment == "200 μmol/m^2/s light")] <- "constant light"
d$photoperiodNote[which(d$datasetID == "zhang21" & d$species == "koraiensis" & d$other.treatment == "20 μmol/m^2/s light")] <- "constant light"
d$photoperiodNote[which(d$datasetID == "zhang21" & d$species == "koraiensis" & d$other.treatment == "0 μmol/m^2/s light")] <- "constant darkness"

d$photoperiodNote[which(d$datasetID == "chien10" & d$species == "glaucescens")] <- "assumed day is warmer temperature"

d$photoperiodNote[which(d$datasetID == "brenchley98" & d$species == "capricorni")] <- "two stage temperature regime not photoperiod"

d$photoperiodNote[which(d$datasetID == "yeom21" & d$figure == "Figure 4" & d$response == "1.904")] <- "two stage temperature regime not photoperiod"

d$germTemp[which(d$datasetID == "markovic20" & d$genus == "Liriodendron")] <- "21.5" 
d$tempClass[which(d$datasetID == "markovic20" & d$genus == "Liriodendron")] <- "constant"
d$tempDay[which(d$datasetID == "markovic20" & d$genus == "Liriodendron")] <- "21.5"
d$tempNight[which(d$datasetID == "markovic20" & d$genus == "Liriodendron")] <- "NA"

d$germTemp[which(d$datasetID == "momonoki79" & d$genus == "Bupleurum" & d$germTemp == "15/25")] <- "25" 
d$tempClass[which(d$datasetID == "momonoki79" & d$genus == "Bupleurum" & d$germTemp == "15/25")] <- "constant"
d$tempDay[which(d$datasetID == "momonoki79" & d$genus == "Bupleurum" & d$germTemp == "15/25")] <- "25"
d$tempNight[which(d$datasetID == "momonoki79" & d$genus == "Bupleurum" & d$germTemp == "15/25")] <- "NA"

d$germTemp[which(d$datasetID == "panayotova15" & d$genus == "Betonica" & d$germTemp == "18/20")] <- "19" 
d$tempClass[which(d$datasetID == "panayotova15" & d$genus == "Betonica" & d$germTemp == "18/20")] <- "constant"
d$tempDay[which(d$datasetID == "panayotova15" & d$genus == "Betonica" & d$germTemp == "18/20")] <- "19"
d$tempNight[which(d$datasetID == "panayotova15" & d$genus == "Betonica" & d$germTemp == "18/20")] <- "NA"

d$germTemp[which(d$datasetID == "prknova15" & d$genus == "Sorbus")] <- "21" 
d$tempClass[which(d$datasetID == "prknova15" & d$genus == "Sorbus")] <- "constant"
d$tempDay[which(d$datasetID == "prknova15" & d$genus == "Sorbus")] <- "21"
d$tempNight[which(d$datasetID == "prknova15" & d$genus == "Sorbus")] <- "NA"

d$germTemp[which(d$datasetID == "yaqoob17" & d$genus == "Ferula")] <- "ambient"
d$tempClass[which(d$datasetID == "yaqoob17" & d$genus == "Ferula")] <- "constant"
d$tempDay[which(d$datasetID == "yaqoob17" & d$genus == "Ferula")] <- "ambient"
d$tempNight[which(d$datasetID == "yaqoob17" & d$genus == "Ferula")] <- "NA"

d$photoperiodNote[which(d$datasetID == "geszprych02" & d$genus == "Rhaponticum")] <- "just alternating temperature not photoperiod"

d$photoperiodNote[which(d$datasetID == "winstead71" & d$genus == "Liquidambar")] <- "photoperiod 15 but alternating temperature at 12 hr interval"

d$photoperiodNote[which(d$datasetID == "zhou08" & d$figure == "Fig 1b")] <- "assumed day is warmer temperature"
d$photoperiodNote[which(d$datasetID == "zhou08" & d$figure == "Fig 1d")] <- "assumed day is warmer temperature"

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Swapping night and day for papers in which night came first when stating their photoperiod
# We don't want to overwrite the original columns, so making copy template columns
d$tempDayCopy <- d$tempDay
d$tempNightCopy <- d$tempNight

d$tempNight[which(d$datasetID == "jiro10" & d$species == "ermanii")] <- d$tempDayCopy[which(d$datasetID == "jiro10" & d$species == "ermanii")]
d$tempDay[which(d$datasetID == "jiro10" & d$species == "ermanii")] <- d$tempNightCopy[which(d$datasetID == "jiro10" & d$species == "ermanii")]

d$tempNight[which(d$datasetID == "jiro10" & d$species == "platyphylla")] <- d$tempDayCopy[which(d$datasetID == "jiro10" & d$species == "platyphylla")]
d$tempDay[which(d$datasetID == "jiro10" & d$species == "platyphylla")] <- d$tempNightCopy[which(d$datasetID == "jiro10" & d$species == "platyphylla")]

d$tempNight[which(d$datasetID == "kato11" & d$species == "sinensis")] <- d$tempDayCopy[which(d$datasetID == "kato11" & d$species == "sinensis")]
d$tempDay[which(d$datasetID == "kato11" & d$species == "sinensis")] <- d$tempNightCopy[which(d$datasetID == "kato11" & d$species == "sinensis")]

d$tempNight[which(d$datasetID == "lee06" & d$species == "sinicus ")] <- d$tempDayCopy[which(d$datasetID == "lee06" & d$species == "sinicus ")]
d$tempDay[which(d$datasetID == "lee06" & d$species == "sinicus ")] <- d$tempNightCopy[which(d$datasetID == "lee06" & d$species == "sinicus ")]

d$tempNight[which(d$datasetID == "liu13" & d$species == "glauca")] <- d$tempDayCopy[which(d$datasetID == "liu13" & d$species == "glauca")]
d$tempDay[which(d$datasetID == "liu13" & d$species == "glauca")] <- d$tempNightCopy[which(d$datasetID == "liu13" & d$species == "glauca")]

d$tempNight[which(d$datasetID == "meyer94" & d$genus == "Penstemon")] <- d$tempDayCopy[which(d$datasetID == "meyer94" & d$genus == "Penstemon")]
d$tempDay[which(d$datasetID == "meyer94" & d$genus == "Penstemon")] <- d$tempNightCopy[which(d$datasetID == "meyer94" & d$genus == "Penstemon")]

d$tempNight[which(d$datasetID == "parmenter96" & d$species == "angustifolia")] <- d$tempDayCopy[which(d$datasetID == "parmenter96" & d$species == "angustifolia")]
d$tempDay[which(d$datasetID == "parmenter96" & d$species == "angustifolia")] <- d$tempNightCopy[which(d$datasetID == "parmenter96" & d$species == "angustifolia")]

d$tempNight[which(d$datasetID == "parmenter96" & d$species == "purpurea")] <- d$tempDayCopy[which(d$datasetID == "parmenter96" & d$species == "purpurea")]
d$tempDay[which(d$datasetID == "parmenter96" & d$species == "purpurea")] <- d$tempNightCopy[which(d$datasetID == "parmenter96" & d$species == "purpurea")]

d$tempNight[which(d$datasetID == "pipinis09" & d$species == "fruiticans")] <- d$tempDayCopy[which(d$datasetID == "pipinis09" & d$species == "fruiticans")]
d$tempDay[which(d$datasetID == "pipinis09" & d$species == "fruiticans")] <- d$tempNightCopy[which(d$datasetID == "pipinis09" & d$species == "fruiticans")]

d$tempNight[which(d$datasetID == "pipinis20" & d$species == "avellana")] <- d$tempDayCopy[which(d$datasetID == "pipinis20" & d$species == "avellana")]
d$tempDay[which(d$datasetID == "pipinis20" & d$species == "avellana")] <- d$tempNightCopy[which(d$datasetID == "pipinis20" & d$species == "avellana")]

d$tempNight[which(d$datasetID == "ren08" & d$genus == "Pedicularis")] <- d$tempDayCopy[which(d$datasetID == "ren08" & d$genus == "Pedicularis")]
d$tempDay[which(d$datasetID == "ren08" & d$genus == "Pedicularis")] <- d$tempNightCopy[which(d$datasetID == "ren08" & d$genus == "Pedicularis")]

d$tempNight[which(d$datasetID == "tang10a" & d$species == "tristaniaecarpa")] <- d$tempDayCopy[which(d$datasetID == "tang10a" & d$species == "tristaniaecarpa")]
d$tempDay[which(d$datasetID == "tang10a" & d$species == "tristaniaecarpa")] <- d$tempNightCopy[which(d$datasetID == "tang10a" & d$species == "tristaniaecarpa")]

d$tempNight[which(d$datasetID == "zhou08" & d$figure == "Fig 1b")] <- d$tempDayCopy[which(d$datasetID == "zhou08" & d$figure == "Fig 1b")]
d$tempDay[which(d$datasetID == "zhou08" & d$figure == "Fig 1b")] <- d$tempNightCopy[which(d$datasetID == "zhou08" & d$figure == "Fig 1b")]
d$tempNight[which(d$datasetID == "zhou08" & d$figure == "Fig 1b")] <- d$tempDayCopy[which(d$datasetID == "zhou08" & d$figure == "Fig 1d")]
d$tempDay[which(d$datasetID == "zhou08" & d$figure == "Fig 1b")] <- d$tempNightCopy[which(d$datasetID == "zhou08" & d$figure == "Fig 1d")]

# Deleting the template columns
d <- d[, -which(names(d) == "tempNightCopy")]
d <- d[, -which(names(d) == "tempDayCopy")]

# Checking that germTempGen never changes from germ.temp 
identical(d$germTempGen, d$germ.temp)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# FROM ISSUE 18 on EGRET repo
# Making germTempGen a column for average temperatures, even for those that alternate; we need to use photoperiod as our thermoperiod
# First we want to make everything numeric and divided by a slash
# I think I can use germTemp as my foundation, since I already did all the removal of extraneous characters and reclassified all of the outlying data values?
# Or even better...use tempDay and tempNight

# Making germTempGen a weighted average based on photoperiod/thermoperiod
unique(d$photoperiod)
d$photoperiodCopy <- d$photoperiod
# Why are there so many that are more than 24???
d$photoperiodCopy[which(d$photoperiod == "16")] <- "16/8"
d$photoperiodCopy[which(d$photoperiod == "8")] <- "8/16"
d$photoperiodCopy[which(d$photoperiod == "18")] <- "18/6"
d$photoperiodCopy[which(d$photoperiod == "12")] <- "12/12"
d$photoperiodCopy[which(d$photoperiod == "12/12 hours")] <- "12/12"
d$photoperiodCopy[which(d$photoperiod == "0")] <- "0/24"
d$photoperiodCopy[which(d$photoperiod == "8:16")] <- "8/16"
d$photoperiodCopy[which(d$photoperiod == "8.00")] <- "8/16"
d$photoperiodCopy[which(d$photoperiod == "12 hr light")] <- "12/12"
d$photoperiodCopy[which(d$photoperiod == "11h light/13h dark")] <- "11/13"
d$photoperiodCopy[which(d$photoperiod == "14")] <- "14/10"
d$photoperiodCopy[which(d$photoperiod == "14:10")] <- "14/10"
d$photoperiodCopy[which(d$photoperiod == "alternating 12/12")] <- "12/12"
d$photoperiodCopy[which(d$photoperiod == "continuous cool light at 28 microE.m^-2.s^-1")] <- "24/0"
d$photoperiodCopy[which(d$photoperiod == "8h cool-white")] <- "8/16"
d$photoperiodCopy[which(d$photoperiod == "2h cool-white")] <- "2/22"
d$photoperiodCopy[which(d$photoperiod == "4h cool-white")] <- "20/4"
d$photoperiodCopy[which(d$photoperiod == "16h cool-white")] <- "16/8"
d$photoperiodCopy[which(d$photoperiod == "24h cool-white")] <- "24/0"
d$photoperiodCopy[which(d$photoperiod == "8h cool-white")] <- "16/8"
d$photoperiodCopy[which(d$photoperiod == "16-Aug")] <- "16/8"
d$photoperiodCopy[which(d$photoperiod == "cool-white throughout the process")] <- "24/0"
d$photoperiodCopy[which(d$photoperiod == "constant light")] <- "24/0"
d$photoperiodCopy[which(d$photoperiod == "alternating 16/8")] <- "16/8"
d$photoperiodCopy[which(d$photoperiod == "alternating 8/16")] <- "8/16"
d$photoperiodCopy[which(d$photoperiod == "constant dark")] <- "0/24"
d$photoperiodCopy[which(d$photoperiod == "cool-white alternating 12/12")] <- "12/12"
d$photoperiodCopy[which(d$photoperiod == "white alternating 16/8")] <- "16/8"
d$photoperiodCopy[which(d$photoperiod == "cool-white 24h")] <- "24/0"
d$photoperiodCopy[which(d$photoperiod == "12-18")] <- "12/18"
d$photoperiodCopy[which(d$datasetID == "tylkowski91" & d$figure == "Table 2")] <- "16/8"
d$photoperiodCopy[which(d$photoperiod == "0/16")] <- "16/8"
d$photoperiodCopy[which(d$photoperiod == "84")] <- "14/10"
d$photoperiodCopy[which(d$photoperiod == "16/9")] <- "16/8"
d$photoperiodCopy[which(d$photoperiod == "16/10")] <- "16/8"
d$photoperiodCopy[which(d$photoperiod == "25" & d$datasetID == "Chen15")] <- "12/12"
d$photoperiodCopy[which(d$datasetID == "yang08" & d$treatment == "warm stratification")] <- "8/16" #yang08 the photoperiod was dragged down so it's a series of values from 8 onward for warm strat. seeds
d$photoperiodCopy[which(d$photoperiod == "25/15")] <- "12/12"
d$photoperiodCopy[which(d$photoperiod == "0/24 ")] <- "0/24"
d$photoperiodCopy[which(d$photoperiod == "white 24h")] <- "24/0"
d$photoperiodCopy[which(d$photoperiod == "not.specified")] <- NA

# Figuring out the papers that these weird photoperiod values came from
# d$datasetID[which(d$photoperiod == "0/16")] #gianni19 and goggans74
#   # it's just 16/8 for both
# d$datasetID[which(d$photoperiod == "84")] #king12
#   # Should be 14/10
# d$datasetID[which(d$photoperiod == "25")] #chen15 and yang08
# d$datasetID[which(d$photoperiod == "25/15")]
#   # The actual photoperiod for chen15 is 12 and yang08 is 8
# d$datasetID[which(d$photoperiod == "16/9")] #rahnama-ghahfarokhi07
#   # It's just 16/8
# d$datasetID[which(d$photoperiod == "16/10")] #rahnama-ghahfarokhi07
#   # It's just 16/8

# AS PER LIZZIE MEETING 23 JULY 2024
d$datasetID[which(d$photoperiod == "0.69")] #bungard97
  # 2 hours every 3 days + 1.5 minutes
  # This is 2.025 hours every 72 hours, then divide it by 3 so it's out of 24
d$photoperiodCopy[which(d$photoperiod == "0.69")] <- "0.675/23.325"

d$datasetID[which(d$photoperiod == "0.17")] #grose57
  # These weren't actually light treatments, it was just when the researchers wanted to check for germination
        # Not rlly photoperiod
d$photoperiodCopy[which(d$photoperiod == "0.17")] <- NA

d$datasetID[which(d$photoperiod == "13/9h")] #Middleton96
  # Uhhh...the paper literally says 13/9 ijbol
        # Assume 13 is correct, but then note it down somewhere
d$photoperiodCopy[which(d$photoperiod == "13/9h")] <- "13/11"
d$photoperiodNote[which(d$photoperiodCopy == "13/11" & d$datasetID == "middleton96")] <- "The paper explicitly said 13/9h which is questionable"

d$datasetID[which(d$photoperiod == "0.25")] #batlla03
d$datasetID[which(d$photoperiod == "1")] #batlla03
  # The treatment itself was a red light pulse exposure for 15 mins, does that count as photoperiod? They only turned the light on to check for germination
        # This would be 15 minutes out of 24 hours
        # But on further review it's not a period of red light
d$photoperiodCopy[which(d$photoperiod == "0.25" & d$datasetID == "batlla03")] <- "NA"
d$photoperiodCopy[which(d$photoperiod == "1" & d$datasetID == "batlla03")] <- "NA"

d$datasetID[which(d$photoperiod == "8/16; 0/24")] #mattana09
  # data points are an average of 6 replicates (3 in light and 3 in dark)
        # This might have to become NA sadly, we might also just treat it as 4/20 and take the mean of day and night
d$photoperiodCopy[which(d$photoperiod == "8/16; 0/24")] <- "4/20"

# Just checking anything I missed
unique(d$photoperiodCopy)
d$photoperiodCopy[which(d$photoperiodCopy == "greenhouse")] <- "ambient"
d$photoperiodCopy[which(is.na(d$photoperiodCopy))] <- "NA"
d$datasetID[which(d$photoperiodCopy == "0 then 24")] #yang20
  #The paper says the seeds were dark incubated and then transferred to light, which was 12/12 in their methods...so for now I will write 12/12 and change as needed
d$photoperiodCopy[which(d$datasetID == "yang20" & d$photoperiod == "0 then 24")] <- "12/12"

# Converting any ambient into numeric placeholder so as.numeric doesn't mess it up
d$photoperiodCopy[which(d$photoperiodCopy == "ambient")] <- "99991"

# Now that the photoperiod business is all cleared up, I can separate it into two columns
# Making a photoperiod night column for easier math
breakbyslashphoto <- strsplit(as.character(d$photoperiodCopy), "/", fixed=TRUE)
d$photoperiodCopyDay <- unlist(lapply(breakbyslashphoto, function(x) x[1]))
d$photoperiodCopyNight <- unlist(lapply(breakbyslashphoto, function(x) x[2]))

# Now I need to make germTempGen a weighted average using photoperiod as a stand-in for thermoperiod
# firstly converting the photoperiodCopy and tempGen day and night columns into numeric
# Just going to convert "ambient" into a numeric placeholder for now so that rowMeans doesn't induce NA
d$tempDay[which(d$tempDay == "ambient")] <- 99991
d$tempNight[which(d$tempNight == "ambient")] <- 99991
d$tempDay[which(d$tempDay == "NA")] <- NA
d$tempNight[which(d$tempNight == "NA")] <- NA

unique(d$photoperiodCopyDay)
unique(d$photoperiodCopyNight)
d$photoperiodCopy[which(is.na(d$photoperiodCopy))] <- NA
d$photoperiodCopyDay <- as.numeric(d$photoperiodCopyDay)
print("This `NAs introduced by coercion' happens when we make photoperiodCopyDay numeric")
d$photoperiodCopyNight <- as.numeric(d$photoperiodCopyNight)

unique(d$tempDay)
unique(d$tempNight)
d$tempDay <- as.numeric(d$tempDay)
d$tempNight <- as.numeric(d$tempNight)

# Can I do this in a for loop??
d$germTempGen <- NA #making this an empty column to populate

# The weighted average is the sum of tempDay and tempNight multiplied by their corresponding ratio of day or night hours out of 24
for(i in 1:nrow(d)){
  if(!is.na(d$tempDay[i]) && !is.na(d$tempNight[i])){
    d$germTempGen[i] <- (d$tempDay[i]*d$photoperiodCopyDay[i])/24+(d$tempNight[i]*d$photoperiodCopyNight[i])/24
  }
}
# Now what about the columns in which there's constant temperature and therefore only values in tempDay and not in tempNight?
# We can do regular mean average in these ones then
for(i in 1:nrow(d)){
  if(is.na(d$tempNight[i] && !is.na(d$tempDay[i]))){
    d$germTempGen[i] <- (d$tempDay[i]+d$tempNight[i])/2
  }
}

# These for loops might assume that darkness treatments where photoperiod is 0 will always be cool, but this is not true, because sometimes seeds incubated in darkness were still exposed to alternating temperatures
for(i in 1:nrow(d)){
  if(!is.na(d$photoperiodCopyDay[i]) && d$photoperiodCopyDay[i] == 0 && d$tempClass[i] == "alternating"){
    d$germTempGen[i] <- (d$tempDay[i]+d$tempNight[i])/2
  }
}

# Converting things back to ambient and character class
d$photoperiodCopyDay <- as.character(d$photoperiodCopyDay)
d$photoperiodCopyNight <- as.character(d$photoperiodCopyNight)
d$photoperiodCopyDay[which(d$photoperiodCopyDay == 99991)] <- "ambient"
d$photoperiodCopyNight[which(d$photoperiodCopyNight == 99991)] <- "ambient"

d$germTempGen <- as.character(d$germTempGen)
d$germTempGen[which(d$tempDay == 99991)] <- "ambient"

d$tempDay[which(d$tempDay == 99991)] <- "ambient"
d$tempNight[which(d$tempNight == 99991)] <- "ambient"

# And if germTempGen is NA, then we can just put in germTemp values
for(i in 1:nrow(d)){
  if(is.na(d$germTempGen[i])){
    d$germTempGen[i] <- d$germTemp[i]
  }
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Some overview for the git issue
# Figuring out how many papers have alternating temperature regimes
# unique(d$germ.temp)
d.filtered <- d[grepl(",|/|alternating|night|-", d$germTemp) & !grepl("\\+/-", d$germTemp), ]
d.filtered <- d.filtered[, c("datasetID", "study", "genus", "species", "germTemp")]
d.summarized <- aggregate(. ~ datasetID, data = d.filtered, FUN = function(x) unique(x))
# d.summarized <- unique(d.filtered$datasetID)
# write.csv(d.summarized,"cleaning/checks/AlternatingTempPaperList.csv")

