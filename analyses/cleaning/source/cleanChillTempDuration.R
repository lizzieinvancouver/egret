## Started 30 January 2024 ##
## By Deirdre to begin ##
## Updated by Ken SM and Lizzie in summer 2024 ##
## Looked at by CRD on 28 January 2025

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

# Do basic overview of the data to look at what needs cleaning 
temp_vals <- unique(d$chill.temp)
neg_temp <- c(3, 4, 5, 6, 9, 43, 50, 97)
dash <- grep("-", temp_vals)
unc <- setdiff(dash, neg_temp)

slash_unc <- grep("/-", temp_vals)
slash <- grep("/", temp_vals)
altern <- setdiff(slash, slash_unc)
altern <- setdiff(altern, neg_temp)

comb <- c(grep(",", temp_vals), grep(">", temp_vals))
comb <- comb[order(comb)]

papers_with_unc <- unique(d$datasetID[d$chill.temp %in% temp_vals[unc]])
papers_with_altern <- unique(d$datasetID[d$chill.temp %in% temp_vals[altern]])
papers_with_comb <- unique(d$datasetID[d$chill.temp %in% temp_vals[comb]])
papers_with_na <- unique(d$datasetID[is.na(d$chill.temp)])

data_points_with_chill <- nrow(d[!is.na(d$chill.temp),])
data_points_with_unc <- nrow(d[d$chill.temp %in% temp_vals[unc],])
data_points_with_altern <- nrow(d[d$chill.temp %in% temp_vals[altern],])
data_points_with_comb <- nrow(d[d$chill.temp %in% temp_vals[comb],])

#find data points
idx <- which(d$chillTemp == unique(d$chillTemp)[15])
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "entered.by",
                                        "treatment", "chill.temp", "chill.duration",
                                        "germ.temp", "germ.duration", "other.treatment",
                                        "photoperiod", "respvar", "response"))
idx <- which(d$datasetID == "airi09")
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "species", "entered.by",
                                        "treatment", "chill.temp", "chill.duration",
                                        "germ.temp", "germ.duration", "other.treatment",
                                        "photoperiod", "respvar", "response", "figure"))

d$chill.duration[which(d$chill.duration == "unknown ")] <- NA
d$chill.duration[which(d$chill.duration == "NA")] <- NA

# no values like "n/a" and "N/A" so far though
d$chill.temp[which(d$chill.temp == "n/a")] <- "NA"
d$chill.temp[which(d$chill.temp == "N/A")] <- "NA"

# Building new columns with cleaner data
d$chillTemp <- d$chill.temp
d$chillDuration <- d$chill.duration
d$chillTempUnc <- NA
d$chillTempCycle <- NA
d$chillLightCycle <- NA

# === === === === === === === === === === === === === === === === === === ===
## Additional cleaning
# === === === === === === === === === === === === === === === === === === ===
#lee06
temp <- c(rep(5, 12), rep(NA, 15))
d$chillTemp[which(d$datasetID == "lee06" & d$figure == "Table 2")] <- temp
temp <- c(rep(c(20, 40, 60), 4), rep(NA, 15))
d$chillDuration[which(d$datasetID == "lee06" & d$figure == "Table 2")] <- temp

#budisavljevic21
d$chillTemp[which(d$datasetID == "budisavljevic21" & d$figure == "Table2e")] <- 23

#barnhill82
d$chillTemp[which(d$datasetID == "barnhill82")] <- 3

#vahdati12: changing weeks to days
d$chillDuration[which(d$datasetID == "vahdati12")] <- as.numeric(d$chill.duration[which(d$datasetID == "vahdati12")]) * 7

#lee21: changing weeks to days
d$chillDuration[which(d$datasetID == "lee21")] <- as.numeric(d$chill.duration[which(d$datasetID == "lee21")]) * 7

# === === === === === === === === === === === === === === === === === === ===
## Clean remaining chill duration
# === === === === === === === === === === === === === === === === === === ===
#chen08
d$chillDuration[which(d$datasetID == "chen08" & d$figure == "Table1")] <-
  as.numeric(d$chill.duration[which(d$datasetID == "chen08" & d$figure == "Table1")]) - 30

#chen06
temp <- rep(c(0, 30, 60, 90, 180, 270, 360), 2)
d$chillDuration[which(d$datasetID == "chen06")] <- temp

#nkomo09 : rounding up to 4 days.
d$chillDuration[which(d$datasetID == "nkomo09" & d$chill.duration == 3.5)] <- 4

#sundaramoorthy93
d$chillDuration[which(d$datasetID == "sundaramoorthy93" & d$chill.temp == 4)] <-
  round(as.numeric(d$chillDuration[which(d$datasetID == "sundaramoorthy93" & d$chill.temp == 4)]))

#chien09: Changing temp of 0 to NA and fixing chilling duration # TO CHECK
d$chillTemp[which(d$datasetID == "chien09" & d$chill.temp == 0)] <- NA
d$chillDuration[which(d$datasetID == "chien09" & d$chill.temp == 0)] <- 0
d$chillDuration[which(d$datasetID == "chien09" & d$chill.temp == 4)] <-
  round(as.numeric(d$chillDuration[which(d$datasetID == "chien09" & d$chill.temp == 4)]))

#jensen97: round up the number of days
d$chillDuration[which(d$datasetID == "jensen97" & d$chill.temp == 4)] <-
  round(as.numeric(d$chillDuration[which(d$datasetID == "jensen97" & d$chill.temp == 4)]))

#ma18 : chill temp for the seeds that were moved to an germ temp of 15C
d$chillDuration[which(d$datasetID == "ma18" & d$chill.duration =="28")] <- 0
d$chillDuration[which(d$datasetID == "ma18" & d$chill.duration =="28 +28")] <- 28
d$chillTemp[which(d$datasetID == "ma18" & d$germ.temp =="5 to 15")] <- 5
d$chillTemp[which(d$datasetID == "ma18" & d$germ.temp =="10 to 15")] <- 10
d$chillTemp[which(d$datasetID == "ma18" & d$germ.temp =="25 to 15")] <- 25

#mattana16: observed germination during strat, so considering it as a germination temperature from 5C to 10C.
d$chillTemp[which(d$datasetID == "mattana16")] <- NA
d$chillDuration[which(d$datasetID == "mattana16")] <- 0

#nin17
dur <- c(15, 30, 60, 90)
d$chillTemp[which(d$datasetID == "nin17" & d$treatment == "non stratified")] <- 4
d$chillDuration[which(d$datasetID == "nin17" & d$treatment == "non stratified")] <- 0
d$chillDuration[which(d$chill.duration == "15;30;60;90")] <- NA # average across all treatments

#nurse08: changing months to days
d$chillDuration[which(d$datasetID == "nurse08" & d$chill.duration == "60")] <- 61
d$chillDuration[which(d$datasetID == "nurse08" & d$chill.duration == "90")] <- 91

# okay11: averaged resp var of the 3 different chilling treatments. Converting to NA.90, 120, 150
d$chillTemp[which(d$datasetID == "okay11" & d$treatment == "Control")] <- NA
d$chillDuration[which(d$datasetID == "okay11" & d$treatment == "Control")] <- 0
d$chillDuration[which(d$chill.duration == "90, 120, 150")] <- NA
d$chillTempUnc[which(d$datasetID == "okay11" & !is.na(d$chill.temp))] <- 1

#tang21: figure 1 was not a chilling treatment, so there is no chilling here. thus NA.
d$chillTemp[which(d$chill.duration == "0, 15, 30, 60")] <- NA
d$chillDuration[which(d$chill.duration == "0, 15, 30, 60")] <- NA

#bibby53: paper NA so setting chill treatments to na
d$chillTemp[which(d$datasetID == "bibby53")] <- NA
d$chillDuration[which(d$datasetID == "bibby53")] <- NA

# === === === === === === === === === === === === === === === === === === ===
## Standardize chillTemp and Duration formats
# === === === === === === === === === === === === === === === === === === ===
#zhang21 - -5/5/25/15
d$chillTemp[which(d$datasetID == "zhang21")] <- gsub("/", " then ", d$chill.temp[which(d$datasetID == "zhang21")])
d$chillDuration[which(d$datasetID == "zhang21" & d$chill.temp == "-5/5")] <- "50 then 50"
d$chillDuration[which(d$datasetID == "zhang21" & d$chill.temp == "-5/5/25")] <- "50 then 50 then 50"
d$chillDuration[which(d$datasetID == "zhang21" & d$chill.temp == "-5/5/25/15")] <- "50 then 50 then 50 then 50"

#tilki07: standardize temp and duration
d$chillTemp[which(d$datasetID == "tilki07" & d$chill.temp == "4/20")] <- "20 then 4"
d$chillTempUnc[which(d$datasetID == "tilki07" & d$chill.temp != "4/20")] <- 1
d$chillTempUnc[which(d$datasetID == "tilki07" & d$chill.temp == "4/20")] <- "1 then 1"
temp <- c(0, 30, 61, 91, "61 then 61", "30 then 61", "30 then 30", 0, "30 then 30", 0, 0,
          30, "30 then 30", 0, 0, "30 then 30", 0, 0, 30, 30)
d$chillDuration[which(d$datasetID == "tilki07")] <- temp

#ochuodho08: mean of 5 and 10 and adding uncertainty
d$chillTemp[which(d$datasetID == "ochuodho08" & d$chill.temp == "5-10")] <- 7.5
d$chillTempUnc[which(d$datasetID == "ochuodho08" & d$chill.temp == "5-10")] <- 2.5

#morozowska02: mean of 5 and 10 and adding uncertainty
d$chillTemp[which(d$datasetID == "morozowska02" & d$chill.temp == "5-10")] <- 7.5
d$chillTempUnc[which(d$datasetID == "morozowska02" & d$chill.temp == "5-10")] <- 2.5

#al-absi10: mean of 4 and 5 and adding uncertainty
d$chillTemp[which(d$datasetID == "al-absi10" & d$chill.temp == "4-5")] <- 4.5
d$chillTempUnc[which(d$datasetID == "al-absi10" & d$chill.temp == "4-5")] <- 0.5

#rizwan18: mean of 4 and 5 and adding uncertainty
d$chillTemp[which(d$datasetID == "rizwan18" & d$chill.temp == "4-5")] <- 4.5
d$chillTempUnc[which(d$datasetID == "rizwan18" & d$chill.temp == "4-5")] <- 0.5

#shahi-gharahlar12: mean of 4 and 5 and adding uncertainty
d$chillTemp[which(d$datasetID == "shahi-gharahlar12" & d$chill.temp == "4-5")] <- 4.5
d$chillTempUnc[which(d$datasetID == "shahi-gharahlar12" & d$chill.temp == "4-5")] <- 0.5

#markovic20: fixing cycle and adjusting temperature
d$chillTemp[which(d$datasetID == "markovic20" & d$chill.temp == "3-5")] <- 4
d$chillTempUnc[which(d$datasetID == "markovic20" & d$chill.temp == "3-5")] <- 1
d$chillTemp[which(d$datasetID == "markovic20" & d$chill.temp == "3/12")] <- "3 and 12"
d$chillTempCycle[which(d$datasetID == "markovic20" & d$chill.temp == "3/12")] <- "168 and 168"
d$chillDuration[which(d$datasetID == "markovic20")] <- round(as.numeric(d$chill.duration[which(d$datasetID == "markovic20")]))

#pipinis20: mean of 4 and 1 and adding uncertainty. 
d$chillTemp[which(d$datasetID == "pipinis20" & d$treatment == "Cold stratification")] <- 4
d$chillTempUnc[which(d$datasetID == "pipinis20" & d$chill.temp == "3-5")] <- 1
temp <- c(rep(4, 4), rep("22.5 then 4", 6))
d$chillTemp[which(d$datasetID == "pipinis20" & d$treatment != "Cold stratification")] <- temp
temp <- c(0, 30, 60, 90, "30 then 30", "30 then 60", "30 then 90", "60 then 30",
          "60 then 60", "60 then 90")
d$chillDuration[which(d$datasetID == "pipinis20" & d$treatment != "Cold stratification")] <- temp

#pipinis12 - 3-5
temp <- c(NA, rep(4, 4), 22.5, rep("22.5 then 4", 4), 22.5, rep("22.5 then 4", 4),
          NA, rep(4, 4), 22.5, rep("22.5 then 4", 4), 22.5, rep("22.5 then 4", 6), 4)
d$chillTemp[which(d$datasetID == "pipinis12" & d$treatment == "warm/cold stratification")] <- temp
temp <- c(NA, rep(1, 4), 2.5, rep("2.5 then 1", 4), 2.5, rep("2.5 then 1", 4),
          NA, rep(1, 4), 2.5, rep("2.5 then 1", 4), 2.5, rep("2.5 then 1", 6), 1)
d$chillTempUnc[which(d$datasetID == "pipinis12" & d$treatment == "warm/cold stratification")] <- temp
temp <- c(seq(0, 120, 30), 30, paste0(30, " then ", seq(30, 120, 30)), 60,
          paste0(60, " then ", seq(30, 120, 30)), seq(0, 120, 30), 30,
          paste0(30, " then ", seq(30, 120, 30)), 60, paste0(60, " then ", seq(30, 120, 30)),
          "60 then 120", "30 then 120", 120)
d$chillDuration[which(d$datasetID == "pipinis12" & d$treatment == "warm/cold stratification")] <- temp
d$chillTemp[which(d$datasetID == "pipinis12" & d$treatment == "chemical")] <- 4
d$chillTempUnc[which(d$datasetID == "pipinis12" & d$treatment == "chemical")] <- 1
temp <- c(rep(seq(0, 120, 30), 8), 120, 120, 120, 90, 90, 60, 60, 30, 120, 120, 120, 90, 90, 90)
d$chillDuration[which(d$datasetID == "pipinis12" & d$treatment == "chemical")] <- temp

#acosta13: no chilling treatment
d$chillTemp[which(d$datasetID == "acosta13")] <- NA
d$chillDuration[which(d$datasetID == "acosta13")] <- NA

#brandel05: adding cycle
temp <- c(NA, NA, 5, 5, 11, 11, 13, 13, 15, 15)
d$chillTemp[which(d$datasetID == "brandel05" & d$other.treatment == "primary dormant")] <- temp
temp <- c(NA, NA, rep(28, 8))
d$chillDuration[which(d$datasetID == "brandel05" & d$other.treatment == "primary dormant")] <- temp

temp <- c("", "", paste0(" then ", c(5, 5, 11, 11, 13, 13, 15, 15)))
d$chillTemp[which(d$datasetID == "brandel05" & d$other.treatment == "secondary dormant")] <- paste0("5 then 25",  temp)
temp <- c("", "", rep(" then 28", 8))
d$chillDuration[which(d$datasetID == "brandel05" & d$other.treatment == "secondary dormant")] <- paste0("28 then 28", temp)

#Boscagli01: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "boscagli01" & d$chill.temp == "07-May")] <- 6
d$chillDuration[which(d$datasetID == "boscagli01" & d$chill.temp == "07-May")] <- 1

#Borghetti86: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "borghetti86" & d$chill.temp == "03-Feb")] <- 2.5
d$chillTempUnc[which(d$datasetID == "borghetti86" & d$chill.temp == "03-Feb")] <- 0.5
d$chillLightCycle[which(d$datasetID == "borghetti86" & d$chill.temp == "03-Feb")] <- 0

#barros12: temperatures and their corresponding duration
d$chillTemp[which(d$datasetID == "barros12" & d$chill.temp == "20/4")] <- "20 then 40"
d$chillDuration[which(d$datasetID == "barros12" & d$chill.temp == "20/4")] <- "30 then 30"

#jabarzare11: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "jabarzare11" & d$chill.temp == "0-5")] <- 2.5
d$chillTempUnc[which(d$datasetID == "jabarzare11" & d$chill.temp == "0-5")] <- 2.5

#javanmard14: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "javanmard14" & d$chill.temp == "4+/-0.5")] <- 4
d$chillTempUnc[which(d$datasetID == "javanmard14" & d$chill.temp == "4+/-0.5")] <- 0.5
d$chillLightCycle[which(d$datasetID == "javanmard14" & d$chill.temp == "4+/-0.5")] <- 0

#jusung16: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "jusung16" & d$chill.temp == "4+/-1")] <- 4
d$chillTempUnc[which(d$datasetID == "jusung16" & d$chill.temp == "4+/-1")] <- 1

#kazaz10: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "kazaz10" & d$chill.temp == "4+/-1")] <- "25 then 4"
d$chillTempUnc[which(d$datasetID == "kazaz10" & d$chill.temp == "4+/-1")] <- "NA then 1"
d$chillDuration[which(d$datasetID == "kazaz10" & d$chill.temp == "4+/-1")] <- "28 then 150"

#parvin15: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "parvin15" & d$chill.temp == "4+/-1")] <- 4
d$chillTempUnc[which(d$datasetID == "parvin15" & d$chill.temp == "4+/-1")] <- 1
d$chillDuration[which(d$datasetID == "parvin15" & is.na(d$chill.temp))] <- 0

#karlsson08: average temp and add uncertainty. Rounding up duration in days
d$chillTemp[which(d$chill.temp == "4.5-5.4")] <- 4.95
d$chillTempUnc[which(d$chill.temp == "4.5-5.4")] <- 0.45
d$chillTemp[which(d$chill.temp == "22-23.2")] <- 22.6
d$chillTempUnc[which(d$chill.temp == "22-23.2")] <- 0.6
d$chillDuration[which(d$datasetID == "karlsson08" & d$chill.duration == 17.5)] <- 18

#kettenring07:"10:10 hours of high and low temperature with a 2-h linear transition between temperature changes". Encoded as 12/12 to ensure daily cycle
d$chillTemp[which(d$datasetID == "kettenring07" & d$chill.temp == "5/1")] <- "5 and 1"
d$chillTempCycle[which(d$datasetID == "kettenring07" & d$chill.temp == "5/1")] <- "12 and 12"

#hawkins19 - 5/1
d$chillTemp[which(d$datasetID == "hawkins19" & d$chill.temp == "5/1")] <- "5 and 1"

#yang20: adjust duration of control to zero and its corresponding light cycle
d$chillTemp[which(d$datasetID == "yang20" & d$treatment == "control")] <- 1
d$chillDuration[which(d$datasetID == "yang20" & d$treatment == "control")] <- 0
d$chillLightCycle[which(d$datasetID == "yang20" & d$treatment == "control")] <- 24

#yang16b
d$chillTemp[which(d$datasetID == "yang16b" & d$treatment == "none")] <- 1 
d$chillDuration[which(d$datasetID == "yang16b" & d$treatment == "none")] <- 0

#yang18a : fixing temp and duration for species "taiwanensis"
d$chillTemp[which(d$datasetID == "yang18a" & d$treatment == "control" & d$figure == "Figure 3")] <- 4
d$chillDuration[which(d$datasetID == "yang18a" & d$treatment == "control" & d$figure == "Figure 3")] <- 0
d$chillTemp[which(d$datasetID == "yang18a" & is.na(d$chill.duration) & d$figure == "Figure 4")] <- 4
d$chillDuration[which(d$datasetID == "yang18a" & is.na(d$chill.duration) & d$figure == "Figure 4")] <- 0
temp <- which(d$datasetID == "yang18a" & !is.na(d$chill.duration) & d$figure == "Figure 4")
for(i in temp){
  if(d$chill.duration[i] >= d$germ.duration[i]){
    d$chillTemp[i] <- NA
    d$chillDuration[i] <- NA
  }
}
d$chillTemp[which(d$datasetID == "yang18a" & is.na(d$chill.duration) & d$figure == "Figure 5")] <- 4
d$chillDuration[which(d$datasetID == "yang18a" & is.na(d$chill.duration) & d$figure == "Figure 5")] <- 0
temp <- which(d$datasetID == "yang18a" & !is.na(d$chill.duration) & d$figure == "Figure 5")
for(i in temp){
  if(d$chill.duration[i] >= d$germ.duration[i]){
    d$chillTemp[i] <- NA
    d$chillDuration[i] <- NA
  }
}

#yang18c
d$chillDuration[which(d$datasetID == "yang18c" & d$figure == "Figure 3")] <-
  round(as.numeric(d$chill.duration[which(d$datasetID == "yang18c" & d$figure == "Figure 3")]))
d$chillDuration[which(d$datasetID == "yang18c" & d$figure == "Figure 3" & d$chill.duration == 0.864)] <-
  0
d$chillTemp[which(d$datasetID == "yang18c" & d$figure != "Figure 3")] <- NA
d$chillDuration[which(d$datasetID == "yang18c" & d$figure != "Figure 3")] <- NA

#muller03: changing duration to 0
d$chillDuration[which(d$datasetID == "muller03" & is.na(d$chill.temp))] <- 0

#tylkowski10
d$chillTemp[which(d$datasetID == "tylkowski07" & d$chill.duration == 0)] <- NA

#tylkowski09: standardize duration and temp formats
d$chillDuration[which(d$datasetID == "tylkowski09" & d$response == 0)] <- 98
temp <- c(rep("15 then NA then 15 then 3", 6), "15 then NA then 3")
d$chillTemp[which(d$datasetID == "tylkowski09" & d$response != 0)] <- temp
temp <- c("14 then NA then 84", "28 then NA then 70", "42 then NA then 56",
          "56 then NA then 42", "70 then NA then 28", "84 then NA then 14", "98 then NA")
d$chillDuration[which(d$datasetID == "tylkowski09" & d$response != 0)] <- paste0(temp, " then 84")

#tylkowski07
d$chillTemp[which(d$datasetID == "tylkowski07" & d$chill.duration == 0)] <- NA

#ren04
d$chillDuration[which(d$datasetID == "ren04" & is.na(d$chill.temp))] <- 0

#ahola99 
d$chillDuration[which(d$datasetID == "ahola99" & is.na(d$chill.temp))] <- 0
d$chillLightCycle[which(d$datasetID == "ahola99" & !is.na(d$chill.temp))] <- 0

#parmenter96: average temp and add uncertainty.
d$chillTemp[which(d$datasetID == "parmenter96" & d$chill.temp == "3-5")] <- 4
d$chillTempUnc[which(d$datasetID == "parmenter96" & d$chill.temp == "3-5")] <- 1
d$chillTemp[which(d$datasetID == "parmenter96" & d$chill.temp == "0-1")] <- 0.5
d$chillTempUnc[which(d$datasetID == "parmenter96" & d$chill.temp == "0-1")] <- 0.5
d$chillDuration[which(d$datasetID == "parmenter96" & is.na(d$chill.temp))] <- 0

#conversa09
d$chillDuration[which(d$datasetID == "conversa09" & is.na(d$chill.temp))] <- 0

#crank92: adding decimal to farenheit --> celcius conversion
d$chillTemp[which(d$datasetID == "crank92" & d$chill.temp == 4)] <- 4.4
d$chillTemp[which(d$datasetID == "crank92" & d$chill.temp == 38)] <- 37.8
d$chillTemp[which(d$datasetID == "crank92" & d$chill.temp == "38/29")] <- "37.8 and 29.4"
d$chillTempCycle[which(d$datasetID == "crank92" & d$chill.temp == "38/29")] <- "8 and 16"

#cuena-lombrana18
d$chillTemp[which(d$datasetID == "cuena-lombrana18" & d$chill.temp == "(25/10)/5/0")] <- "25 and 10 then 5 then 0"
d$chillDuration[which(d$datasetID == "cuena-lombrana18" & d$chill.temp == "(25/10)/5/0")] <- "90 then 30 then 90"
d$chillTempCycle[which(d$datasetID == "cuena-lombrana18" & d$chill.temp == "(25/10)/5/0")] <- "NA then NA then NA"
d$chillLightCycle[which(d$datasetID == "cuena-lombrana18" & d$chill.temp == "(25/10)/5/0")] <- "12 then 12 then 0"

#cousins10: taking the mean of duration when pooled accross 3 temp and vice versa
d$chill.duration[which(d$datasetID == "cousins10" & d$response == "71.3")] <- "28/56/64"
d$chill.duration[which(d$datasetID == "cousins10" & d$response == "81")] <- "28/56/64"
d$chill.duration[which(d$datasetID == "cousins10" & d$response == "69.4")] <- "28/56/64"
d$chillDuration[which(d$datasetID == "cousins10" & d$response == "71.3")] <- 56
d$chillDuration[which(d$datasetID == "cousins10" & d$response == "81")] <- 56
d$chillDuration[which(d$datasetID == "cousins10" & d$response == "69.4")] <- 56
d$chill.temp[which(d$datasetID == "cousins10" & d$response == "76.4")] <- "5/10/15"
d$chill.temp[which(d$datasetID == "cousins10" & d$response == "76.9")] <- "5/10/15"
d$chill.temp[which(d$datasetID == "cousins10" & d$response == "68.5")] <- "5/10/15"
d$chillTemp[which(d$datasetID == "cousins10" & d$response == "76.4")] <- 10
d$chillTemp[which(d$datasetID == "cousins10" & d$response == "76.9")] <- 10
d$chillTemp[which(d$datasetID == "cousins10" & d$response == "68.5")] <- 10

#zadeh15
d$chillTemp[which(d$datasetID == "zadeh15" & d$chill.duration != 10)] <- 5
d$chillDuration[which(d$datasetID == "zadeh15" & is.na(d$chill.duration))] <- 0
d$chillDuration[which(d$datasetID == "zadeh15" & d$chill.duration != 10)] <- c(5, 10, 15)
d$chillLightCycle[which(d$datasetID == "zadeh15" & d$chill.duration != 10)] <- 0

#king12
d$chillDuration[which(d$datasetID == "king12" & d$treatment == "control, no treatment")] <- 0

#naseri18: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "naseri18")] <- 3
d$chillTempUnc[which(d$datasetID == "naseri18")] <- 1
d$chillDuration[which(d$datasetID == "naseri18" & d$chill.duration == "Continuous cold stratification")] <- NA

#gimenez-benavides13: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "gimenez-benavides13" & !is.na(d$chill.temp))] <- 3
d$chillTempUnc[which(d$datasetID == "gimenez-benavides13" & !is.na(d$chill.temp))] <- 1
d$chillLightCycle[which(d$datasetID == "gimenez-benavides13" & !is.na(d$chill.temp))] <- 0

#skordilis95: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "skordilis95" & !is.na(d$chill.temp))] <- 3
d$chillTempUnc[which(d$datasetID == "skordilis95" & !is.na(d$chill.temp))] <- 1
d$chillLightCycle[which(d$datasetID == "skordilis95" & !is.na(d$chill.temp))] <- 0

#hatzilazarou21: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "hatzilazarou21")] <- 3
d$chillTempUnc[which(d$datasetID == "hatzilazarou21")] <- 1

#maithani90: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "maithani90" & d$chill.temp == "2-4")] <- 3
d$chillTempUnc[which(d$datasetID == "maithani90" & d$chill.temp == "2-4")] <- 1

#nawrot-chorabik21
d$chillLightCycle[which(d$datasetID == "nawrot-chorabik21" & !is.na(d$chill.temp))] <- 0

#yeom21: cold strat is 5C for 8 weeks (56 days)
d$chillTemp[which(d$datasetID == "yeom21" & d$figure == "Figure 5")] <- 5
d$chillDuration[which(d$datasetID == "yeom21" & d$figure == "Figure 5")] <- 56
d$chillTemp[which(d$datasetID == "yeom21" & d$figure == "Figure 6")] <- 5
d$chillDuration[which(d$datasetID == "yeom21" & d$figure == "Figure 6")] <- 56
d$chillTemp[which(d$datasetID == "yeom21" & d$figure == "Figure 7")] <- 5
d$chillDuration[which(d$datasetID == "yeom21" & d$figure == "Figure 7")] <- 56

#song20: standardize format
tempfig <- c("5", "5 then 10", "5 then 10 then 15", "5 then 10 then 15 then 20", "5 then 10 then 15 then 20 then 25")
durfig <- c("31", "31 then 30", "31 then 30 then 31", "31 then 30 then 31 then 30", "31 then 30 then 31 then 30 then 31")

d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 3a")] <- tempfig
d$chillDuration[which(d$datasetID == "song20" & d$figure == "Figure 3a")] <- durfig
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 3b")] <- 5
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 3c")] <- 10
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 3d")] <- 15
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 3e")] <- 20

d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 4a")] <- tempfig
d$chillDuration[which(d$datasetID == "song20" & d$figure == "Figure 4a")] <- durfig
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 4b")] <- 5
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 4c")] <- 10
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 4d")] <- 15
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 4e")] <- 20

d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 5a")] <- tempfig
d$chillDuration[which(d$datasetID == "song20" & d$figure == "Figure 5a")] <- durfig
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 5b")] <- 5
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 5c")] <- 10
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 5d")] <- 15
d$chillTemp[which(d$datasetID == "song20" & d$figure == "Figure 5e")] <- 20

#schutz02: formatting duration and temp
d$chillTemp[which(d$datasetID == "schutz02")] <- 1.5
d$chillTempUnc[which(d$datasetID == "schutz02")] <- 1.5
temp <- c(0, 0, 0, 0, 30, 30, 30, 30, 60, 60, 60, 60, 91, 91, 91, 91, 183, 183, 183, 183)
d$chillDuration[which(d$datasetID == "schutz02" & d$study == "exp1")] <- temp
temp <- c(0, 0, 0, 0, 91, 91, 91, 91, 183, 183, 183, 183)
d$chillDuration[which(d$datasetID == "schutz02" & d$study == "exp3")] <- temp

#omar21: adding uncertainty
d$chillTempUnc[which(d$datasetID == "omar21" & !is.na(d$chill.temp))] <- 1

#grose57: change every temp so it's 4.4 C instead of 3.3 to 5C
d$chillTemp[which(d$chill.temp == 4.44 | d$chill.temp == "3.3-5")] <- 4.4
d$chillTemp[which(d$datasetID == "grose57" & d$figure == "Text")] <- NA
d$chillDuration[which(d$datasetID == "grose57" & d$figure == "Text")] <- NA
d$chillTemp[which(d$datasetID == "grose57" &
                    d$treatment == "cold stratification" & 
                    d$figure == "Table 3")] <-
  paste0(d$germ.temp[which(d$datasetID == "grose57" &
                             d$treatment == "strengthened dormancy " & 
                             d$figure == "Table 3")],
         " then ", d$chill.temp[which(d$datasetID == "grose57" &
                                       d$treatment == "cold stratification" & 
                                       d$figure == "Table 3")])
d$chillDuration[which(d$datasetID == "grose57" &
                    d$treatment == "cold stratification" & 
                    d$figure == "Table 3")] <-
  paste0(d$germ.duration[which(d$datasetID == "grose57" &
                             d$treatment == "strengthened dormancy " & 
                             d$figure == "Table 3")],
         " then ", d$chill.duration[which(d$datasetID == "grose57" &
                                       d$treatment == "cold stratification" & 
                                       d$figure == "Table 3")])

#pipinis09: standardize format of duration, temp and cycle
d$chillTemp[which(d$chill.temp == "20/25")] <- "20 and 25"
d$chillTempCycle[which(d$chill.temp == "20/25")] <- "16 and 8"
d$chillLightCycle[which(d$chill.temp == "20/25")] <- 8

d$chillTemp[which(d$datasetID == "pipinis09" & d$chill.temp == "2-4")] <- 3
d$chillTempUnc[which(d$datasetID == "pipinis09" & d$chill.temp == "2-4")] <- 1
d$chillLightCycle[which(d$datasetID == "pipinis09" & d$chill.temp == "2-4")] <- 0

d$chillTemp[which(d$chill.temp == "20/25 (warm); 4-6 (cold)")] <- "20 and 25 then 3"
d$chillTempUnc[which(d$chill.temp == "20/25 (warm); 4-6 (cold)")] <- "NA and 1"
d$chillTempCycle[which(d$chill.temp == "20/25 (warm); 4-6 (cold)")] <- "16 and 8 then NA"
d$chillLightCycle[which(d$chill.temp == "20/25 (warm); 4-6 (cold)")] <- "8 then 0"

temp <- c("30 then 30", "30 then 60", "60 then 30", "60 then 60")
d$chillDuration[which(d$chill.temp == "20/25 (warm); 4-6 (cold)")] <- temp

#pliszko18
d$chillLightCycle[which(d$datasetID == "pliszko18")] <- 0

#pritchard93: standardize cycles for temp and duration # TO CHECK. need to look back in the paper 
d$chillTemp[grep("6°C", d$chill.duration[d$datasetID == "pritchard93"])] <- "6 then 16"
temp <- c(364, "14 then 350", "28 then 336", "42 then 322", "56 then 308",
          "70 then 294", "84 then 280", "112 then 252", "140 then 224", "161 then 203",
          "182 then 182", "210 then 154", 364)
d$chillDuration[grep("6°C", d$chill.duration[d$datasetID == "pritchard93"])] <- temp
d$chillTemp[grep("5°C", d$chill.duration)] <- "25 then 5"
temp <- c(84, "14 then 84", "21 then 84", "28 then 84", "42 then 84", "63 then 84")
d$chillDuration[grep("5°C", d$chill.duration)] <- temp
d$chillTemp[which(d$datasetID == "pritchard93" & d$chillDuration == 84)] <- 5
d$chillLightCycle[which(d$datasetID == "pritchard93" & !is.na(d$chill.temp))] <- 0

# redondo-gomez11
d$chillTemp[which(d$datasetID == "redondo-gomez11" & d$figure == "Table 1" & d$responseVar == "percent.germ")] <- c(rep(NA, 5), rep(NA, 5), rep(5, 5))
d$chillDuration[which(d$datasetID == "redondo-gomez11" & d$figure == "Table 1" & d$responseVar == "percent.germ")] <- c(rep(NA, 5), rep(NA, 5), rep(30, 5))

#tylkowski91: standardize cycles for temp and duration
###exp1
temp <- c("20 and 30", "20 and 30", "20 and 30", "20 and 30", "15 and 25", "15 and 25",
          "15 and 25", "15 and 25", "10 and 20", "10 and 20", "10 and 20", "10 and 20", 20, 20, 20, 20,
          "20 and 30", "20 and 30", "20 and 30", "20 and 30", "15 and 25",
          "15 and 25", "15 and 25", "10 and 20", "10 and 20", "10 and 20", "10 and 20", 20, 20, 20, 20,
          "20 and 30", "20 and 30", "20 and 30", "20 and 30", "15 and 25", "15 and 25",
          "15 and 25", "15 and 25", "10 and 20", "10 and 20", "10 and 20", "10 and 20", 20, 20, 20, 20)
d$chillTemp[which(d$datasetID == "tylkowski91" & d$study == "exp1" & !d$treatment == "thermal stratification (cold phase)")][-48] <- paste0(temp, " then 3")
temp <- c("20 and 30", "20 and 30", "20 and 30", "20 and 30", "15 and 25", "15 and 25",
          "15 and 25", "15 and 25", "10 and 20", "10 and 20", "10 and 20", "10 and 20")
d$chillTemp[which(d$datasetID == "tylkowski91" & d$study == "exp1" & d$treatment == "thermal stratification (cold phase)")] <- temp
temp <- c(126, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168,
          126, 126, 168, 168, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168, 
          126, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168)
temp2 <- c(126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168,
           126, 168, 126, 168, 126, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168,
           126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168)
d$chillDuration[which(d$datasetID == "tylkowski91" & d$study == "exp1" & !d$treatment == "thermal stratification (cold phase)")][-48] <- paste0(temp, " then ", temp2)
d$chillDuration[which(d$datasetID == "tylkowski91" & d$study == "exp1" & d$treatment == "thermal stratification (cold phase)")] <- c(126, 126, 168, 168)
###exp2
temp <- c("15 and 25", "15 and 25", "15 and 25", "20 and 30", "20 and 30", "20 and 30",
          "15 and 25", "15 and 25", "15 and 25", "20 and 30", "20 and 30", "20 and 30")
d$chillTemp[which(d$datasetID == "tylkowski91" & d$study == "exp2")] <- paste0(temp, " then 3")
temp <- c(126, 119, 105, 126, 119, 105, 112, 105, 105, 112, 105, 105)
d$chillDuration[which(d$datasetID == "tylkowski91" & d$study == "exp2")] <- paste0(c(98, 112, 126), " then ", temp)
###exp3
temp <- c("15 and 25", "20 and 30", "15 and 25", "20 and 30")
d$chillTemp[which(d$datasetID == "tylkowski91" & d$study == "exp3")] <- paste0(temp, " then 3")
d$chillDuration[which(d$datasetID == "tylkowski91" & d$study == "exp3")] <- "126 then 126"
###exp 1, 2, 3
d$chillTemp[which(d$datasetID == "tylkowski91" & d$study == "exp 1, 2, 3")] <- "20 and 30 then 3"
d$chillDuration[which(d$datasetID == "tylkowski91" & d$study == "exp 1, 2, 3")] <- "126 then 126"
###tempCycle
d$chillTempCycle[which(d$datasetID == "tylkowski91" & !d$treatment == "thermal stratification (cold phase)")] <- "24 and 24 then NA"
d$chillTempCycle[which(d$datasetID == "tylkowski91" & d$chillTemp == "20 then 3")] <- "24 then NA"
d$chillTempCycle[which(d$datasetID == "tylkowski91" & d$treatment == "thermal stratification (cold phase)")] <- "24 and 24"

#wytsalucy21: per germ values not differentiated by both hormone and temperature because their interaction is not deemed significant
temp <- c(4, 7, 10)
for(i in 1:length(temp)){
  d$chillTemp[which(d$datasetID == "wytsalucy21" & d$chill.duration == 56 & d$chill.temp == temp[i])] <- paste0(temp[i], " then 20 then ", temp[i])
}
d$chillDuration[which(d$datasetID == "wytsalucy21" & d$chill.duration == 56 & d$chill.temp != 20)] <- "28 then 14 then 28"
d$chillDuration[which(d$datasetID == "wytsalucy21" & d$chill.duration == 56 & d$chill.temp == 20)] <- 70
d$chillTemp[which(d$datasetID == "wytsalucy21" & d$chill.temp == "4, 7, 10")] <- "ave"

#yusefi-tanha19
d$chillLightCycle[which(d$datasetID == "yusefi-tanha19")] <- 0

#edwards73: average temp and add uncertainty
d$chillTemp[which(d$chill.temp == "1 - 4")] <- 2.5
d$chillTempUnc[which(d$chill.temp == "1 - 4")] <- 1.5

#madeiras07: average temp and add uncertainty
d$chillTemp[which(d$datasetID == "madeiras07" & d$chill.temp == "5 +/- 2")] <- 5
d$chillTempUnc[which(d$datasetID == "madeiras07" & d$chill.temp == "5 +/- 2")] <- 2
d$chillLightCycle[which(d$datasetID == "madeiras07" & d$chill.temp == "5 +/- 2")] <- 0

#esmaeili09: average temp and add uncertainty
d$chillTemp[which(d$chill.temp == "15 - 25")] <- 20
d$chillTempUnc[which(d$chill.temp == "15 - 25")] <- 5

#markovic19: average temp and add uncertainty
d$chillTemp[which(d$chill.temp == "3 - 5")] <- 4
d$chillTempUnc[which(d$chill.temp == "3 - 5")] <- 1

#mamut20: standardize format and now i need to 
# 1. change germ duration
# 2. change photoperiodCor
d$chillDuration[which(d$datasetID == "mamut20" & d$chill.duration == "0 (control)")] <- NA
d$chillTemp[which(d$datasetID == "mamut20" & d$figure =="figure 3" & d$treatment =="cold strat")] <- "5 and 2"
d$chillTemp[which(d$datasetID == "mamut20" & d$figure =="figure 3" & d$treatment =="warm strat")] <- "25 and 15"
temp <- c(rep("25 and 15", 6))
dur <- c(rep(28, 3), rep(56, 3), rep(28, 3), rep(56, 3))
d$chillTemp[which(d$datasetID == "mamut20" & d$figure =="table 2")] <- temp 
d$chillDuration[which(d$datasetID == "mamut20" & d$figure =="table 2")] <- dur

#wang09: standardize format
d$chillTemp[which(d$chill.temp == "alternating 15/5")] <- "15 and 5"
d$chillLightCycle[which(d$datasetID == "wang09")] <- 0

#watanabe02: standardize format and average and uncertainty
d$chillTemp[which(d$chill.temp == "25 -> 5 +/- 2")] <- "25 then 5"
d$chillLightCycle[which(d$chill.temp == "25 -> 5 +/- 2")] <- "0 then NA"
d$chillTemp[which(d$datasetID == "watanabe02" & d$chill.temp == "5 +/- 2")] <- 5
d$chillTempUnc[which(d$datasetID == "watanabe02" & !is.na(d$chill.temp))] <- 2
d$chillDuration[which(d$chill.temp == "25 -> 5 +/- 2")] <- "30 then 30"

#zhou03: standardize format
d$chillTemp[which(d$chill.temp == "4,22")] <- "4 and 22"
d$chillTemp[which(d$chill.temp == "4,22,4,22")] <- "4 and 22"
d$chillTemp[which(d$chill.temp == "4,22,4,22,4,22,4,22")] <- "4 and 22"
d$chillTemp[which(d$chill.temp == "4,22,4,22,4,22,4,22,4,22,4,22,4,22,4,22")] <- "4 and 22"
d$chillTempCycle[which(d$chill.temp == "4,22")] <- "1344 and 1344"
d$chillTempCycle[which(d$chill.temp == "4,22,4,22")] <- "672 and 672"
d$chillTempCycle[which(d$chill.temp == "4,22,4,22,4,22,4,22")] <- "336 and 336"
d$chillTempCycle[which(d$chill.temp == "4,22,4,22,4,22,4,22,4,22,4,22,4,22,4,22")] <- "168 and 168"
d$chillDuration[which(d$datasetID == "zhou03")] <- 112

#zhou08: standardize format
d$chillTemp[which(d$datasetID == "zhou08" & d$treatment == "stratification")] <- 5
d$chillDuration[which(d$datasetID == "zhou08" & d$treatment == "stratification" & is.na(d$chill.temp))] <- 84
d$chillTemp[which(d$chill.temp == "20,10")] <- "25 then 5"
d$chillLightCycle[which(d$datasetID == "zhou08" & d$treatment == "stratification")] <- c(24, 24, 0, 0, "24 then 24", "24 then 24") 
d$chillDuration[which(d$chill.temp == "20,10")] <- "28 then 56"
d$chillTemp[which(d$datasetID == "zhou08" & d$treatment == "temperature")] <- "25 then 5"
d$chillLightCycle[which(d$datasetID == "zhou08" & d$treatment == "temperature")] <- "24 then 24"
d$chillDuration[which(d$datasetID == "zhou08" & d$treatment == "temperature")] <- "28 then 56"
d$chillTemp[which(d$datasetID == "zhou08" & d$treatment == "water stress")] <- "25 then 5"
d$chillLightCycle[which(d$datasetID == "zhou08" & d$treatment == "water stress")] <- "24 then 24"
d$chillDuration[which(d$datasetID == "zhou08" & d$treatment == "water stress")] <- "28 then 56"

#yang08 - 30/20 warm, cold strat---it is a day night alternating temp regime for 12 weeks
d$chillTemp[which(d$datasetID == "yang08" & d$chill.temp == "30/20")] <- "23.33 then 4"

d$chillTempCycle[which(d$datasetID == "yang08" & d$chill.temp == "30/20")] <- "30 then 20"
d$chillLightCycle[which(d$datasetID == "yang08" & d$chill.temp == "30/20")] <- "8 then NA"
temp1 <- rep(c(60, 90, 120, 150, 180), each = 5)
temp2 <- rep(c(60, 90, 120, 150, 180), 5)
d$chillDuration[which(d$datasetID == "yang08" & d$chill.temp == "30/20")] <- paste0(temp1, " then ", temp2)
d$chillTemp[which(d$datasetID == "yang08" & d$figure == "Figure 2")]  <- NA
d$chillTemp[which(d$datasetID == "yang08" & d$figure == "Figure 4")]  <- NA
d$chillTemp[which(d$datasetID == "yang08" & d$figure == "Figure 5")]  <- NA
d$chillDuration[which(d$datasetID == "yang08" & d$figure == "Figure 2")]  <- NA
d$chillDuration[which(d$datasetID == "yang08" & d$figure == "Figure 4")]  <- NA
d$chillDuration[which(d$datasetID == "yang08" & d$figure == "Figure 5")]  <- NA

#zulfiqar15: average temp and add uncertainty
d$chillTemp[which(d$chill.temp == "4 +/- 2 ")] <- 4
d$chillTempUnc[which(d$chill.temp == "4 +/- 2 ")] <- 2

# battaglia97 : missing chill duration in the figure # TO CHECK


#fulbright86: standardize format
d$chillTemp[which(d$chill.temp == "30/7")] <- "30 then 7"
d$chillDuration[which(d$chill.temp == "30/7")] <- "7 then 14"

#gianni19: average temp and add uncertainty
d$chillTemp[which(d$chill.temp == "2+-0.1")] <- 2
d$chillTempUnc[which(d$chill.temp == "2+-0.1")] <- 0.1
d$chillLightCycle[which(d$chill.temp == "2+-0.1")] <- "0 and 24"

#rezvani14: average temp and add uncertainty
d$chillTemp[which(d$chill.temp == "5+-1")] <- 5
d$chillTempUnc[which(d$chill.temp == "5+-1")] <- 1

#kolodziejek18: average temp and add uncertainty
d$chillTemp[which(d$chill.temp == "4.5-5.1")] <- 4.8
d$chillTempUnc[which(d$chill.temp == "4.5-5.1")] <- 0.3
d$chillLightCycle[which(d$chill.temp == "4.5-5.1")] <- 0

#amooaghaie09: average temp and add uncertainty
d$chillTemp[which(d$chill.temp == "4-6")] <- 5
d$chillTempUnc[which(d$chill.temp == "4-6")] <- 1

# boscagli01: fixing format and added chillTempUnc
d$chillTemp[which(d$chill.temp == "07-May")] <- 6
d$chillTempUnc[which(d$chill.temp == "07-May")] <- 1

# borghetti86: fixing format and added chillTempUnc
d$chillTemp[which(d$chill.temp == "03-Feb")] <- 2.5
d$chillTempUnc[which(d$chill.temp == "03-Feb")] <- 0.5

# meyer94 and meyer95
# 'preincubation' or 'incubation' (in other.treatment) should be warm. strat. (discussed 26 July 2025)
pattern <- "4 wk preincubatiion at 10-20C"
d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillTemp'] <- 
  paste0('15 then ', d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillTemp'])
d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillDuration'] <- 
  paste0('28 then ', d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillDuration'])
pattern <- '4 wks of incubation at 10-20C after 4 wks of chilling, then chilling resumes for remaining 8 wks'
d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillTemp'] <- 
  paste0(d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillTemp'], ' then 15 then ', 
         d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillTemp'])
d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillDuration'] <- '28 then 28 then 56'
pattern <- '4 wks of incubation at 10-20C after 8 wks of chilling, then chilling resumes for remaining 4 wks'
d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillTemp'] <- 
  paste0(d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillTemp'], ' then 15 then ', 
         d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillTemp'])
d[d$datasetID %in% c('meyer94', 'meyer95') & d$other.treatment %in% pattern, 'chillDuration'] <- '56 then 28 then 28'

# al-absi10
d[d$chillTemp %in% '05-Apr' & d$datasetID %in% 'al-absi10', 'chillTemp'] <- 4.5

# pritchard93
d[d$chillTemp %in% '6/16' & d$datasetID %in% 'pritchard93', 'chillTemp'] <- '6 then 16'
d[d$chillTemp %in% '6/16' & d$datasetID %in% 'pritchard93', 'chillDuration'] <- 
  unname(sapply(d[d$chillTemp %in% '6/16' & d$datasetID %in% 'pritchard93', 'chillDuration'],
              function(i) paste0(as.numeric(na.omit(as.numeric(unlist(stringr::str_split(i, pattern = '[()+ ]'))))), collapse = ' then ')))

#check
unique(d$chillTemp)
unique(d$chillDuration)
unique(d$chillTempUnc)
unique(d$chillTempCycle)
unique(d$chillLightCycle)

length(which(!is.na(d$chillTempUnc)))
length(which(!is.na(d$chillTempCycle)))
length(which(!is.na(d$chillLightCycle)))
