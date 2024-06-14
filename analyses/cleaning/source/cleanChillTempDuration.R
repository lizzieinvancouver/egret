## Upded 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#to clean
unique(d$chill.temp)
unique(d$chill.duration)

#Check unusual values:
# what does "90/30/90" mean
#"Continuous cold stratification" kept in cold whole experiment? How long was experiment?
# is 0 a true zero? 

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
idx <- which(d$datasetID == check$datasetID[1])
unique(check$chill.temp)
idx <- which(d$chill.temp == unique(d$chill.temp)[43])
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "entered.by",
                                        "treatment", "chill.temp", "chill.duration",
                                        "germ.temp", "other.treatment", "photoperiod",
                                        "respvar", "response"))


#d <- within(d, forcetemp[dasetID== 'falusi96' & study == 'exp3'] <- 24)
d$chill.duration[which(d$chill.duration == "unknown")] <- "NA"

#no values like "n/a" and "N/A" so far though
d$chill.temp[which(d$chill.temp == "n/a")] <- "NA"
d$chill.temp[which(d$chill.temp == "N/A")] <- "NA"

d$chill.tempCor <- d$chill.temp
d$chill.durationCor <- d$chill.duration
d$chill.tempUnc <- NA
d$chill.tempCycle <- NA
d$chill.lightCycle <- NA


#grose57 - 3.3-5 paper pending

#pipinis09 - 20/25
d$chill.tempCor[which(d$chill.temp == "20/25")] <- "20 and 25"
d$chill.tempCycle[which(d$chill.temp == "20/25")] <- "16 and 8"
d$chill.lightCycle[which(d$chill.temp == "20/25")] <- 8

d$chill.tempCor[which(d$datasetID == "pipinis09" & d$chill.temp == "2-4")] <- 3
d$chill.tempUnc[which(d$datasetID == "pipinis09" & d$chill.temp == "2-4")] <- 1
d$chill.lightCycle[which(d$datasetID == "pipinis09" & d$chill.temp == "2-4")] <- 0

d$chill.tempCor[which(d$chill.temp == "20/25 (warm); 4-6 (cold)")] <- "20 and 25 then 3"
d$chill.tempUnc[which(d$chill.temp == "20/25 (warm); 4-6 (cold)")] <- "NA and 1"
d$chill.tempCycle[which(d$chill.temp == "20/25 (warm); 4-6 (cold)")] <- "16 and 8 then NA"
d$chill.lightCycle[which(d$chill.temp == "20/25 (warm); 4-6 (cold)")] <- "8 then 0"

temp <- c("30 then 30", "30 then 60", "60 then 30", "60 then 60")
d$chill.durationCor[which(d$chill.temp == "20/25 (warm); 4-6 (cold)")] <- temp
#changed chill temp for 20/25

#pliszko18 - -18, 4, 25
d$chill.lightCycle[which(d$datasetID == "pliszko18")] <- 0

#pritchard93 - 6/16
#will get back to it later

#tylkowski91 - 3, 3-15, 3-20
#exp1
temp <- c("20 and 30", "20 and 30", "20 and 30", "20 and 30", "15 and 25", "15 and 25",
          "15 and 25", "15 and 25", "10 and 20", "10 and 20", "10 and 20", "10 and 20", 20, 20, 20, 20)
d$chill.tempCor[which(d$datasetID == "tylkowski91" & d$study == "exp1" & !d$treatment == "thermal stratification (cold phase)")] <- paste0(temp, " then 3")
d$chill.tempCor[which(d$datasetID == "tylkowski91" & d$study == "exp1" & d$treatment == "thermal stratification (cold phase)")] <- temp
temp <- c(126, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168)
d$chill.durationCor[which(d$datasetID == "tylkowski91" & d$study == "exp1" & !d$treatment == "thermal stratification (cold phase)")] <- paste0(temp, " then ", c(126, 168))
#d$chill.durationCor[which(d$datasetID == "tylkowski91" & d$study == "exp1" & d$treatment == "thermal stratification (cold phase)")] <- temp
#exp2
temp <- c("15 and 25", "15 and 25", "15 and 25", "20 and 30", "20 and 30", "20 and 30",
          "15 and 25", "15 and 25", "15 and 25", "20 and 30", "20 and 30", "20 and 30")
d$chill.tempCor[which(d$datasetID == "tylkowski91" & d$study == "exp2")] <- paste0(temp, " then 3")
temp <- c(126, 119, 105, 126, 119, 105, 112, 105, 105, 112, 105, 105)
d$chill.durationCor[which(d$datasetID == "tylkowski91" & d$study == "exp2")] <- paste0(c(98, 112, 126), " then ", temp)
#exp3
temp <- c("15 and 25", "20 and 30", "15 and 25", "20 and 30")
d$chill.tempCor[which(d$datasetID == "tylkowski91" & d$study == "exp3")] <- paste0(temp, " then 3")
d$chill.durationCor[which(d$datasetID == "tylkowski91" & d$study == "exp3")] <- "126 then 126"
#exp 1, 2, 3
d$chill.tempCor[which(d$datasetID == "tylkowski91" & d$study == "exp 1, 2, 3")] <- "20 and 30 then 3"
d$chill.durationCor[which(d$datasetID == "tylkowski91" & d$study == "exp 1, 2, 3")] <- "126 then 126"

#tempCycle
d$chill.tempCycle[which(d$datasetID == "tylkowski91" & !d$treatment == "thermal stratification (cold phase)")] <- "24 and 24 then NA"
d$chill.tempCycle[which(d$datasetID == "tylkowski91" & d$treatment == "thermal stratification (cold phase)")] <- "24 and 24"
#chill temp indicated in treatment
#data seems to be for germination temp instead

#wytsalucy21 - 4, 7, 10 paper pending
d$chill.tempCor[which(d$chill.temp == "4, 7, 10")] <- "4 then 7 then 10"

#yusefi-tanha19 - 3, 6, 9, 12, 15
d$chill.lightCycle[which(d$datasetID == "yusefi-tanha19")] <- 0

#edwards73 - 1 - 4 both papers pending
d$chill.tempCor[which(d$chill.temp == "1 - 4")] <- 2.5
d$chill.tempUnc[which(d$chill.temp == "1 - 4")] <- 1.5

#madeiras07 - 5 +/- 2 dark
d$chill.tempCor[which(d$datasetID == "Madeiras07" & d$chill.temp == "5 +/- 2")] <- 5
d$chill.tempUnc[which(d$datasetID == "Madeiras07" & d$chill.temp == "5 +/- 2")] <- 2
d$chill.lightCycle[which(d$datasetID == "Madeiras07" & d$chill.temp == "5 +/- 2")] <- 0

#esmaeili09 - 15 - 25
d$chill.tempCor[which(d$chill.temp == "15 - 25")] <- 20
d$chill.tempUnc[which(d$chill.temp == "15 - 25")] <- 5

#markovic19 - 3 - 5 two papers with datasetID
d$chill.tempCor[which(d$chill.temp == "3 - 5")] <- 4
d$chill.tempUnc[which(d$chill.temp == "3 - 5")] <- 1

#mamut20 - alternating 5/2 and alternating 25/15
d$chill.tempCor[which(d$chill.temp == "alternating 5/2")] <- "5 and 2"
d$chill.tempCor[which(d$chill.temp == "alternating 25/15")] <- "25 and 15"
d$chill.lightCycle[which(d$other.treatment == "cold strat during germ" & !d$photoperiod == 0)] <- 24
d$chill.lightCycle[which(d$other.treatment == "cold strat during germ" & d$photoperiod == 0)] <- 0
d$chill.lightCycle[22079:(22079+21)] <- 24
d$chill.lightCycle[22101:(22101+21)] <- 0
#germination set at same time of stratification
#what to do

#sharma03 - 24 paper pending

#wang09 - alternating 15/5 dark
d$chill.tempCor[which(d$chill.temp == "alternating 15/5")] <- "15 and 5"
d$chill.lightCycle[which(d$datasetID == "Wang09")] <- 0

#watanabe02 - 25 -> 5 +/- 2 warm to cold strat
d$chill.tempCor[which(d$chill.temp == "25 -> 5 +/- 2")] <- "25 then 5"
d$chill.lightCycle[which(d$chill.temp == "25 -> 5 +/- 2")] <- "0 then NA"
d$chill.tempCor[which(d$datasetID == "Watanabe02" & d$chill.temp == "5 +/- 2")] <- 5
d$chill.tempUnc[which(d$datasetID == "Watanabe02" & !is.na(d$chill.temp))] <- 2
d$chill.durationCor[which(d$chill.temp == "25 -> 5 +/- 2")] <- "30 then 30"
#photoperiod set at 12h but indicated in d are whether in dark or light

#zhou03 - 4, 22, and other combinations
d$chill.tempCor[which(d$chill.temp == "4,22")] <- "4 and 22"
d$chill.tempCor[which(d$chill.temp == "4,22,4,22")] <- "4 and 22"
d$chill.tempCor[which(d$chill.temp == "4,22,4,22,4,22,4,22")] <- "4 and 22"
d$chill.tempCor[which(d$chill.temp == "4,22,4,22,4,22,4,22,4,22,4,22,4,22,4,22")] <- "4 and 22"
d$chill.tempCycle[which(d$chill.temp == "4,22")] <- "1344 and 1344"
d$chill.tempCycle[which(d$chill.temp == "4,22,4,22")] <- "672 and 672"
d$chill.tempCycle[which(d$chill.temp == "4,22,4,22,4,22,4,22")] <- "336 and 336"
d$chill.tempCycle[which(d$chill.temp == "4,22,4,22,4,22,4,22,4,22,4,22,4,22,4,22")] <- "168 and 168"
d$chill.durationCor[which(d$datasetID == "zhou03")] <- 112

#zhou08 - 20,10 & 5,15 & 10,20 &, 15,25 & 20,30
d$chill.tempCor[which(d$datasetID == "zhou08" & d$treatment == "stratification")] <- 5
d$chill.durationCor[which(d$datasetID == "zhou08" & d$treatment == "stratification" & is.na(d$chill.temp))] <- 84
d$chill.tempCor[which(d$chill.temp == "20,10")] <- "25 then 5"
d$chill.lightCycle[which(d$datasetID == "zhou08" & d$treatment == "stratification")] <- c(24, 0, "24 then 24")
d$chill.durationCor[which(d$chill.temp == "20,10")] <- "28 then 56"
d$chill.tempCor[which(d$datasetID == "zhou08" & d$treatment == "temperature")] <- "25 then 5"
d$chill.lightCycle[which(d$datasetID == "zhou08" & d$treatment == "temperature")] <- "24 then 24"
d$chill.durationCor[which(d$datasetID == "zhou08" & d$treatment == "temperature")] <- "28 then 56"
d$chill.tempCor[which(d$datasetID == "zhou08" & d$treatment == "water stress")] <- "25 then 5"
d$chill.lightCycle[which(d$datasetID == "zhou08" & d$treatment == "water stress")] <- "24 then 24"
d$chill.durationCor[which(d$datasetID == "zhou08" & d$treatment == "water stress")] <- "28 then 56"
#data seems to be for germination temp instead
#warm + cold + incubation (but no duration)
#should incubation be included

#yang08 - 30/20 warm, cold strat
d$chill.tempCor[which(d$datasetID == "yang08" & d$chill.temp == "30/20")] <- "30 and 20 then 4"
d$chill.tempCycle[which(d$datasetID == "yang08" & d$chill.temp == "30/20")] <- "12 and 12 then NA"
d$chill.lightCycle[which(d$datasetID == "yang08" & d$chill.temp == "30/20")] <- "8 then NA"
temp <- c("60 then 60", "60 then 90", "60 then 120", "60 then 150", "60 then 180",
         "90 then 60", "90 then 90", "90 then 120", "90 then 150", "90 then 180",
         "120 then 60", "120 then 90", "120 then 120", "120 then 150", "120 then 180",
         "150 then 60", "150 then 90", "150 then 120", "150 then 150", "150 then 180",
         "180 then 60", "180 then 90", "180 then 120", "180 then 150", "180 then 180",
         "60 then 60", "60 then 90", "60 then 120", "60 then 150", "60 then 180",
         "90 then 60", "90 then 90", "90 then 120", "90 then 150", "90 then 180",
         "120 then 60", "120 then 90", "120 then 120", "120 then 150", "120 then 180",
         "150 then 60", "150 then 90", "150 then 120", "150 then 150", "150 then 180",
         "180 then 60", "180 then 90", "180 then 120", "180 then 150", "180 then 180")
d$chill.durationCor[which(d$datasetID == "yang08" & d$chill.temp == "30/20")] <- temp
#chilling seems to be warm + cold
#should I change the treatment and photoperiod, since they don't seem to correspond with the results

#zulfiqar15 - 4 +/- 2 doi currently down
d$chill.tempCor[which(d$chill.temp == "4 +/- 2 ")] <- 4
d$chill.tempUnc[which(d$chill.temp == "4 +/- 2 ")] <- 2

#battaglia97 paper pending
#missing chill duration

#fulbright86 - 30/7 moist heat, moist prechill
d$chill.tempCor[which(d$chill.temp == "30/7")] <- "30 then 7"
d$chill.durationCor[which(d$chill.temp == "30/7")] <- "3 then 14"

#gianni19 - 2+-0.1 dark
d$chill.tempCor[which(d$chill.temp == "2+-0.1")] <- 2
d$chill.tempUnc[which(d$chill.temp == "2+-0.1")] <- 0.1
d$chill.lightCycle[which(d$chill.temp == "2+-0.1")] <- "0 and 24"

#rezvani14 - 5+-1
d$chill.tempCor[which(d$chill.temp == "5+-1")] <- 5
d$chill.tempUnc[which(d$chill.temp == "5+-1")] <- 1

#kolodziejek18 - 4.5-5.1 paper pending
d$chill.tempCor[which(d$chill.temp == "4.5-5.1")] <- 4.8
d$chill.tempUnc[which(d$chill.temp == "4.5-5.1")] <- 0.3

#Amooaghaie09 - 4-6
d$chill.tempCor[which(d$chill.temp == "4-6")] <- 5
d$chill.tempUnc[which(d$chill.temp == "4-6")] <- 1

#check
unique(d$chill.tempCor)
unique(d$chill.durationCor)
unique(d$chill.tempUnc)
unique(d$chill.tempCycle)
unique(d$chill.lightCycle)

length(which(!is.na(d$chill.tempUnc)))
length(which(!is.na(d$chill.tempCycle)))
length(which(!is.na(d$chill.lightCycle)))
