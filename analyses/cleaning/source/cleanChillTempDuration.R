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
idx <- which(d$chill.temp == unique(d$chill.temp)[38])
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


#cousins10
#yang18
#yang10
#zadeh15
#king12

#naseri18 - 2-4
#cold strat temp guessed from other treatments
d$chill.tempCor[which(d$datasetID == "Naseri18")] <- 3
d$chill.tempUnc[which(d$datasetID == "Naseri18")] <- 1
d$chill.durationCor[which(d$datasetID == "Naseri18" & d$chill.duration == "Continuous cold stratification")] <- NA

#gimenez-benavides13 - 2-4
d$chill.tempCor[which(d$datasetID == "gimenez-benavides13" & !is.na(d$chill.temp))] <- 3
d$chill.tempUnc[which(d$datasetID == "gimenez-benavides13" & !is.na(d$chill.temp))] <- 1
d$chill.lightCycle[which(d$datasetID == "gimenez-benavides13" & !is.na(d$chill.temp))] <- 0

#skordilis95 - 2-4
d$chill.tempCor[which(d$datasetID == "skordilis95" & !is.na(d$chill.temp))] <- 3
d$chill.tempUnc[which(d$datasetID == "skordilis95" & !is.na(d$chill.temp))] <- 1
d$chill.lightCycle[which(d$datasetID == "skordilis95" & !is.na(d$chill.temp))] <- 0

#hatzilazarou21 - 2-4
d$chill.tempCor[which(d$datasetID == "hatzilazarou21")] <- 3
d$chill.tempUnc[which(d$datasetID == "hatzilazarou21")] <- 1

#nawrot-chorabik21 - 7
d$chill.lightCycle[which(d$datasetID == "Nawrot-chorabik21" & !is.na(d$chill.temp))]

#sacande04 - 5, 10, 15, 20, 25
#the chill temp data seem to be for germ temp

#yeom21 - 5, 10, 15, 20, 25
#chill temp data seem to be for germ temp
#notes on mgt and T50 for 25degC air temp for without pericarp may be mislabeled
d$chill.tempCor[which(d$datasetID == "yeom21" & d$chill.duration == 35)] <- "5 then 25"
d$chill.durationCor[which(d$datasetID == "yeom21" & d$chill.duration == 35)] <- "35 then 42"

d$chill.tempCor[which(d$datasetID == "yeom21" & d$treatment == "cold stratification" & d$photoperiod == "16/8")] <-
  5
d$chill.durationCor[which(d$datasetID == "yeom21" & d$treatment == "cold stratification" & d$photoperiod == "16/8")] <-
  56
temp <- c(rep(5, 5), rep(25, 5))
d$chill.tempCor[which(d$datasetID == "yeom21" & d$respvar != "per.germ")] < temp

#song20 - 5, 10, 15, 20
temp <- c(rep(5, 5), rep(10, 4), rep(15, 3), rep(20, 2))
d$chill.tempCor[which(d$other.treatment == "cold stratification for 6 months")] <- paste0("NA then ", temp)
d$chill.durationCor[which(d$other.treatment == "cold stratification for 6 months")] <-
  paste0("186 then ", d$chill.durationCor[which(d$other.treatment == "cold stratification for 6 months")])

#schutz02 - 22/10
#recommend to double check response variable and values and germ temp
d$chill.tempCor[which(d$datasetID == "Schutz02")] <- 1.5
d$chill.tempUnc[which(d$datasetID == "Schutz02")] <- 1.5
temp <- c(0, 0, 0, 0, 30, 30, 30, 30, 60, 60, 60, 60, 91, 91, 91, 91, 183, 183, 183, 183)
d$chill.durationCor[which(d$datasetID == "Schutz02" & d$study == "exp1")] <- temp
temp <- c(0, 0, 0, 0, 91, 91, 91, 91, 183, 183, 183, 183)
d$chill.durationCor[which(d$datasetID == "Schutz02" & d$study == "exp3")] <- temp

#omar21 - 3 and -4 
d$chill.tempUnc[which(d$datasetID == "omar21" & !is.na(d$chill.temp))] <- 1

#grose57 - 3.3-5 paper pending
#d[which(d$datasetID == "grose57" & d$species == "regnans" & d$response == 61), ]
d$chill.tempCor[which(d$chill.temp == 4.44 | d$chill.temp == "3.3-5")] <- 4.4

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
#recommend to double check year and response variable values manually
d$chill.durationCor[which(d$datasetID == "pritchard93" & d$treatment == "Cold stratification" & is.na(d$error.type))] <-
  as.numeric(d$chill.duration[which(d$datasetID == "pritchard93" & d$treatment == "Cold stratification" & is.na(d$error.type))]) * 7

d$chill.tempCor[grep("6°C", d$chill.duration)] <- "6 then 16"
temp <- c(52, "0 then 52", "0 then 52", "0 then 52", "0 then 52", "0 then 52", "0 then 52",
          "0 then 52", "0 then 52", "0 then 52", "0 then 52", "0 then 52", 52)
d$chill.durationCor[grep("6°C", d$chill.duration)] <- temp
d$chill.tempCor[which(d$datasetID == "pritchard93" & d$chill.durationCor == 52)] <- c(16, 6)

d$chill.tempCor[grep("5°C", d$chill.duration)] <- "25 then 5"
temp <- c(84, "14 then 84", "21 then 84", "28 then 84", "42 then 84", "63 then 84")
d$chill.durationCor[grep("5°C", d$chill.duration)] <- temp
d$chill.tempCor[which(d$datasetID == "pritchard93" & d$chill.durationCor == 84)] <- 5

d$chill.lightCycle[which(d$datasetID == "pritchard93" & !is.na(d$chill.temp))] <- 0

#tylkowski91 - 3, 3-15, 3-20
#exp1
temp <- c("20 and 30", "20 and 30", "20 and 30", "20 and 30", "15 and 25", "15 and 25",
          "15 and 25", "15 and 25", "10 and 20", "10 and 20", "10 and 20", "10 and 20", 20, 20, 20, 20,
          "20 and 30", "20 and 30", "20 and 30", "20 and 30", "15 and 25",
          "15 and 25", "15 and 25", "10 and 20", "10 and 20", "10 and 20", "10 and 20", 20, 20, 20, 20,
          "20 and 30", "20 and 30", "20 and 30", "20 and 30", "15 and 25", "15 and 25",
          "15 and 25", "15 and 25", "10 and 20", "10 and 20", "10 and 20", "10 and 20", 20, 20, 20, 20)
d$chill.tempCor[which(d$datasetID == "tylkowski91" & d$study == "exp1" & !d$treatment == "thermal stratification (cold phase)")] <- paste0(temp, " then 3")
temp <- c("20 and 30", "20 and 30", "20 and 30", "20 and 30", "15 and 25", "15 and 25",
          "15 and 25", "15 and 25", "10 and 20", "10 and 20", "10 and 20", "10 and 20")
d$chill.tempCor[which(d$datasetID == "tylkowski91" & d$study == "exp1" & d$treatment == "thermal stratification (cold phase)")] <- temp
temp <- c(126, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168,
          126, 126, 168, 168, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168, 
          126, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168, 126, 126, 168, 168)
temp2 <- c(126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168,
           126, 168, 126, 168, 126, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168,
           126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168, 126, 168)
d$chill.durationCor[which(d$datasetID == "tylkowski91" & d$study == "exp1" & !d$treatment == "thermal stratification (cold phase)")] <- paste0(temp, " then ", temp2)
d$chill.durationCor[which(d$datasetID == "tylkowski91" & d$study == "exp1" & d$treatment == "thermal stratification (cold phase)")] <- c(126, 126, 168, 168)

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

#wytsalucy21 - 4, 7, 10
temp <- c(4, 7, 10)
for(i in 1:length(temp)){
  d$chill.tempCor[which(d$datasetID == "wytsalucy21" & d$chill.duration == 56 & d$chill.temp == temp[i])] <- paste0(temp[i], " then 20 then ", temp[i])
}
d$chill.durationCor[which(d$datasetID == "wytsalucy21" & d$chill.duration == 56 & d$chill.temp != 20)] <- "28 then 14 then 28"
d$chill.durationCor[which(d$datasetID == "wytsalucy21" & d$chill.duration == 56 & d$chill.temp == 20)] <- 70
#some data from exp4 is for exp3
#per germ values not differentiated by both hormone and temperature because their interaction is not deemed significant

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

#kolodziejek18 - 4.5-5.1 dark
d$chill.tempCor[which(d$chill.temp == "4.5-5.1")] <- 4.8
d$chill.tempUnc[which(d$chill.temp == "4.5-5.1")] <- 0.3
d$chill.lightCycle[which(d$chill.temp == "4.5-5.1")] <- 0

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
