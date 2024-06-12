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
idx <- which(d$chill.temp == unique(d$chill.temp)[70])
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
d$chill.cycle <- NA

#mamut20 - alternating 5/2 and alternating 25/15
#check germ.temp if it should be chill.temp

#wang09 - alternating 15/5 light dark
d$chill.tempCor[which(d$chill.temp == "alternating 15/5")] <- "15/5"
d$chill.cycle[which(d$chill.temp == "alternating 15/5")] <- "12,12"

#watanabe02 - 25 -> 5 +/- 2 warm to cold strat
d$chill.tempCor[which(d$chill.temp == "25 -> 5 +/- 2")] <- "25,5"
d$chill.tempCor[which(d$chill.temp == "5 +/- 2")] <- 5
d$chill.tempUnc[which(d$datasetID == "Watanabe02") & which(!is.na(d$chill.temp))] <- 2
d$chill.durationCor[which(d$chill.temp == "25 -> 5 +/- 2")] <- "30,30"
d$chill.tempCor[which(d$datasetID == "zhou08" & d$treatment == "stratification" & is.na(d$chill.temp))] <- 84
#photoperiod set at 12h but indicated in d are whether in dark or light

#zhou03 - 4, 22, and other combinations

#zhou08 - 20,10 & 5,15 & 10,20 &, 15,25 & 20,30
d$chill.tempCor[which(d$chill.temp == "20,10")] <- "25,5"
d$chill.tempCor[which(d$datasetID == "zhou08" & d$treatment == "stratification" & is.na(d$chill.temp))] <- 5
d$chill.durationCor[which(d$chill.temp == "20,10")] <- "28,56"
d$chill.durationCor[which(d$datasetID == "zhou08" & d$treatment == "stratification" & is.na(d$chill.temp))] <- 84
#added chill temps for some data points

#yang08 - 30/20 warm, cold strat, light dark
#should I change the treatment and photoperiod, since they don't seem to correspond with the results

#zulfiqar15 - 4 +/- 2 doi currently down
d$chill.tempCor[which(d$chill.temp == "4 +/- 2")] <- 4
d$chill.tempUnc[which(d$chill.temp == "4 +/- 2")] <- 2

#fulbright86 - 30/7 moist heat, moist prechill
d$chill.tempCor[which(d$chill.temp == "30/7")] <- "30,7"
d$chill.tempCor[which(d$chill.temp == "30/7")] <- NA
d$chill.durationCor[which(d$chill.temp == "30/7")] <- "3,14"

#gianni19 - 2+-0.1
d$chill.tempCor[which(d$chill.temp == "2+-0.1")] <- 2 #dark
d$chill.tempUnc[which(d$chill.temp == "2+-0.1")] <- 0.1 #dark

#rezvani14 - 5+-1
d$chill.tempCor[which(d$chill.temp == "5+-1")] <- 5
d$chill.tempUnc[which(d$chill.temp == "5+-1")] <- 1

#kolodziejek18 - 4.5-5.1 paper pending
d$chill.tempCor[which(d$chill.temp == "4.5-5.1")] <- 4.8
d$chill.tempUnc[which(d$chill.temp == "4.5-5.1")] <- 0.3

#Amooaghaie09 - 4-6
d$chill.tempCor[which(d$chill.temp == "4-6")] <- 5
d$chill.tempUnc[which(d$chill.temp == "4-6")] <- 1

