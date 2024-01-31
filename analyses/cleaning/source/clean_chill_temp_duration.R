## Updated 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#1. chill.duration
unique(dat$chill.duration)

#dat <- within(dat, forcetemp[datasetID== 'falusi96' & study == 'exp3'] <- 24)
dat$chill.duration[which(dat$chill.duration == "unknown")] <- "NA"

#Check unusual values:
# what does "90/30/90" mean
#"Continuous cold stratification" kept in cold whole experiment? How long was experiment?
# is 0 a true zero? 

# Values that are transformed ie averaged or rounded:
dat$chill.durationCor <- dat$chill.duration


# 2. chill.temp
unique(dat$chill.duration)

#dat <- within(dat, forcetemp[datasetID== 'falusi96' & study == 'exp3'] <- 24)
dat$chill.temp[which(dat$chill.temp == "n/a")] <- "NA"
dat$chill.temp[which(dat$chill.temp == "N/A")] <- "NA"

#Check unusual values:
# what does "90/30/90" mean
#"Continuous cold stratification" kept in cold whole experiment? How long was experiment?
# is 0 a true zero? 

# Values that are transformed ie averaged or rounded:
dat$chill.tempCor <- dat$chill.temp



