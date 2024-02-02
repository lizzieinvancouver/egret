## Upded 30 January 2024 ##
## By Deirdre ##

## This contains code to clean chill duration and temperature ##
## Original code taken from file called cleaningDL.R ##

#1. chill.duration
unique(d$chill.duration)

#d <- within(d, forcetemp[dasetID== 'falusi96' & study == 'exp3'] <- 24)
d$chill.duration[which(d$chill.duration == "unknown")] <- "NA"

#Check unusual values:
# what does "90/30/90" mean
#"Continuous cold stratification" kept in cold whole experiment? How long was experiment?
# is 0 a true zero? 

# Values that are transformed ie averaged or rounded:
d$chill.durationCor <- d$chill.duration


# 2. chill.temp
unique(d$chill.duration)

#d <- within(d, forcetemp[dasetID== 'falusi96' & study == 'exp3'] <- 24)
d$chill.temp[which(d$chill.temp == "n/a")] <- "NA"
d$chill.temp[which(d$chill.temp == "N/A")] <- "NA"

#Check unusual values:
# what does "90/30/90" mean
#"Continuous cold stratification" kept in cold whole experiment? How long was experiment?
# is 0 a true zero? 

# Values that are transformed ie averaged or rounded:
d$chill.tempCor <- d$chill.temp



