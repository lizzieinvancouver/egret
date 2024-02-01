## Updated 30 January 2024 ##
## By Deirdre ##

## This contains cleaning of germination temperature ##
## Original code taken from file called cleaningDL.R ##

#1. germination tempearture
unique(dat$germ.temp)

#dat <- within(dat, forcetemp[datasetID== 'falusi96' & study == 'exp3'] <- 24)
# ALERT: these changes should be in a NEW column, not overwritting the current column. 
dat$germ.temp[which(dat$germ.temp == "unknown")] <- "NA"
dat$germ.temp[which(dat$germ.temp == "didn't mention")] <- "NA"

#Check unusual values:
# open field---does this qualify for this study ie controlled environment?

# Values that are transformed ie averaged or rounded:
dat$germ.tempCor <- dat$germ.temp

# Now make new column with heavy duty cleaning
dat$germTemp  <- dat$germ.temp

# 2. germ.duration
unique(dat$germ.duration)

#This variable is important---if NA or unknown or negative please double check the paper


