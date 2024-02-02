## Upded 30 January 2024 ##
## By Deirdre ##

## This contains cleaning of germination temperature ##
## Original code taken from file called cleaningDL.R ##

#1. germination tempearture
unique(d$germ.temp)

#d <- within(d, forcetemp[dasetID== 'falusi96' & study == 'exp3'] <- 24)
# ALERT: these changes should be in a NEW column, not overwritting the current column. 
d$germ.temp[which(d$germ.temp == "unknown")] <- "NA"
d$germ.temp[which(d$germ.temp == "didn't mention")] <- "NA"

#Check unusual values:
# open field---does this qualify for this study ie controlled environment?

# Values that are transformed ie averaged or rounded:
d$germ.tempCor <- d$germ.temp

# Now make new column with heavy duty cleaning
d$germTemp  <- d$germ.temp

# 2. germ.duration
unique(d$germ.duration)

#This variable is important---if NA or unknown or negative please double check the paper


