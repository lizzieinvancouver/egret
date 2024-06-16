## Upded 11 June 2024 ##
## By Deirdre ##

## This contains code to clean the response variable names and values ##

unique(d$responseVar) # 166 unique resp vars

# germination rate
d$responseVar <- d$respvar
d$responseVar[which(d$responseVar == "germ.speed")] <- "germ.rate"
d$responseVar[which(d$responseVar == "rates.germ")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germ.rate (days)")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germ.rate.total")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germ.rt")] <- "germ.rate"
d$responseVar[which(d$responseVar == "germination rate")] <- "germ.rate"
d$responseVar[which(d$responseVar == "rate of seed germ")] <- "germ.rate"
d$responseVar[which(d$responseVar == "speed.emerge")] <- "germ.rate"

# Proporation germination
d$responseVar[which(d$responseVar == "germ.proportion")] <- "prop.germ"
d$responseVar[which(d$responseVar == "proportion of viable seeds germinated")] <- "prop.germ"

#mean germination time
d$responseVar[which(d$responseVar == "mtg")] <- "mgt"
d$responseVar[which(d$responseVar == "mean.germ.time")] <- "mgt"
d$responseVar[which(d$responseVar == "MGT")] <- "mgt"
d$responseVar[which(d$responseVar == "MTG")] <- "mgt"
d$responseVar[which(d$responseVar == "M.T.G")] <- "mgt"
d$responseVar[which(d$responseVar == "mean time (day)")] <- "mgt"
d$responseVar[which(d$responseVar == "meandaystogerm")] <- "mgt"
d$responseVar[which(d$responseVar == "mean germ.time")] <- "mgt"

d$responseVar[which(d$responseVar == "median.germ.time")] <- "median germ time"
d$responseVar[which(d$responseVar == "med.time.germ")] <- "median germ time"

d$responseVar[which(d$responseVar == "days of first observation of seedling emergence ")] <- "day first emergence"
d$responseVar[which(d$responseVar == "days taken until first seed germination")] <- "day first emergence"
d$responseVar[which(d$responseVar == "days to 1st germination")] <- "day first emergence"
d$responseVar[which(d$responseVar == "days to 1st seed germinated")] <- "day first emergence"

d$responseVar[which(d$responseVar == "days to 50% emergence")] <- "50% germ"
d$responseVar[which(d$responseVar == "D50")] <- "50% germ"
d$responseVar[which(d$responseVar == "days50germ")] <- "50% germ"
d$responseVar[which(d$responseVar == "D50")] <- "50% germ"
d$responseVar[which(d$responseVar == "T50")] <- "50% germ"
d$responseVar[which(d$responseVar == "time taken to 50% germ")] <- "50% germ"
d$responseVar[which(d$responseVar == "germ.time (50%)")] <- "50% germ"
d$responseVar[which(d$responseVar == "germ.rate ((l/t50)")] <- "50% germ"
d$responseVar[which(d$responseVar == "half time (days)")] <- "50% germ"

d$responseVar[which(d$responseVar == "embryo/seed ratio")] <- "embryo:seed.ratio"

d$responseVar[which(d$responseVar == "germ time (days)")] <- "germ time"
d$responseVar[which(d$responseVar == "germ.time")] <- "germ time"
d$responseVar[which(d$responseVar == "germ.time (mean)")] <- "germ time"
d$responseVar[which(d$responseVar == "mean germ.time")] <- "germ time"

d$responseVar[which(d$responseVar == "germ value (D)")] <- "germ time"
d$responseVar[which(d$responseVar == "germ value")] <- "germ time"
d$responseVar[which(d$responseVar == "germ.value")] <- "germ time"
d$responseVar[which(d$responseVar == "germ.val")] <- "germ time"
d$responseVar[which(d$responseVar == "germ rate")] <- "germ time"
d$responseVar[which(d$responseVar == "germ rate (days)")] <- "germ time"
d$responseVar[which(d$responseVar == "germ.days")] <- "germ time"
d$responseVar[which(d$responseVar == "average germination speed")] <- "germ time"
d$responseVar[which(d$responseVar == "days to germ.")] <- "germ time"

d$responseVar[which(d$responseVar == "mean daily germ")] <- "mean daily germ"
d$responseVar[which(d$responseVar == "mean.daily.germ")] <- "mean daily germ"

# percent germination
d$responseVar[which(d$responseVar == "final per.germ")] <- "percent germ"
d$responseVar[which(d$responseVar == "mean per germ")] <- "percent germ"
d$responseVar[which(d$responseVar == "mean per.germ")] <- "percent germ"
d$responseVar[which(d$responseVar == "per.emerge")] <- "percent germ"
d$responseVar[which(d$responseVar == "per.germ")] <- "percent germ"
d$responseVar[which(d$responseVar == "per.germ.cumulative")] <- "percent germ"
d$responseVar[which(d$responseVar == "per.germ.cumulative.cumulative")] <- "percent germ"

d$responseVar[which(d$responseVar == "meandailygerm")] <- "mean daily germ"

# TO CHECK
# is germ.rt germ rate?
# what is germ.prob
# D50 same as T50 and half time (days)
#days to germ.
# germ rate (days)---actually rate or germ days?
#germ value
d$responseValue <- (d$response)

View(sort(unique(d$responseValue)))
# what do the -ve values mean?
#battaglia93
#ren 15

# I assume NG means no germination, should this not be 0
d$response[which(d$response == "NG")] <- "0"
d$response[which(d$response == "NA*")] <- "NA"

#TO CHECK what is "NA*"
d$errorType <- d$error.type
sort(unique(d$errorType))
d$errorType[which(d$errorType == "mean+/-SE")] <- "SE"
d$errorType[which(d$errorType == "standard error")] <- "SE"
d$errorType[which(d$errorType == "mean standard error")] <- "SE"
d$errorType[which(d$errorType == " +/-SE")] <- "SE"

d$errorType[which(d$errorType == "mean+/-SD")] <- "SD"
d$errorType[which(d$errorType == "standard deviation")] <- "SD"

d$errorType[which(d$errorType == "95 % confidence interval")] <- "95% CI"
d$errorType[which(d$errorType == "95CI")] <- "95% CI"

d$errorType[which(d$errorType == "not.specified")] <- "not specified"
d$errorType[which(d$errorType == "unknown")] <- "not.specified"

# what is xz--- Cho18 Fig 5
temp <- subset(d, !is.na(resp.error))

noError <- c("not specified", "not.specified", "unknown")

temp <- temp[!temp$errorType %in% noError, ]
nrow(temp)/nrow(d)  # 35.9% of the data has some sort of entry for error

# How many studies have sample size---n.per.rep and reps
temp <- subset(d, !is.na(n.per.rep))
nrow(temp)/nrow(d) # 88% 

sort(unique(temp$n.per.rep))
#"100 + " "1196" "168" "598" "1"

temp <- subset(d, !is.na(reps))
nrow(temp)/nrow(d) # 86% 
sort(unique(temp$reps))

#"0, 0.5, 50, 60, 70, 100

temp <- d[,c("datasetID", "responseVar"#, "responseValue", "errorType"
             )]
temp <- unique(temp)
