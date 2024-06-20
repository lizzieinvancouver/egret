## Upded 11 June 2024 ##
## By Deirdre ##

## This contains code to clean the response variable names and values ##

unique(d$responseVar) # 166 unique resp vars

# germination rate
d$responseVar <- d$respvar
d$responseVar[which(d$responseVar == "germ.speed")] <- "germ rate"
d$responseVar[which(d$responseVar == "rates.germ")] <- "germ rate"
d$responseVar[which(d$responseVar == "germ.rate (days)")] <- "germ rate"
d$responseVar[which(d$responseVar == "germ.rate.total")] <- "germ rate"
d$responseVar[which(d$responseVar == "germ.rt")] <- "germ rate"
d$responseVar[which(d$responseVar == "germination rate")] <- "germ rate"
d$responseVar[which(d$responseVar == "rate of seed germ")] <- "germ rate"
d$responseVar[which(d$responseVar == "speed.emerge")] <- "germ rate"
d$responseVar[which(d$responseVar == "germ.rate")] <- "germ rate"


# Proporation germination
d$responseVar[which(d$responseVar == "germ.proportion")] <- "prop germ"
d$responseVar[which(d$responseVar == "proportion of viable seeds germinated")] <- "prop germ"

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

d$responseVar[which(d$responseVar == "embryo/seed ratio")] <- "embryo:seed ratio"
d$responseVar[which(d$responseVar == "embryo:seed.ratio")] <- "embryo:seed ratio"


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
# Checking odd entries
d$responseVar[which(d$datasetID == "liu13" & d$responseVar == "a")] <- "source of variation mean square" # maximum cumulative germination percentage equivalent to germination capacity
# b, a mathematical parameter controlling the shape and steepness of the curve
# c the time required to achieve 50% germination = R50
# TMGR, the time of maximum germination rate; 
# lag, the time of germination onset; 
# Dlag-50, the duration between lag and c; 
# DI, dormancy index, the area between the germination curves of control and any pre-treatment.
# data of % germ from table 1 missing

#	zhang21 not in englidh
# Boscagli01 coefficient rate of germination: (sum(days since sowing*no. of seeds germinated on day since sowing)/sum(no. of seeds germinated on day since sowing))*100
# germination progress curve AGUPC

#ahola99 --- germ.prob germ.rate.index, germ.speed(%/day); germination.energy= germ.energy; GSI Werner13; imbi.period Fetouh14; IVG santos19; lag LAG liu13 Naseri18 Nin17; peak.value = edwards96 tilki97; proportion of viable seeds = prop.germ harrison14; NA  = liu13, edwards96, tan10_2; castro95; huang14; downie91;strazisar13

d$responseVar[which(d$datasetID == "zlesak07" & d$responseVar == "AGUPC")] <- "germination progress curve" 
d$responseVar[which(d$datasetID == "Conversa09" & d$responseVar == "cumulative.germ")] <- "percent germ" 
d$responseVar[which(d$datasetID == "ranil15" & d$responseVar == "early per.germ")] <- "early per germ day 4" 

d$responseVar[which(d$datasetID == "grose57" & d$responseVar == "G.E.I")] <- "germinitive energy" 

d$responseVar[which(d$datasetID == "ranil15" & d$responseVar == "early per.germ")] <- "early per germ day 4" 
d$responseVar[which(d$datasetID == "ranil15" & d$responseVar == "early per.germ")] <- "early per germ day 4" 
d$responseVar[which(d$datasetID == "ranil15" & d$responseVar == "early per.germ")] <- "early per germ day 4" 

# cleaning values
d$responseValue <- (d$response)
d$responseValueNum <- round(as.numeric(d$responseValue),2)
View(sort(unique(d$responseValueNum)))

temp <- subset(d, responseVar == "percent germ")
# yan18 -ve value Fig 6; tabatabaeian18, Acosta12, Washitani85, battaglia93 > 100
#"1/5" 
temp <- subset(d, responseVar == "prop germ") # values look reasonable
temp <- subset(d, responseVar == "germ rate") 
#raisi13 - entered with backslashes, table 6
temp <- subset(d, responseVar == "mgt") 
# brenchley98 >100
temp <- subset(d, responseVar == "50% germ") 
#Necajeva13 pritchard93 bytnerowicz14 pritchard93 50% germ 332?
temp <- subset(d, responseVar == "germ time") # values look reasonable
#wytsalucy21 table 2 no resp data winstead 71 table 1

# what do the -ve values mean?
#battaglia93
#ren 15

# I assume NG means no germination, should this not be 0
d$response[which(d$response == "NG")] <- "0"
d$response[which(d$response == "NA*")] <- "NA"

#TO CHECK what is "NA*"
d$errorType <- d$error.type
sort(unique(d$errorType))

# wat is SEM, max standard
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

temp <- subset(d, errorType == "SEM")  #"bungard97"
temp <- subset(d, errorType == "max standard") #"Washitani89"


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


