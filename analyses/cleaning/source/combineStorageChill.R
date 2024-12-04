## Started 20 August 2024
## By Ken

## Combine current storage and chill columns

library(stringr)

length(which(d$storage.humidity != 0 & !is.na(d$storage.humidity))) #3364
length(which(is.na(d$storage.humidity))) # 27571
length(which(d$storage.humidity == 0)) # 2

length(which(!is.na(d$storage.temp))) #18858
length(which(!is.na(d$storage.time) & !is.na(d$storage.temp))) #12430

d$dormancyTemp <- NA
d$dormancyDuration <- NA

d$storageTemp[is.na(d$storageTemp)] <- "NA"
d$storageDuration[is.na(d$storageDuration)] <- "NA"
d$chillTemp[is.na(d$chillTemp)] <- "NA"
d$chillDuration[is.na(d$chillDuration)] <- "NA"

idx <- which(d$datasetID == "momonoki79")
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "species",
                                        "storageTemp", "storageDuration",
                                        "chillTemp", "chillDuration",
                                        "dormancyTemp", "dormancyDuration",
                                        "storageType", "storageDetails",
                                        "respvar", "response", "figure"))
idx <- which(d$storageDuration == unique(d$storageDuration)[30])
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "species",
                                        "storageTemp", "storageDuration",
                                        "chillTemp", "chillDuration",
                                        "dormancyTemp", "dormancyDuration",
                                        "storageType", "storageDetails",
                                        "respvar", "response", "figure"))

for(i in 1:nrow(d)){
  #creating list to be appended with suitable conditions
  dormancyTemp <- list()
  dormancyDuration <- list()
  dormancyCond <- 0
  
  #checking storage conditions
  storTemp <- strsplit(d$storageTemp[i], " then ")
  storDur <- strsplit(d$storageDuration[i], " then ")
  storCond <- str_count(d$storageTemp[i], " then ") + 1
  
  for(n in 1:storCond){
    if(!(d$storageType[i] %in% c("moist", "moist/cold")) |
       is.na(as.numeric(storTemp[[1]][n])) |
       is.na(as.numeric(storDur[[1]][n]))) {next}
    if(as.numeric(storTemp[[1]][n]) > -20 &
       as.numeric(storTemp[[1]][n]) < 10){
      dormancyCond <- dormancyCond + 1
      dormancyTemp[[dormancyCond]] <- storTemp[[1]][n]
      dormancyDuration[[dormancyCond]] <- storDur[[1]][n]
    }
  }
  
  #checking chilling conditions
  chillTemp <- strsplit(d$chillTemp[i], " then ")
  chillDur <- strsplit(d$chillDuration[i], " then ")
  chillCond <- str_count(d$chillTemp[i], " then ") + 1
  
  for(n in 1:chillCond){
    if(is.na(as.numeric(chillTemp[[1]][n])) |
       is.na(as.numeric(chillDur[[1]][n]))) {next}
    if(as.numeric(chillTemp[[1]][n]) > -20 &
       as.numeric(chillTemp[[1]][n]) < 10){
      dormancyCond <- dormancyCond + 1
      dormancyTemp[[dormancyCond]] <- chillTemp[[1]][n]
      dormancyDuration[[dormancyCond]] <- chillDur[[1]][n]
    }
  }
  
  #combining conditions
  if(dormancyCond > 0){
    d$dormancyTemp[i] <- paste(dormancyTemp, collapse = ", ")
    d$dormancyDuration[i] <- sum(as.numeric(unlist(dormancyDuration)))
  } else{
    d$dormancyDuration[i] <- 0
  }
}
print("These `NAs introduced by coercion' errors happen because we shift between string and numeric types when checking whether each temperature condition is within the acceptable range of -20degC to 10degC")
