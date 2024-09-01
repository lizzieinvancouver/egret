## Started 20 August 2024
## By Ken

## Combine current storage and chill columns

length(which(d$storage.humidity != 0 & !is.na(d$storage.humidity))) #3284
length(which(is.na(d$storage.humidity))) # 27235
length(which(d$storage.humidity == 0)) # 2

length(which(!is.na(d$storage.temp))) #18749
length(which(!is.na(d$storage.time) & !is.na(d$storage.temp)))

d$dormancyTemp <- NA
d$dormancyDuration <- NA
d$dormancyWet <- NA

idx <- which(d$datasetID == "airi2009")
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "species",
                                        "storageTemp", "storageDuration",
                                        "chillTemp", "chillDuration",
                                        "dormancyTemp", "dormancyDuration",
                                        "dormancyWet", "storageType", "storageDetails",
                                        "respvar", "response", "figure"))

for(i in 1:nrow(d)){
  if(is.na(d$storageTemp[i]) & is.na(d$storageDuration[i]) &
     is.na(d$chillTemp[i]) & is.na(d$chillDuration[i])) {next}
  
  if(is.na(d$storageTemp[i]) & is.na(d$storageDuration[i])){
    d$dormancyTemp[i] <- d$chillTemp[i]
    d$dormancyDuration[i] <- d$chillDuration[i]
    
    num_stor <- str_count(d$storageTemp[i], " then ")
    if(str_count(d$storageType, "moist") == 0){
      d$dormancyWet[i] <- "N"
      for(i in 1:num_stor){
        d$dormancyWet[i] <- paste0(d$dormancyWet[i], " then N")
      }
    } else{
      d$dormancyWet[i] <- "Y"
      for(i in 1:num_stor){
        d$dormancyWet[i] <- paste0(d$dormancyWet[i], " then Y")
      }
    }
  } else if(is.na(d$chillTemp[i]) & is.na(d$chillDuration[i])){
    d$dormancyTemp[i] <- d$storageTemp[i]
    d$dormancyDuration[i] <- d$storageDuration[i]
    d$dormancyWet[i] <- "Y"
    
    num_chill <- str_count(d$chillTemp[i], " then ")
    for(i in 1:num_chill){
      d$dormancyWet[i] <- paste0(d$dormancyWet[i], " then Y")
    }
  } else{
    d$dormancyTemp[i] <- paste0(d$storageTemp[i], " then ", d$chillTemp[i])
    d$dormancyDuration[i] <- paste0(d$storageDuration[i], " then ", d$chillDuration[i])
    
    num_stor <- str_count(d$storageTemp[i], " then ")
    num_chill <- str_count(d$chillTemp[i], " then ")
    
    num_stor <- str_count(d$storageTemp[i], " then ")
    if(str_count(d$storageType, "moist") == 0){
      d$dormancyWet[i] <- "N"
      for(i in 1:num_stor){
        d$dormancyWet[i] <- paste0(d$dormancyWet[i], " then N")
      }
    } else{
      d$dormancyWet[i] <- "Y"
      for(i in 1:num_stor){
        d$dormancyWet[i] <- paste0(d$dormancyWet[i], " then Y")
      }
    }
    d$dormancyWet[i] <- paste0(d$dormancyWet[i], " then Y")
    for(i in 1:num_chill){
      d$dormancyWet[i] <- paste0(d$dormancyWet[i], " then Y")
    }
  }
}
