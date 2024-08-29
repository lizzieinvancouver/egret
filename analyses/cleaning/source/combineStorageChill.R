## Started 20 August 2024
## By Ken

## Combine current storage and chill columns

length(which(d$storage.humidity != 0 & !is.na(d$storage.humidity))) #3284
length(which(is.na(d$storage.humidity))) # 27235
length(which(d$storage.humidity == 0)) # 2

length(which(!is.na(d$storage.temp))) #18749
length(which(!is.na(d$storage.time) & !is.na(d$storage.temp)))

d$chillingTemp <- d$storage.temp
d$chillingDuration <- d$storage.time
d$chillingWet <- d$storage.humidity


check <- d[which(is.na(d$storage.type) &
                   is.na(d$storage.temp) &
                   is.na(d$storage.time) &
                   is.na(d$storage.humidity)),]

idx <- which(d$datasetID == "airi2009")
check <- d[idx,]
check_short <- subset(check, select = c("datasetID", "study", "species", "storage.type", "storage.temp",
                                        "storage.time", "storage.humidity", "treatment", "chillTemp",
                                        "chillDuration", "chillingTemp", "chillingDuration",
                                        "chillingWet", "storageType", "storageDetails",
                                        "respvar", "response", "figure"))

no_storage <- which(is.na(d$storage.type) &
                      is.na(d$storage.temp) &
                      is.na(d$storage.time) &
                      is.na(d$storage.humidity))

d$chillingWet[which(!is.na(d$storage.humidity) & d$storage.humidity != 0)] <- "Y"
d$chillingWet[which(is.na(d$storage.humidity & d$storage.humidity == 0))] <- "N"
d$chillingWet[no_storage] <- NA

for(i in 1:nrow(d)){
  if(is.na(d$chillTemp) & is.na(d$chillDuration)) {next}
  
  temp_set <- str_count(d$chillTemp[i], " then ")
  if(i %in% no_storage){
    d$chillingWet[i] <- "Y"
    if(temp_set == 0) {next}
    for(j in 1:temp_set){
      d$chillingWet[i] <- paste0(d$chillingWet[i], " then Y")
    }
  } else {
    for(j in 1:(temp_set + 1)){
      d$chillingWet[i] <- paste0(d$chillingWet[i], " then Y")
    }
  }
}
