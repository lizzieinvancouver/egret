## Started 11 August 2026
## by Ken

## Estimate overall storage and stratification conditions

d$storTempEst <- NA
d$storDurEst <- NA
d$stratTempEst <- NA
d$stratDurEst <- NA

dryStorIdx <- which(!(d$storageType %in% c('moist', 'moist/cold', 'moisture-controlled')))
moistStorIdx <- which(d$storageType %in% c('moist', 'moist/cold', 'moisture-controlled'))

for(i in 1:length(dryStorIdx)){
  storTemp <- d$storageTemp[dryStorIdx[i]]
  storDur <- d$storageDuration[dryStorIdx[i]]
  
  if(!is.na(storTemp) & !is.na(storDur)){
    storCount <- str_count(storTemp, " then ") + 1
    storTemp <- strsplit(storTemp, " then ")
    storDur <- strsplit(storDur, " then ")
    
    storTempSeq <- c()
    storDurSeq <- c()
    storSeqCount <- 0
    
    for(j in 1:storCount){
      if(!is.na(as.numeric(storTemp[[1]][j])) & !is.na(as.numeric(storDur[[1]][j]))){
        storSeqCount <- storSeqCount + 1
        storTempSeq[storSeqCount] <- as.numeric(storTemp[[1]][j])
        storDurSeq[storSeqCount] <- as.numeric(storDur[[1]][j])
      }
    }
    
    if(storSeqCount > 0){
      d$storTempEst[dryStorIdx[i]] <- sum(storTempSeq * storDurSeq) / sum(storDurSeq)
      d$storDurEst[dryStorIdx[i]] <- sum(storDurSeq)
    }
  }
  
  stratTemp <- d$chillTemp[dryStorIdx[i]]
  stratDur <- d$chillDuration[dryStorIdx[i]]
  
  if(!is.na(stratTemp) & !is.na(stratDur)){
    stratCount <- str_count(stratTemp, " then ") + 1
    stratTemp <- strsplit(stratTemp, " then ")
    stratDur <- strsplit(stratDur, " then ")
    
    stratTempSeq <- c()
    stratDurSeq <- c()
    stratSeqCount <- 0
    
    for(j in 1:stratCount){
      if(!is.na(as.numeric(stratTemp[[1]][j])) & !is.na(as.numeric(stratDur[[1]][j]))){
        stratSeqCount <- stratSeqCount + 1
        stratTempSeq[stratSeqCount] <- as.numeric(stratTemp[[1]][j])
        stratDurSeq[stratSeqCount] <- as.numeric(stratDur[[1]][j])
      }
    }
    
    if(stratSeqCount > 0){
      d$stratTempEst[dryStorIdx[i]] <- sum(stratTempSeq * stratDurSeq) / sum(stratDurSeq)
      d$stratDurEst[dryStorIdx[i]] <- sum(stratDurSeq)
    }
  }
}

# check <- d[dryStorIdx, c('storageTemp', 'storageDuration', 'chillTemp', 'chillDuration',
#                          'storTempEst', 'storDurEst', 'stratTempEst', 'stratDurEst')]

for(i in 1:length(moistStorIdx)){
  storTemp <- d$storageTemp[moistStorIdx[i]]
  storDur <- d$storageDuration[moistStorIdx[i]]
  
  storCount <- NA
  lastStorTemp <- NA
  
  if(!is.na(storTemp) & !is.na(storDur)){
    storCount <- str_count(storTemp, " then ") + 1
    storTemp <- strsplit(storTemp, " then ")
    storDur <- strsplit(storDur, " then ")
    
    lastStorTemp <- 0
    
    for(j in storCount:1){
      if(as.numeric(unlist(storTemp)[j]) < -10 | as.numeric(unlist(storTemp)[j]) > 40){
        lastStorTemp <- j
        next
      }
    }
    
    if(lastStorTemp != 0){
      storTempSeq <- c()
      storDurSeq <- c()
      storSeqCount <- 0
      
      for(j in 1:lastStorTemp){
        if(!is.na(as.numeric(storTemp[[1]][j])) & !is.na(as.numeric(storDur[[1]][j]))){
          storSeqCount <- storSeqCount + 1
          storTempSeq[storSeqCount] <- as.numeric(storTemp[[1]][j])
          storDurSeq[storSeqCount] <- as.numeric(storDur[[1]][j])
        }
      }
      
      if(storSeqCount > 0){
        d$storTempEst[moistStorIdx[i]] <- sum(storTempSeq * storDurSeq) / sum(storDurSeq)
        d$storDurEst[moistStorIdx[i]] <- sum(storDurSeq)
      }
    }
  }
  
  stratTempSeq <- c()
  stratDurSeq <- c()
  stratSeqCount <- 0
  
  if(!is.na(lastStorTemp < storCount) & lastStorTemp < storCount){
    for(j in 1:(storCount - lastStorTemp)){
      if(!is.na(as.numeric(storTemp[[1]][lastStorTemp + j])) & !is.na(as.numeric(storDur[[1]][lastStorTemp + j]))){
        stratSeqCount <- stratSeqCount + 1
        stratTempSeq[stratSeqCount] <- as.numeric(storTemp[[1]][lastStorTemp + j])
        stratDurSeq[stratSeqCount] <- as.numeric(storDur[[1]][lastStorTemp + j])
      }
    }
  }
  
  stratTemp <- d$chillTemp[moistStorIdx[i]]
  stratDur <- d$chillDuration[moistStorIdx[i]]

  if(!is.na(stratTemp) & !is.na(stratDur)){
    stratCount <- str_count(stratTemp, " then ") + 1
    stratTemp <- strsplit(stratTemp, " then ")
    stratDur <- strsplit(stratDur, " then ")

    for(j in 1:stratCount){
      if(!is.na(as.numeric(stratTemp[[1]][j])) & !is.na(as.numeric(stratDur[[1]][j]))){
        stratSeqCount <- stratSeqCount + 1
        stratTempSeq[stratSeqCount] <- as.numeric(stratTemp[[1]][j])
        stratDurSeq[stratSeqCount] <- as.numeric(stratDur[[1]][j])
      }
    }
  }
  
  if(stratSeqCount > 0){
    d$stratTempEst[moistStorIdx[i]] <- sum(stratTempSeq * stratDurSeq) / sum(stratDurSeq)
    d$stratDurEst[moistStorIdx[i]] <- sum(stratDurSeq)
  }
}

# check <- d[moistStorIdx, c('storageTemp', 'storageDuration', 'chillTemp', 'chillDuration',
#                            'storTempEst', 'storDurEst', 'stratTempEst', 'stratDurEst')]

check <- d[c('storageTemp', 'storageDuration', 'chillTemp', 'chillDuration',
             'storTempEst', 'storDurEst', 'stratTempEst', 'stratDurEst')]
