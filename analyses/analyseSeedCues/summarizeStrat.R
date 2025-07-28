
# detailled info
stratSequence <- rep(NA, nrow(d))
stratTemp <- rep(NA, nrow(d))
stratDur <- rep(NA, nrow(d))

# condensed info
stratSequence_cdsd <- rep(NA, nrow(d))
stratTemp_cdsd  <- rep(NA, nrow(d))
stratDur_cdsd  <- rep(NA, nrow(d))

for(i in 1:nrow(d)){
  stratSequence_i <- c()
  stratTemp_i <- c()
  stratDur_i <- c()
  stratSequence_cdsd_i <- c()
  stratTemp_cdsd_i <- c()
  stratDur_cdsd_i <- c()
  
  # lookup into storage
  storTemp_i <- unlist(stringr::str_split(d$storageTemp[i], " then "))
  storDur_i <- unlist(stringr::str_split(d$storageDuration[i], " then "))
  for(n in 1:length(storTemp_i) ){
    len <- length(stratSequence_i)
    if(d$storageType[i] %in% c("moist", "moist/cold") & !is.na(as.numeric(storTemp_i[n])) & !is.na(as.numeric(storDur_i[n]))){                                                                                                  
      if(as.numeric(storTemp_i[n]) >= 15){
        stratSequence_i <- c(stratSequence_i, 'warm (moist stor.)')
        stratTemp_i <- c(stratTemp_i, storTemp_i[n])
        stratDur_i <- c(stratDur_i, storDur_i[n])
        # condensed
        if(length(stratSequence_i) == 1){
          stratSequence_cdsd_i <- 'warm'
          stratTemp_cdsd_i <- stratTemp_i
          stratDur_cdsd_i <- stratDur_i
        }else if(length(stratSequence_i) > 1){
          if(!grepl('warm', stratSequence_i[len])){
            stratSequence_cdsd_i <- c(stratSequence_cdsd_i, 'warm')
            stratTemp_cdsd_i <- c(stratTemp_cdsd_i, stratTemp_i[length(stratTemp_i)])
            stratDur_cdsd_i <- c(stratDur_cdsd_i, stratDur_i[length(stratDur_i)])
          }else if(grepl('warm', stratSequence_i[len])){
            stratTemp_cdsd_i[length(stratSequence_cdsd_i)] <- weighted.mean(as.numeric(stratTemp_i[c(len, len+1)]), as.numeric(stratDur_i[c(len, len+1)]))
            stratDur_cdsd_i[length(stratDur_cdsd_i)] <-  sum(as.numeric(stratDur_i[c(len, len+1)]))
          }
        }
      }else if(as.numeric(storTemp_i[n]) < 10 & as.numeric(storTemp_i[n]) > -20){
        stratSequence_i <- c(stratSequence_i, 'cold (moist stor.)')
        stratTemp_i <- c(stratTemp_i, storTemp_i[n])
        stratDur_i <- c(stratDur_i, storDur_i[n])
        # condensed
        if(length(stratSequence_i) == 1){
          stratSequence_cdsd_i <- 'cold'
          stratTemp_cdsd_i <- stratTemp_i
          stratDur_cdsd_i <- stratDur_i
        }else if(length(stratSequence_i) > 1){
          if(!grepl('cold', stratSequence_i[len])){
            stratSequence_cdsd_i <- c(stratSequence_cdsd_i, 'cold')
            stratTemp_cdsd_i <- c(stratTemp_cdsd_i, stratTemp_i[length(stratTemp_i)])
            stratDur_cdsd_i <- c(stratDur_cdsd_i, stratDur_i[length(stratDur_i)])
          }else if(grepl('cold', stratSequence_i[len])){
            stratTemp_cdsd_i[length(stratSequence_cdsd_i)] <- weighted.mean(as.numeric(stratTemp_i[c(len, len+1)]), as.numeric(stratDur_i[c(len, len+1)]))
            stratDur_cdsd_i[length(stratDur_cdsd_i)] <-  sum(as.numeric(stratDur_i[c(len, len+1)]))
          }
        }
      }else if(as.numeric(storTemp_i[n]) >= 10 & as.numeric(storTemp_i[n]) < 15){
        stratSequence_i <- c(stratSequence_i, 'mild (moist stor.)')
        stratTemp_i <- c(stratTemp_i, storTemp_i[n])
        stratDur_i <- c(stratDur_i, storDur_i[n])
        # condensed
        if(length(stratSequence_i) == 1){
          stratSequence_cdsd_i <- 'mild'
          stratTemp_cdsd_i <- stratTemp_i
          stratDur_cdsd_i <- stratDur_i
        }else if(length(stratSequence_i) > 1){
          if(!grepl('mild', stratSequence_i[len])){
            stratSequence_cdsd_i <- c(stratSequence_cdsd_i, 'mild')
            stratTemp_cdsd_i <- c(stratTemp_cdsd_i, stratTemp_i[length(stratTemp_i)])
            stratDur_cdsd_i <- c(stratDur_cdsd_i, stratDur_i[length(stratDur_i)])
          }else if(grepl('mild', stratSequence_i[len])){
            stratTemp_cdsd_i[length(stratSequence_cdsd_i)] <- weighted.mean(as.numeric(stratTemp_i[c(len, len+1)]), as.numeric(stratDur_i[c(len, len+1)]))
            stratDur_cdsd_i[length(stratDur_cdsd_i)] <-  sum(as.numeric(stratDur_i[c(len, len+1)]))
          }
        }
      }
    }
  }
  
  nsave <- length(stratSequence_i)
  
  # lookup into chilling conditions
  chillTemp_i <- unlist(stringr::str_split(d$chillTemp[i], " then "))
  chillDur_i <- unlist(stringr::str_split(d$chillDuration[i], " then "))
  for(n in 1:3){
    len <- length(stratSequence_i)
    if(!is.na(as.numeric(chillTemp_i[n])) & !is.na(as.numeric(chillDur_i[n]))){                                                                                                  
      if(as.numeric(chillTemp_i[n]) >= 15){
        stratSequence_i <- c(stratSequence_i, 'warm (strat.)')
        stratTemp_i <- c(stratTemp_i, chillTemp_i[n])
        stratDur_i <- c(stratDur_i, chillDur_i[n])
        # condensed
        if(length(stratSequence_i) == 1){
          stratSequence_cdsd_i <- 'warm'
          stratTemp_cdsd_i <- stratTemp_i
          stratDur_cdsd_i <- stratDur_i
        }else if(length(stratSequence_i) > 1){
          if(!grepl('warm', stratSequence_i[len])){
            stratSequence_cdsd_i <- c(stratSequence_cdsd_i, 'warm')
            stratTemp_cdsd_i <- c(stratTemp_cdsd_i, stratTemp_i[length(stratTemp_i)])
            stratDur_cdsd_i <- c(stratDur_cdsd_i, stratDur_i[length(stratDur_i)])
          }else if(grepl('warm', stratSequence_i[len])){
            stratTemp_cdsd_i[length(stratSequence_cdsd_i)] <- weighted.mean(as.numeric(stratTemp_i[c(len, len+1)]), as.numeric(stratDur_i[c(len, len+1)]))
            stratDur_cdsd_i[length(stratDur_cdsd_i)] <- sum(as.numeric(stratDur_i[c(len, len+1)]))
          }
        }
      }else if(as.numeric(chillTemp_i[n]) < 10 & as.numeric(chillTemp_i[n]) > -20){
        stratSequence_i <- c(stratSequence_i, 'cold (strat.)')
        stratTemp_i <- c(stratTemp_i, chillTemp_i[n])
        stratDur_i <- c(stratDur_i, chillDur_i[n])
        # condensed
        if(length(stratSequence_i) == 1){
          stratSequence_cdsd_i <- 'cold'
          stratTemp_cdsd_i <- stratTemp_i
          stratDur_cdsd_i <- stratDur_i
        }else if(length(stratSequence_i) > 1){
          if(!grepl('cold', stratSequence_i[len])){
            stratSequence_cdsd_i <- c(stratSequence_cdsd_i, 'cold')
            stratTemp_cdsd_i <- c(stratTemp_cdsd_i, stratTemp_i[length(stratTemp_i)])
            stratDur_cdsd_i <- c(stratDur_cdsd_i, stratDur_i[length(stratDur_i)])
          }else if(grepl('cold', stratSequence_i[len])){
            stratTemp_cdsd_i[length(stratSequence_cdsd_i)] <- weighted.mean(as.numeric(stratTemp_i[c(len, len+1)]), as.numeric(stratDur_i[c(len, len+1)]))
            stratDur_cdsd_i[length(stratDur_cdsd_i)] <- sum(as.numeric(stratDur_i[c(len, len+1)]))
          }
        }
      }else if(as.numeric(chillTemp_i[n]) >= 10 & as.numeric(chillTemp_i[n]) < 15){
        stratSequence_i <- c(stratSequence_i, 'mild (strat.)')
        stratTemp_i <- c(stratTemp_i, chillTemp_i[n])
        stratDur_i <- c(stratDur_i, chillDur_i[n])
        # condensed
        if(length(stratSequence_i) == 1){
          stratSequence_cdsd_i <- 'mild'
          stratTemp_cdsd_i <- stratTemp_i
          stratDur_cdsd_i <- stratDur_i
        }else if(length(stratSequence_i) > 1){
          if(!grepl('mild', stratSequence_i[len])){
            stratSequence_cdsd_i <- c(stratSequence_cdsd_i, 'mild')
            stratTemp_cdsd_i <- c(stratTemp_cdsd_i, stratTemp_i[length(stratTemp_i)])
            stratDur_cdsd_i <- c(stratDur_cdsd_i, stratDur_i[length(stratDur_i)])
          }else if(grepl('mild', stratSequence_i[len])){
            stratTemp_cdsd_i[length(stratSequence_cdsd_i)] <- weighted.mean(as.numeric(stratTemp_i[c(len, len+1)]), as.numeric(stratDur_i[c(len, len+1)]))
            stratDur_cdsd_i[length(stratDur_cdsd_i)] <-  sum(as.numeric(stratDur_i[c(len, len+1)]))
          }
        }
      }
    }
  }
  
  stratSequence[i] <- ifelse(is.null(stratSequence_i), NA, paste0(stratSequence_i, collapse = ' then '))
  stratTemp[i] <- ifelse(is.null(stratTemp_i), NA, paste0(stratTemp_i, collapse = ' then '))
  stratDur[i] <- ifelse(is.null(stratDur_i), NA, paste0(stratDur_i, collapse = ' then '))
  
  stratSequence_cdsd[i] <- ifelse(is.null(stratSequence_cdsd_i), NA, paste0(stratSequence_cdsd_i, collapse = ' then '))
  stratTemp_cdsd[i] <- ifelse(is.null(stratTemp_cdsd_i), NA, paste0(stratTemp_cdsd_i, collapse = ' then '))
  stratDur_cdsd[i] <- ifelse(is.null(stratDur_cdsd_i), NA, paste0(stratDur_cdsd_i, collapse = ' then '))
}

d$stratSequence <- stratSequence
d$stratSequence_condensed <- stratSequence_cdsd

# For Christophe: grepl('warm', d$stratSequence_condensed)
