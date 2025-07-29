
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

strat_summary <- data.frame(table(unique(d[,c('datasetID', 'stratSequence_condensed')])$stratSequence_condensed))
names(strat_summary) <- c('sequence', 'count')

strat_summary_plotdf <- data.frame()
for(i in 1:nrow(strat_summary)){
  
  seq <- unlist(stringr::str_split(strat_summary$sequence[i], ' then '))
  seqplot <- rep(NA, 3)
  if(length(seq) == 1){
    seqplot[3] <- seq[1]
  }else if(length(seq) == 2){
    seqplot[2:3] <- seq[1:2]
  }else{
    seqplot <- seq
  }
  names(seqplot) <- paste0('step', 1:3)
  strat_summary_plotdf <- rbind(
    strat_summary_plotdf,
    data.frame(count = strat_summary$count[i], t(seqplot))
  )
}

strat_summary_plotdf <- strat_summary_plotdf[order(strat_summary_plotdf$count),]
strat_summary_plotdf$y <- 1:nrow(strat_summary_plotdf)

strat_summary_plot <- ggplot(data = strat_summary_plotdf) +
  geom_point(aes(y = y, x = 1, color = step1), size = 5) +
  geom_point(aes(y = y, x = 1.5, color = step2), size = 5) +
  geom_point(aes(y = y, x = 2, color = step3), size = 5) +
  geom_text(aes(y = y, x = 0.3, label = paste0('n=',count))) +
  scale_color_manual(breaks = c('cold', 'mild', 'warm'), values = c('#178a94','#bfe1bf','#ee8080'), na.value = NA) +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.margin = margin(5,5,5,10)) +
  coord_cartesian(xlim = c(0,2))
