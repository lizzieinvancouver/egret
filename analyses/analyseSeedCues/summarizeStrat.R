
# Summarizing stratification treatments in a more handy format

# modifying chilling in tang21
d[d$datasetID %in% 'zhou03' & d$study %in% 'exp1' & d$chillTempCycle %in% "1344 and 1344", 'chillDuration'] <- '56 then 56'
d[d$datasetID %in% 'zhou03' & d$study %in% 'exp1' & d$chillTempCycle %in% "1344 and 1344", 'chillTemp'] <- '4 then 22'
d[d$datasetID %in% 'zhou03' & d$study %in% 'exp1' & d$chillTempCycle %in% "672 and 672", 'chillDuration'] <- '28 then 28 then 28 then 28'
d[d$datasetID %in% 'zhou03' & d$study %in% 'exp1' & d$chillTempCycle %in% "672 and 672", 'chillTemp'] <- '4 then 22 then 4 then 22'
d[d$datasetID %in% 'zhou03' & d$study %in% 'exp1' & d$chillTempCycle %in% "336 and 336", 'chillDuration'] <- '14 then 14 then 14 then 14 then 14 then 14 then 14 then 14'
d[d$datasetID %in% 'zhou03' & d$study %in% 'exp1' & d$chillTempCycle %in% "336 and 336", 'chillTemp'] <- '4 then 22 then 4 then 22 then 4 then 22 then 4 then 22'
d[d$datasetID %in% 'zhou03' & d$study %in% 'exp1' & d$chillTempCycle %in% "168 and 168", 'chillDuration'] <- '7 then 7 then 7 then 7 then 7 then 7 then 7 then 7 then 7 then 7 then 7 then 7 then 7 then 7 then 7'
d[d$datasetID %in% 'zhou03' & d$study %in% 'exp1' & d$chillTempCycle %in% "168 and 168", 'chillTemp'] <- '4 then 22 then 4 then 22 then 4 then 22 then 4 then 22 then 4 then 22 then 4 then 22 then 4 then 22 then 4 then 22'

# detailed info
cold_range <- c(-10, 10)
warm_range <- c(10, 40) # exclude 10

# detailed info
stratSequence <- rep(NA, nrow(d))
stratTemp <- rep(NA, nrow(d))
stratDur <- rep(NA, nrow(d))

# condensed info
stratSequence_cdsd <- rep(NA, nrow(d))
stratTemp_cdsd  <- rep(NA, nrow(d))
stratDur_cdsd  <- rep(NA, nrow(d))

# new storage column (excluding strat. conditions)
# so we don't need to then re-do this again and again later (e.g. in decision rules)
storageNoStratTemp <- d$storageTemp
storageNoStratDur <- d$storageDuration
storageNoStratDetails <- d$storageDetails

for(i in 1:nrow(d)){
  stratSequence_i <- c()
  stratTemp_i <- c()
  stratDur_i <- c()
  stratSequence_cdsd_i <- c()
  stratTemp_cdsd_i <- c()
  stratDur_cdsd_i <- c()
  storageNoStratTemp_i <- c()
  storageNoStratDur_i <- c()
  storageNoStratDetails_i <- c()
  storSequence_i <- c()
  
  # quick lookup into storage
  storageNoStratTemp_i <- storTemp_i <- unlist(stringr::str_split(d$storageTemp[i], " then "))
  storageNoStratDur_i <- storDur_i <- unlist(stringr::str_split(d$storageDuration[i], " then "))
  storSequence_i <- rep(FALSE, length(storageNoStratTemp_i))
  for(n in 1:length(storTemp_i) ){
    if(d$storageType[i] %in% c("moist", "moist/cold") & !is.na(as.numeric(storTemp_i[n])) & !is.na(as.numeric(storDur_i[n]))){                                                                                                  
      if(as.numeric(storTemp_i[n]) > warm_range[1] & as.numeric(storTemp_i[n]) <= warm_range[2]){
        storSequence_i[n] <- TRUE # considered as strat.
      }else if(as.numeric(storTemp_i[n]) >= cold_range[1] & as.numeric(storTemp_i[n]) <= cold_range[2]){
        storSequence_i[n] <- TRUE # considered as strat.
      }
    }
  }
  
  # if there is no interruption between last storage and strat.
  if(storSequence_i[length(storSequence_i)]){
    # lookup more closely into storage
    for(n in 1:length(storTemp_i) ){
      len <- length(stratSequence_i)
      if(d$storageType[i] %in% c("moist", "moist/cold") & !is.na(as.numeric(storTemp_i[n])) & !is.na(as.numeric(storDur_i[n]))){                                                                                                  
        if(as.numeric(storTemp_i[n]) > warm_range[1] & as.numeric(storTemp_i[n]) <= warm_range[2]){
          stratSequence_i <- c(stratSequence_i, 'warm (moist stor.)')
          stratTemp_i <- c(stratTemp_i, storTemp_i[n])
          stratDur_i <- c(stratDur_i, storDur_i[n])
          storageNoStratTemp_i[n] <- 'STRAT' # considered as strat.
          storageNoStratDur_i[n] <- 'STRAT' # considered as strat.
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
        }else if(as.numeric(storTemp_i[n]) >= cold_range[1] & as.numeric(storTemp_i[n]) <= cold_range[2]){
          stratSequence_i <- c(stratSequence_i, 'cold (moist stor.)')
          stratTemp_i <- c(stratTemp_i, storTemp_i[n])
          stratDur_i <- c(stratDur_i, storDur_i[n])
          storageNoStratTemp_i[n] <- 'STRAT' # considered as strat.
          storageNoStratDur_i[n] <- 'STRAT' # considered as strat.
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
        }
      }
    }
    
    storageNoStratTemp[i] <- ifelse(is.null(storageNoStratTemp_i[which(storageNoStratTemp_i != 'STRAT')]), NA, paste0(storageNoStratTemp_i[which(storageNoStratTemp_i != 'STRAT')], collapse = ' then '))
    storageNoStratDur[i] <- ifelse(is.null(storageNoStratDur_i[which(storageNoStratDur_i != 'STRAT')]), NA, paste0(storageNoStratDur_i[which(storageNoStratDur_i != 'STRAT')], collapse = ' then '))
  }

  nsave <- length(stratSequence_i)
  
  # lookup into chilling conditions
  chillTemp_i <- unlist(stringr::str_split(d$chillTemp[i], " then "))
  chillDur_i <- unlist(stringr::str_split(d$chillDuration[i], " then "))
  for(n in 1:length(chillTemp_i)){
    len <- length(stratSequence_i)
    if(!is.na(as.numeric(chillTemp_i[n])) & !is.na(as.numeric(chillDur_i[n]))){                                                                                                  
      if(as.numeric(chillTemp_i[n]) > warm_range[1] & as.numeric(chillTemp_i[n]) <= warm_range[2]){
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
      }else if(as.numeric(chillTemp_i[n]) >= cold_range[1] & as.numeric(chillTemp_i[n]) <= cold_range[2]){
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
      }else if(as.numeric(chillTemp_i[n]) < cold_range[1] | as.numeric(chillTemp_i[n]) > warm_range[2]){
        stratSequence_i <- c(stratSequence_i, 'undefined (strat.)')
        stratTemp_i <- c(stratTemp_i, chillTemp_i[n])
        stratDur_i <- c(stratDur_i, chillDur_i[n])
        # condensed
        if(length(stratSequence_i) == 1){
          stratSequence_cdsd_i <- 'undefined'
          stratTemp_cdsd_i <- stratTemp_i
          stratDur_cdsd_i <- stratDur_i
        }else if(length(stratSequence_i) > 1){
          if(!grepl('undefined', stratSequence_i[len])){
            stratSequence_cdsd_i <- c(stratSequence_cdsd_i, 'undefined')
            stratTemp_cdsd_i <- c(stratTemp_cdsd_i, stratTemp_i[length(stratTemp_i)])
            stratDur_cdsd_i <- c(stratDur_cdsd_i, stratDur_i[length(stratDur_i)])
          }else if(grepl('undefined', stratSequence_i[len])){
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

# d$stratSequence <- stratSequence
d$storageNoStratTemp <- storageNoStratTemp
d$storageNoStratDur <- storageNoStratDur
d$stratSequence_condensed <- stratSequence_cdsd
d$stratTemp_condensed <- stratTemp_cdsd
d$stratDur_condensed <- stratDur_cdsd

# For Deirdre and merging with USDA
d$warmStratTemp <- as.numeric(sapply(1:nrow(d), function(i){
  seq <-  unlist(stringr::str_split(d$stratSequence_condensed[i], ' then '))
  temp <-  unlist(stringr::str_split(d$stratTemp_condensed[i], ' then '))
  id <- which(seq == 'warm')
  return(ifelse(is.null(id), NA, temp[id]))
}))

# If you want to plot 
# strat_summary <- data.frame(table(unique(d[,c('datasetID', 'stratSequence_condensed')])$stratSequence_condensed))
# names(strat_summary) <- c('sequence', 'count')
# strat_summary_plotdf <- data.frame()
# for(i in 1:nrow(strat_summary)){
#   seq <- unlist(stringr::str_split(strat_summary$sequence[i], ' then '))
#   seqplot <- rep(NA, 3)
#   if(length(seq) == 1){
#     seqplot[3] <- seq[1]
#   }else if(length(seq) == 2){
#     seqplot[2:3] <- seq[1:2]
#   }else{
#     seqplot <- seq
#   }
#   names(seqplot) <- paste0('step', 1:3)
#   strat_summary_plotdf <- rbind(
#     strat_summary_plotdf,
#     data.frame(count = strat_summary$count[i], t(seqplot))
#   )
# }
# strat_summary_plotdf <- strat_summary_plotdf[order(strat_summary_plotdf$count),]
# strat_summary_plotdf$y <- 1:nrow(strat_summary_plotdf)
# strat_summary_plot <- ggplot(data = strat_summary_plotdf) +
#   geom_point(aes(y = y, x = 1, color = step1), size = 5) +
#   geom_point(aes(y = y, x = 1.5, color = step2), size = 5) +
#   geom_point(aes(y = y, x = 2, color = step3), size = 5) +
#   geom_text(aes(y = y, x = 0.3, label = paste0('n=',count))) +
#   scale_color_manual(breaks = c('undefined', 'cold', 'warm'), values = c('grey80', '#178a94', '#ee8080'), na.value = NA) +
#   theme_void() +
#   theme(legend.position = 'bottom',
#         legend.title = element_blank(),
#         plot.margin = margin(5,5,5,10)) +
#   guides(color = guide_legend(override.aes = list(size = 2))) +
#   coord_cartesian(xlim = c(0,2))
