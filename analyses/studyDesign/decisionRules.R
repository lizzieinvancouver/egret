

particularStudies <- unitreat[unitreat$nscarif > 1 | unitreat$nchem > 1, c('datasetID', 'study', 'genusspecies')]

chem.toremove <- unique(d[grepl('GA|BAP|ABA|hormone|regulator|kinetin|fertilizer|probiotic|herbicide|fungicide|captan|PEG|KNO3|IBA|thiourea|benzyladenine|Tween', 
                                          d$chemicalCor),'chemicalCor'])
chem.notremoved <- unique(d[!grepl('GA|BAP|ABA|hormone|regulator|kinetin|fertilizer|probiotic|herbicide|fungicide|captan|PEG|KNO3|IBA|thiourea|benzyladenine|Tween', 
                                             d$chemicalCor),'chemicalCor'])
chem.notremoved

misc.toremove <- unique(d[grepl('stress', d$other.treatment),'other.treatment'])


particulard <- d %>%
  # dplyr::filter(paste0(datasetID,study,genusspecies) %in% paste0(particularStudies$datasetID,particularStudies$study,particularStudies$genusspecies)) %>%
  dplyr::filter(responseVar == 'percent.germ' & !(chemicalCor %in% chem.toremove) & !(other.treatment %in% misc.toremove)) %>%
  dplyr::filter(!is.na(germTempGen))

particulard$responseValue <- as.numeric(particulard$responseValue)

singularities <- particulard %>%
  dplyr::group_by(datasetID, study, genusspecies) %>%
  dplyr::reframe(nstorage = n_distinct(storageType, storageTemp, storageDuration, storageDetails),
                 nscarif = n_distinct(scarifType, scarifTypeGen, scarifTypeSpe),
                 nchem = n_distinct(chemicalCor),
                 nother = n_distinct(other.treatment),
                 nstrat = n_distinct(dormancyTemp, dormancyDuration),
                 ngerm = n_distinct(germTempGen, germDuration)) %>%
  as.data.frame()
 
ids <- singularities[c('datasetID', 'study', 'genusspecies')]

ids <- unique(particulard[c('datasetID', 'study', 'genusspecies')]) %>%
  dplyr::filter(!(datasetID == 'forbes09' & study == 'exp20'))



newd <- data.frame()
ids$misc.tokeep <- ids$misc.check <- NA
ids$scarif.tokeep <- ids$scarif.check <- NA
ids$chem.tokeep <- ids$chem.check <- NA
ids$stor.tokeep <- ids$stor.check <- NA

for(i in 1:nrow(ids)){
  
  di <- particulard[paste0(particulard$datasetID,particulard$study,particulard$genusspecies) == paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
  cat(paste0("\nID: ", i, ' | ',paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ' '), "\n"))
  
  # MISC. TREATMENT
  ids[i, 'misc.check']  <- FALSE
  if(length(unique(di$other.treatment)) > 1){
    
    cat(paste0(" > Number of misc. treat.: ", length(unique(di$other.treatment)), "\n"))
    cat('   - [');cat(paste(unique(di$other.treatment)), sep = ', ');cat(']\n')
    
    ntreati <- di %>%
      dplyr::group_by(other.treatment) %>%
      dplyr::reframe(nstrat = n_distinct(dormancyTemp, dormancyDuration),
                     ngerm = n_distinct(germTempGen, germDuration),
                     ninterest = nstrat + ngerm) %>% as.data.frame()
    
    # we keep misc. with max. no. of chill/forc treatments
    treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'other.treatment'] 
    
    if(length(treat.tokeep) > 1){ # then, if needed, we keep the one with max. resp.
      
      maxresp <- max(di[di$other.treatment %in% treat.tokeep, 'responseValue'], na.rm = TRUE)
      treat.tokeep <- unique(di[di$other.treatment %in% treat.tokeep & di$responseValue %in% maxresp, 'other.treatment']) 
      
      if(length(treat.tokeep) > 1 & any(is.na(treat.tokeep))){ # if still not enough, we keep the control (if there is one)
        
        treat.tokeep <- NA
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + max. resp. + control)\n'))
        
      }else if(length(treat.tokeep) > 1 & !any(is.na(treat.tokeep))){ # if still not enough, we keep the one with max. average resp.
        
        avgrespi <- di %>%
          dplyr::group_by(other.treatment) %>%
          dplyr::filter(other.treatment %in% treat.tokeep) %>%
          dplyr::reframe(avgresp = mean(responseValue, na.rm = TRUE)) %>%
          as.data.frame()
        
        treat.tokeep <- avgrespi[avgrespi$avgresp == max(avgrespi$avgresp), 'other.treatment']
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + max. resp. + control + max. avg. resp.)\n'))
        
      }else if(length(treat.tokeep) == 1){
        
        cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + max. resp.)\n'))
        
      }else{
        stop('Missing conditions')
      }
      
    }else if(length(treat.tokeep) == 1){
      
      cat(paste0('   - Keeping: ', treat.tokeep,' (max. no. of chill./forc. treat.)\n'))
      
    }else{
      stop('Missing conditions')
    }
    
    ids[i, 'misc.tokeep'] <- treat.tokeep
    ids[i, 'misc.check']  <- TRUE
    
    
  }else if(length(unique(di$other.treatment)) == 1){
    
    cat(paste0(" > No misc. treat.\n"))
    treat.tokeep <- unique(di$other.treatment)
    ids[i, 'misc.tokeep'] <- treat.tokeep
    ids[i, 'misc.check']  <- TRUE
    
  }else{
    stop('Missing conditions?') # check
  }
  
  # Summary
  if(any(!ids$misc.check, na.rm = TRUE)){stop()} # check
  nrow.before <- nrow(di)
  di <- di[di$other.treatment %in% ids[i, 'misc.tokeep'], ]
  if(length(unique(di$other.treatment)) > 1){stop()} # check
  nrow.after <- nrow(di)
  if(nrow.before!=nrow.after){
    message(paste0('   => Loosing ', nrow.before-nrow.after,' rows (out of ',nrow.before,')'))
  }
  
  # SCARIFICATION
  ids[i, 'scarif.check']  <- FALSE
  if(length(unique(di$scarifType)) > 1){
    
    cat(paste0(" > Number of scarif. treat.: ", length(unique(di$scarifType)), "\n"))
    cat('   - [');cat(paste(unique(di$scarifType)), sep = ', ');cat(']\n')
    
    ntreati <- di %>%
      dplyr::group_by(scarifType) %>%
      dplyr::reframe(nscarif = n_distinct(scarifType, scarifTypeGen, scarifTypeSpe),
                     nstrat = n_distinct(dormancyTemp, dormancyDuration),
                     ngerm = n_distinct(germTempGen, germDuration),
                     ninterest = nstrat + ngerm) %>% as.data.frame()
    if(any(ntreati$nscarif > 1)){stop()} # check
    
    if(length(unique(ntreati$ninterest)) == 1){
      
      cat('   - Same number of chill./forc. treat. across scarif. treat.\n')
      
      if(length(unique(di$scarifType)) == 2 & any(is.na(unique(di$scarifType)))){ # if only two scarif., and one is control
        
        treat.tokeep <- unique(di$scarifType)[which(!is.na(unique(di$scarifType)))] # we keep scarified
        cat(paste0('   - Keeping: ', treat.tokeep,' (instead of control)\n'))
        
        #newd <- rbind(newd, di[di$scarifType %in% treat.tokeep,])
        ids[i, 'scarif.tokeep'] <- treat.tokeep
        ids[i, 'scarif.check']  <- TRUE
        
        
      }else if(length(unique(di$scarifType)) > 2){
        
        treat.tokeep <- unique(di[di$responseValue == max(di$responseValue ), 'scarifType']) # we keep scarif. with max. germ. rate
        cat(paste0('   - Keeping: ', treat.tokeep,' (max. germ. rate observed)\n'))
        
        # newd <- rbind(newd, di[di$scarifType %in% treat.tokeep,])
        ids[i, 'scarif.tokeep'] <- treat.tokeep
        ids[i, 'scarif.check']  <- TRUE
        
      }else{
        stop('Missing conditions?') # check
      }
      
    }else if(length(unique(ntreati$ninterest)) > 1){
      
      cat('   - Different number of chill./forc. treat. across scarif. treat.\n')
      treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'scarifType'] # we keep scarif. with max. no. of chill/forc treatments
      
      if(length(treat.tokeep) > 1){ # then, if needed, we choose the treatment with max. germ. rate
        
        treat.tokeep <- treat.tokeep[which(!is.na(treat.tokeep))]
        cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + scarified)\n'))
        
        if(length(treat.tokeep) > 1){ # if still not enough, we keep the scarified treatment...
          
          maxresp <- max(di[di$scarifType %in% treat.tokeep, 'responseValue'])
          treat.tokeep <- unique(di[di$scarifType %in% treat.tokeep & di$responseValue == maxresp, 'scarifType']) 
          cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + scarified + max. resp.)\n'))
          
        }
        
      }else if(length(treat.tokeep) == 1){
        
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat.)\n'))
        
      }
      
      # newd <- rbind(newd, di[di$scarifType %in% treat.tokeep,])
      ids[i, 'scarif.tokeep'] <- treat.tokeep
      ids[i, 'scarif.check']  <- TRUE
      
    }else{
      stop('Missing conditions?') # check
    }
    
  }else if(length(unique(di$scarifType)) == 1){
    
    cat(paste0(" > Only one scarif. treat.\n"))
    treat.tokeep <- unique(di$scarifType)
    ids[i, 'scarif.tokeep'] <- treat.tokeep
    ids[i, 'scarif.check']  <- TRUE
    
  }else{
    stop('Missing conditions?') # check
  }
  
  # Summary
  if(any(!ids$scarif.check, na.rm = TRUE)){stop()} # check
  nrow.before <- nrow(di)
  di <- di[di$scarifType %in% ids[i, 'scarif.tokeep'], ]
  if(length(unique(di$scarifType)) > 1){stop()} # check
  nrow.after <- nrow(di)
  if(nrow.before!=nrow.after){
    message(paste0('   => Loosing ', nrow.before-nrow.after,' rows (out of ',nrow.before,')'))
  }
  
  # CHEMICAL
  ids[i, 'chem.check']  <- FALSE
  if(length(unique(di$chemicalCor)) > 1){
    
    cat(paste0(" > Number of chem. treat.: ", length(unique(di$chemicalCor)), "\n"))
    cat('   - [');cat(paste(unique(di$chemicalCor)), sep = ', ');cat(']\n')
    
    ntreati <- di %>%
      dplyr::group_by(chemicalCor) %>%
      dplyr::reframe(nstrat = n_distinct(dormancyTemp, dormancyDuration),
                     ngerm = n_distinct(germTempGen, germDuration),
                     ninterest = nstrat + ngerm) %>% as.data.frame()
    
    if(length(unique(ntreati$ninterest)) == 1){ # same no. of chill/forc treatments
      
      cat('   - Same number of chill./forc. treat. across chem. treat.\n')
      
      if(any(is.na(unique(di$chemicalCor)))){ # if there is a control, we keep it
        
        treat.tokeep <- NA
        cat(paste0('   - Keeping: [', treat.tokeep,'] (control)\n'))
        
      }else if(any(unique(di$chemicalCor) %in% c('H2O'))){ # or there is water?
        
        treat.tokeep <- 'H2O'
        cat(paste0('   - Keeping: [', treat.tokeep,'] (control)\n'))
        
      }else if(!any(unique(di$chemicalCor) %in% c(NA, 'H2O'))){ # if no control
        
        # we can just keep the chemical treat. with max. germ. rate?
        maxresp <- max(di$responseValue)
        treat.tokeep <- unique(di[di$responseValue == maxresp, 'chemicalCor']) 
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. resp.)\n'))
        
      }else{
        stop('Missing conditions?') # check
      }
      
    }else if(length(unique(ntreati$ninterest)) > 1){ # different no. of chill/forc treatments
      
      cat('   - Different number of chill./forc. treat. across chem. treat.\n')
      treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'chemicalCor'] # chem. treat. with max. no. of chill/forc treatments
      
      if(length(treat.tokeep) > 1 & any(is.na(unique(treat.tokeep)))){ # then if needed: if there is control, we keep it!
        
        treat.tokeep <- NA
        cat(paste0('   - Keeping: ', treat.tokeep,' (max. no. of chill./forc. treat. + control)\n'))
        
      }else if(length(treat.tokeep) > 1 & any(unique(treat.tokeep) %in% c('H2O'))){ # or there is water?
        
        treat.tokeep <- 'H2O'
        cat(paste0('   - Keeping: ', treat.tokeep,' (max. no. of chill./forc. treat. + control)\n'))
        
      }else if(length(treat.tokeep) > 1 & !any(is.na(unique(treat.tokeep)))){ # no control, we have to choose something else
        
        # here, we need to make a decision with concentration information?
        # stop('Need concentration here')
        # or we can just keep the chemical treat. with max. germ. rate?
        maxresp <- max(di[di$chemicalCor %in% treat.tokeep, 'responseValue'])
        treat.tokeep <- unique(di[di$chemicalCor %in% treat.tokeep & di$responseValue == maxresp, 'scarifType']) 
        cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + max. resp.)\n'))
        
      }else if(length(treat.tokeep) == 1){
        
        cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat.)\n'))
        
      }else{
        stop("Missing conditions") # check
      }
      
    }
    
    ids[i, 'chem.tokeep'] <- treat.tokeep
    ids[i, 'chem.check'] <- TRUE
    
  }else if(length(unique(di$chemicalCor)) == 1){
    
    cat(paste0(" > Only one chemical. treat.\n"))
    treat.tokeep <- unique(di$chemicalCor)
    ids[i, 'chem.tokeep'] <- treat.tokeep
    ids[i, 'chem.check'] <- TRUE
    
  }else{
    stop("Missing conditions") # check
  }
  
  # Summary
  if(any(!ids$chem.check, na.rm = TRUE)){stop()} # check
  nrow.before <- nrow(di)
  di <- di[di$chemicalCor %in% ids[i, 'chem.tokeep'], ]
  if(length(unique(di$chemicalCor)) > 1){stop()} # check
  nrow.after <- nrow(di)
  if(nrow.before!=nrow.after){
    message(paste0('   => Loosing ', nrow.before-nrow.after,' rows (out of ',nrow.before,')'))
  }
  
  # STORAGE
  ids[i, 'stor.check']  <- FALSE
  # some storage conditions may correspond to chilling
  # moist + cold
  for(s in 1:nrow(di)){
    # between -20 and 10 => we consider these as chilling, not storage
    if(di[s, 'storageType']  %in% c("moist", "moist/cold") & !is.na(as.numeric(di[s, 'storageTemp'])) &
       as.numeric(di[s, 'storageTemp']) <= 10 & as.numeric(di[s, 'storageTemp']) >= - 20){
      di[s, 'storageType'] <- di[s, 'storageTemp'] <- di[s, 'storageDuration'] <- NA
    }
  }
  
  di$storConditions <- paste(di$storageType, di$storageTemp, di$storageDuration)
  if(length(unique(di$storConditions)) > 1){
    
    cat(paste0(" > Number of stor. treat.: ", length(unique(di$storConditions)), "\n"))
    cat('   - [');cat(paste(unique(di$storConditions)), sep = ', ');cat(']\n')
    
    ntreati <- di %>%
      dplyr::group_by(storConditions) %>%
      dplyr::reframe(nstrat = n_distinct(dormancyTemp, dormancyDuration),
                     ngerm = n_distinct(germTempGen, germDuration),
                     ninterest = nstrat + ngerm) %>% as.data.frame()
    
    # we keep misc. with max. no. of chill/forc treatments
    treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'storConditions']
    
    if(length(treat.tokeep) == 1){
      
      cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat.)\n'))
      
    }else if(length(treat.tokeep) > 1){
      
      maxresp <- max(di[di$storConditions %in% treat.tokeep, 'responseValue'])
      treat.tokeep <- unique(di[di$storConditions %in% treat.tokeep & di$responseValue == maxresp, 'storConditions']) 
      cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + max. resp.)\n'))
      
      if(length(treat.tokeep) > 1){
        stop("Missing conditions")
      }
      
    }else{
      stop("Missing conditions") # check
    }
    
    ids[i, 'stor.tokeep'] <- treat.tokeep
    ids[i, 'stor.check'] <- TRUE
    
    
  }else if(length(unique(di$storConditions)) == 1){
    
    cat(paste0(" > Only one stor. treat.\n"))
    treat.tokeep <- unique(di$storConditions)
    ids[i, 'stor.tokeep'] <- treat.tokeep
    ids[i, 'stor.check'] <- TRUE
    
  }else{
    stop("Missing conditions") # check
  }
  
  
}

newd <- data.frame()
for(i in 1:nrow(ids)){
  
  di <- particulard[paste0(particulard$datasetID,particulard$study,particulard$genusspecies) == paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
  di <- di[di$other.treatment %in% ids[i, 'misc.tokeep'], ]
  di <- di[di$scarifType %in% ids[i, 'scarif.tokeep'], ]
  di <- di[di$chemicalCor %in% ids[i, 'chem.tokeep'], ]
  di$storConditions <- paste(di$storageType, di$storageTemp, di$storageDuration)
  di <- di[di$storConditions %in% ids[i, 'stor.tokeep'], ]
  newd <- rbind(newd, di)
  
}

newd$germTempGen <- as.numeric(newd$germTempGen)
newd$germDuration <- as.numeric(newd$germDuration)
newd$chillTemp <- as.numeric(newd$chillTemp)
newd$chillDuration <- as.numeric(newd$chillDuration)
ggplot(data = newd[newd$datasetID == 'raisi13',]) +
  geom_point(aes(x = germTempGen*germDuration , y = responseValue, col = chillTemp*chillDuration))+
  geom_line(aes(x = germTempGen*germDuration , y = responseValue, col = chillTemp*chillDuration, group = paste0(datasetID,study,genusspecies, chillTemp))) +
  theme(legend.position = 'none')


singularities <- newd %>%
  dplyr::group_by(datasetID, study, genusspecies) %>%
  dplyr::reframe(nstorage = n_distinct(storageType, storageTemp, storageDuration, storageDetails),
                 nscarif = n_distinct(scarifType, scarifTypeGen, scarifTypeSpe),
                 nchem = n_distinct(chemicalCor),
                 nother = n_distinct(other.treatment),
                 nstrat = n_distinct(dormancyTemp, dormancyDuration),
                 ngerm = n_distinct(germTempGen, germDuration)) %>%
  dplyr::filter(nstrat > 1 | ngerm > 1) %>%
  as.data.frame() 




