

particularStudies <- unitreat[unitreat$nscarif > 1 | unitreat$nchem > 1, c('datasetID', 'study', 'genusspecies')]

chem.toremove <- unique(particulard[grepl('GA|BAP|ABA|hormone|regulator|kinetin|fertilizer|probiotic|herbicide|fungicide|captan|PEG|KNO3|IBA|thiourea', 
                                          particulard$chemicalCor),'chemicalCor'])
chem.notremoved <- unique(particulard[!grepl('GA|BAP|ABA|hormone|regulator|kinetin|fertilizer|probiotic|herbicide|fungicide|captan|PEG|KNO3|IBA|thiourea', 
                                             particulard$chemicalCor),'chemicalCor'])
chem.notremoved

misc.toremove <- unique(particulard[grepl('stress', 
                                          particulard$other.treatment),'other.treatment'])


particulard <- d %>%
  dplyr::filter(paste0(datasetID,study,genusspecies) %in% paste0(particularStudies$datasetID,particularStudies$study,particularStudies$genusspecies)) %>%
  dplyr::filter(responseVar == 'percent.germ' & !(chemicalCor %in% chem.toremove) & !(other.treatment %in% misc.toremove)) 

particulard$responseValue <- as.numeric(particulard$responseValue)

singularities <- particulard %>%
  dplyr::group_by(datasetID, study, genusspecies) %>%
  dplyr::reframe(nstorage = n_distinct(storageType, storageTemp, storageDuration, storageDetails),
                 nscarif = n_distinct(scarifType, scarifTypeGen, scarifTypeSpe),
                 nchem = n_distinct(chemicalCor),
                 nother = n_distinct(other.treatment),
                 nstrat = n_distinct(chillTemp, chillDuration, chillTempUnc, chillTempCycle, chillLightCycle),
                 ngerm = n_distinct(germTempGen, germDuration)) %>%
  as.data.frame()
 
ids <- singularities[c('datasetID', 'study', 'genusspecies')]

newd <- data.frame()
ids$misc.tokeep <- ids$misc.check <- NA
ids$scarif.tokeep <- ids$scarif.check <- NA
ids$chem.tokeep <- ids$chem.check <- NA

for(i in 1:nrow(ids)){
  
  di <- particulard[paste0(particulard$datasetID,particulard$study,particulard$genusspecies) == paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
  cat(paste0("\nID: ", i, ' | ',paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ' '), "\n"))
  
  # MISC. TREATMENT
  ids[i, 'misc.check']  <- FALSE
  if(length(unique(di$other.treatment)) > 1){
    
    cat(paste0(" > Number of misc. treat.: ", length(unique(di$other.treatment)), "\n"))
    cat('  - [');cat(paste(unique(di$other.treatment)), sep = ', ');cat(']\n')
    treat.tokeep <- unique(di[di$responseValue %in% max(di$responseValue, na.rm = TRUE), 'other.treatment']) # we keep the one with max. germ. perc
    
    if(length(treat.tokeep) > 1){ # then, if needed, we keep misc. with max. no. of chill/forc treatments
      
      ntreati <- di %>%
        dplyr::group_by(other.treatment) %>%
        dplyr::reframe(nstrat = n_distinct(chillTemp, chillDuration, chillTempUnc, chillTempCycle, chillLightCycle),
                       ngerm = n_distinct(germTempGen, germDuration),
                       ninterest = nstrat + ngerm) %>% as.data.frame()
      
      treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'other.treatment'] 

    }
    
    ids[i, 'misc.tokeep'] <- treat.tokeep
    ids[i, 'misc.check']  <- TRUE
    cat(paste0('   - Keeping: ', treat.tokeep,' (max. germ. perc observed)\n'))
    
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
  cat(paste0('   => Loosing ', nrow.before-nrow.after,' rows (out of ',nrow.before,')\n'))
  
  # SCARIFICATION
  ids[i, 'scarif.check']  <- FALSE
  if(length(unique(di$scarifType)) > 1){
    
    cat(paste0(" > Number of scarif. treat.: ", length(unique(di$scarifType)), "\n"))
    cat('   - [');cat(paste(unique(di$scarifType)), sep = ', ');cat(']\n')
    
    ntreati <- di %>%
      dplyr::group_by(scarifType) %>%
      dplyr::reframe(nscarif = n_distinct(scarifType, scarifTypeGen, scarifTypeSpe),
                     nstrat = n_distinct(chillTemp, chillDuration, chillTempUnc, chillTempCycle, chillLightCycle),
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
        cat(paste0('  - Keeping: ', treat.tokeep,' (max. germ. rate observed)\n'))
        
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
          cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + scarified + max. germ. perc.)\n'))
          
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
  cat(paste0('   => Loosing ', nrow.before-nrow.after,' rows (out of ',nrow.before,')\n'))
  
  # CHEMICAL
  ids[i, 'chem.check']  <- FALSE
  if(length(unique(di$chemicalCor)) > 1){
    
    cat(paste0(" > Number of chem. treat.: ", length(unique(di$chemicalCor)), "\n"))
    cat('  - [');cat(paste(unique(di$chemicalCor)), sep = ', ');cat(']\n')
    
    ntreati <- di %>%
      dplyr::group_by(chemicalCor) %>%
      dplyr::reframe(nstrat = n_distinct(chillTemp, chillDuration, chillTempUnc, chillTempCycle, chillLightCycle),
                     ngerm = n_distinct(germTempGen, germDuration),
                     ninterest = nstrat + ngerm) %>% as.data.frame()
    
    if(length(unique(ntreati$ninterest)) == 1){ # same no. of chill/forc treatments
      
      cat('   - Same number of chill./forc. treat. across chem. treat.\n')
      
      if(any(is.na(unique(di$chemicalCor)))){ # if there is a control, we keep it
        
        treat.tokeep <- NA
        ids[i, 'chem.tokeep'] <- treat.tokeep
        ids[i, 'chem.check'] <- TRUE
        
      }else if(FALSE){
        
        # here, we need to make a decision with concentration information?
        
      }
      
    }else if(length(unique(ntreati$ninterest)) > 1){ # different no. of chill/forc treatments
      
      cat('   - Different number of chill./forc. treat. across chem. treat.\n')
      # here, we need to make a de
      
    }
    
    
  }else if(length(unique(di$chemicalCor)) == 1){
    
    cat(paste0(" > Only one chemical. treat.\n"))
    treat.tokeep <- unique(di$chemicalCor)
    ids[i, 'chem.tokeep'] <- treat.tokeep
    ids[i, 'chem.check'] <- TRUE
    
  }
  
}

newd <- data.frame()
for(i in 1:nrow(ids)){
  
  di <- particulard[paste0(particulard$datasetID,particulard$study,particulard$genusspecies) == paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
  di <- di[di$other.treatment %in% ids[i, 'misc.tokeep'], ]
  di <- di[di$scarifType %in% ids[i, 'scarif.tokeep'], ]
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
                 nstrat = n_distinct(chillTemp, chillDuration, chillTempUnc, chillTempCycle, chillLightCycle),
                 ngerm = n_distinct(germTempGen, germDuration)) %>%
  as.data.frame()
