# Started Feb 10, 2025
# Aim: selecting some treatments/experiments to include in the model, following various decision rules
# by Victor

if(length(grep("deirdre", getwd()) > 0)) {
  setwd("~/Documents/github/egret/analyses")
} else if(length(grep("lizzie", getwd()) > 0)) {
  setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
} else if(length(grep("sapph", getwd()) > 0)) {
  setwd("/Users/sapph/Documents/ubc things/work/egret/analyses")
} else if(length(grep("danielbuonaiuto", getwd()) > 0)) {
  setwd("/Users/danielbuonaiuto/Documents/git/egret/analyses")
} else if(length(grep("Xiaomao", getwd()) > 0)) {
  setwd("C:/PhD/Project/egret/analyses")
} else if(length(grep("britanywuuu", getwd()) > 0)) {
  setwd("/Documents/ubc/year5/TemporalEcologyLab/egret/analyses")
} else if(length(grep("Ken", getwd())) > 0){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/egret/analyses")
} else if(length(grep("christophe_rouleau-desrochers", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# 0. Get the cleaned data
d <- read.csv('output/egretclean.csv')
treats <- c('datasetID', 'study',
            'genus', 'species', 'variety',
            
            "treatment", # I guess it's a summary?
            
            # STORAGE-RELATED
            'storageType', 'storageTemp', "storageDuration", "storageDetails",
            
            # SCARIFICATION-RELATED
            "scarifType", "scarifTypeGen", "scarifTypeSpe", 
            
            # CHEMICAL-RELATED (warning: concentration not cleaned)
            "chemicalCor", "chemical.concent",
            
            # SOAKING-RELATED (IMBIBITION)
            "soaking", "soaked.in", "soaking.duration", # I guess it's not cleaned?
            
            # STRATIFICATION-RELATED
            "chillTemp", "chillDuration", "chillTempUnc", "chillTempCycle", "chillLightCycle",
            
            # STRAT + STORAGE
            "dormancyTemp", "dormancyDuration",
            
            # GERMINATION-RELATED
            "germTempGen", "germTemp", "germDuration", "tempClass", "tempDay", "tempNight",
            "germPhotoperiod", "germPhotoperiodDay", "germPhotoperiodNight", "photoperiodCor",
            
            # MISC (e.g. sowing depth)
            "other.treatment"
            
)

# RESPONSE
resp <- c("responseVar", "responseValue")

# Unique treatments per study
dtreat <- unique(d[,treats])
dtreat$uniqueID <- 1:nrow(dtreat)
d <- unique(d[, c(treats, resp)])
d <- merge(x = d, y = dtreat, by = treats, all = TRUE)
d$genusspecies <- paste0(d$genus, '_', d$species)

# Save initial number of unique dataset*experiment*species
oldids <- unique(d[c('datasetID', 'study', 'genusspecies')])

# First veto: response variable
priority <- c("percent.germ") # here, just in case, we could set several responses variables we want to prioritize
d <- d[order(d$uniqueID, match(d$responseVar, priority)), ]
d <- d[!duplicated(d$uniqueID), ]
filteredd <- d[d$responseVar == 'percent.germ',]
filteredd$responseValue <- as.numeric(filteredd$responseValue)

# Second veto: chemical we absolutely want to remove
vetochems <- 'GA|BAP|ABA|hormone|regulator|kinetin|fertilizer|probiotic|herbicide|fungicide|captan|PEG|KNO3|IBA|thiourea|benzyladenine|Tween'
chem.toremove <- unique(d[grepl(vetochems, d$chemicalCor),'chemicalCor'])
# chem.notremoved <- unique(d[!grepl(vetochems, d$chemicalCor),'chemicalCor'])
# chem.notremoved
filteredd <- filteredd[!(filteredd$chemicalCor %in% chem.toremove),]

# Third veto: misc. treatment we absolutely want to remove
misc.toremove <- unique(d[grepl('stress', d$other.treatment),'other.treatment'])
filteredd <- filteredd[!(filteredd$chemicalCor %in% misc.toremove),]

# For Ken: id of studies remaining at this step
# ids <- unique(filteredd[c('datasetID', 'study', 'genusspecies')])
# saveRDS(ids, file.path('studyDesign', 'ids_for_ken', 'ids_after_step3.rds'))

# Fourth veto: no info on germination temperature
filteredd <- filteredd[!is.na(filteredd$germTempGen),]

ids <- unique(filteredd[c('datasetID', 'study', 'genusspecies')])
ids <- ids[!(ids$datasetID == 'forbes09' & ids$study == 'exp20'),]
ids <- ids[!(ids$datasetID == 'kolodziejek19' & ids$study == 'exp0'),] # I don't understand photoperiodCor in this study, nor cleanPhotoperiod.R, so I don't want to make any mistake (TO CHECK!)

ids$misc.tokeep <- ids$misc.check <- NA
ids$scarif.tokeep <- ids$scarif.check <- NA
ids$chem.tokeep <- ids$chem.check <- NA
ids$stor.tokeep <- ids$stor.check <- NA
ids$photo.tokeep <- ids$photo.check <- NA
for(i in 1:nrow(ids)){
  
  di <- filteredd[paste0(filteredd$datasetID,filteredd$study,filteredd$genusspecies) == paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
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
    
    # Base R solution? but drop NA group, will look into it later
    # ntreati <- aggregate(
    #   list(nstrat = paste(di$dormancyTemp, di$dormancyDuration, sep = "_"), 
    #        ngerm = paste(di$germTempGen, di$germDuration, sep = "_")),
    #   by = list(other.treatment = di$other.treatment),
    #   FUN = function(x) length(unique(x))
    # )
    # ntreati$ninterest <- ntreati$nstrat + ntreati$ngerm
    
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
        
        ids[i, 'scarif.tokeep'] <- treat.tokeep
        ids[i, 'scarif.check']  <- TRUE
        
        
      }else if(length(unique(di$scarifType)) > 2){
        
        treat.tokeep <- unique(di[di$responseValue == max(di$responseValue), 'scarifType']) # we keep scarif. with max. germ. rate
        cat(paste0('   - Keeping: ', treat.tokeep,' (max. germ. rate observed)\n'))
        
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
      
    }else if(length(treat.tokeep) > 1){ # if not enough...
      
      maxresp <- max(di[di$storConditions %in% treat.tokeep, 'responseValue'], na.rm = TRUE)
      treat.tokeep <- unique(di[di$storConditions %in% treat.tokeep & di$responseValue %in% maxresp, 'storConditions']) 
      cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + max. resp.)\n'))
      
      if(length(treat.tokeep) > 1){
        
        avgrespi <- di %>%
          dplyr::group_by(storConditions) %>%
          dplyr::filter(storConditions %in% treat.tokeep) %>%
          dplyr::reframe(avgresp = mean(responseValue, na.rm = TRUE)) %>%
          as.data.frame()
        
        treat.tokeep <- avgrespi[avgrespi$avgresp == max(avgrespi$avgresp), 'storConditions']
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + max. resp. + max. avg. resp.)\n'))
        
        if(length(treat.tokeep) > 1){
          stop("Missing conditions") # check
        }
        
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
  
  # Summary
  if(any(!ids$stor.check, na.rm = TRUE)){stop()} # check
  nrow.before <- nrow(di)
  di <- di[di$storConditions %in% ids[i, 'stor.tokeep'], ]
  if(length(unique(di$storConditions)) > 1){stop()} # check
  nrow.after <- nrow(di)
  if(nrow.before!=nrow.after){
    message(paste0('   => Loosing ', nrow.before-nrow.after,' rows (out of ',nrow.before,')'))
  }
  
  # PHOTOPERIOD
  ids[i, 'photo.check']  <- FALSE
  if(length(unique(di$germPhotoperiod)) > 1){
    
    cat(paste0(" > Number of photo. treat.: ", length(unique(di$germPhotoperiod)), "\n"))
    cat('   - [');cat(paste(unique(di$germPhotoperiod)), sep = ', ');cat(']\n')
    
    ntreati <- di %>%
      dplyr::group_by(germPhotoperiod) %>%
      dplyr::reframe(nstrat = n_distinct(dormancyTemp, dormancyDuration),
                     ngerm = n_distinct(germTempGen, germDuration),
                     ninterest = nstrat + ngerm) %>% as.data.frame()
    
    # we keep misc. with max. no. of chill/forc treatments
    treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'germPhotoperiod']
    
    if(length(treat.tokeep) == 1){
      
      cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat.)\n'))
      
    }else if(length(treat.tokeep) > 1){ # if not enough...
      
      maxresp <- max(di[di$germPhotoperiod %in% treat.tokeep, 'responseValue'], na.rm = TRUE)
      treat.tokeep <- unique(di[di$germPhotoperiod %in% treat.tokeep & di$responseValue %in% maxresp, 'germPhotoperiod']) 
      
      if(length(treat.tokeep) > 1){
        
        avgrespi <- di %>%
          dplyr::group_by(germPhotoperiod) %>%
          dplyr::filter(germPhotoperiod %in% treat.tokeep) %>%
          dplyr::reframe(avgresp = mean(responseValue, na.rm = TRUE)) %>%
          as.data.frame()
        
        treat.tokeep <- avgrespi[avgrespi$avgresp == max(avgrespi$avgresp), 'germPhotoperiod']
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + max. resp. + max. avg. resp.)\n'))
        
        if(length(treat.tokeep) > 1){
          stop("Missing conditions") # check
        }
        
      }else if(length(treat.tokeep) == 1){
        
        cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat. + max. resp.)\n'))
        
      }else{
        stop("Missing conditions") # check
      }
      
    }else{
      stop("Missing conditions") # check
    }
    
    ids[i, 'photo.tokeep'] <- treat.tokeep
    ids[i, 'photo.check'] <- TRUE
    
    
  }else if(length(unique(di$germPhotoperiod)) == 1){
    
    cat(paste0(" > Only one photo. treat.\n"))
    treat.tokeep <- unique(di$germPhotoperiod)
    ids[i, 'photo.tokeep'] <- treat.tokeep
    ids[i, 'photo.check'] <- TRUE
    
  }else{
    stop("Missing conditions") # check
  }
  
  # Summary
  if(any(!ids$photo.check, na.rm = TRUE)){stop()} # check
  nrow.before <- nrow(di)
  di <- di[di$germPhotoperiod %in% ids[i, 'photo.tokeep'], ]
  if(length(unique(di$germPhotoperiod)) > 1){stop()} # check
  nrow.after <- nrow(di)
  if(nrow.before!=nrow.after){
    message(paste0('   => Loosing ', nrow.before-nrow.after,' rows (out of ',nrow.before,')'))
  }
  
}

# Create the new dataset
newd <- data.frame()
for(i in 1:nrow(ids)){
  
  di <- filteredd[paste0(filteredd$datasetID,filteredd$study,filteredd$genusspecies) == paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
  di <- di[di$other.treatment %in% ids[i, 'misc.tokeep'], ]
  di <- di[di$scarifType %in% ids[i, 'scarif.tokeep'], ]
  di <- di[di$chemicalCor %in% ids[i, 'chem.tokeep'], ]
  
  # some storage conditions may correspond to chilling (moist + cold)
  for(s in 1:nrow(di)){
    # between -20 and 10 => we consider these as chilling, not storage
    if(di[s, 'storageType']  %in% c("moist", "moist/cold") & !is.na(as.numeric(di[s, 'storageTemp'])) &
       as.numeric(di[s, 'storageTemp']) <= 10 & as.numeric(di[s, 'storageTemp']) >= - 20){
      di[s, 'storageType'] <- di[s, 'storageTemp'] <- di[s, 'storageDuration'] <- NA
    }
  }
  
  di$storConditions <- paste(di$storageType, di$storageTemp, di$storageDuration)
  di <- di[di$storConditions %in% ids[i, 'stor.tokeep'], ]
  di <- di[di$germPhotoperiod %in% ids[i, 'photo.tokeep'], ]
  newd <- rbind(newd, di)
  
}

# ntreats <- newd %>%
#   dplyr::filter(!is.na(germDuration) & !is.na(germTemp)) %>%
#   dplyr::group_by(datasetID, study, genusspecies) %>%
#   dplyr::reframe(nstrat = n_distinct(dormancyTemp, dormancyDuration),
#                  ngerm = n_distinct(germTempGen, germDuration)) %>% 
#   as.data.frame()
# newids <- unique(ntreats[c('datasetID', 'study', 'genusspecies')])
# 
# newids_chill <- unique(ntreats[ntreats$nstrat > ntreats$ngerm, c('datasetID', 'study', 'genusspecies')])
# newids_forc <- unique(ntreats[ntreats$ngerm >= ntreats$nstrat, c('datasetID', 'study', 'genusspecies')])

# pdf(file=paste0("figures/studyDesign/chillhours.pdf"), height = 15, width = 18)
# par(mfrow = c(11,13), mar=c(1.4,0,0,0)+0.7, mgp=c(0,0.5,0))
# for(i in 1:nrow(newids_chill)){
# 
#   di <- newd[paste0(newd$datasetID,newd$study,newd$genusspecies) == 
#                paste0(newids_chill[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),
#              c('dormancyDuration', 'dormancyTemp', 
#                'germTempGen', 'germDuration', 'germPhotoperiod',
#                'responseValue')]
#   
#   di$chillhours <- as.numeric(di$dormancyDuration) * 24 * as.numeric(as.numeric(di$dormancyTemp) < 10 & as.numeric(di$dormancyTemp) > -20)
#   di$forc <- as.numeric(di$germDuration) * as.numeric(di$germTempGen)
#   
#   plot.new()
#   limits <- c(min(di$chillhours, na.rm = T), max(di$chillhours, na.rm = T))
#   plot.window(xlim = limits, ylim = c(0,100))
#   grid()
#   
#   points(di$responseValue ~ di$chillhours, pch = 19, col = '#498ba7', cex = 0.5)
#   # for(f in unique(paste0(di$germTempGen, di$germDuration))){
#   #   subi <- di[paste0(di$germTempGen, di$germDuration) == f, ]
#   #   lines(subi$responseValue ~ subi$chillhours, col = '#498ba7')
#   # }
#   
#   title(ylab = "Germ. perc.", cex.lab = 0.5)
#   title(xlab = "Chill hours", cex.lab = 0.5, line = -0.1)
#   title(paste("ID", i, paste(newids_chill[i,c('datasetID', 'study', 'genusspecies')], collapse = '|')), adj=0, cex.main = 0.5)
#   
# }
# dev.off()
# 
# pdf(file=paste0("figures/studyDesign/forcingtime.pdf"), height = 15, width = 18)
# par(mfrow = c(11,13), mar=c(1.4,0,0,0)+0.7, mgp=c(0,0.5,0))
# for(i in 1:nrow(newids_forc)){
# 
#   di <- newd[paste0(newd$datasetID,newd$study,newd$genusspecies) == 
#                paste0(newids_forc[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),
#              c('dormancyDuration', 'dormancyTemp', 
#                'germTempGen', 'germDuration', 'germPhotoperiod',
#                'responseValue')]
#   
#   di$chillhours <- as.numeric(di$dormancyDuration) * 24 * as.numeric(as.numeric(di$dormancyTemp) < 10 & as.numeric(di$dormancyTemp) > -20)
#   di$forc <- as.numeric(di$germTempGen)
#   di$t <- as.numeric(di$germDuration)
#   
#   plot.new()
#   limits <- c(min(di$t, na.rm = T), max(di$t, na.rm = T))
#   plot.window(xlim = limits, ylim = c(0,100))
#   grid()
#   
#   points(di$responseValue ~ di$t, pch = 19, col = '#498ba7', cex = 0.5)
#   # for(f in unique(paste0(di$germTempGen, di$germDuration))){
#   #   subi <- di[paste0(di$germTempGen, di$germDuration) == f, ]
#   #   lines(subi$responseValue ~ subi$chillhours, col = '#498ba7')
#   # }
#   
#   title(ylab = "Germ. perc.", cex.lab = 0.5)
#   title(xlab = "Time", cex.lab = 0.5, line = -0.1)
#   title(paste("ID", i, paste(newids_forc[i,c('datasetID', 'study', 'genusspecies')], collapse = '|')), adj=0, cex.main = 0.5)
#   
# }
# dev.off()

rm(list=ls()[which(!(ls() %in% c('d', 'newd')))]) 
