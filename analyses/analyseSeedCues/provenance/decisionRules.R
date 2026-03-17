# Started Feb 10, 2025
# Aim: selecting some treatments/experiments to include in the model, following various decision rules
# by Victor

# This specific script is for the species*provenance model
# Started Oct. 28

rm(list=ls()) 

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
  setwd("/Users/christophe_rouleau-desrochers/github/egret/analyses")
} else if(length(grep("crouleau", getwd())) > 0){
  setwd("/home/crouleau/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# 0. Get the cleaned data
source('cleaning/cleanAll.R')
originald <- d
source('analyseSeedCues/summarizeStrat.R')
#d <- read.csv('output/egretclean.csv')
rm(list=ls()[which(!(ls() %in% c('originald', 'd', 'idsnotcorrected')))])

treats <- c('datasetID', 'study',
            'genus', 'species', 'variety',
            
            'provLatLonAlt', # seems useful
            
            "treatment", # I guess it's a summary?
            
            # STORAGE-RELATED (without conditions equivalent to strat., if last)
            'storageType', 'storageNoStratTemp', "storageNoStratDur", "storageDetails",
            
            # SCARIFICATION-RELATED
            "scarifTypeGen", "scarifTypeSpe", #scarifType was discarded?(without any notice...)
            
            # CHEMICAL-RELATED 
            "chemicalCor", "chemicalConcent",
            
            # SOAKING-RELATED (IMBIBITION)
            "soaking", "soaked.in", "soaking.duration", # I guess it's not cleaned?
            
            # STRATIFICATION-RELATED
            "chillTemp", "chillDuration", "chillTempUnc", "chillTempCycle", "chillLightCycle",
            'stratSequence_condensed', # new fancy column
            
            # STRAT + STORAGE
            "stratTemp_condensed", "stratDur_condensed",
            
            # GERMINATION-RELATED
            "germTempGen", "germTemp", "germDuration", "germTempClass", "germTempDay", "germTempNight",
            "germPhotoperiod", "germPhotoperiodDay", "germPhotoperiodNight", # "photoperiodCor",
            
            # MISC (e.g. sowing depth)
            "other.treatment"
            
)


# RESPONSE
resp <- c("responseVar", "responseValueNum")

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
filteredd$responseValueNum <- as.numeric(filteredd$responseValueNum)

# Second veto: chemical we absolutely want to remove
vetochems <- 'GA|BAP|ABA|hormone|regulator|kinetin|fertilizer|probiotic|herbicide|fungicide|captan|PEG|KNO3|IBA|thiourea|benzyladenine|Tween'
chem.toremove <- unique(d[grepl(vetochems, d$chemicalCor),'chemicalCor'])
# chem.notremoved <- unique(d[!grepl(vetochems, d$chemicalCor),'chemicalCor'])
# chem.notremoved
filteredd <- filteredd[!(filteredd$chemicalCor %in% chem.toremove),]

# Second veto updated: chemical --- during soaking --- we absolutely want to remove
vetochems_soaking <- 'GA|BA|ABA|Tween|fungicide|Benomyl|captan|Ethephon|ACC|cytokinin|paclobutrazol|KNO3|KN'
chem.toremove <- unique(d[grepl(vetochems_soaking, d$soaked.in),'soaked.in'])
chem.notremoved <- unique(d[!grepl(vetochems_soaking, d$soaked.in),'soaked.in'])
filteredd <- filteredd[!(filteredd$soaked.in %in% chem.toremove),]

# Third veto: misc. treatment we absolutely want to remove
misc.toremove <- unique(d[grepl('stress', d$other.treatment),'other.treatment'])
filteredd <- filteredd[!(filteredd$chemicalCor %in% misc.toremove),]

# For Ken: id of studies remaining at this step
ids <- unique(filteredd[c('datasetID', 'study', 'genusspecies')])
write.csv(ids, file.path('studyDesign', 'ids_for_ken', 'ids_after_step3.csv'))

# Fourth veto: no info on germination temperature
filteredd <- filteredd[!(filteredd$germTempGen %in% c(NA, 'NA')),]

# Here I'm discarding studies where I knopw the germTempGen could not be corrected earlier (~ 300 rows, who cares?!)
for(i in 1:nrow(idsnotcorrected)){
  filteredd <- filteredd[which(!(filteredd$datasetID %in% idsnotcorrected[i, 'datasetID'] & filteredd$study %in% idsnotcorrected[i, 'study'] &
                                   filteredd$genus %in% idsnotcorrected[i, 'genus'] & filteredd$species %in% idsnotcorrected[i, 'species'])),]
}

ids <- unique(filteredd[c('datasetID', 'study', 'genusspecies')])
ids <- ids[!(ids$datasetID == 'forbes09' & ids$study == 'exp20'),]
ids <- ids[!(ids$datasetID == 'kolodziejek19' & ids$study == 'exp0'),] # I don't understand photoperiodCor in this study, nor cleanPhotoperiod.R, so I don't want to make any mistake (TO CHECK!)

ids$misc.tokeep <- ids$misc.check <- NA
ids$scarif.tokeep <- ids$scarif.check <- NA
ids$chem.tokeep <- ids$chem.check <- NA
ids$soak.tokeep <- ids$soak.check <- NA
ids$stor.tokeep <- ids$stor.check <- NA
ids$photo.tokeep <- ids$photo.check <- NA
ids$warmstrat.tokeep <- ids$warmstrat.check <- NA 

## A particular case of misc. treatment: the color of the seeds
# We want to keep both brown and black, so we're dealing with this
# before entering the loop (it's easier, I'm lazy)
filteredd[filteredd$other.treatment %in% c('brown seeds', 'black seeds') &
            filteredd$datasetID == 'nurse08', 'other.treatment'] <- 'brown/black seeds'

## Another particular case of misc. treatment: the color of the seeds
filteredd[filteredd$other.treatment %in% c('Brown seed', 'Black seed') &
            filteredd$datasetID == 'li11', 'other.treatment'] <- 'brown/black seeds'

## Another particular case of misc. treatment: some locations that should not be here
# It's also in the source.population
filteredd[filteredd$datasetID == 'airi09', 'other.treatment'] <- NA

## Another particular case of misc. treatment: some locations that should not be here
filteredd[filteredd$datasetID == 'crank92', 'other.treatment'] <- NA

## Another particular case of misc. treatment: strat. should not be here
filteredd[filteredd$datasetID == 'battaglia93' & filteredd$study == 'exp4', 'other.treatment'] <- NA

## Another particular case of misc. treatment: scarification should not be here
filteredd[filteredd$datasetID == 'wytsalucy21' & filteredd$study == 'exp1', 'other.treatment'] <- NA

## Another particular case of misc. treatment: cold stratification should not be here
filteredd[filteredd$datasetID == 'barros12' & filteredd$study == 'exp1', 'other.treatment'] <- NA

## Another particular case of misc. treatment: cold stratification should not be here
filteredd[filteredd$datasetID == 'veiga-barbosa14' & filteredd$study == 'exp1', 'other.treatment'] <- NA

## But 'after-ripening' should also be in other.treatment
filteredd[filteredd$datasetID %in% 'tang21'& filteredd$study %in% 'exp1' & grepl('ripening', filteredd$treatment),'other.treatment'] <- 
  filteredd[filteredd$datasetID %in% 'tang21'& filteredd$study %in% 'exp1' & grepl('ripening', filteredd$treatment),'treatment']

## In this study, there is a problem of thermoperiod vs. photoperiod, so doing this for now
filteredd[filteredd$datasetID == 'alhelal96' & filteredd$study == 'exp1', 'other.treatment'] <- filteredd[filteredd$datasetID == 'alhelal96' & filteredd$study == 'exp1', 'treatment'] 

## Added Oct 28:
subsetdataset_christophe <- read.csv("output/moreThan1Prov.csv")
helpful <- unique(subsetdataset_christophe[,c('datasetID', 'study', 'genus', 'species')])
helpful$genusspecies <- paste0(helpful$genus, '_', helpful$species)
ids <- ids[paste0(ids$datasetID, ids$study, ids$genusspecies) %in% paste0(helpful$datasetID, helpful$study, helpful$genusspecies) ,]

# -----------------------------
# FOR PEOPLE CHECKING: MODIFY HERE
allids <- 1:nrow(ids)
# example: Lizzie is doing 1:100
# allids <- 201:250

for(i in allids){
  
  di <- filteredd[paste0(filteredd$datasetID,filteredd$study,filteredd$genusspecies) == paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
  cat(paste0("\nID: ", i, ' | ',paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ' '), "\n"))
  # if(unique(di$datasetID) %in% 'seng20'){
  #   stop(i)
  # }
  
  # MISC. TREATMENT
  ids[i, 'misc.check']  <- FALSE
  if(length(unique(di$other.treatment)) > 1){
    
    cat(paste0(" > Number of misc. treat.: ", length(unique(di$other.treatment)), "\n"))
    cat('   - [');cat(paste(unique(di$other.treatment)), sep = ', ');cat(']\n')
    
    di$nstrat <- paste0(di$stratTemp_condensed, di$stratDur_condensed)
    di$ngerm <- paste0(di$germTempGen, di$germDuration)
    ntreati <- merge(
      aggregate(nstrat ~ factor(other.treatment, exclude = NULL), data = di, function(x) length(unique(x))),
      aggregate(ngerm ~ factor(other.treatment, exclude = NULL), data = di, function(x) length(unique(x)))
    )
    ntreati$ninterest <- ntreati$nstrat + ntreati$ngerm
    names(ntreati)[1] <- 'other.treatment'
    
    # we keep misc. with max. no. of chill/forc treatments
    treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'other.treatment'] 
    
    if(length(treat.tokeep) > 1){ # then, if needed, we keep the one with max. resp.
      
      maxresp <- max(di[di$other.treatment %in% treat.tokeep, 'responseValueNum'], na.rm = TRUE)
      treat.tokeep <- unique(di[di$other.treatment %in% treat.tokeep & di$responseValueNum %in% maxresp, 'other.treatment']) 
      
      if(length(treat.tokeep) > 1 & any(is.na(treat.tokeep))){ # if still not enough, we keep the control (if there is one)
        
        treat.tokeep <- NA
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + max. resp. + control)\n'))
        
      }else if(length(treat.tokeep) > 1 & !any(is.na(treat.tokeep))){ # if still not enough, we keep the one with max. average resp.
        
        avgrespi <- aggregate(responseValueNum ~ factor(other.treatment, exclude = NULL), data = di[di$other.treatment %in% treat.tokeep,], mean)
        names(avgrespi)[1] <- 'other.treatment'
        
        treat.tokeep <- avgrespi[avgrespi$responseValueNum == max(avgrespi$responseValueNum), 'other.treatment']
        if(length(treat.tokeep) > 1){
          
          stop('Missing conditions (line 219)')
          
        }
        
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + max. resp. + control + max. avg. resp.)\n'))
        
      }else if(length(treat.tokeep) == 1){
        
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + max. resp.)\n'))
        
      }else{
        stop('Missing conditions (line 184)')
      }
      
    }else if(length(treat.tokeep) == 1){
      
      cat(paste0('   - Keeping: ', treat.tokeep,' (max. no. of strat./forc. treat.)\n'))
      
    }else{
      stop('Missing conditions (line 192)')
    }
    
    ids[i, 'misc.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'misc.check']  <- TRUE
    
    
  }else if(length(unique(di$other.treatment)) == 1){
    
    cat(paste0(" > No misc. treat.\n"))
    treat.tokeep <- unique(di$other.treatment)
    ids[i, 'misc.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'misc.check']  <- TRUE
    
  }else{
    stop('Missing conditions (line 207)') # check
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
  if(length(unique(di$scarifTypeSpe)) > 1){
    
    cat(paste0(" > Number of scarif. treat.: ", length(unique(di$scarifTypeSpe)), "\n"))
    cat('   - [');cat(paste(unique(di$scarifTypeSpe)), sep = ', ');cat(']\n')
    
    di$nstrat <- paste0(di$stratTemp_condensed, di$stratDur_condensed)
    di$ngerm <- paste0(di$germTempGen, di$germDuration)
    ntreati <- merge(aggregate(nstrat ~ factor(scarifTypeSpe, exclude = NULL), data = di, function(x) length(unique(x))),
                     aggregate(ngerm ~ factor(scarifTypeSpe, exclude = NULL), data = di, function(x) length(unique(x))))
    ntreati$ninterest <- ntreati$nstrat + ntreati$ngerm
    names(ntreati)[1] <- 'scarifTypeSpe'
    
    if(length(unique(ntreati$ninterest)) == 1){
      
      cat('   - Same number of strat./forc. treat. across scarif. treat.\n')
      
      if(length(unique(di$scarifTypeSpe)) == 2 & any(is.na(unique(di$scarifTypeSpe)))){ # if only two scarif., and one is control
        
        treat.tokeep <- unique(di$scarifTypeSpe)[which(!is.na(unique(di$scarifTypeSpe)))] # we keep scarified
        cat(paste0('   - Keeping: ', treat.tokeep,' (instead of control)\n'))
        
        ids[i, 'scarif.tokeep'] <- as.character(treat.tokeep)
        ids[i, 'scarif.check']  <- TRUE
        
        
      }else if(length(unique(di$scarifTypeSpe)) > 2){
        
        treat.tokeep <- unique(di[di$responseValueNum == max(di$responseValueNum), 'scarifTypeSpe']) # we keep scarif. with max. germ. rate
        
        if(length(treat.tokeep) > 1){
          
          stop('Missing conditions (line 219)')
          
        }
        cat(paste0('   - Keeping: ', treat.tokeep,' (max. germ. rate observed)\n'))
        
        ids[i, 'scarif.tokeep'] <- as.character(treat.tokeep)
        ids[i, 'scarif.check']  <- TRUE
        
      }else{
        stop('Missing conditions (line 262)') # check
      }
      
    }else if(length(unique(ntreati$ninterest)) > 1){
      
      cat('   - Different number of strat./forc. treat. across scarif. treat.\n')
      treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'scarifTypeSpe'] # we keep scarif. with max. no. of chill/forc treatments
      
      if(length(treat.tokeep) > 1){ # then, if needed, we choose the treatment with max. germ. rate
        
        treat.tokeep <- treat.tokeep[which(!is.na(treat.tokeep))]
        cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + scarified)\n'))
        
        if(length(treat.tokeep) > 1){ # if still not enough, we keep the scarified treatment...
          
          maxresp <- max(di[di$scarifTypeSpe %in% treat.tokeep, 'responseValueNum'])
          treat.tokeep <- unique(di[di$scarifTypeSpe %in% treat.tokeep & di$responseValueNum == maxresp, 'scarifTypeSpe']) 
          cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + scarified + max. resp.)\n'))
          
        }
        
      }else if(length(treat.tokeep) == 1){
        
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat.)\n'))
        
      }
      
      ids[i, 'scarif.tokeep'] <- as.character(treat.tokeep)
      ids[i, 'scarif.check']  <- TRUE
      
    }else{
      stop('Missing conditions (line 293)') # check
    }
    
  }else if(length(unique(di$scarifTypeSpe)) == 1){
    
    cat(paste0(" > Only one scarif. treat.\n"))
    treat.tokeep <- unique(di$scarifTypeSpe)
    ids[i, 'scarif.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'scarif.check']  <- TRUE
    
  }else{
    stop('Missing conditions?') # check
  }
  
  # Summary
  if(any(!ids$scarif.check, na.rm = TRUE)){stop()} # check
  nrow.before <- nrow(di)
  di <- di[di$scarifTypeSpe %in% ids[i, 'scarif.tokeep'], ]
  if(length(unique(di$scarifTypeSpe)) > 1){stop()} # check
  nrow.after <- nrow(di)
  if(nrow.before!=nrow.after){
    message(paste0('   => Loosing ', nrow.before-nrow.after,' rows (out of ',nrow.before,')'))
  }
  
  # CHEMICAL (updated 26 July 2025, we don't care about control anymore)
  ids[i, 'chem.check']  <- FALSE
  di$chemicalTreat <- paste0(di$chemicalCor, di$chemicalConcent)
  if(length(unique(di$chemicalTreat)) > 1){
    
    cat(paste0(" > Number of chem. treat.: ", length(unique(di$chemicalTreat)), "\n"))
    cat('   - [');cat(paste(unique(di$chemicalTreat)), sep = ', ');cat(']\n')
    
    di$nstrat <- paste0(di$stratTemp_condensed, di$stratDur_condensed)
    di$ngerm <- paste0(di$germTempGen, di$germDuration)
    ntreati <- merge(
      aggregate(nstrat ~ factor(chemicalTreat, exclude = NULL), data = di, function(x) length(unique(x))),
      aggregate(ngerm ~ factor(chemicalTreat, exclude = NULL), data = di, function(x) length(unique(x)))
    )
    ntreati$ninterest <- ntreati$nstrat + ntreati$ngerm
    names(ntreati)[1] <- 'chemicalTreat'
    
    if(length(unique(ntreati$ninterest)) == 1){ # same no. of chill/forc treatments
      
      cat('   - Same number of strat./forc. treat. across chem. treat.\n')
      
      # we can just keep the chemical treat. with max. germ. rate?
      maxresp <- max(di$responseValueNum)
      treat.tokeep <- unique(di[di$responseValueNum == maxresp, 'chemicalTreat']) 
      cat(paste0('   - Keeping: [', treat.tokeep,'] (max. resp.)\n'))
      
      if(length(treat.tokeep) > 1){
        stop('Missing conditions (line 366)') # check
      }
      
      
    }else if(length(unique(ntreati$ninterest)) > 1){ # different no. of chill/forc treatments
      
      cat('   - Different number of strat./forc. treat. across chem. treat.\n')
      treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'chemicalTreat'] # chem. treat. with max. no. of chill/forc treatments
      
      if(length(treat.tokeep) > 1){
        maxresp <- max(di[di$chemicalTreat %in% treat.tokeep, 'responseValueNum'])
        treat.tokeep <- unique(di[di$chemicalTreat %in% treat.tokeep & di$responseValueNum == maxresp, 'chemicalTreat']) 
        
        if(length(treat.tokeep) > 1){
          
          stop("Missing conditions (line 468)") # check
          
        }
        
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + max. resp.)\n'))
      }else if(length(treat.tokeep) == 1){
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat.)\n'))
      }else{
        stop("Missing conditions (line 410)") # check
      }
      
    }
    
    ids[i, 'chem.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'chem.check'] <- TRUE
    
  }else if(length(unique(di$chemicalTreat)) == 1){
    
    cat(paste0(" > Only one chemical. treat.\n"))
    treat.tokeep <- unique(di$chemicalTreat)
    ids[i, 'chem.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'chem.check'] <- TRUE
    
  }else{
    stop("Missing conditions (line 426)") # check
  }
  
  # Summary
  if(any(!ids$chem.check, na.rm = TRUE)){stop()} # check
  nrow.before <- nrow(di)
  di <- di[di$chemicalTreat %in% ids[i, 'chem.tokeep'], ]
  if(length(unique(di$chemicalTreat)) > 1){stop()} # check
  nrow.after <- nrow(di)
  if(nrow.before!=nrow.after){
    message(paste0('   => Loosing ', nrow.before-nrow.after,' rows (out of ',nrow.before,')'))
  }
  
  # SOAKING (update in May 2025: keep control) (reupdated in July 2025: we now keep the max. germ. + take into account duration)
  ids[i, 'soak.check']  <- FALSE
  di$soakedTreat <- paste0(di$soaked.in, di$soaking.duration)
  if(length(unique(di$soakedTreat)) > 1){
    
    cat(paste0(" > Number of soak. treat.: ", length(unique(di$soakedTreat)), "\n"))
    cat('   - [');cat(paste(unique(di$soakedTreat)), sep = ', ');cat(']\n')
    
    di$nstrat <- paste0(di$stratTemp_condensed, di$stratDur_condensed)
    di$ngerm <- paste0(di$germTempGen, di$germDuration)
    ntreati <- merge(
      aggregate(nstrat ~ factor(soakedTreat, exclude = NULL), data = di, function(x) length(unique(x))),
      aggregate(ngerm ~ factor(soakedTreat, exclude = NULL), data = di, function(x) length(unique(x)))
    )
    ntreati$ninterest <- ntreati$nstrat + ntreati$ngerm
    names(ntreati)[1] <- 'soakedTreat'
    
    if(length(unique(ntreati$ninterest)) == 1){ # same no. of chill/forc treatments
      
      cat('   - Same number of strat./forc. treat. across chem. treat.\n')
      
      # we can just keep the chemical treat. with max. germ. rate?
      maxresp <- max(di$responseValueNum)
      treat.tokeep <- unique(di[di$responseValueNum == maxresp, 'soakedTreat']) 
      cat(paste0('   - Keeping: [', treat.tokeep,'] (max. resp.)\n'))
      
      if(length(treat.tokeep) > 1){
        stop("Boulbiboulba") # check
      }
      
    }else if(length(unique(ntreati$ninterest)) > 1){ # different no. of chill/forc treatments
      
      cat('   - Different number of strat./forc. treat. across chem. treat.\n')
      treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'soakedTreat'] # chem. treat. with max. no. of chill/forc treatments
      
      if(length(treat.tokeep) > 1){ 
        
        maxresp <- max(di[di$soakedTreat %in% treat.tokeep, 'responseValueNum'])
        treat.tokeep <- unique(di[di$soakedTreat %in% treat.tokeep & di$responseValueNum == maxresp, 'soakedTreat']) 
        cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + max. resp.)\n'))
        
        if(length(treat.tokeep) > 1){
          stop("Boulbiboulba2") # check
        }
        
      }else if(length(treat.tokeep) == 1){
        cat(paste0('  - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat.)\n'))
      }
    }
    
    ids[i, 'soak.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'soak.check'] <- TRUE
    
  }else if(length(unique(di$soakedTreat)) == 1){
    cat(paste0(" > Only one soak. treat.\n"))
    treat.tokeep <- unique(di$soakedTreat)
    ids[i, 'soak.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'soak.check'] <- TRUE
  }else{
    stop("Boulbiboulba2") # check
  }
  
  # Summary
  if(any(!ids$soak.check, na.rm = TRUE)){stop()} # check
  nrow.before <- nrow(di)
  di <- di[di$soakedTreat %in% ids[i, 'soak.tokeep'], ]
  if(length(unique(di$soakedTreat)) > 1){stop()} # check
  nrow.after <- nrow(di)
  if(nrow.before!=nrow.after){
    message(paste0('   => Loosing ', nrow.before-nrow.after,' rows (out of ',nrow.before,')'))
  }
  
  # STORAGE
  ids[i, 'stor.check']  <- FALSE
  di$storConditions <- paste(di$storageType, di$storageNoStratTemp, di$storageNoStratDur)
  if(length(unique(di$storConditions)) > 1){
    
    cat(paste0(" > Number of stor. treat.: ", length(unique(di$storConditions)), "\n"))
    cat('   - [');cat(paste(unique(di$storConditions)), sep = ', ');cat(']\n')
    
    di$nstrat <- paste0(di$stratTemp_condensed, di$stratDur_condensed)
    di$ngerm <- paste0(di$germTempGen, di$germDuration)
    ntreati <- merge(
      aggregate(nstrat ~ factor(storConditions, exclude = NULL), data = di, function(x) length(unique(x))),
      aggregate(ngerm ~ factor(storConditions, exclude = NULL), data = di, function(x) length(unique(x)))
    )
    ntreati$ninterest <- ntreati$nstrat + ntreati$ngerm
    names(ntreati)[1] <- 'storConditions'
    
    # we keep misc. with max. no. of chill/forc treatments
    treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'storConditions']
    
    if(length(treat.tokeep) == 1){
      
      cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat.)\n'))
      
    }else if(length(treat.tokeep) > 1){ # if not enough...
      
      maxresp <- max(di[di$storConditions %in% treat.tokeep, 'responseValueNum'], na.rm = TRUE)
      treat.tokeep <- unique(di[di$storConditions %in% treat.tokeep & di$responseValueNum %in% maxresp, 'storConditions']) 
      
      if(length(treat.tokeep) > 1){
        
        avgrespi <- aggregate(responseValueNum ~ factor(storConditions, exclude = NULL), data = di[di$storConditions %in% treat.tokeep,], mean)
        names(avgrespi)[1] <- 'storConditions'
        
        treat.tokeep <- avgrespi[avgrespi$responseValueNum == max(avgrespi$responseValueNum), 'storConditions']
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + max. resp. + max. avg. resp.)\n'))
        
        if(length(treat.tokeep) > 1){
          stop("Missing conditions (line 627)") # check
        }
        
      }else if(length(treat.tokeep) == 1){
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + max. resp.)\n'))
      }
      
    }else{
      stop("Missing conditions (line 633)") # check
    }
    
    ids[i, 'stor.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'stor.check'] <- TRUE
    
    
  }else if(length(unique(di$storConditions)) == 1){
    
    cat(paste0(" > Only one stor. treat.\n"))
    treat.tokeep <- unique(di$storConditions)
    ids[i, 'stor.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'stor.check'] <- TRUE
    
  }else{
    stop("Missing conditions (line 648)") # check
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
    
    di$nstrat <- paste0(di$stratTemp_condensed, di$stratDur_condensed)
    di$ngerm <- paste0(di$germTempGen, di$germDuration)
    ntreati <- merge(
      aggregate(nstrat ~ factor(germPhotoperiod, exclude = NULL), data = di, function(x) length(unique(x))),
      aggregate(ngerm ~ factor(germPhotoperiod, exclude = NULL), data = di, function(x) length(unique(x)))
    )
    ntreati$ninterest <- ntreati$nstrat + ntreati$ngerm
    names(ntreati)[1] <- 'germPhotoperiod'
    
    # we keep misc. with max. no. of chill/forc treatments
    treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'germPhotoperiod']
    
    if(length(treat.tokeep) == 1){
      
      cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of chill./forc. treat.)\n'))
      
    }else if(length(treat.tokeep) > 1){ # if not enough...
      
      maxresp <- max(di[di$germPhotoperiod %in% treat.tokeep, 'responseValueNum'], na.rm = TRUE)
      treat.tokeep <- unique(di[di$germPhotoperiod %in% treat.tokeep & di$responseValueNum %in% maxresp, 'germPhotoperiod']) 
      
      if(length(treat.tokeep) > 1){
        
        avgrespi <- aggregate(responseValueNum ~ factor(germPhotoperiod, exclude = NULL), data = di[di$germPhotoperiod %in% treat.tokeep,], mean)
        names(avgrespi)[1] <- 'germPhotoperiod'
        
        treat.tokeep <- avgrespi[avgrespi$responseValueNum == max(avgrespi$responseValueNum), 'germPhotoperiod']
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + max. resp. + max. avg. resp.)\n'))
        
        if(length(treat.tokeep) > 1){
          stop("Missing conditions") # check
        }
        
      }else if(length(treat.tokeep) == 1){
        
        cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. + max. resp.)\n'))
        
      }else{
        stop("Missing conditions (line 706)") # check
      }
      
    }else{
      stop("Missing conditions (line 710)") # check
    }
    
    ids[i, 'photo.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'photo.check'] <- TRUE
    
    
  }else if(length(unique(di$germPhotoperiod)) == 1){
    
    cat(paste0(" > Only one photo. treat.\n"))
    treat.tokeep <- unique(di$germPhotoperiod)
    ids[i, 'photo.tokeep'] <- as.character(treat.tokeep)
    ids[i, 'photo.check'] <- TRUE
    
  }else{
    stop("Missing conditions (line 725)") # check
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
  
  # And now: WARM STRAT!
  ids[i, 'warmstrat.check']  <- FALSE
  # first, is there some warm. strat.?
  if(any(grepl('warm', di$stratSequence_condensed))){
    
    # We want to consider only warm. strat. treatments in the process
    di$warmStratConditions <- unlist(lapply(1:nrow(di), function(i){
      temp <- stringr::str_split(di$stratTemp_condensed[i], " then ")
      dur <- stringr::str_split(di$stratDur_condensed[i], " then ")
      warmstrat <- grepl('warm', stringr::str_split(di$stratSequence_condensed[i], " then ")[[1]])
      
      if(length(temp[[1]][!warmstrat]) > 0){
        temp[[1]][!warmstrat] <- rep('chill', length(temp[[1]][!warmstrat])) 
        dur[[1]][!warmstrat] <- rep('chill', length(dur[[1]][!warmstrat])) 
      }
      return(paste(paste0(temp[[1]], collapse =  ' then '), paste0(dur[[1]], collapse =  ' then ')))
    }))
    
    cat(paste0(" > Number of warm. strat. treat.: ", length(unique(di$warmStratConditions)), "\n"))
    cat('   - [');cat(paste(unique(di$warmStratConditions)), sep = ', ');cat(']\n')
    
    if(length(unique(di$warmStratConditions)) == 1){
      
      cat(paste0(" > Only one warm. strat. treat.\n"))
      treat.tokeep <- unique(di$warmStratConditions)
      ids[i, 'warmstrat.tokeep'] <- as.character(treat.tokeep)
      ids[i, 'warmstrat.check'] <- TRUE
      
    }else if(length(unique(di$warmStratConditions)) > 1){
      
      di$nstrat <- paste0(di$stratTemp_condensed, di$stratDur_condensed)
      di$ngerm <- paste0(di$germTempGen, di$germDuration)
      ntreati <- merge(
        aggregate(nstrat ~ factor(warmStratConditions, exclude = NULL), data = di, function(x) length(unique(x))),
        aggregate(ngerm ~ factor(warmStratConditions, exclude = NULL), data = di, function(x) length(unique(x)))
      )
      ntreati$ninterest <- ntreati$nstrat + ntreati$ngerm
      names(ntreati)[1] <- 'warmStratConditions'
      
      # we keep misc. with max. no. of forc. treatments
      treat.tokeep <- ntreati[ntreati$ninterest == max(ntreati$ninterest),'warmStratConditions']
      
      if(length(treat.tokeep) == 1){
        
        if(sum(grepl('chill', stringr::str_split(treat.tokeep, ' ')[[1]])) == length(stringr::str_split(treat.tokeep, ' ')[[1]])){
          treat.tokeep <- NA
          ids[i, 'warmstrat.tokeep'] <- NA
          ids[i, 'warmstrat.check'] <- TRUE
          cat(paste0('   - Discarding all warm. strat. treatments (max. no. of chill./forc. treat.)\n'))
          
        }else{
          cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat.)\n'))
          ids[i, 'warmstrat.tokeep'] <- as.character(treat.tokeep)
          ids[i, 'warmstrat.check'] <- TRUE
        }
        
        
      }else if(length(treat.tokeep) > 1){
        
        maxresp <- max(di[di$warmStratConditions %in% treat.tokeep, 'responseValueNum'], na.rm = TRUE)
        treat.tokeep <- unique(di[di$warmStratConditions %in% treat.tokeep & di$responseValueNum %in% maxresp, 'warmStratConditions']) 
        
        if(length(treat.tokeep) == 1){
          
          if(sum(grepl('chill', stringr::str_split(treat.tokeep, ' ')[[1]])) == length(stringr::str_split(treat.tokeep, ' ')[[1]])){
            treat.tokeep <- NA
            ids[i, 'warmstrat.tokeep'] <- NA
            ids[i, 'warmstrat.check'] <- TRUE
            cat(paste0('   - Discarding all warm. strat. treatments (max. no. of chill./forc. treat. +  max. resp.)\n'))
            
          }else{
            cat(paste0('   - Keeping: [', treat.tokeep,'] (max. no. of strat./forc. treat. +  max. resp.)\n'))
            ids[i, 'warmstrat.tokeep'] <- as.character(treat.tokeep)
            ids[i, 'warmstrat.check'] <- TRUE
          }
          
          
        }else if(length(treat.tokeep) > 1){
          
          stop('Missing conditions line 745')
          
        }
      }
      
    }else{
      
      
      stop('Missing conditions line 753')
      
    }
  
    
  }else if(all(!grepl('warm', di$stratSequence_condensed))){
    
    cat(paste0(" > No warm. strat. treat.\n"))
    ids[i, 'warmstrat.tokeep'] <- NA
    ids[i, 'warmstrat.check'] <- TRUE
    
  }else{
    stop("Missing conditions (line 765)") # check
  }
  
}

# Create the new dataset
# Commented for now, so people checking the loop iterations don't panick - 26 July
newd <- data.frame()
for(i in 1:nrow(ids)){
  
  di <- filteredd[paste0(filteredd$datasetID,filteredd$study,filteredd$genusspecies) == paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
  di <- di[di$other.treatment %in% ids[i, 'misc.tokeep'], ]
  di <- di[di$scarifTypeSpe %in% ids[i, 'scarif.tokeep'], ]
  di <- di[paste0(di$chemicalCor, di$chemicalConcent) %in% ids[i, 'chem.tokeep'], ]
  di <- di[paste0(di$soaked.in, di$soaking.duration) %in% ids[i, 'soak.tokeep'], ]
  
  di$storConditions <- paste(di$storageType, di$storageNoStratTemp, di$storageNoStratDur)
  di <- di[di$storConditions %in% ids[i, 'stor.tokeep'], ]
  di <- di[di$germPhotoperiod %in% ids[i, 'photo.tokeep'], ]
  
  if(is.na(ids[i, 'warmstrat.tokeep'])){
    
    if(all(grepl('warm', di$stratSequence_condensed))){
      stop('Something went wrong')
    }
    
    di <- di[!grepl('warm', di$stratSequence_condensed), ]
    
  }else{
    
    # We recreate this column (temporarily)
    di$warmStratConditions <- unlist(lapply(1:nrow(di), function(i){
      temp <- stringr::str_split(di$stratTemp_condensed[i], " then ")
      dur <- stringr::str_split(di$stratDur_condensed[i], " then ")
      warmstrat <- grepl('warm', stringr::str_split(di$stratSequence_condensed[i], " then ")[[1]])
      
      if(length(temp[[1]][!warmstrat]) > 0){
        temp[[1]][!warmstrat] <- rep('chill', length(temp[[1]][!warmstrat])) 
        dur[[1]][!warmstrat] <- rep('chill', length(dur[[1]][!warmstrat])) 
      }
      return(paste(paste0(temp[[1]], collapse =  ' then '), paste0(dur[[1]], collapse =  ' then ')))
    }))
    
    di <- di[di$warmStratConditions %in% ids[i, 'warmstrat.tokeep'], ]
    di$warmStratConditions <- NULL # and then we remove the column
    
  }

  newd <- rbind(newd, di)
  
}

rm(list=ls()[which(!(ls() %in% c('originald','d', 'newd', 'oldids')))])
newids <- unique(newd[c('datasetID', 'study', 'genusspecies')])

countprov <- aggregate(provLatLonAlt ~ datasetID + study + genusspecies, data = newd, FUN = function(x) length(unique(x)))
countprov <- countprov[countprov$provLatLonAlt > 1,]
newd <- newd[paste0(newd$datasetID, newd$study, newd$genusspecies) %in% paste0(countprov$datasetID, countprov$study, countprov$genusspecies),]
