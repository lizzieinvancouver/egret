# Started Feb 10, 2025
# Aim: selecting some treatments/experiments to include in the model, following various decision rules
# by Victor

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
  setwd("/Users/christophe_rouleau-desrochers/Documents/github/egret/analyses")
} else if(length(grep("victor", getwd())) > 0){
  setwd('~/projects/egret/analyses')
} 

# 0. Get the cleaned data
source('cleaning/cleanAll.R')
originald <- d
source('analyseSeedCues/summarizeStrat.R')
#d <- read.csv('output/egretclean.csv')
rm(list=ls()[which(!(ls() %in% c('originald', 'd', 'idsnotcorrected')))])

coltokeep <- c('datasetID', 'study',
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
dtreat <- unique(d[,coltokeep])
dtreat$uniqueID <- 1:nrow(dtreat)
d <- unique(d[, c(coltokeep, resp)])
d <- merge(x = d, y = dtreat, by = coltokeep, all = TRUE)
d$genusspecies <- paste0(d$genus, '_', d$species)

# Save initial number of unique dataset*experiment*species
oldids <- unique(d[c('datasetID', 'study', 'genusspecies')])

# First veto: response variable
priority <- c("percent.germ") # here, just in case, we could set several responses variables we want to prioritize
d <- d[order(d$uniqueID, match(d$responseVar, priority)), ]
d <- d[!duplicated(d$uniqueID), ]
filteredd <- d[d$responseVar == 'percent.germ',]
filteredd$responseValueNum <- as.numeric(filteredd$responseValueNum)
filteredd <- filteredd[!(filteredd$responseValueNum %in% c(NA, 'NA')),]

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

# subsetd <- filteredd[filteredd$datasetID %in% unique(filteredd$datasetID)[1:30],]
subsetd <- filteredd

treats <- c(
            # GEO. OR GENETIC-RELATED
            'variety', 'provLatLonAlt', # seems useful
            
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
            "germTempGen", "germTemp", "germTempClass", "germTempDay", "germTempNight",
            "germPhotoperiod", "germPhotoperiodDay", "germPhotoperiodNight", # "photoperiodCor",
            
            # MISC (e.g. sowing depth)
            "other.treatment"
            
)

idxs <- unique(subsetd[c('datasetID', 'study', 'genusspecies')])
idxs$id <- 1:nrow(idxs)

treat_responses <- data.frame()
for(i in 1:nrow(idxs)){
  di <- subsetd[paste0(subsetd$datasetID,subsetd$study,subsetd$genusspecies) == 
                  paste0(idxs[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
  
  treats_di <- di[,treats]
  coltokeep <- which(sapply(treats_di, function(x) return(length(unique(x)) != 1)))
  name_diff_treats <- names(which(sapply(treats_di, function(x) return(length(unique(x)) != 1))))
  diff_treats_di <- as.data.frame(treats_di[,coltokeep])
  names(diff_treats_di) <- name_diff_treats
  
  uniq_treats <- unique(diff_treats_di)
  cat(paste0(nrow(di), ' lines of data, ',  nrow(uniq_treats), ' unique treats.\n'))
  
  
  
  di$temp <- sapply(1:nrow(di), function(x) paste0(di[x,name_diff_treats], collapse ='|'))
  name_diff_treats <- paste0(name_diff_treats, collapse ='|')
  
  for(t in 1:nrow(uniq_treats)){
    
    treat <-  paste0(uniq_treats[t,], collapse ='|')
    di_tr <- di[ di$temp == treat,]
    
    if(nrow(di_tr) == 1){
      resp_di <- data.frame(id = i,  name_treat = name_diff_treats, treat, dur = di_tr$germDuration, resp = di_tr$responseValueNum)
    }else if(nrow(di_tr) > 1){
      di_tr <- di_tr[di_tr$responseValueNum %in% range(di_tr$responseValueNum),] # keep min and max
      resp_di <- data.frame(id = i,  name_treat = name_diff_treats, treat, dur = di_tr$germDuration, resp = di_tr$responseValueNum)
    }else{
      stop()
    }
    
    treat_responses <- rbind(treat_responses, resp_di)
  }
}

biggest_effect <- data.frame()
for(i in 1:nrow(idxs)){
  
  resp_i <- treat_responses[treat_responses$id == i,]
  
  max_resps_t <- aggregate(resp ~ treat, data = resp_i, FUN = max)
  max_resp <- max_resps_t[max_resps_t$resp == max(max_resps_t$resp),] # no duration
  max_resp_wdur <- merge(max_resp, resp_i[c('treat', 'resp', 'dur')]) # merge to get duration back
  
  if(nrow(max_resp_wdur) > nrow(max_resp)){
    max_resp_wdur <- aggregate(dur ~ treat + resp, data = max_resp_wdur, FUN = min) # if plateau, keep the first 
    # time when max. is obtained
  }
  
  max_resp <- max_resp_wdur
  
  
  min_resps_t <- aggregate(resp ~ treat, data = resp_i[resp_i$dur %in% max_resp$dur,], FUN = min)
  min_resp <- min_resps_t[min_resps_t$resp == min(min_resps_t$resp),]

  if(nrow(min_resp) == 0 & length(unique(resp_i$treat)) > 1){
    min_resp_diffdur <- min_resps_t[min_resps_t$resp == min(min_resps_t$resp),]
  }

  # biggest_effect_i <- data.frame(
  #   id = i,
  #   treat_collapsed = max_resp$treat,
  #   response = max_resp$resp,
  #   minresp = ifelse(nrow(min_resp) > 0, min(min_resp$resp), NA),
  #   dur = max_resp$dur
  # )
  
  biggest_effect_i <- data.frame(
    id = i,
    n_treats = length(unique(resp_i$treat)),
    n_max = nrow(max_resp),
    max_resp = ifelse(nrow(max_resp) == 1, max_resp$resp, unique(max_resp$resp)),
    n_min = nrow(min_resp),
    min_resp = ifelse(nrow(min_resp) == 1, min_resp$resp, unique(min_resp$resp))
  )

  biggest_effect <- rbind(biggest_effect, biggest_effect_i)
  
}


# What if we consider 'any scarification'?

treat_responses <- data.frame()
for(i in 1:nrow(idxs)){
  di <- subsetd[paste0(subsetd$datasetID,subsetd$study,subsetd$genusspecies) == 
                  paste0(idxs[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
  
  # we discard scarifTypeSpe, and consider all scarification treatments identical
  di$scarifTypeSpe <- NA
  di$scarifTypeGen <- ifelse(!is.na(di$scarifTypeGen), 'notNA', NA)
  
  treats_di <- di[,treats]
  coltokeep <- which(sapply(treats_di, function(x) return(length(unique(x)) != 1)))
  name_diff_treats <- names(which(sapply(treats_di, function(x) return(length(unique(x)) != 1))))
  diff_treats_di <- as.data.frame(treats_di[,coltokeep])
  names(diff_treats_di) <- name_diff_treats
  
  uniq_treats <- unique(diff_treats_di)
  cat(paste0(nrow(di), ' lines of data, ',  nrow(uniq_treats), ' unique treats.\n'))
  
  di$temp <- sapply(1:nrow(di), function(x) paste0(di[x,name_diff_treats], collapse ='|'))
  name_diff_treats <- paste0(name_diff_treats, collapse ='|')
  
  for(t in 1:nrow(uniq_treats)){
    
    treat <-  paste0(uniq_treats[t,], collapse ='|')
    di_tr <- di[ di$temp == treat,]
    
    if(nrow(di_tr) == 1){
      resp_di <- data.frame(id = i,  name_treat = name_diff_treats, treat, dur = di_tr$germDuration, resp = di_tr$responseValueNum)
    }else if(nrow(di_tr) > 1){
      di_tr <- di_tr[di_tr$responseValueNum %in% range(di_tr$responseValueNum),] # keep min and max
      resp_di <- data.frame(id = i,  name_treat = name_diff_treats, treat, dur = di_tr$germDuration, resp = di_tr$responseValueNum)
    }else{
      stop()
    }
    
    treat_responses <- rbind(treat_responses, resp_di)
  }
}

biggest_effect <- data.frame()
for(i in 1:nrow(idxs)){
  
  resp_i <- treat_responses[treat_responses$id == i,]
  
  max_resps_t <- aggregate(resp ~ treat, data = resp_i, FUN = max)
  max_resp <- max_resps_t[max_resps_t$resp == max(max_resps_t$resp),] # no duration
  max_resp_wdur <- merge(max_resp, resp_i[c('treat', 'resp', 'dur')]) # merge to get duration back
  
  if(nrow(max_resp_wdur) > nrow(max_resp)){
    if(all(is.na(max_resp_wdur$dur))){
      max_resp_wdur <- max_resp_wdur[1,]
      next
    }
    max_resp_wdur <- aggregate(dur ~ treat + resp, data = max_resp_wdur, FUN = min) # if plateau, keep the first 
    # time when max. is obtained
  }
  
  max_resp <- max_resp_wdur
  
  
  min_resps_t <- aggregate(resp ~ treat, data = resp_i[resp_i$dur %in% max_resp$dur,], FUN = min)
  min_resp <- min_resps_t[min_resps_t$resp == min(min_resps_t$resp),]
  
  if(nrow(min_resp) == 0 & length(unique(resp_i$treat)) > 1){
    min_resp_diffdur <- min_resps_t[min_resps_t$resp == min(min_resps_t$resp),]
  }
  
  # biggest_effect_i <- data.frame(
  #   id = i,
  #   treat_collapsed = max_resp$treat,
  #   response = max_resp$resp,
  #   minresp = ifelse(nrow(min_resp) > 0, min(min_resp$resp), NA),
  #   dur = max_resp$dur
  # )
  
  biggest_effect_i <- data.frame(
    id = i,
    n_treats = length(unique(resp_i$treat)),
    n_max = nrow(max_resp),
    max_resp = ifelse(nrow(max_resp) == 1, max_resp$resp, unique(max_resp$resp)),
    n_min = nrow(min_resp),
    min_resp = ifelse(nrow(min_resp) == 1, min_resp$resp, unique(min_resp$resp))
  )
  
  biggest_effect <- rbind(biggest_effect, biggest_effect_i)
  
}




