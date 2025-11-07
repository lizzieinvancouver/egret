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



subsetd <- filteredd[filteredd$datasetID %in% unique(filteredd$datasetID)[1:3],]


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
  
  uniq_treats <- unique(di[,treats])
  cat(paste0(nrow(di), ' lines of data, ',  nrow(uniq_treats), ' unique treats.\n'))
  
  di$temp <- sapply(1:nrow(di), function(x) paste0(di[x,treats], collapse =''))
  
  
  for(t in 1:nrow(uniq_treats)){
    
    treat <-  paste0(uniq_treats[t,], collapse ='')
    di_tr <- di[ di$temp == treat,]
    
    if(nrow(di_tr) == 1){
      resp_di <- data.frame(id = i, treat, dur = di_tr$germDuration, resp = di_tr$responseValueNum)
    }else if(nrow(di_tr) > 1){
      di_tr <- di_tr[di_tr$responseValueNum %in% range(di_tr$responseValueNum),]
      resp_di <- data.frame(id = i, treat, dur = di_tr$germDuration, resp = di_tr$responseValueNum)
    }else{
      stop()
    }
    
    treat_responses <- rbind(treat_responses, resp_di)
  }
}

for(i in 1:nrow(idxs)){
  
  resp_i <- treat_responses[treat_responses$id == i,]
  
  max_resps_t <- merge(aggregate(resp ~ treat, data = resp_i, FUN = max), resp_i)
  min_resps_t <- merge(aggregate(resp ~ treat, data = resp_i, FUN = min), resp_i)
  
  max_resp <- max_resps_t[max_resps_t$resp == max(max_resps_t$resp),]
  min_resp <- min_resps_t[min_resps_t$resp == min(min_resps_t$resp),]
  
}

