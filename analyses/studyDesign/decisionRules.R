

particularStudies <- unitreat[unitreat$nscarif > 1 | unitreat$nchem > 1, c('datasetID', 'study', 'genusspecies')]

particulard <- d %>%
  dplyr::filter(paste0(datasetID,study,genusspecies) %in% paste0(particularStudies$datasetID,particularStudies$study,particularStudies$genusspecies))

singularities <- particulard %>%
  dplyr::group_by(datasetID, study, genusspecies) %>%
  dplyr::reframe(nstorage = n_distinct(storageType, storageTemp, storageDuration, storageDetails),
                 nscarif = n_distinct(scarifType, scarifTypeGen, scarifTypeSpe),
                 nchem = n_distinct(chemicalCor),
                 nstrat = n_distinct(chillTemp, chillDuration, chillTempUnc, chillTempCycle, chillLightCycle),
                 ngerm = n_distinct(germTempGen, germDuration)) %>%
  as.data.frame()

ids <- singularities[c('datasetID', 'study', 'genusspecies')]


for(i in 1:nrow(ids)){
  
  ni <- singularities[paste0(singularities$datasetID,singularities$study,singularities$genusspecies) == paste0(ids[i,], collapse = ''),]
  di <- particulard[paste0(particulard$datasetID,particulard$study,particulard$genusspecies) == paste0(ids[i,], collapse = ''),]
  
  
  
}