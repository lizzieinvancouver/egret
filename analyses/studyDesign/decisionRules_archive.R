
# Create the new dataset
# Commented for now, so people checking the loop iterations don't panick - 26 July 
# newd <- data.frame()
# for(i in 1:nrow(ids)){
#   
#   di <- filteredd[paste0(filteredd$datasetID,filteredd$study,filteredd$genusspecies) == paste0(ids[i,c('datasetID', 'study', 'genusspecies')], collapse = ''),]
#   di <- di[di$other.treatment %in% ids[i, 'misc.tokeep'], ]
#   di <- di[di$scarifType %in% ids[i, 'scarif.tokeep'], ]
#   di <- di[di$chemicalCor %in% ids[i, 'chem.tokeep'], ]
#   
#   # some storage conditions may correspond to chilling (moist + cold)
#   for(s in 1:nrow(di)){
#     # between -20 and 10 => we consider these as chilling, not storage
#     if(di[s, 'storageType']  %in% c("moist", "moist/cold") & !is.na(as.numeric(di[s, 'storageTemp'])) &
#        as.numeric(di[s, 'storageTemp']) <= 10 & as.numeric(di[s, 'storageTemp']) >= - 20){
#       di[s, 'storageType'] <- di[s, 'storageTemp'] <- di[s, 'storageDuration'] <- NA
#     }
#   }
#   
#   di$storConditions <- paste(di$storageType, di$storageTemp, di$storageDuration)
#   di <- di[di$storConditions %in% ids[i, 'stor.tokeep'], ]
#   di <- di[di$germPhotoperiod %in% ids[i, 'photo.tokeep'], ]
#   newd <- rbind(newd, di)
#   
# }

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

# rm(list=ls()[which(!(ls() %in% c('d', 'newd')))]) 
